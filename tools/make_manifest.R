#!/usr/bin/env Rscript
# ============================================================================
# Generate docs/pipeline_manifest.md, a static map of the pipeline in code/
#
# Reads every code/*.R without running it (code/archive/ is skipped) and
# writes one markdown file saying, for each script, which files it reads and
# writes, in which folders, which scripts are on the other end, and what has
# to be rerun after changing it, plus a list of checks.
#
# Which file names a script reads and writes comes from analyze_file() in
# tools/check_static.R, loaded here WITHOUT running the checker, so the two
# tools always agree on file names. The manifest adds two things the checker
# leaves out on purpose:
#   - run order: a script reads the copy of a file written by the last
#     script before it in name order, which exposes in-place rewrites and
#     gives true rerun lists;
#   - folders: each path is resolved to a real folder using the PATH_
#     definitions in the script that writes paths.Rdata, so a script reading
#     a file from a different folder than the one it was written to is caught.
#
# Usage, from the repository root:
#   Rscript tools/make_manifest.R           # rewrite docs/pipeline_manifest.md
#   Rscript tools/make_manifest.R --check   # exit 1 if the manifest is stale
#
# Never edit docs/pipeline_manifest.md by hand: change the scripts, rerun this.
# ============================================================================

# Libraries ===================================================================

# Base R only (utils::getParseData, utils::adist).

# Inputs ======================================================================

CODE_DIR      <- "code"
ARCHIVE_DIR   <- file.path(CODE_DIR, "archive")
CHECKER_FILE  <- file.path("tools", "check_static.R")
MANIFEST_FILE <- file.path("docs", "pipeline_manifest.md")

CHECK_ONLY <- "--check" %in% commandArgs(trailingOnly = TRUE)

if (!dir.exists(CODE_DIR) || !file.exists(CHECKER_FILE)) {
  stop("Run this from the repository root (it needs code/ and ",
       CHECKER_FILE, ").", call. = FALSE)
}

# Constants & Variables =======================================================

PIPELINE_RE <- "^[0-9]{2}[a-z]?_.+\\.R$"  # numbered scripts, run in name order
PARKED_RE   <- "^XX_.+\\.R$"              # parked: checked for links only
HELPER_RE   <- "^_.+\\.R$"                # sourced helpers such as _source.R
SHARED_FILE <- "_source.R"                # defines the shared constants
PATHS_FILE  <- "paths.Rdata"              # holds the PATH_ folders
BASE_FOLDER <- "PATH_SAVE"                # per-machine root, _machine_paths.R

RANDOM_FUNS <- c("sample", "sample.int", "runif", "rnorm", "rbinom", "rpois",
                 "rexp", "rgamma", "rbeta", "rlnorm", "rmultinom", "rweibull",
                 "rnbinom", "rgeom", "rhyper", "rcauchy", "rlogis", "rt",
                 "rchisq")
PARALLEL_FUNS <- c("mclapply", "mcmapply", "mcMap", "pvec", "parLapply",
                   "parSapply", "parApply", "clusterApply", "clusterMap",
                   "makeCluster", "registerDoParallel", "foreach",
                   "future_lapply", "future_map")
NETWORK_FUNS <- c("download.file", "curl_download", "GET", "POST",
                  "download_data")
FIGURE_FUNS  <- c("ggsave", "png", "pdf", "jpeg", "tiff", "svg", "cairo_pdf")
LINKED       <- c("ok", "inplace", "order")  # read statuses that form a link
SCALAR_WIDTH <- 30   # show a constant's value only for short single values
CAPS_RE      <- "^[A-Z][A-Z0-9_]{2,}$"

# Functions ===================================================================

# Load the checker's functions and UPPER_CASE settings without running its
# main part: only top-level assignments of functions or CONSTANTS are
# evaluated, so nothing is printed, written or exited.
load_checker <- function(path) {
  env <- new.env(parent = globalenv())
  exprs <- parse(path, keep.source = FALSE)
  for (i in seq_along(exprs)) {
    e <- exprs[[i]]
    if (!is.call(e) || !is.symbol(e[[1]]) ||
        !(as.character(e[[1]]) %in% c("<-", "=")) || !is.symbol(e[[2]])) next
    rhs <- e[[3]]
    is_fun <- is.call(rhs) && is.symbol(rhs[[1]]) &&
      identical(as.character(rhs[[1]]), "function")
    if (is_fun || grepl("^[A-Z][A-Z0-9_]*$", as.character(e[[2]]))) {
      eval(e, envir = env)
    }
  }
  needed <- c("analyze_file", "walk_tree", "get_call_name", "expr_symbols",
              "deparse_short", "arg_is_missing", "get_named_or_positional",
              "literal_value_of", "build_literal_table", "basename_of",
              "READ_FUNCS", "WRITE_FUNCS", "EXTERNAL_RAW_BASENAMES")
  gone <- needed[!vapply(needed, exists, logical(1), envir = env,
                         inherits = FALSE)]
  if (length(gone)) {
    stop(path, " no longer defines: ", paste(gone, collapse = ", "),
         ". Update tools/make_manifest.R to match.", call. = FALSE)
  }
  env
}

read_lines_lf <- function(path) {
  sub("\r$", "", readLines(path, warn = FALSE, encoding = "UTF-8"))
}

# First sentence of the opening comment block, skipping rules and headings.
header_title <- function(lines) {
  i <- 1L
  while (i <= length(lines) && !nzchar(trimws(lines[i]))) i <- i + 1L
  block <- character(0)
  while (i <= length(lines) && grepl("^\\s*#", lines[i])) {
    block <- c(block, lines[i])
    i <- i + 1L
  }
  txt <- trimws(sub("^\\s*#+'?", "", block))
  txt[grepl("^[-=#*_~ ]+$", txt) | startsWith(txt, "!") |
        grepl("====*\\s*$", txt)] <- ""
  start <- which(nzchar(txt))[1]
  if (is.na(start)) return("")
  end <- start
  while (end < length(txt) && nzchar(txt[end + 1L])) end <- end + 1L
  para <- paste(txt[start:end], collapse = " ")
  cut <- regexpr("\\.\\s", para)
  if (cut > 0) para <- substr(para, 1, cut)
  para <- sub("\\.$", "", gsub("\\|", "/", para))
  if (nchar(para) > 120) {
    para <- paste0(sub("\\s+\\S*$", "", substr(para, 1, 117)), "...")
  }
  para
}

# Resolve an expression to a path string using a name -> path map, or NA.
canon <- function(e, map) {
  if (is.character(e) && length(e) == 1) return(e)
  if (is.symbol(e)) {
    v <- map[[as.character(e)]]
    return(if (is.null(v)) NA_character_ else v)
  }
  nm <- if (is.call(e)) ck$get_call_name(e) else NA_character_
  if (is.na(nm) || !(nm %in% c("file.path", "paste0"))) return(NA_character_)
  args <- as.list(e)[-1]
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  parts <- character(0)
  for (k in seq_along(args)) {
    if (ck$arg_is_missing(args, k) || nzchar(nms[k])) next
    p <- canon(args[[k]], map)
    if (is.na(p)) return(NA_character_)
    parts <- c(parts, p)
  }
  if (!length(parts)) return(NA_character_)
  if (nm == "paste0") return(paste(parts, collapse = ""))
  gsub("/+", "/", paste(parts, collapse = "/"))
}

# Folder of a file-path expression; the file name itself may stay unknown.
canon_dir <- function(e, map) {
  nm <- if (is.call(e)) ck$get_call_name(e) else NA_character_
  if (identical(nm, "file.path")) {
    args <- as.list(e)[-1]
    nms <- names(args)
    if (is.null(nms)) nms <- rep("", length(args))
    idx <- which(!nzchar(nms))
    idx <- idx[!vapply(idx, function(k) ck$arg_is_missing(args, k), logical(1))]
    if (!length(idx)) return(NA_character_)
    head <- character(0)
    for (k in idx[-length(idx)]) {
      p <- canon(args[[k]], map)
      if (is.na(p)) return(NA_character_)
      head <- c(head, p)
    }
    last <- canon(args[[idx[length(idx)]]], map)
    if (!is.na(last) && grepl("/", last)) head <- c(head, dirname(last))
    if (!length(head)) return(NA_character_)
    return(gsub("/+", "/", paste(head, collapse = "/")))
  }
  full <- canon(e, map)
  if (!is.na(full) && grepl("/", full)) dirname(full) else NA_character_
}

# Top-level name -> path assignments of a parsed script, added to `map`.
add_path_assignments <- function(parsed, map, only_re = NULL) {
  for (i in seq_along(parsed)) {
    e <- parsed[[i]]
    if (!is.call(e) || !is.symbol(e[[1]]) ||
        !(as.character(e[[1]]) %in% c("<-", "=")) || !is.symbol(e[[2]])) next
    nm <- as.character(e[[2]])
    if (!is.null(only_re) && !grepl(only_re, nm)) next
    v <- canon(e[[3]], map)
    if (!is.na(v)) map[[nm]] <- v
  }
  map
}

# Folder of every read/write call, keyed the same way as analyze_file().
folder_events <- function(f, gmap) {
  parsed <- tryCatch(parse(file.path(CODE_DIR, f), keep.source = TRUE),
                     error = function(e) NULL)
  if (is.null(parsed) || !length(parsed)) return(NULL)
  srcrefs <- attr(parsed, "srcref")
  top <- as.list(parsed)
  lmap <- add_path_assignments(parsed, gmap)
  lit_tbl <- ck$build_literal_table(top)
  rows <- list()
  for (i in seq_along(top)) {
    ln <- as.integer(srcrefs[[i]][1])
    ck$walk_tree(top[[i]], on_call = function(x) {
      nm <- ck$get_call_name(x)
      if (is.na(nm)) return(invisible())
      spec <- ck$READ_FUNCS[[nm]]
      kind <- "read"
      if (is.null(spec)) {
        spec <- ck$WRITE_FUNCS[[nm]]
        kind <- "write"
      }
      if (is.null(spec)) return(invisible())
      arg <- ck$get_named_or_positional(x, spec$names, spec$pos)
      if (is.null(arg)) return(invisible())
      res <- ck$literal_value_of(arg, lit_tbl)
      key <- if (!is.null(res) && length(res$values)) {
        paste(sort(unique(ck$basename_of(res$values))), collapse = "|")
      } else NA_character_
      rows[[length(rows) + 1]] <<- data.frame(
        script = f, line = ln, kind = kind, func = nm, key = key,
        dir = canon_dir(arg, lmap), stringsAsFactors = FALSE
      )
    })
  }
  if (length(rows)) do.call(rbind, rows) else NULL
}

# Calls, packages, sourced files, symbols, assigned names and CONSTANTS.
scan_extras <- function(path) {
  out <- list(ok = TRUE, n_expr = 0L, calls = character(0),
              packages = character(0), sources = character(0),
              symbols = character(0), assigned = character(0),
              constants = list())
  parsed <- tryCatch(parse(path, keep.source = FALSE, encoding = "UTF-8"),
                     error = function(e) NULL)
  if (is.null(parsed)) {
    out$ok <- FALSE
    return(out)
  }
  out$n_expr <- length(parsed)
  for (i in seq_along(parsed)) {
    e <- parsed[[i]]
    out$symbols <- c(out$symbols, ck$expr_symbols(e))
    ck$walk_tree(e, on_call = function(x) {
      nm <- ck$get_call_name(x)
      if (is.na(nm)) return(invisible())
      out$calls <<- c(out$calls, nm)
      if (nm %in% c("::", ":::") && length(x) == 3) {
        out$packages <<- c(out$packages, as.character(x[[2]]))
      }
      if (nm %in% c("library", "require", "requireNamespace") &&
          length(x) >= 2 && !("character.only" %in% names(x)) &&
          (is.symbol(x[[2]]) || is.character(x[[2]]))) {
        out$packages <<- c(out$packages, as.character(x[[2]]))
      }
      if (nm == "source" && length(x) >= 2 && is.character(x[[2]])) {
        out$sources <<- c(out$sources, basename(x[[2]]))
      }
      if (nm %in% c("<-", "=", "<<-", ":=") && length(x) == 3 &&
          is.symbol(x[[2]])) {
        out$assigned <<- c(out$assigned, as.character(x[[2]]))
      }
      if (nm == "for" && length(x) >= 2 && is.symbol(x[[2]])) {
        out$assigned <<- c(out$assigned, as.character(x[[2]]))
      }
      if (nm == "function" && length(x) >= 2 && !is.null(x[[2]])) {
        out$assigned <<- c(out$assigned, names(x[[2]]))
      }
      if (nm == "assign" && length(x) >= 2 && is.character(x[[2]])) {
        out$assigned <<- c(out$assigned, x[[2]])
      }
    })
    if (is.call(e) && is.symbol(e[[1]]) &&
        as.character(e[[1]]) %in% c("<-", "=") && is.symbol(e[[2]]) &&
        grepl("^[A-Z][A-Z0-9_]*$", as.character(e[[2]]))) {
      out$constants[[length(out$constants) + 1]] <- list(
        name = as.character(e[[2]]), value = ck$deparse_short(e[[3]], 60),
        rhs = e[[3]]
      )
    }
  }
  out$calls <- unique(out$calls)
  out$packages <- sort(unique(out$packages), method = "radix")
  out$sources <- unique(out$sources)
  out$symbols <- unique(out$symbols)
  out$assigned <- unique(out$assigned)
  keep <- !duplicated(vapply(out$constants, `[[`, "", "name"), fromLast = TRUE)
  out$constants <- out$constants[keep]
  out
}

# Lines where one of the given years is typed as a number.
year_lines <- function(path, years) {
  pd <- tryCatch(
    utils::getParseData(parse(path, keep.source = TRUE, encoding = "UTF-8")),
    error = function(e) NULL
  )
  if (is.null(pd) || !nrow(pd) || !length(years)) return(integer(0))
  num <- pd[pd$token == "NUM_CONST", c("line1", "text")]
  val <- suppressWarnings(as.numeric(num$text))
  sort(unique(num$line1[!is.na(val) & val %in% years]))
}

# Everything reachable from `start` along producer -> consumer edges.
downstream_of <- function(start, edges) {
  seen <- character(0)
  queue <- start
  while (length(queue)) {
    cur <- queue[1]
    queue <- queue[-1]
    nxt <- setdiff(unique(edges$to[edges$from == cur]), c(seen, start))
    seen <- c(seen, nxt)
    queue <- c(queue, nxt)
  }
  seen
}

# Short names of scripts, joining runs of 3+ consecutive scripts as "A to B".
list_ids <- function(files, empty = "none") {
  files <- unique(files)
  if (!length(files)) return(empty)
  pipe <- files[files %in% names(run_order)]
  other <- setdiff(files, pipe)
  parts <- character(0)
  if (length(pipe)) {
    pos <- sort(unname(run_order[pipe]))
    runs <- split(pos, cumsum(c(1, diff(pos) != 1)))
    parts <- vapply(runs, function(r) {
      if (length(r) >= 3) {
        paste0(ids[[pipeline[r[1]]]], " to ", ids[[pipeline[r[length(r)]]]])
      } else paste(ids[pipeline[r]], collapse = ", ")
    }, character(1))
  }
  paste(c(parts, unname(ids[other])), collapse = ", ")
}

folder_label <- function(dir, root) {
  if (is.na(dir)) return(root)
  vals <- unlist(gmap)
  hit <- names(vals)[vals == dir]
  if (length(hit)) return(hit[1])
  pre <- which(startsWith(dir, paste0(vals, "/")))
  if (length(pre)) {
    k <- pre[which.max(nchar(vals[pre]))]
    return(paste0(names(vals)[k], substring(dir, nchar(vals[k]) + 1)))
  }
  dir
}

const_text <- function(cs) {
  paste(vapply(cs, function(x) {
    r <- x$rhs
    single <- (is.atomic(r) && length(r) == 1) ||
      (is.call(r) && identical(ck$get_call_name(r), "-") && length(r) == 2 &&
         is.numeric(r[[2]]))
    if (single && nchar(x$value) <= SCALAR_WIDTH) {
      paste0(x$name, " = ", x$value)
    } else x$name
  }, character(1)), collapse = ", ")
}

md <- function(x) paste0("`", x, "`")

# Analysis ====================================================================

ck <- load_checker(CHECKER_FILE)

code_files <- sort(list.files(CODE_DIR, pattern = "\\.R$"), method = "radix")
pipeline <- code_files[grepl(PIPELINE_RE, code_files)]
parked   <- code_files[grepl(PARKED_RE, code_files)]
helpers  <- code_files[grepl(HELPER_RE, code_files)]
archived <- if (dir.exists(ARCHIVE_DIR)) {
  sort(list.files(ARCHIVE_DIR, pattern = "\\.R$"), method = "radix")
} else character(0)
analysed <- c(helpers, pipeline, parked)
run_order <- setNames(seq_along(pipeline), pipeline)

# Short names: the numeric prefix; prefix + last word when a prefix repeats.
ids <- setNames(analysed, analysed)
stems <- sub("\\.R$", "", pipeline)
prefix <- sub("_.*$", "", stems)
dup <- prefix %in% prefix[duplicated(prefix)]
alt <- paste0(prefix, "_", sub("^.*_", "", stems))
ids[pipeline] <- ifelse(!dup, prefix,
                        ifelse(alt %in% alt[duplicated(alt)], stems, alt))
ids[parked] <- sub("\\.R$", "", parked)

info <- setNames(lapply(analysed, function(f) {
  path <- file.path(CODE_DIR, f)
  list(res = ck$analyze_file(path), extras = scan_extras(path),
       title = header_title(read_lines_lf(path)))
}), analysed)

# One row per file name read or written, as analyze_file() reports them.
io_rows <- list()
for (f in analysed) {
  for (ev in info[[f]]$res$io_events) {
    root <- ev$raw_symbols[grepl("^PATH_", ev$raw_symbols)]
    key <- if (ev$literal) {
      paste(sort(unique(ev$basenames)), collapse = "|")
    } else NA_character_
    for (b in if (ev$literal) ev$basenames else NA_character_) {
      io_rows[[length(io_rows) + 1]] <- data.frame(
        script = f, kind = ev$kind, func = ev$func,
        line = as.integer(ev$line), file = b, key = key,
        root = if (length(root)) root[1] else "",
        raw = any(grepl("RAW", ev$raw_symbols)) ||
          (!is.na(b) && b %in% ck$EXTERNAL_RAW_BASENAMES),
        dynamic = if (ev$literal) NA_character_ else
          paste0(ev$func, "(", ev$dynamic_text, ")"),
        stringsAsFactors = FALSE
      )
    }
  }
}
io <- do.call(rbind, io_rows)

# Real folders: PATH_ definitions from the script(s) that write paths.Rdata.
gmap <- list()
gmap[[BASE_FOLDER]] <- paste0("<", BASE_FOLDER, ">")
for (f in unique(io$script[io$kind == "write" & io$file %in% PATHS_FILE])) {
  gmap <- add_path_assignments(parse(file.path(CODE_DIR, f),
                                     keep.source = FALSE), gmap, "^PATH_")
}
fe <- do.call(rbind, lapply(analysed, folder_events, gmap = gmap))
fe <- fe[!duplicated(fe[, c("script", "line", "kind", "func", "key")]), ]
m <- match(paste(io$script, io$line, io$kind, io$func, io$key),
           paste(fe$script, fe$line, fe$kind, fe$func, fe$key))
io$dir <- fe$dir[m]
io$folder <- mapply(folder_label, io$dir, io$root, USE.NAMES = FALSE)

lit <- io[!is.na(io$file), ]
writes <- lit[lit$kind == "write", ]
reads <- lit[lit$kind == "read" & lit$script %in% c(pipeline, helpers), ]

# Which script's copy each read gets, applying run order.
resolve_read <- function(r) {
  w <- writes[writes$file == r$file, ]
  w_pipe <- unique(w$script[w$script %in% pipeline])
  w_park <- unique(w$script[w$script %in% parked])
  if (r$script %in% helpers) {
    return(list(status = if (length(w_pipe)) "ok" else "missing",
                from = w_pipe))
  }
  o <- run_order[[r$script]]
  earlier <- w_pipe[run_order[w_pipe] < o]
  own <- w$line[w$script == r$script]
  if (length(earlier)) {
    return(list(status = if (length(own)) "inplace" else "ok",
                from = earlier[which.max(run_order[earlier])]))
  }
  if (length(own)) {
    return(list(status = if (r$line > min(own)) "internal" else "self_before",
                from = character(0)))
  }
  later <- w_pipe[run_order[w_pipe] > o]
  if (length(later)) {
    return(list(status = "order", from = later[which.min(run_order[later])]))
  }
  if (length(w_park)) return(list(status = "parked", from = w_park))
  if (r$raw) return(list(status = "raw", from = character(0)))
  list(status = "missing", from = character(0))
}
reads$status <- NA_character_
reads$from <- NA_character_
for (k in seq_len(nrow(reads))) {
  rr <- resolve_read(reads[k, ])
  reads$status[k] <- rr$status
  reads$from[k] <- paste(rr$from, collapse = ";")
}
split_from <- function(x) strsplit(x, ";", fixed = TRUE)[[1]]

# Dependency edges: producer -> reader, and helper -> scripts sourcing it.
edges <- data.frame(from = character(0), to = character(0))
for (k in which(reads$status %in% LINKED)) {
  for (p in split_from(reads$from[k])) {
    edges <- rbind(edges, data.frame(from = p, to = reads$script[k]))
  }
}
for (f in c(helpers, pipeline)) {
  for (h in intersect(info[[f]]$extras$sources, helpers)) {
    edges <- rbind(edges, data.frame(from = h, to = f))
  }
}
edges <- unique(edges)

readers_of <- function(script, file) {
  hit <- reads[reads$file == file & reads$status %in% LINKED, ]
  keep <- vapply(hit$from, function(x) script %in% split_from(x), logical(1))
  setdiff(unique(hit$script[keep]), script)
}

# Shared names: helpers' CONSTANTS plus the PATH_ folders from paths.Rdata.
shared <- if (SHARED_FILE %in% helpers) info[[SHARED_FILE]]$extras$constants else list()
shared_names <- vapply(shared, `[[`, character(1), "name")
helper_names <- unique(c(names(gmap), unlist(lapply(helpers, function(h)
  vapply(info[[h]]$extras$constants, `[[`, character(1), "name")))))
study_years <- unlist(lapply(shared, function(s) {
  if (is.numeric(s$rhs) && length(s$rhs) == 1 && s$rhs >= 1800 &&
      s$rhs <= 2200) s$rhs
}))

facts <- setNames(lapply(pipeline, function(f) {
  ex <- info[[f]]$extras
  sy <- ex$symbols
  own <- vapply(ex$constants, `[[`, character(1), "name")
  mine <- io[io$script == f, ]
  explained <- unique(c(
    mine$root[mine$kind == "write"],
    reads$root[reads$script == f & reads$status %in% c(LINKED, "internal")]
  ))
  raw_syms <- setdiff(grep("^PATH_.*RAW", sy, value = TRUE), own)
  caps <- sy[grepl(CAPS_RE, sy)]
  undefined <- setdiff(caps, c(ex$assigned, helper_names, ex$calls))
  undefined <- undefined[!vapply(undefined, exists, logical(1),
                                 envir = baseenv())]
  list(
    parse_ok = ex$ok,
    empty = ex$ok && ex$n_expr == 0,
    network = any(ex$calls %in% NETWORK_FUNS),
    raw = any(reads$status[reads$script == f] == "raw") ||
      length(setdiff(raw_syms, explained)) > 0 ||
      (any(mine$kind == "read" & is.na(mine$file)) && length(raw_syms) > 0),
    has_reads = any(mine$kind == "read"),
    dynamic_read = any(mine$kind == "read" & is.na(mine$file)),
    dynamic_write = any(mine$kind == "write" & is.na(mine$file)),
    random = intersect(ex$calls, RANDOM_FUNS),
    seeded = "set.seed" %in% ex$calls,
    parallel = intersect(ex$calls, PARALLEL_FUNS),
    os_guard = any(c(".Platform", "Sys.info") %in% sy),
    shared_used = sort(intersect(sy, shared_names), method = "radix"),
    extra_pkgs = setdiff(ex$packages, info[[SHARED_FILE]]$extras$packages),
    sources_shared = SHARED_FILE %in% ex$sources,
    undefined = sort(undefined, method = "radix"),
    years = year_lines(file.path(CODE_DIR, f), study_years)
  )
}), pipeline)

runs_label <- function(f) {
  x <- facts[[f]]
  if (!x$parse_ok) return("does not parse")
  if (x$empty) return("empty")
  if (x$network) return("downloads")
  if (x$raw) return("raw data")
  if (!x$has_reads) return("no inputs")
  "upstream outputs"
}

# Checks ----------------------------------------------------------------------

checks <- list(fail = character(0), silent = character(0),
               consistency = character(0), unlinked = character(0))
add_check <- function(group, ...) {
  checks[[group]] <<- c(checks[[group]], paste0(...))
}
all_written <- unique(writes$file[writes$script %in% c(pipeline, parked)])

for (k in which(reads$status == "missing")) {
  r <- reads[k, ]
  hint <- ""
  d <- utils::adist(r$file, all_written)[1, ]
  if (length(d) && min(d) <= max(3, 0.3 * nchar(r$file))) {
    best <- all_written[which.min(d)]
    wb <- writes[writes$file == best, ]
    hint <- paste0(" Closest written name: ", md(best), " (",
                   list_ids(unique(wb$script)), ", in ",
                   paste(unique(wb$folder[nzchar(wb$folder)]), collapse = ", "),
                   ").")
  }
  add_check("fail", "**Missing input.** ", ids[[r$script]], " line ", r$line,
            " reads ", md(r$file), ", which no script writes.", hint)
}
for (k in which(reads$status %in% LINKED)) {
  r <- reads[k, ]
  src <- split_from(r$from)
  wdirs <- unique(writes$folder[writes$file == r$file & writes$script %in% src &
                                  !is.na(writes$dir)])
  if (!is.na(r$dir) && length(wdirs) && !(r$folder %in% wdirs)) {
    add_check("fail", "**Wrong folder.** ", ids[[r$script]], " line ", r$line,
              " reads ", md(r$file), " from ", r$folder, ", but ",
              list_ids(src), " writes it to ", paste(wdirs, collapse = ", "),
              ". It fails, or reads an old copy.")
  }
}
for (f in pipeline) {
  x <- facts[[f]]
  if (length(x$undefined)) {
    add_check("fail", "**Undefined names.** ", ids[[f]], " uses ",
              paste(x$undefined, collapse = ", "), ", defined neither in the ",
              "script nor in the shared setup.")
  }
  if (!x$parse_ok) add_check("fail", "**Does not parse.** ", ids[[f]], ".")
  if (x$empty) add_check("fail", "**Empty script.** ", ids[[f]], ".")
}
for (k in which(reads$status == "order")) {
  r <- reads[k, ]
  add_check("fail", "**Out of order.** ", ids[[r$script]], " reads ",
            md(r$file), ", which only the later script ", ids[[r$from]],
            " writes.")
}
for (k in which(reads$status == "self_before")) {
  r <- reads[k, ]
  add_check("fail", "**Needs its own earlier run.** ", ids[[r$script]],
            " reads ", md(r$file), " before writing it.")
}
for (k in which(reads$status == "parked")) {
  r <- reads[k, ]
  add_check("fail", "**Reads a parked script's output.** ", ids[[r$script]],
            " reads ", md(r$file), ", written only by ",
            list_ids(split_from(r$from)), ".")
}

inplace <- reads[reads$status == "inplace", ]
for (fl in sort(unique(inplace$file), method = "radix")) {
  w <- unique(writes$script[writes$file == fl & writes$script %in% pipeline])
  first <- w[which.min(run_order[w])]
  again <- unique(inplace$script[inplace$file == fl])
  add_check("silent", "**Overwritten in place.** ", md(fl), " is written by ",
            ids[[first]], " and overwritten by ", list_ids(again),
            ". Rerunning ", if (length(again) > 1) "one of these" else "it",
            " alone applies its change to an already-changed file; rerun ",
            "from ", ids[[first]], ".")
}
for (fl in sort(unique(writes$file[writes$script %in% pipeline]),
                method = "radix")) {
  w <- unique(writes$script[writes$file == fl & writes$script %in% pipeline])
  plain <- setdiff(w, inplace$script[inplace$file == fl])
  if (length(plain) > 1) {
    add_check("silent", "**Written by several scripts.** ", md(fl),
              " is written by ", list_ids(plain), "; each run replaces the ",
              "other's copy.")
  }
}
for (f in pipeline) {
  x <- facts[[f]]
  if (length(x$random) && !x$seeded) {
    add_check("silent", "**Random draws without set.seed().** ", ids[[f]],
              " uses ", paste(x$random, collapse = ", "), ".")
  }
  if (length(x$years)) {
    lines <- if (length(x$years) > 6) c(x$years[1:6], "...") else x$years
    add_check("consistency", "**Study years typed as numbers.** ", ids[[f]],
              " (lines ", paste(lines, collapse = ", "), "), instead of the ",
              "period constants in _source.R.")
  }
  own <- vapply(info[[f]]$extras$constants, `[[`, character(1), "name")
  shadow <- intersect(own, shared_names)
  if (length(shadow)) {
    add_check("consistency", "**Redefines shared constants.** ", ids[[f]],
              " sets ", paste(shadow, collapse = ", "),
              ", also set in _source.R.")
  }
}
const_rows <- do.call(rbind, lapply(pipeline, function(f) {
  cs <- info[[f]]$extras$constants
  if (!length(cs)) return(NULL)
  data.frame(script = f, name = vapply(cs, `[[`, "", "name"),
             value = vapply(cs, `[[`, "", "value"), stringsAsFactors = FALSE)
}))
for (nm in sort(unique(const_rows$name), method = "radix")) {
  x <- const_rows[const_rows$name == nm, ]
  if (length(unique(x$value)) > 1) {
    add_check("consistency", "**Same constant, different values.** ", nm,
              ": ", paste0(md(x$value), " in ", ids[x$script],
                           collapse = "; "), ".")
  }
}
dyn <- io[!is.na(io$dynamic) & io$script %in% pipeline, ]
for (f in unique(dyn$script)) {
  add_check("unlinked", ids[[f]], ": ",
            paste(md(dyn$dynamic[dyn$script == f]), collapse = ", "))
}

# Outputs =====================================================================

out <- character(0)
w <- function(...) out <<- c(out, paste0(...))
n_checks <- sum(lengths(checks[c("fail", "silent", "consistency")]))

w("# Pipeline manifest")
w("")
w("<!-- Generated by tools/make_manifest.R. Do not edit by hand: change the ",
  "scripts in code/, then run `Rscript tools/make_manifest.R`. -->")
w("")
w("A map of the scripts in `code/`, made by reading the code without running ",
  "it. Start here: find the scripts a task touches and open only those. ",
  "After changing a script, rerun what its *Rerun after changing* line lists.")
w("")
w("- ", length(pipeline), " pipeline scripts, run in name order; ",
  length(parked), " parked (`XX_`); ", length(archived), " archived.")
w("- Scripts load `_source.R`: packages (",
  paste(info[[SHARED_FILE]]$extras$packages, collapse = ", "),
  "), shared constants, and the `PATH_` folders that 00a saves in ",
  "`paths.Rdata`. `PATH_SAVE` is set per machine in `_machine_paths.R`.")
w("- A script reads the copy of a file written by the last script before it ",
  "in name order.")
w("- *Runs*: **raw data** and **downloads** need the machine with the raw ",
  "inputs; **upstream outputs** runs anywhere the earlier outputs exist.")
w("")

w("## Checks")
w("")
titles <- c(fail = "Will fail when run",
            silent = "Can silently give wrong results",
            consistency = "Consistency",
            unlinked = "Links not visible here (file names built at run time)")
if (n_checks == 0 && !length(checks$unlinked)) w("None.")
for (g in names(titles)) {
  if (!length(checks[[g]])) next
  w("**", titles[[g]], "**")
  w("")
  for (x in checks[[g]]) w("- ", x)
  w("")
}

w("## Run order")
w("")
w("| Script | What it does | Reads from | Runs |")
w("|---|---|---|---|")
for (f in pipeline) {
  up <- unique(unlist(lapply(reads$from[reads$script == f &
                                          reads$status %in% LINKED],
                             split_from)))
  up <- up[order(run_order[up])]
  from <- c(if (length(up)) paste(ids[up], collapse = ", "),
            if (facts[[f]]$dynamic_read) "run-time names")
  title <- if (nzchar(info[[f]]$title)) info[[f]]$title else "(no header)"
  w("| ", ids[[f]], " | ", title, " | ",
    if (length(from)) paste(from, collapse = " + ") else "-", " | ",
    runs_label(f), " |")
}
w("")

w("## Dependency graph")
w("")
w("```mermaid")
w("flowchart TD")
node <- function(f) paste0("s", gsub("[^A-Za-z0-9]", "_", ids[[f]]))
for (st in unique(substr(pipeline, 1, 2))) {
  w("  subgraph g", st, "[\"Stage ", st, "\"]")
  for (f in pipeline[substr(pipeline, 1, 2) == st]) {
    w("    ", node(f), "[\"", gsub("_", " ", sub("\\.R$", "", f)), "\"]")
  }
  w("  end")
}
pe <- edges[edges$from %in% pipeline & edges$to %in% pipeline, ]
pe <- pe[order(run_order[pe$from], run_order[pe$to]), ]
for (k in seq_len(nrow(pe))) {
  late <- run_order[[pe$from[k]]] > run_order[[pe$to[k]]]
  w("  ", node(pe$from[k]), if (late) " -. later .-> " else " --> ",
    node(pe$to[k]))
}
miss <- reads[reads$status == "missing", ]
for (k in seq_len(nrow(miss))) {
  w("  x", k, "[\"missing: ", miss$file[k], "\"]:::missing")
  w("  x", k, " -.-> ", node(miss$script[k]))
}
w("  classDef missing fill:#fdecea,stroke:#c62828,stroke-dasharray: 4 3")
w("  classDef inplace stroke:#ef6c00,stroke-width:3px")
w("  classDef empty fill:#f5f5f5,stroke:#9e9e9e,stroke-dasharray: 2 2")
ip <- intersect(pipeline, inplace$script)
if (length(ip)) w("  class ", paste(vapply(ip, node, ""), collapse = ","),
                  " inplace")
em <- pipeline[vapply(pipeline, function(f) facts[[f]]$empty, logical(1))]
if (length(em)) w("  class ", paste(vapply(em, node, ""), collapse = ","),
                  " empty")
w("```")
w("")
w("Orange border: overwrites its input in place. Red: missing input.")

w("")
w("## Scripts")
for (f in pipeline) {
  x <- facts[[f]]
  w("")
  w("### ", f)
  w("")
  r <- reads[reads$script == f, ]
  r <- r[!duplicated(r[, c("file", "folder")]), ]
  for (fd in unique(r$folder)) {
    rr <- r[r$folder == fd, ]
    what <- vapply(seq_len(nrow(rr)), function(k) {
      src <- switch(rr$status[k],
                    ok = , inplace = , order = , parked =
                      list_ids(split_from(rr$from[k])),
                    raw = "raw input", internal = "written earlier here",
                    self_before = "own earlier run", missing = "MISSING")
      paste0(rr$file[k], " (", src, ")")
    }, character(1))
    w("- Reads", if (nzchar(fd)) paste0(" from ", fd), ": ",
      paste(what, collapse = ", "))
  }
  ww <- writes[writes$script == f, ]
  ww <- ww[!duplicated(ww[, c("file", "folder")]), ]
  for (fd in unique(ww$folder)) {
    wr <- ww[ww$folder == fd, ]
    what <- vapply(seq_len(nrow(wr)), function(k) {
      if (wr$func[k] %in% FIGURE_FUNS) return(paste0(wr$file[k], " (figure)"))
      cons <- readers_of(f, wr$file[k])
      tag <- if (wr$file[k] %in% inplace$file[inplace$script == f]) {
        "overwritten in place; " } else ""
      paste0(wr$file[k], " (", tag, if (length(cons))
        paste0("read by ", list_ids(cons)) else "not read", ")")
    }, character(1))
    w("- Writes", if (nzchar(fd)) paste0(" to ", fd), ": ",
      paste(what, collapse = ", "))
  }
  d <- io$dynamic[!is.na(io$dynamic) & io$script == f]
  if (length(d)) w("- Names built at run time: ", paste(md(d), collapse = ", "))

  down <- downstream_of(f, edges)
  down <- down[down %in% pipeline]
  chain <- character(0)
  for (fl in unique(inplace$file[inplace$script == f])) {
    wr <- unique(writes$script[writes$file == fl & writes$script %in% pipeline])
    chain <- c(chain, wr[run_order[wr] < run_order[[f]]])
  }
  chain <- unique(chain)
  if (length(chain)) {
    w("- Rerun after changing: ", list_ids(chain), ", then this script (it ",
      "overwrites ", paste(md(unique(inplace$file[inplace$script == f])),
                           collapse = ", "), " in place), then ",
      list_ids(down, "nothing else"))
  } else if (!length(down) && (x$network || x$dynamic_write)) {
    w("- Rerun after changing: not tracked (outputs are downloads or have ",
      "names built at run time)")
  } else {
    w("- Rerun after changing: ", list_ids(down, "nothing downstream"))
  }
  notes <- c(
    if (length(x$extra_pkgs)) paste0("packages: ",
                                      paste(x$extra_pkgs, collapse = ", ")),
    if (length(x$shared_used)) paste0("shared: ",
                                       paste(x$shared_used, collapse = ", ")),
    if (length(x$random)) paste0("random: ", paste(x$random, collapse = ", "),
                                 if (x$seeded) " (seeded)" else " (no seed)"),
    if (length(x$parallel)) paste0("parallel: ",
                                   paste(x$parallel, collapse = ", "),
                                   if (x$os_guard) " (Windows guard)"),
    if (!x$sources_shared && !x$empty) "does not source _source.R"
  )
  if (length(notes)) w("- ", paste(notes, collapse = "; "))
  cs <- info[[f]]$extras$constants
  if (length(cs)) w("- Constants: ", const_text(cs))
}

w("")
w("## Shared setup")
w("")
for (h in helpers) {
  users <- c(pipeline, helpers)[vapply(c(pipeline, helpers), function(g)
    h %in% info[[g]]$extras$sources, logical(1))]
  who <- if (length(users) > 5) paste0(length(users), " scripts") else
    list_ids(users)
  title <- info[[h]]$title
  w("- `", h, "`: ", if (nzchar(title)) paste0(title, ". "), "Sourced by ",
    who, ".")
}
if (length(shared)) {
  deps <- lapply(shared, function(s) intersect(ck$expr_symbols(s$rhs),
                                                shared_names))
  names(deps) <- shared_names
  uses_of <- function(nm) {
    # shared constants that depend on nm, directly or through others
    hit <- nm
    repeat {
      more <- names(deps)[vapply(deps, function(d) any(d %in% hit),
                                 logical(1))]
      new <- setdiff(more, hit)
      if (!length(new)) break
      hit <- c(hit, new)
    }
    hit
  }
  w("")
  w("| Shared constant | Value | Used by (directly or through another) |")
  w("|---|---|---|")
  for (s in shared) {
    via <- uses_of(s$name)
    users <- pipeline[vapply(pipeline, function(f)
      any(via %in% facts[[f]]$shared_used), logical(1))]
    w("| ", s$name, " | ", md(s$value), " | ", list_ids(users, "-"), " |")
  }
}

w("")
w("## Not part of the pipeline")
w("")
for (f in parked) {
  w("- `code/", f, "` (parked)", if (nzchar(info[[f]]$title))
    paste0(": ", info[[f]]$title))
}
for (f in archived) w("- `code/archive/", f, "` (archived)")
if (!length(parked) && !length(archived)) w("None.")

# Validation ==================================================================

if (anyNA(m)) {
  stop("The folder lookup missed ", sum(is.na(m)), " read/write call(s) ",
       "that tools/check_static.R reports; update folder_events() in ",
       "tools/make_manifest.R to match the checker.", call. = FALSE)
}
stopifnot(
  length(pipeline) > 0,
  all(edges$from %in% analysed), all(edges$to %in% analysed),
  !anyNA(reads$status)
)

if (CHECK_ONLY) {
  current <- if (file.exists(MANIFEST_FILE)) read_lines_lf(MANIFEST_FILE) else ""
  if (!identical(current, out)) {
    message(MANIFEST_FILE, " is out of date. Run: Rscript tools/make_manifest.R")
    if (!interactive()) quit(save = "no", status = 1L)
  } else {
    message(MANIFEST_FILE, " is up to date.")
  }
} else {
  dir.create(dirname(MANIFEST_FILE), showWarnings = FALSE, recursive = TRUE)
  con <- file(MANIFEST_FILE, open = "wb")  # "wb" keeps LF line endings on Windows
  writeLines(out, con, sep = "\n", useBytes = TRUE)
  close(con)
  message("Wrote ", MANIFEST_FILE, ": ", length(pipeline), " scripts, ",
          n_checks, " checks, ", sum(nchar(out, type = "bytes") + 1L),
          " bytes.")
}
