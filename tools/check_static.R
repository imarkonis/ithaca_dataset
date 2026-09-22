#!/usr/bin/env Rscript
# ============================================================================
# check_static.R -- Layer-1 static contract checker for the ithaca_dataset R
# pipeline (code/*.R, scripts 00-05).
#
# PURPOSE
#   Detect broken contracts BETWEEN scripts before the pipeline is run:
#   files a script reads that nothing produces, hard-coded machine-specific
#   paths, unguarded mclapply() (fork parallelism is Unix-only), stale header
#   documentation, and missing convention sections. This is a mechanical
#   sweep, not a linter and not a refactor tool: it does not run any code,
#   does not fix anything, and does not fully evaluate dynamic R expressions.
#
# SCOPE
#   All code/*.R files whose name starts with a numeric prefix 00-05
#   (e.g. 00a_initialize.R, 01g_set_region_classes.R), PLUS code/_source.R.
#   _source.R is not numbered but is sourced by nearly every numbered script
#   and is exactly the kind of shared file whose contracts matter most (it is
#   also, as of this writing, where a hard-coded drive-letter path lives) --
#   excluding it would defeat the point of the checker. Subdirectories of
#   code/ (archive/, and any future ones) and out-of-range scripts (e.g. the
#   07* sensitivity scripts, XX_*.R parked scripts) are intentionally NOT
#   scanned; see the printed report for anything that looks relevant there.
#
# METHOD
#   Each script is parsed with base R's parse(..., keep.source = TRUE) and
#   walked as an R language tree (no getParseData token reconstruction --
#   parse()'s own AST is a legitimate, simpler base-R static-analysis
#   surface for this purpose). For each call to a known reader
#   (readRDS/read_fst/fread/load/rast/brick/...) or writer
#   (saveRDS/write_fst/fwrite/save/saveNC/ggsave/...), the file-path argument
#   is symbolically rendered:
#     - string literals resolve directly;
#     - file.path(...) resolves via its LAST argument only (this codebase's
#       convention is always file.path(<directory symbols>, <filename>), and
#       matching is by basename anyway -- see below -- so directory
#       arguments, almost always runtime-loaded PATH_* symbols, are not
#       required to resolve);
#     - paste0()/paste() resolve only if EVERY part resolves, by literal
#       string concatenation (no I/O, no evaluation) -- these build the
#       filename itself in this codebase, so a partial answer would be wrong;
#     - a symbol resolves if it was assigned a literal string/character
#       vector at the TOP LEVEL of the same script (e.g. OUTPUT_FILE <-
#       file.path(...)), including named vectors such as
#       c(a = "x.Rds", b = "y.Rds");
#     - X[["k"]] / X$k / X[k] on a resolved named vector resolves to that
#       key's value when the key is itself a literal, and conservatively to
#       ALL of X's values when the key is dynamic (e.g. a loop variable) --
#       this over-approximates on purpose so a real cross-script link is not
#       missed, at the small cost of occasionally over-matching;
#     - anything else (a function call, a data.table column reference, an
#       unresolved symbol) is left DYNAMIC. There is no attempt to trace
#       values through user-defined functions, apply-family indirection, or
#       loop bodies -- per the brief, arbitrary dynamic expressions are not
#       solved, only reported as unresolved.
#   Matching is by BASENAME only (directory-variable parts are irrelevant to
#   whether a contract holds), across the whole scanned set: any read whose
#   resolved basename is written by ANY scanned script (at any point, so a
#   script may legitimately read-then-rewrite its own output) is OK.
#
# WHAT THIS IS NOT
#   Not a column/schema checker (it never looks at data.table column names),
#   not a value-flow/interprocedural analyzer, not a substitute for actually
#   running the pipeline. It only checks: does *some* scanned script produce
#   the file this one is about to read, by name.
#
# USAGE
#   Rscript tools/check_static.R          # from the repository root
#   Exit status is non-zero iff at least one ERROR finding exists.
# ============================================================================

suppressWarnings(RNGversion(getRversion()))

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

CODE_DIR <- "code"
REPORT_DIR <- file.path("tools", "verification")

# function name -> how to find its file-path argument (named arg tried
# first, in order; positional fallback counts only UNNAMED arguments).
READ_FUNCS <- list(
  readRDS  = list(names = "file", pos = 1),
  read_fst = list(names = "path", pos = 1),
  fread    = list(names = c("file", "input"), pos = 1),
  load     = list(names = "file", pos = 1),
  rast     = list(names = "x", pos = 1),
  brick    = list(names = "x", pos = 1)
)

WRITE_FUNCS <- list(
  saveRDS  = list(names = "file", pos = 2),
  write_fst = list(names = "path", pos = 2),
  fwrite   = list(names = "file", pos = 2),
  save     = list(names = "file", pos = NA),
  saveNC   = list(names = "filename", pos = 2),
  ggsave   = list(names = "filename", pos = 1)
)

# A very small, explicit allow-list of basenames that are genuine raw /
# external inputs but do not live under a PATH_OUTPUT_RAW* directory symbol
# (so the RAW-directory heuristic below would not otherwise catch them).
# Extend this only for real, verified external inputs -- do not use it to
# silence a genuine missing-producer finding.
EXTERNAL_RAW_BASENAMES <- character(0)

DRIVE_LETTER_RE <- "^[A-Za-z]:[/\\\\]"
USER_PATH_RE <- "[/\\\\][Uu]sers[/\\\\][^/\\\\\"']+"
HOME_PATH_RE <- "^/home/[^/\"']+"

HEADER_LINE_RE <- "^#+\\s*([A-Za-z][A-Za-z0-9 &/_-]{1,40}?)\\s*=+\\s*$"
BULLET_FILE_RE <- "^#+\\s*[-*]\\s+([A-Za-z0-9_.-]+\\.(Rds|rds|fst|csv|nc|Rdata|RData))\\s*$"

SECTION_SYNONYMS <- list(
  libraries  = c("libraries", "library", "librariespackages", "packages"),
  inputs     = c("inputs", "input", "inputdatasets", "inputdataset", "inputdata"),
  outputs    = c("outputs", "output", "outputfiles"),
  validation = c("validation", "validate", "validating")
)

# ---------------------------------------------------------------------------
# Generic language-tree helpers
# ---------------------------------------------------------------------------

# R's "missing argument" sentinel (e.g. the empty slot in data.table's
# dt[, .(x)] -- extremely common in this codebase) is toxic: once it is
# bound to an ordinary variable and that variable is evaluated again, R
# raises "argument ... is missing, with no default", even though nothing is
# actually wrong. The only safe way to test for it is to re-derive the
# element fresh from its container each time, never via an intermediate
# variable. See tools/check_static.R development notes; do not "simplify"
# this by caching lst[[i]] in a local.
arg_is_missing <- function(lst, i) {
  is.symbol(lst[[i]]) && identical(lst[[i]], quote(expr = ))
}

get_call_name <- function(expr) {
  if (!is.call(expr)) return(NA_character_)
  head <- expr[[1]]
  if (is.symbol(head)) return(as.character(head))
  if (is.call(head) && length(head) == 3 &&
        is.symbol(head[[1]]) && as.character(head[[1]]) %in% c("::", ":::")) {
    return(as.character(head[[3]]))
  }
  NA_character_
}

# Visits every call node (on_call) and every single string constant
# (on_string) reachable from expr, recursing into function bodies, blocks,
# loops, and formal-argument default values.
walk_tree <- function(expr, on_call = NULL, on_string = NULL) {
  if (is.call(expr)) {
    if (!is.null(on_call)) on_call(expr)
    parts <- as.list(expr)
    for (i in seq_along(parts)) {
      if (arg_is_missing(parts, i)) next
      walk_tree(parts[[i]], on_call, on_string)
    }
  } else if (is.pairlist(expr)) {
    parts <- as.list(expr)
    for (i in seq_along(parts)) {
      if (arg_is_missing(parts, i)) next
      walk_tree(parts[[i]], on_call, on_string)
    }
  } else if (is.character(expr) && length(expr) == 1 && !is.null(on_string)) {
    on_string(expr)
  }
}

get_named_or_positional <- function(expr, names_priority, pos) {
  args <- as.list(expr)[-1]
  if (length(args) == 0) return(NULL)
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  for (nm in names_priority) {
    hit <- which(nms == nm)
    if (length(hit)) return(args[[hit[1]]])
  }
  if (!is.na(pos)) {
    unnamed <- which(nms == "")
    if (length(unnamed) >= pos) return(args[[unnamed[pos]]])
  }
  NULL
}

expr_symbols <- function(expr) {
  out <- character(0)
  collect <- function(e) {
    if (is.call(e)) {
      parts <- as.list(e)
      for (i in seq_along(parts)) {
        if (arg_is_missing(parts, i)) next
        collect(parts[[i]])
      }
    } else if (is.symbol(e)) {
      nm <- as.character(e)
      if (nzchar(nm)) out <<- c(out, nm)
    }
  }
  collect(expr)
  unique(out)
}

deparse_short <- function(expr, width = 80) {
  txt <- paste(deparse(expr, width.cutoff = 500L), collapse = " ")
  txt <- gsub("\\s+", " ", txt)
  if (nchar(txt) > width) txt <- paste0(substr(txt, 1, width - 3), "...")
  txt
}

# ---------------------------------------------------------------------------
# Literal string / vector resolution (no evaluation, structural only)
# ---------------------------------------------------------------------------

# Combine N character vectors positionally with an outer-product join,
# capped to avoid combinatorial blow-up on pathological inputs.
combine_parts <- function(parts_list, sep) {
  acc <- NULL
  for (vals in parts_list) {
    if (is.null(acc)) {
      acc <- vals
    } else {
      if (length(acc) * length(vals) > 200) return(NULL)
      acc <- as.vector(outer(acc, vals, FUN = function(a, b) paste0(a, sep, b)))
    }
  }
  if (is.null(acc)) "" else acc
}

# Resolves expr to list(values = character vector, names = character vector
# or NULL) when it is (transitively) a literal, else NULL.
literal_value_of <- function(expr, tbl) {
  if (is.character(expr) && length(expr) >= 1) {
    return(list(values = as.character(expr), names = names(expr)))
  }
  if (is.symbol(expr)) {
    nm <- as.character(expr)
    return(tbl[[nm]])
  }
  if (!is.call(expr)) return(NULL)
  fname <- get_call_name(expr)
  if (is.na(fname)) return(NULL)

  if (identical(fname, "c")) {
    args <- as.list(expr)[-1]
    if (length(args) == 0) return(list(values = character(0), names = NULL))
    arg_names <- names(args)
    if (is.null(arg_names)) arg_names <- rep("", length(args))
    all_vals <- character(0)
    all_names <- character(0)
    for (i in seq_along(args)) {
      sub <- literal_value_of(args[[i]], tbl)
      if (is.null(sub)) return(NULL)
      n <- length(sub$values)
      sub_names <- sub$names
      if (is.null(sub_names)) sub_names <- rep("", n)
      if (nzchar(arg_names[i]) && n == 1) sub_names <- arg_names[i]
      all_vals <- c(all_vals, sub$values)
      all_names <- c(all_names, sub_names)
    }
    return(list(values = all_vals, names = if (any(nzchar(all_names))) all_names else NULL))
  }

  if (identical(fname, "file.path")) {
    # Matching is by basename only (see file header), and this codebase's
    # convention is always file.path(<directory symbols...>, <filename>) --
    # so only the LAST (non-fsep) argument is resolved. Directory arguments
    # are almost always symbols loaded at runtime from paths.Rdata and are
    # never locally resolvable; requiring them to resolve too would make
    # every single file.path() call "dynamic", which defeats the checker.
    parts <- as.list(expr)[-1]
    if (length(parts) == 0) return(list(values = "", names = NULL))
    arg_names <- names(parts)
    if (is.null(arg_names)) arg_names <- rep("", length(parts))
    keep_idx <- which(arg_names != "fsep")
    if (length(keep_idx) == 0) return(list(values = "", names = NULL))
    last_idx <- keep_idx[length(keep_idx)]
    return(literal_value_of(parts[[last_idx]], tbl))
  }

  if (fname %in% c("paste0", "paste")) {
    # Unlike file.path, paste0()/paste() are used in this codebase to build
    # the filename itself (e.g. paste0("mc_selection_", run_id, ".Rds")), so
    # every part must resolve for the concatenation to be trustworthy.
    parts <- as.list(expr)[-1]
    arg_names <- names(parts)
    if (is.null(arg_names)) arg_names <- rep("", length(parts))
    sep <- if (fname == "paste0") "" else " "
    drop_idx <- integer(0)
    for (i in seq_along(parts)) {
      if (fname == "paste" && arg_names[i] == "sep") {
        sv <- literal_value_of(parts[[i]], tbl)
        if (is.null(sv) || length(sv$values) != 1) return(NULL)
        sep <- sv$values
        drop_idx <- c(drop_idx, i)
      } else if (arg_names[i] == "collapse") {
        drop_idx <- c(drop_idx, i)
      }
    }
    if (length(drop_idx)) parts <- parts[-drop_idx]
    if (length(parts) == 0) return(list(values = "", names = NULL))
    resolved <- lapply(parts, literal_value_of, tbl = tbl)
    if (any(vapply(resolved, is.null, logical(1)))) return(NULL)
    combined <- combine_parts(lapply(resolved, `[[`, "values"), sep)
    if (is.null(combined)) return(NULL)
    return(list(values = combined, names = NULL))
  }

  if (fname %in% c("[[", "$")) {
    base_val <- literal_value_of(expr[[2]], tbl)
    if (is.null(base_val)) return(NULL)
    key_literal <- NULL
    if (fname == "$") {
      key_literal <- as.character(expr[[3]])
    } else {
      kv <- literal_value_of(expr[[3]], tbl)
      if (!is.null(kv) && length(kv$values) == 1) key_literal <- kv$values
    }
    if (!is.null(key_literal) && !is.null(base_val$names) &&
          key_literal %in% base_val$names) {
      return(list(values = base_val$values[which(base_val$names == key_literal)[1]],
                  names = NULL))
    }
    # Dynamic / unrecognised key: conservatively offer every value of the
    # base vector as a candidate rather than giving up (see file header).
    return(list(values = unique(base_val$values), names = NULL))
  }

  if (identical(fname, "[")) {
    base_val <- literal_value_of(expr[[2]], tbl)
    if (is.null(base_val)) return(NULL)
    return(list(values = unique(base_val$values), names = NULL))
  }

  NULL
}

# Top-level literal table: NAME -> list(values, names), from simple
# top-level `NAME <- <literal-ish expr>` assignments only (not inside
# functions or loops -- see file header for why that boundary is drawn).
build_literal_table <- function(top_exprs) {
  tbl <- list()
  for (e in top_exprs) {
    if (is.call(e) && get_call_name(e) %in% c("<-", "=", "<<-") && length(e) == 3) {
      lhs <- e[[2]]
      if (is.symbol(lhs)) {
        val <- literal_value_of(e[[3]], tbl)
        if (!is.null(val)) tbl[[as.character(lhs)]] <- val
      }
    }
  }
  tbl
}

basename_of <- function(x) {
  x <- gsub("\\\\", "/", x)
  sub(".*/", "", x)
}

# ---------------------------------------------------------------------------
# Per-file analysis
# ---------------------------------------------------------------------------

analyze_file <- function(path) {
  script <- basename(path)
  findings <- list()   # each: list(category, script, line, check, message)
  io_events <- list()  # each: list(script, line, kind ["read"/"write"], func,
                        #             literal, basenames, raw, dynamic_text)

  add_finding <- function(category, line, check, message) {
    findings[[length(findings) + 1]] <<- list(
      category = category, script = script, line = line,
      check = check, message = message
    )
  }

  raw_lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))

  parsed <- tryCatch(
    parse(path, keep.source = TRUE),
    error = function(e) e
  )

  if (inherits(parsed, "error")) {
    add_finding("ERROR", NA_integer_, "parse_error",
                paste0("Script does not parse as R code: ", conditionMessage(parsed)))
    return(list(findings = findings, io_events = io_events, script = script))
  }

  if (length(parsed) == 0) {
    add_finding("WARNING", NA_integer_, "empty_script",
                "Script is empty (no executable content).")
    return(list(findings = findings, io_events = io_events, script = script))
  }

  top_exprs <- as.list(parsed)
  srcrefs <- attr(parsed, "srcref")
  line_of <- function(i) {
    if (!is.null(srcrefs) && length(srcrefs) >= i) srcrefs[[i]][1] else NA_integer_
  }

  literal_tbl <- build_literal_table(top_exprs)

  analysis_ok <- TRUE

  # --- hard-coded path scan (any string literal, anywhere) -----------------
  for (i in seq_along(top_exprs)) {
    ok <- tryCatch({
      walk_tree(top_exprs[[i]], on_string = function(s) {
        if (grepl(DRIVE_LETTER_RE, s) || grepl(USER_PATH_RE, s) || grepl(HOME_PATH_RE, s)) {
          add_finding("ERROR", line_of(i), "hardcoded_path",
                      paste0("Hard-coded machine-specific absolute path: \"", s, "\""))
        }
      })
      TRUE
    }, error = function(e) FALSE)
    if (!ok) analysis_ok <- FALSE
  }

  # --- mclapply-without-guard scan ------------------------------------------
  has_mclapply <- FALSE
  mclapply_lines <- integer(0)
  has_os_guard <- FALSE
  for (i in seq_along(top_exprs)) {
    ok <- tryCatch({
      walk_tree(top_exprs[[i]], on_call = function(e) {
        nm <- get_call_name(e)
        if (identical(nm, "mclapply")) {
          has_mclapply <<- TRUE
          mclapply_lines <<- c(mclapply_lines, line_of(i))
        }
        if (identical(nm, "Sys.info")) has_os_guard <<- TRUE
        if (identical(nm, "$") && is.symbol(e[[2]]) &&
              identical(as.character(e[[2]]), ".Platform")) {
          has_os_guard <<- TRUE
        }
      })
      TRUE
    }, error = function(e) FALSE)
    if (!ok) analysis_ok <- FALSE
  }
  if (has_mclapply && !has_os_guard) {
    for (ln in unique(mclapply_lines)) {
      add_finding("WARNING", ln, "mclapply_no_guard",
                  paste0("mclapply() is used with no visible .Platform$OS.type / ",
                         "Sys.info() guard; fork parallelism is not supported on ",
                         "Windows (mclapply silently falls back to mc.cores = 1)."))
    }
  }

  # --- read/write call scan -------------------------------------------------
  has_source_or_library <- FALSE
  for (i in seq_along(top_exprs)) {
    ok <- tryCatch({
      walk_tree(top_exprs[[i]], on_call = function(e) {
        nm <- get_call_name(e)
        if (is.na(nm)) return(invisible())

        if (nm %in% c("source", "library", "require")) has_source_or_library <<- TRUE

        spec <- READ_FUNCS[[nm]]
        kind <- "read"
        if (is.null(spec)) { spec <- WRITE_FUNCS[[nm]]; kind <- "write" }
        if (is.null(spec)) return(invisible())

        arg_expr <- get_named_or_positional(e, spec$names, spec$pos)
        if (is.null(arg_expr)) return(invisible())

        resolved <- literal_value_of(arg_expr, literal_tbl)
        raw_symbols <- expr_symbols(arg_expr)

        if (!is.null(resolved) && length(resolved$values) > 0) {
          bnames <- unique(basename_of(resolved$values))
          io_events[[length(io_events) + 1]] <<- list(
            script = script, line = line_of(i), kind = kind, func = nm,
            literal = TRUE, basenames = bnames, raw_symbols = raw_symbols,
            dynamic_text = NA_character_
          )
        } else {
          io_events[[length(io_events) + 1]] <<- list(
            script = script, line = line_of(i), kind = kind, func = nm,
            literal = FALSE, basenames = character(0), raw_symbols = raw_symbols,
            dynamic_text = deparse_short(arg_expr)
          )
        }
      })
      TRUE
    }, error = function(e) FALSE)
    if (!ok) analysis_ok <- FALSE
  }

  if (!analysis_ok) {
    add_finding("WARNING", NA_integer_, "analysis_incomplete",
                paste0("One or more statements in this script could not be fully walked by ",
                       "the static analyzer (unexpected syntax shape); findings for this ",
                       "file may be incomplete."))
  }

  # --- section-header convention (usage-tied, see file header) -------------
  header_titles <- character(0)
  m <- regmatches(raw_lines, regexec(HEADER_LINE_RE, raw_lines))
  for (x in m) if (length(x) >= 2) header_titles <- c(header_titles, x[2])
  normalize_title <- function(t) {
    s <- tolower(gsub("[^A-Za-z0-9]", "", t))
    for (canon in names(SECTION_SYNONYMS)) {
      if (s %in% SECTION_SYNONYMS[[canon]]) return(canon)
    }
    s
  }
  norm_titles <- vapply(header_titles, normalize_title, character(1))
  has_section <- function(canon) canon %in% norm_titles

  reads_here  <- Filter(function(x) x$kind == "read",  io_events)
  writes_here <- Filter(function(x) x$kind == "write", io_events)

  if (length(reads_here) > 0 && !has_section("inputs")) {
    add_finding("WARNING", NA_integer_, "missing_section",
                "Script reads external data but has no '# Inputs' header section.")
  }
  if (length(writes_here) > 0 && !has_section("outputs")) {
    add_finding("WARNING", NA_integer_, "missing_section",
                "Script writes data but has no '# Outputs' header section.")
  }
  if (has_source_or_library && !has_section("libraries")) {
    add_finding("WARNING", NA_integer_, "missing_section",
                "Script calls source()/library()/require() but has no '# Libraries' header section.")
  }
  if (length(writes_here) > 0 && !has_section("validation")) {
    add_finding("WARNING", NA_integer_, "missing_section",
                "Script writes data but has no '# Validation' header section.")
  }

  # --- header-declared output filenames vs actual writes --------------------
  bullet_files <- character(0)
  bm <- regmatches(raw_lines, regexec(BULLET_FILE_RE, raw_lines))
  for (x in bm) if (length(x) >= 2) bullet_files <- c(bullet_files, x[2])

  actual_written <- unique(unlist(lapply(writes_here, function(x) if (x$literal) x$basenames else character(0))))
  if (length(bullet_files) > 0 && length(actual_written) > 0) {
    stale <- setdiff(bullet_files, actual_written)
    for (f in stale) {
      add_finding("WARNING", NA_integer_, "stale_header_doc",
                  paste0("Header comment lists output '", f,
                         "' but the script's actual saveRDS/write_fst/... calls ",
                         "never write that filename (writes: ",
                         paste(actual_written, collapse = ", "), ")."))
    }
  }

  list(findings = findings, io_events = io_events, script = script)
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

if (!dir.exists(CODE_DIR)) {
  stop("Run this script from the repository root (expected to find '", CODE_DIR,
       "/').", call. = FALSE)
}

all_code_files <- list.files(CODE_DIR, pattern = "\\.R$", full.names = FALSE)
numbered <- all_code_files[grepl("^(0[0-5])[a-zA-Z]?_", all_code_files)]
scope_files <- sort(unique(c(numbered, if ("_source.R" %in% all_code_files) "_source.R")))

if (length(scope_files) == 0) {
  stop("No scripts matching 00-05 (or _source.R) found under '", CODE_DIR, "'.",
       call. = FALSE)
}

cat("ITHACA static contract checker (tools/check_static.R)\n")
cat("Scope: ", paste(scope_files, collapse = ", "), "\n\n", sep = "")

all_findings <- list()
all_io <- list()

for (f in scope_files) {
  res <- analyze_file(file.path(CODE_DIR, f))
  all_findings <- c(all_findings, res$findings)
  all_io <- c(all_io, res$io_events)
}

# --- cross-script matching: producer index, then classify each read --------

write_events <- Filter(function(x) x$kind == "write", all_io)
read_events  <- Filter(function(x) x$kind == "read",  all_io)

producer_index <- new.env()
for (w in write_events) {
  if (!w$literal) next
  for (bn in w$basenames) {
    cur <- producer_index[[bn]]
    producer_index[[bn]] <- unique(c(cur, w$script))
  }
}

relationship_rows <- list()  # artifact, producers, consumers, status

for (r in read_events) {
  if (r$literal) {
    hits <- unique(unlist(lapply(r$basenames, function(bn) producer_index[[bn]])))
    matched_bn <- Filter(function(bn) !is.null(producer_index[[bn]]), r$basenames)
    if (length(hits) > 0) {
      relationship_rows[[length(relationship_rows) + 1]] <- list(
        artifact = paste(matched_bn, collapse = "|"), producers = paste(hits, collapse = ";"),
        consumer = r$script, status = "OK"
      )
      next
    }
    is_raw <- any(grepl("RAW", r$raw_symbols)) ||
      any(r$basenames %in% EXTERNAL_RAW_BASENAMES)
    if (is_raw) {
      relationship_rows[[length(relationship_rows) + 1]] <- list(
        artifact = paste(r$basenames, collapse = "|"), producers = "(external/raw input)",
        consumer = r$script, status = "OK"
      )
      next
    }
    all_findings[[length(all_findings) + 1]] <- list(
      category = "ERROR", script = r$script, line = r$line,
      check = "missing_producer",
      message = paste0("No scanned script (00-05, _source.R) writes '",
                       paste(r$basenames, collapse = " / "), "', read here via ",
                       r$func, "(). Not on the small raw/external allow-list either.")
    )
  } else {
    all_findings[[length(all_findings) + 1]] <- list(
      category = "DYNAMIC", script = r$script, line = r$line,
      check = "dynamic_filename",
      message = paste0(r$func, "(", r$dynamic_text, "): filename cannot be resolved ",
                       "statically; not automatically treated as an error.")
    )
    relationship_rows[[length(relationship_rows) + 1]] <- list(
      artifact = paste0("<dynamic: ", r$dynamic_text, ">"), producers = "(unresolved)",
      consumer = r$script, status = "DYNAMIC"
    )
  }
}

for (w in write_events) {
  if (!w$literal) {
    all_findings[[length(all_findings) + 1]] <- list(
      category = "DYNAMIC", script = w$script, line = w$line,
      check = "dynamic_filename",
      message = paste0(w$func, "(", w$dynamic_text, "): filename cannot be resolved ",
                       "statically; not automatically treated as an error.")
    )
  }
}

# ---------------------------------------------------------------------------
# Report
# ---------------------------------------------------------------------------

order_key <- function(f) match(f$script, scope_files)
all_findings <- all_findings[order(vapply(all_findings, order_key, integer(1)),
                                    vapply(all_findings, function(f) ifelse(is.na(f$line), 0L, f$line), integer(1)))]

print_group <- function(cat_name, label) {
  items <- Filter(function(f) f$category == cat_name, all_findings)
  cat("## ", label, " (", length(items), ")\n", sep = "")
  if (length(items) == 0) {
    cat("  (none)\n\n")
    return(invisible())
  }
  for (f in items) {
    loc <- if (is.na(f$line)) f$script else paste0(f$script, ":", f$line)
    cat("  [", loc, "] (", f$check, ") ", f$message, "\n", sep = "")
  }
  cat("\n")
}

cat("============================================================\n")
cat("FINDINGS\n")
cat("============================================================\n")
print_group("ERROR", "ERRORS")
print_group("WARNING", "WARNINGS")
print_group("DYNAMIC", "DYNAMIC (unresolved, non-failing)")

cat("============================================================\n")
cat("ARTIFACT MAP  (producer script -> artifact -> consumer script)\n")
cat("============================================================\n")
if (length(relationship_rows) == 0) {
  cat("  (no file-level relationships detected)\n\n")
} else {
  for (rr in relationship_rows) {
    cat("  ", rr$producers, " -> ", rr$artifact, " -> ", rr$consumer, "\n", sep = "")
  }
  cat("\n")
}

n_error <- sum(vapply(all_findings, function(f) f$category == "ERROR", logical(1)))
n_warn  <- sum(vapply(all_findings, function(f) f$category == "WARNING", logical(1)))
n_dyn   <- sum(vapply(all_findings, function(f) f$category == "DYNAMIC", logical(1)))

cat("============================================================\n")
cat("SUMMARY\n")
cat("============================================================\n")
cat("Scripts scanned : ", length(scope_files), "\n", sep = "")
cat("ERROR           : ", n_error, "\n", sep = "")
cat("WARNING         : ", n_warn, "\n", sep = "")
cat("DYNAMIC         : ", n_dyn, "\n", sep = "")

# ---------------------------------------------------------------------------
# CSV output (disposable, regenerated -- see tools/verification/.gitignore)
# ---------------------------------------------------------------------------

dir.create(REPORT_DIR, recursive = TRUE, showWarnings = FALSE)

findings_df <- data.frame(
  category = vapply(all_findings, `[[`, character(1), "category"),
  script   = vapply(all_findings, `[[`, character(1), "script"),
  line     = vapply(all_findings, function(f) ifelse(is.na(f$line), NA_integer_, f$line), integer(1)),
  check    = vapply(all_findings, `[[`, character(1), "check"),
  message  = vapply(all_findings, `[[`, character(1), "message"),
  stringsAsFactors = FALSE
)
write.csv(findings_df, file.path(REPORT_DIR, "check_static_findings.csv"), row.names = FALSE)

artifacts_df <- data.frame(
  artifact = vapply(relationship_rows, `[[`, character(1), "artifact"),
  producers = vapply(relationship_rows, `[[`, character(1), "producers"),
  consumer = vapply(relationship_rows, `[[`, character(1), "consumer"),
  status = vapply(relationship_rows, `[[`, character(1), "status"),
  stringsAsFactors = FALSE
)
write.csv(artifacts_df, file.path(REPORT_DIR, "check_static_artifacts.csv"), row.names = FALSE)

cat("\nCSV summaries written to ", REPORT_DIR, "/\n", sep = "")

if (!interactive()) {
  quit(save = "no", status = if (n_error > 0) 1L else 0L)
}
