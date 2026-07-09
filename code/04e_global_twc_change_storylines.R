# ============================================================================
# Classify global change storylines
#
# Output:
#   sim_id
#   global_change_storyline
#   flux_abs_change
#   avail_abs_change
# ============================================================================


# Inputs ======================================================================

source("source/twc_change.R")

mc_global_metrics_base <- readRDS(
  file.path(PATH_OUTPUT_DATA,
    "mc_global_metrics_base.Rds")
  )

# Constants & Variables =======================================================

SCENARIO_USE <- "base"

SIGN_EPS <- 1e-10

OUTPUT_FILE_RDS <- file.path(
  PATH_OUTPUT_DATA,
  "global_change_storylines_base.Rds"
)

GLOBAL_CHANGE_STORYLINE_LEVELS <- c(
  "wet_accelerated_p_up_e_up",
  "dry_accelerated_p_down_e_up",
  "dry_accelerated_p_up_e_up",
  "dry_decelerated_p_down_e_down",
  "dry_decelerated_p_down_e_up",
  "other"
)


# Functions ===================================================================

classify_global_change_storyline <- function(
    prec_abs_change,
    evap_abs_change,
    flux_abs_change,
    avail_abs_change
) {
  
  fcase(
    avail_abs_change > SIGN_EPS &
      flux_abs_change > SIGN_EPS &
      prec_abs_change > SIGN_EPS &
      evap_abs_change > SIGN_EPS,
    "wet_accelerated_p_up_e_up",
    
    avail_abs_change < -SIGN_EPS &
      flux_abs_change > SIGN_EPS &
      prec_abs_change < -SIGN_EPS &
      evap_abs_change > SIGN_EPS,
    "dry_accelerated_p_down_e_up",
    
    avail_abs_change < -SIGN_EPS &
      flux_abs_change > SIGN_EPS &
      prec_abs_change > SIGN_EPS &
      evap_abs_change > SIGN_EPS,
    "dry_accelerated_p_up_e_up",
    
    avail_abs_change < -SIGN_EPS &
      flux_abs_change < -SIGN_EPS &
      prec_abs_change < -SIGN_EPS &
      evap_abs_change < -SIGN_EPS,
    "dry_decelerated_p_down_e_down",
    
    avail_abs_change < -SIGN_EPS &
      flux_abs_change < -SIGN_EPS &
      prec_abs_change < -SIGN_EPS &
      evap_abs_change > SIGN_EPS,
    "dry_decelerated_p_down_e_up",
    
    default = "other"
  )
}


# Analysis ====================================================================

global_change_wide <- dcast(
  mc_global_metrics_base[scenario == SCENARIO_USE],
  sim + scenario ~ variable,
  value.var = "diff_2002_2021_minus_1982_2001"
)

setnames(
  global_change_wide,
  old = c("sim", "prec", "evap", "flux", "avail"),
  new = c(
    "sim_id",
    "prec_abs_change",
    "evap_abs_change",
    "flux_abs_change",
    "avail_abs_change"
  )
)

global_change_wide[, global_change_storyline := classify_global_change_storyline(
  prec_abs_change = prec_abs_change,
  evap_abs_change = evap_abs_change,
  flux_abs_change = flux_abs_change,
  avail_abs_change = avail_abs_change
)]

global_change_storylines <- global_change_wide[
  ,
  .(
    sim_id,
    global_change_storyline,
    flux_abs_change,
    avail_abs_change
  )
][
  order(sim_id)
]

global_change_storylines[, global_change_storyline := factor(
  global_change_storyline,
  levels = GLOBAL_CHANGE_STORYLINE_LEVELS
)]


# Outputs =====================================================================

saveRDS(
  global_change_storylines,
  OUTPUT_FILE_RDS
)

print(global_change_storylines)


# Validation ==================================================================

storyline_counts <- global_change_storylines[
  ,
  .(n_members = .N),
  by = global_change_storyline
][
  order(match(
    as.character(global_change_storyline),
    GLOBAL_CHANGE_STORYLINE_LEVELS
  ))
]

print(storyline_counts)

message("Saved RDS: ", OUTPUT_FILE_RDS)
