# survival_utils.R
# Utilities to (1) enforce a consistent survival contrast orientation
# and (2) compute log(HR) and its SE for random-effects meta-analysis.

# ---------------------------------------------------------------------
# orient_survival_hr()
# ---------------------------------------------------------------------
# Enforce that HR < 1 consistently means "dual-negative better".
# If a study reports the reverse contrast (dual-negative is NOT the
# reference), invert HR and its CI.
#
# Expected columns (customisable via args):
#   hr_col:  hazard ratio (numeric, >0)
#   lcl_col: lower 95% CI  (numeric, >0)
#   ucl_col: upper 95% CI  (numeric, >0)
#   ref_flag_col: logical flag; TRUE if dual-negative is the reference
#                 (if absent, we assume TRUE and emit a warning)
#
# Returns the input data.frame with (potentially) inverted HR/LCL/UCL
# and a normalised logical column 'dual_negative_is_reference'.

orient_survival_hr <- function(df,
                               hr_col  = "hr",
                               lcl_col = "lcl",
                               ucl_col = "ucl",
                               ref_flag_col = "dual_negative_is_reference") {
  stopifnot(is.data.frame(df))
  
  # Basic column existence checks
  need <- c(hr_col, lcl_col, ucl_col)
  miss <- setdiff(need, names(df))
  if (length(miss)) {
    stop("Missing required columns in data: ", paste(miss, collapse = ", "))
  }
  
  # Ensure numeric
  for (nm in need) {
    df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  }
  
  # Add/normalise the reference flag
  if (!ref_flag_col %in% names(df)) {
    warning("No '", ref_flag_col, "' column detected; assuming HRs already use ",
            "dual-negative as reference (no inversion applied).")
    df[[ref_flag_col]] <- TRUE
  } else {
    df[[ref_flag_col]] <- as.logical(df[[ref_flag_col]])
  }
  
  # Which rows must be inverted (reverse contrast)?
  flip <- is.finite(df[[hr_col]]) & df[[hr_col]] > 0 & !isTRUE(df[[ref_flag_col]])
  
  if (any(flip, na.rm = TRUE)) {
    hr  <- df[[hr_col]]
    lcl <- df[[lcl_col]]
    ucl <- df[[ucl_col]]
    
    # Invert HR and swap/invert CI bounds
    df[[hr_col]][flip]  <- 1 / hr[flip]
    df[[lcl_col]][flip] <- 1 / ucl[flip]
    df[[ucl_col]][flip] <- 1 / lcl[flip]
    
    # After inversion, flag is TRUE (now referenced to dual-negative)
    df[[ref_flag_col]][flip] <- TRUE
  }
  
  df
}

# ---------------------------------------------------------------------
# add_loghr_se()
# ---------------------------------------------------------------------
# Compute log(HR) (yi) and its standard error (sei) from HR and 95% CI.
# z is based on conf.level; default 0.95.
#
# Returns df with columns:
#   logHR (yi), SE (sei); preserves original hr/lcl/ucl.

add_loghr_se <- function(df,
                         hr_col  = "hr",
                         lcl_col = "lcl",
                         ucl_col = "ucl",
                         conf.level = 0.95) {
  stopifnot(is.data.frame(df))
  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  
  need <- c(hr_col, lcl_col, ucl_col)
  miss <- setdiff(need, names(df))
  if (length(miss)) {
    stop("Missing required columns in data: ", paste(miss, collapse = ", "))
  }
  
  for (nm in need) df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  
  # Guard against non-positive bounds
  bad <- !is.finite(df[[hr_col]]) | !is.finite(df[[lcl_col]]) | !is.finite(df[[ucl_col]]) |
    df[[hr_col]] <= 0 | df[[lcl_col]] <= 0 | df[[ucl_col]] <= 0
  if (any(bad, na.rm = TRUE)) {
    warning("Some rows have non-finite or non-positive HR/CI; producing NA yi/se for those rows.")
  }
  
  logHR <- rep(NA_real_, nrow(df))
  SE    <- rep(NA_real_, nrow(df))
  
  ok <- !bad
  if (any(ok, na.rm = TRUE)) {
    logHR[ok] <- log(df[[hr_col]][ok])
    SE[ok]    <- (log(df[[ucl_col]][ok]) - log(df[[lcl_col]][ok])) / (2 * z)
  }
  
  df$logHR <- logHR
  df$SE    <- SE
  df
}

# ---------------------------------------------------------------------
# prepare_survival_meta()
# ---------------------------------------------------------------------
# One-call convenience:
#  * Enforce orientation (HR<1 = dual-negative better)
#  * Compute logHR and SE for meta-analysis (yi/sei)
#
# Returns: data.frame with added columns 'logHR' and 'SE'.

prepare_survival_meta <- function(df,
                                  hr_col  = "hr",
                                  lcl_col = "lcl",
                                  ucl_col = "ucl",
                                  ref_flag_col = "dual_negative_is_reference",
                                  conf.level = 0.95) {
  df1 <- orient_survival_hr(df, hr_col, lcl_col, ucl_col, ref_flag_col)
  df2 <- add_loghr_se(df1, hr_col, lcl_col, ucl_col, conf.level)
  # Metafor-friendly aliases
  df2$yi  <- df2$logHR
  df2$sei <- df2$SE
  df2
}
