# ==============================================
# File: R/tables.R  (hardened)
# ==============================================

.path_join <- function(...) {
  if (requireNamespace("here", quietly = TRUE)) do.call(here::here, list(...)) else file.path(...)
}
.ensure_tables_dir <- function(){
  td <- .path_join("tables")
  if (!dir.exists(td)) dir.create(td, recursive = TRUE, showWarnings = FALSE)
  invisible(td)
}

excel_tbl <- function(df, file, sheet = "Sheet1"){
  .ensure_tables_dir()
  path <- .path_join("tables", file)
  if (!requireNamespace("writexl", quietly = TRUE)) {
    warning("writexl not installed; skipping Excel write for ", file)
    return(path)
  }
  writexl::write_xlsx(stats::setNames(list(df), sheet), path)
  path
}

safe_chr <- function(x, n) if (is.null(x)) rep(NA_character_, n) else as.character(x)
safe_num <- function(x, n) if (is.null(x)) rep(NA_real_, n) else suppressWarnings(as.numeric(x))

# ---- Canonical mapping helpers (DO NOT CHANGE) -------------------------------
.abcd_keys <- function() c(
  "pct_mrdneg_petpos",  # a: MRD− / PET+
  "pct_mrdneg_petneg",  # b: MRD− / PET−
  "pct_mrdpos_petneg",  # c: MRD+ / PET−
  "pct_mrdpos_petpos"   # d: MRD+ / PET+
)

# Build 2×2 with rows(MRD−,MRD+), cols(PET−,PET+).
# a=MRD−/PET+, b=MRD−/PET−, c=MRD+/PET−, d=MRD+/PET+.
.abcd_matrix <- function(a, b, c, d) {
  matrix(
    c(b, a,
      c, d),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("MRD−","MRD+"), c("PET−","PET+"))
  )
}

# Numeric close check (NA-safe)
.near <- function(x, y, tol = 1e-8) {
  both <- is.finite(x) & is.finite(y)
  out  <- rep(TRUE, length(x))                     # NA treated as pass (compare only when both finite)
  out[both] <- abs(x[both] - y[both]) <= tol
  out
}

# Recompute %agree, Cohen's kappa, McNemar p from a,b,c,d (per-row)
.recalc_concordance <- function(df) {
  a <- as.numeric(df$a); b <- as.numeric(df$b); c <- as.numeric(df$c); d <- as.numeric(df$d)
  n <- a + b + c + d
  bad <- !is.finite(n) | n <= 0
  
  po <- ifelse(bad, NA_real_, (b + d) / n)
  pct_agree <- 100 * po
  
  r1 <- a + b; r2 <- c + d
  c1 <- b + c; c2 <- a + d
  pe <- ifelse(bad, NA_real_, (r1 * c1 + r2 * c2) / (n^2))
  kappa <- (po - pe) / pmax(1 - pe, .Machine$double.eps)
  
  # Edwards continuity correction for McNemar
  cc <- ifelse(a + c > 0, (abs(a - c) - 1)^2 / (a + c), NA_real_)
  mcnemar_p <- ifelse(is.na(cc), NA_real_, stats::pchisq(cc, df = 1, lower.tail = FALSE))
  data.frame(pct_agree = pct_agree, Kappa = kappa, McNemar_p = mcnemar_p)
}

# Show a DataTable with just the table (no search box, no "show entries", no info bar)
show_table_clean <- function(df, caption = NULL) {
  if (!requireNamespace("DT", quietly = TRUE)) {
    return(knitr::kable(df, caption = caption))
  }
  DT::datatable(
    df,
    rownames = FALSE,
    caption = if (!is.null(caption))
      htmltools::tags$caption(style = "caption-side: top; text-align: left;", caption) else NULL,
    class = "compact stripe hover",
    options = list(dom = 't', paging = FALSE, searching = FALSE, lengthChange = FALSE, info = FALSE, ordering = TRUE, autoWidth = TRUE)
  )
}

emit_table <- function(df, caption, xlsx_name = NULL, sheet = "Sheet") {
  if (!is.null(xlsx_name)) excel_tbl(df, xlsx_name, sheet)
  print(show_table_clean(df, caption))   # important: print the widget
  invisible(df)
}

# -------------------------
# Table 1 — Study features
# -------------------------
table1_characteristics <- function(mrd_df){
  df <- janitor::clean_names(mrd_df)
  n_rows <- nrow(df)
  
  pick_chr <- function(primary, fallback) {
    p <- if (is.null(primary)) rep(NA_character_, n_rows) else as.character(primary)
    f <- if (is.null(fallback)) rep(NA_character_, n_rows) else as.character(fallback)
    ifelse(!is.na(p) & nzchar(p), p, f)
  }
  pick1_vec <- function(..., .n) {  # same helper you used in the Rmd
    xs <- list(...)
    for (i in seq_along(xs)) {
      if (is.null(xs[[i]])) xs[[i]] <- rep(NA_character_, .n)
      xs[[i]] <- as.character(xs[[i]])
      if (length(xs[[i]]) == 0) xs[[i]] <- rep(NA_character_, .n)
      if (length(xs[[i]]) == 1) xs[[i]] <- rep(xs[[i]][1], .n)
    }
    out <- xs[[1]]
    if (length(xs) > 1) for (i in 2:length(xs)) {
      idx <- is.na(out) | !nzchar(out); out[idx] <- xs[[i]][idx]
    }
    out
  }
  
  # --- HR available: robust to column name variants ---
  hr_num <- suppressWarnings(as.numeric(
    dplyr::coalesce(df$pfs_dual_neg, df$pfs_hr, df$hr)
  ))
  hr_ci  <- pick1_vec(df$pfs_dual_neg_ci, df$pfs_ci, df$ci, .n = n_rows)
  has_ci <- ifelse(is.na(hr_ci), FALSE, grepl("[0-9]", hr_ci))
  hr_available <- ifelse(is.finite(hr_num) | has_ci, "Yes", "No/NA")
  
  out <- tibble::tibble(
    Study            = pick_chr(df$study_id, pick_chr(df$study, pick_chr(df$authors, df$title))),
    Year             = suppressWarnings(as.numeric(df$year)),
    N                = suppressWarnings(as.numeric(df$n)),
    `MRD platform`   = pick_chr(df$mrd_method, df$mrd_assay),
    `PET criteria`   = pick_chr(df$pet_interpretation, df$pet_deauville_below_4),
    `Simultaneous MRD and PET/CT Assessments` =
      pick_chr(df$simultaneous_mrd_pet, df$simultaneous_mrd_and_pet),
    `HR available`   = hr_available
  )
  
  # write files
  excel_tbl(out, "Table_1_Characteristics.xlsx", "Table 1")
  .ensure_tables_dir()
  readr::write_csv(out, .path_join("tables", "Table_1_Characteristics.csv"))
  
}


# ----------------------------------------
# Table 2 — Concordance (counts + metrics)
# ----------------------------------------
table2_concordance <- function(conc_per_study,
                               file_excel = "Table_2_Concordance_Augmented.xlsx",
                               file_csv   = "Table2_Kappa_Concordance_Analysis.csv",
                               include_ac1_pabak = TRUE){
  # Strong schema check
  req <- c("study_id","n","a","b","c","d")
  if (!all(req %in% names(conc_per_study))) {
    stop("table2_concordance: conc_per_study must have columns: ", paste(req, collapse = ", "))
  }
  # Ensure numeric counts
  conc_per_study <- conc_per_study %>%
    dplyr::mutate(
      a = as.numeric(.data$a),
      b = as.numeric(.data$b),
      c = as.numeric(.data$c),
      d = as.numeric(.data$d),
      n = as.numeric(.data$n)
    )
  
  tot <- conc_per_study$a + conc_per_study$b + conc_per_study$c + conc_per_study$d
  mismatch <- is.finite(conc_per_study$n) & is.finite(tot) & (conc_per_study$n != tot)
  if (any(mismatch, na.rm = TRUE)) {
    if (isTRUE(as.logical(Sys.getenv("ALLOW_N_MISMATCH", "FALSE")))) {
      conc_per_study$n[which(mismatch)] <- tot[which(mismatch)]
      warning("table2_concordance: corrected n to a+b+c+d for some rows.")
    } else {
      stop("table2_concordance: n != a+b+c+d for some rows. Set ALLOW_N_MISMATCH=1 to auto-correct.")
    }
  }
  
  # Recompute from canonical mapping
  recomputed <- .recalc_concordance(conc_per_study)
  
  # Compare only where both are finite; otherwise fill from recomputed
  if ("pct_agree" %in% names(conc_per_study)) {
    idx <- is.finite(conc_per_study$pct_agree) & is.finite(recomputed$pct_agree)
    if (any(!.near(conc_per_study$pct_agree[idx], recomputed$pct_agree[idx], tol = 1e-6))) {
      stop("pct_agree mismatch with canonical a/b/c/d mapping; recompute upstream using the same orientation.")
    }
    conc_per_study$pct_agree <- dplyr::coalesce(conc_per_study$pct_agree, recomputed$pct_agree)
  } else conc_per_study$pct_agree <- recomputed$pct_agree
  
  if ("Kappa" %in% names(conc_per_study)) {
    idx <- is.finite(conc_per_study$Kappa) & is.finite(recomputed$Kappa)
    if (any(!.near(conc_per_study$Kappa[idx], recomputed$Kappa[idx], tol = 1e-6))) {
      stop("Kappa mismatch with canonical a/b/c/d mapping; recompute upstream using the same orientation.")
    }
    conc_per_study$Kappa <- dplyr::coalesce(conc_per_study$Kappa, recomputed$Kappa)
  } else conc_per_study$Kappa <- recomputed$Kappa
  
  if ("McNemar_p" %in% names(conc_per_study)) {
    idx <- is.finite(conc_per_study$McNemar_p) & is.finite(recomputed$McNemar_p)
    # Allow small numeric deltas; mcnemar.test vs closed form can differ slightly
    if (any(!.near(conc_per_study$McNemar_p[idx], recomputed$McNemar_p[idx], tol = 5e-3))) {
      stop("McNemar_p mismatch with canonical a/b/c/d mapping; recompute upstream using the same orientation.")
    }
    conc_per_study$McNemar_p <- dplyr::coalesce(conc_per_study$McNemar_p, recomputed$McNemar_p)
  } else conc_per_study$McNemar_p <- recomputed$McNemar_p
  
  # Optional: AC1 and PABAK (+CIs) if helper functions exist
  if (isTRUE(include_ac1_pabak)) {
    has_ac1   <- exists("ac1_bin",           mode = "function", inherits = TRUE)
    has_ac1ci <- exists("ac1_bin_est",       mode = "function", inherits = TRUE)
    has_pbci  <- exists("pabak_ci",          mode = "function", inherits = TRUE)
    
    if (has_ac1) {
      conc_per_study$AC1 <- mapply(function(a,b,c,d) ac1_bin(a,b,c,d),
                                   conc_per_study$a, conc_per_study$b, conc_per_study$c, conc_per_study$d)
    }
    if (has_ac1ci) {
      lst <- mapply(function(a,b,c,d) ac1_bin_est(a,b,c,d),
                    conc_per_study$a, conc_per_study$b, conc_per_study$c, conc_per_study$d, SIMPLIFY = FALSE)
      conc_per_study$AC1_se  <- vapply(lst, function(x) x$se %||% NA_real_, numeric(1))
      conc_per_study$AC1_lcl <- vapply(lst, function(x) if (is.null(x$ci)) NA_real_ else x$ci[1], numeric(1))
      conc_per_study$AC1_ucl <- vapply(lst, function(x) if (is.null(x$ci)) NA_real_ else x$ci[2], numeric(1))
    }
    if (has_pbci) {
      pb <- t(mapply(pabak_ci, conc_per_study$a, conc_per_study$b, conc_per_study$c, conc_per_study$d))
      suppressWarnings({
        conc_per_study$PABAK     <- 2 * (conc_per_study$pct_agree / 100) - 1
        conc_per_study$PABAK_lcl <- as.numeric(pb[, "lcl"])
        conc_per_study$PABAK_ucl <- as.numeric(pb[, "ucl"])
      })
    }
  }
  
  out <- conc_per_study %>%
    dplyr::mutate(
      pct_agree = round(.data$pct_agree, 1),
      Kappa     = round(.data$Kappa, 3),
      #SE        = if ("SE" %in% names(conc_per_study)) round(.data$SE, 3) else NA_real_,
      McNemar_p = signif(.data$McNemar_p, 3)
    ) %>%
    dplyr::rename(
      Study        = .data$study_id,
      `MRD-/PET+`  = .data$a,
      `MRD-/PET-`  = .data$b,
      `MRD+/PET-`  = .data$c,
      `MRD+/PET+`  = .data$d
    ) %>%
    dplyr::select(
      .data$Study, .data$n,
      `MRD-/PET+`, `MRD-/PET-`, `MRD+/PET-`, `MRD+/PET+`,
      .data$pct_agree, .data$Kappa, .data$McNemar_p,
      dplyr::any_of(c("AC1","AC1_se","AC1_lcl","AC1_ucl","PABAK","PABAK_lcl","PABAK_ucl"))
    )
  
  excel_tbl(out, file_excel, "Table 2")
  .ensure_tables_dir()
  readr::write_csv(out, .path_join("tables", file_csv))
  invisible(out)
}

# ---------------------------------------------------
# Table 3 — Subgroup outcomes (as reported per study)
# ---------------------------------------------------
parse_ci2 <- function(x){
  if (is.null(x) || is.na(x) || !nzchar(x)) return(c(NA_real_, NA_real_))
  s <- gsub("[^0-9eE+\\-.,–— ]", " ", x)
  s <- gsub("[,]", ".", s)
  parts <- unlist(strsplit(s, "[–—-]"))
  nums <- suppressWarnings(as.numeric(trimws(parts)))
  nums <- nums[is.finite(nums)]
  if (length(nums) < 2) return(c(NA_real_, NA_real_))
  lo <- min(nums[1], nums[2]); hi <- max(nums[1], nums[2])
  c(lo, hi)
}

table3_subgroup_outcomes <- function(mrd_df){
  cols <- c("study_id","hr_mrdpos_petpos","ci_mpp",
            "hr_mrdpos_petneg","ci_mpn",
            "hr_mrdneg_petpos","ci_mnp")
  for (nm in cols) if (!nm %in% names(mrd_df)) mrd_df[[nm]] <- NA
  
  out <- mrd_df %>%
    dplyr::select(dplyr::all_of(cols)) %>%
    dplyr::rename(
      Study                 = .data$study_id,
      `MRD+/PET+ HR`        = .data$hr_mrdpos_petpos,
      `MRD+/PET+ 95% CI`    = .data$ci_mpp,
      `MRD+/PET- HR`        = .data$hr_mrdpos_petneg,
      `MRD+/PET- 95% CI`    = .data$ci_mpn,
      `MRD-/PET+ HR`        = .data$hr_mrdneg_petpos,
      `MRD-/PET+ 95% CI`    = .data$ci_mnp
    )
  
  excel_tbl(out, "Table_3_Subgroup_Outcomes.xlsx", "Table 3")
  invisible(out)
}

# ---------------------------------------------------------
# Table 4 — Meta-regression (S6) writer, if tbl_mr provided
# ---------------------------------------------------------
table4_meta_regression <- function(tbl_mr,
                                   file_xlsx = "Supplementary_Table_S6_MetaRegression.xlsx",
                                   sheet     = "S6 Meta-Regression"){
  if (missing(tbl_mr) || !is.data.frame(tbl_mr) || !nrow(tbl_mr)) return(invisible(NULL))
  excel_tbl(tbl_mr, file_xlsx, sheet)
  invisible(tbl_mr)
}

# ---------------------------
# Table 5 — MRI placeholder
# ---------------------------
table5_mri_evidence <- function(){ invisible(NULL) }

# ---------------------------------------
# Convenience: export the 3 main tables
# ---------------------------------------
export_all_tables_xlsx <- function(mrd_df, conc_per_study, file = "All_Tables.xlsx"){
  .ensure_tables_dir()
  t1 <- table1_characteristics(mrd_df)
  t2 <- table2_concordance(conc_per_study)
  t3 <- table3_subgroup_outcomes(mrd_df)
  sheets <- list("Table 1" = t1, "Table 2" = t2, "Table 3" = t3)
  if (!requireNamespace("writexl", quietly = TRUE)) {
    warning("writexl not installed; cannot write combined workbook: ", file)
    return(invisible(.path_join("tables", file)))
  }
  writexl::write_xlsx(sheets, .path_join("tables", file))
  invisible(.path_join("tables", file))
}

# =========================================
# Supplementary Table S1 — Fréchet–Hoeffding κ bounds
# =========================================
if (is_df(fh_tbl)) {
  DT::datatable(
    fh_tbl %>%
      transmute(
        Study,
        `FH κ bounds` = sprintf("[%.3f, %.3f]", kappa_min, kappa_max),
        `FH agreement bounds` = sprintf("[%.1f%%, %.1f%%]", 100*po_min, 100*po_max)
      ),
    options = list(pageLength = 10, dom = "Bfrtip", buttons = c("copy","csv","excel")),
    extensions = "Buttons",
    caption = htmltools::tags$caption(style="caption-side: top; text-align: left;",
                                      htmltools::HTML("<b>Table S1.</b> Fréchet–Hoeffding bounds for agreement and Cohen’s κ (no imputation)."))
  )
} else {
  cat("*Table S1 omitted: could not compute FH bounds.*")
}