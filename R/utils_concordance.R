# ============================================
# utils_concordance.R 
# ============================================

# Canonical 2×2 mapping (used everywhere):
#   a = MRD− / PET+   (NP, discordant)
#   b = MRD− / PET−   (NN, concordant)
#   c = MRD+ / PET−   (PN, discordant)
#   d = MRD+ / PET+   (PP, concordant)
#
#  Rows = MRD (neg, pos); Cols = PET (neg, pos)
#            PET−   PET+
# MRD−         b      a
# MRD+         c      d
#
# Outputs:
#   per_study: study_id, n, a,b,c,d, pct_agree, cells_source, imputed,
#              Kappa, SE, lower, upper, AC1, PABAK, McNemar_p, McNemar_midp,
#              dir_logOR, dir_se
#   pooled   : IV fixed-effect summary of κ with 95% CI
# ============================================

# ---------- small helpers ----------
if (!exists("%||%")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
  }
}

safe_num <- function(x) suppressWarnings(as.numeric(x))

.lr_counts <- function(p_vec, n) {
  # Largest-remainder integerization from % (a,b,c,d) to counts that sum exactly to n
  if (length(p_vec) != 4L) stop(".lr_counts expects length-4 numeric vector (a,b,c,d).")
  if (!is.finite(n) || n < 0) return(rep(NA_integer_, 4L))
  p <- pmax(0, as.numeric(p_vec))
  s <- sum(p)
  if (!is.finite(s) || s <= 0) return(c(0L, n, 0L, 0L))  # neutral fallback into NN
  scaled <- p * n / s
  base   <- floor(scaled)
  left   <- as.integer(round(n - sum(base)))
  if (left != 0L) {
    frac <- scaled - base
    ord  <- order(frac, decreasing = (left > 0L), method = "radix")
    i <- 1L
    while (left != 0L) {
      idx  <- ord[ ((i - 1L) %% 4L) + 1L ]
      step <- if (left > 0L) 1L else -1L
      if (!(base[idx] == 0L && step < 0L)) { base[idx] <- base[idx] + step; left <- left - step }
      i <- i + 1L
    }
  }
  pmax(as.integer(base), 0L)
}

scale_four_to_100 <- function(pa, pb, pc, pd, tol_sum = 1.0) {
  pa <- as.numeric(pa); pb <- as.numeric(pb); pc <- as.numeric(pc); pd <- as.numeric(pd)
  ok <- is.finite(pa) & is.finite(pb) & is.finite(pc) & is.finite(pd)
  if (!all(ok)) return(list(pa=pa, pb=pb, pc=pc, pd=pd))
  s <- pa + pb + pc + pd
  sc <- 100 / s
  pa2 <- pa * sc; pb2 <- pb * sc; pc2 <- pc * sc; pd2 <- pd * sc
  rem <- 100 - (pa2 + pb2 + pc2 + pd2)
  if (abs(rem) > 1e-8) {
    vec <- c(pa2, pb2, pc2, pd2)
    idx <- order(vec, decreasing = TRUE)[1]
    vec[idx] <- vec[idx] + rem
    pa2 <- vec[1]; pb2 <- vec[2]; pc2 <- vec[3]; pd2 <- vec[4]
  }
  list(pa=pa2, pb=pb2, pc=pc2, pd=pd2)
}

as_num_perc <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- trimws(x)
  x[x == ""] <- NA
  x <- gsub("%", "", x, fixed = TRUE)
  x <- gsub(",", ".", x, fixed = TRUE)  # 12,3 -> 12.3
  suppressWarnings(as.numeric(x))
}


# Largest-remainder integerization to sum exactly to n (no negatives)
rebalance_counts <- function(n, a, b, c, d) {
  n   <- as.integer(round(n %||% 0))
  vec <- as.numeric(c(a %||% 0, b %||% 0, c %||% 0, d %||% 0))
  if (!is.finite(n) || n < 0L) return(list(a=NA_integer_, b=NA_integer_, c=NA_integer_, d=NA_integer_))
  vec[!is.finite(vec)] <- 0
  vec <- pmin(pmax(vec, 0), n)
  s <- sum(vec)
  if (s == n) return(list(a=vec[1], b=vec[2], c=vec[3], d=vec[4]))
  if (s == 0) {
    out <- c(0,0,0,0); out[2] <- n  # drop mass into NN (neutral) to avoid bias (why)
    return(list(a=out[1], b=out[2], c=out[3], d=out[4]))
  }
  scaled <- vec * n / s
  base   <- floor(scaled)
  left   <- as.integer(round(n - sum(base)))
  if (left != 0) {
    frac <- scaled - base
    ord  <- order(frac, decreasing = (left > 0), method = "radix")
    i <- 1L
    while (left != 0) {
      idx <- ord[ ((i-1) %% length(base)) + 1 ]
      step <- if (left > 0) 1L else -1L
      if (!(base[idx] == 0 && step < 0)) { base[idx] <- base[idx] + step; left <- left - step }
      i <- i + 1L
    }
  }
  base <- pmax(base, 0L)
  list(a=base[1], b=base[2], c=base[3], d=base[4])
}

# Exact two-sided & mid-p McNemar using discordants (a vs c)
mcnemar_p_two_sided <- function(a, c) {
  n <- a + c
  ifelse(n <= 0, NA_real_, 2 * stats::pbinom(q = min(a, c), size = n, prob = 0.5))
}


# Gwet AC1 for 2×2 with our mapping
ac1_bin <- function(a,b,c,d){
  n <- a+b+c+d
  if (!is.finite(n) || n <= 0) return(NA_real_)
  # observed agreement = diagonal = b + d
  Po <- (b + d) / n
  p_neg_A <- (a + b)/n; p_pos_A <- 1 - p_neg_A   # MRD
  p_neg_B <- (b + c)/n; p_pos_B <- 1 - p_neg_B   # PET
  p_neg <- (p_neg_A + p_neg_B)/2; p_pos <- (p_pos_A + p_pos_B)/2
  Pe <- p_neg*(1-p_neg) + p_pos*(1-p_pos)
  den <- 1 - Pe
  if (den <= 0) return(NA_real_)
  (Po - Pe) / den
}

# ---------- internal guard: cross-check counts vs % (canonical) --------------
# Accepts tol_sum even if you don't use it internally
assert_canonical_from_perc <- function(df, tol_count = 1L, tol_sum = 1.0) {
  req <- c("a","b","c","d","n",
           "pct_mrdneg_petpos","pct_mrdneg_petneg","pct_mrdpos_petneg","pct_mrdpos_petpos")
  if (!all(req %in% names(df))) return(invisible(TRUE))
  
  P <- df[, c("pct_mrdneg_petpos","pct_mrdneg_petneg","pct_mrdpos_petneg","pct_mrdpos_petpos")]
  sums <- rowSums(P, na.rm = TRUE)
  
  # Only test rows with 4 finite %s that already sum ~100
  ok <- is.finite(sums) & abs(sums - 100) <= 1 &
    apply(is.finite(as.matrix(P)), 1, all)
  if (!any(ok)) return(invisible(TRUE))
  
  # Rebuild counts from % using your largest-remainder helper
  recon <- t(vapply(which(ok), function(i) .lr_counts(as.numeric(P[i, ]), df$n[i]), numeric(4)))
  
  diffs <- abs(recon - as.matrix(df[ok, c("a","b","c","d")]))
  bad <- which(rowSums(diffs > tol_count) > 0)
  
  if (length(bad)) {
    stop("Counts a/b/c/d disagree with canonical mapping from % at rows: ",
         paste(which(ok)[bad], collapse = ", "))
  }
  invisible(TRUE)
}


# ---------- main entry ----------
compute_concordance <- function(mrd_df, strict = FALSE, tol_sum = 1.0) {
  df <- janitor::clean_names(mrd_df)
  
  # n: prefer provided; else derive from counts if present
  if ("n_concordance" %in% names(df)) df$n <- df$n_concordance
  if (!"n" %in% names(df) && all(c("a","b","c","d") %in% names(df)))
    df$n <- safe_num(df$a) + safe_num(df$b) + safe_num(df$c) + safe_num(df$d)
  if (!"n" %in% names(df)) stop("Need an 'n' column (or 'n_concordance' / infer from counts).")
  df$n <- as.integer(round(df$n))
  
  # alias finders
  norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  nms <- names(df); nn <- norm(nms)
  find_col <- function(cands) {
    cands_n <- norm(cands); hit <- match(cands_n, nn, nomatch = 0); hit <- hit[hit > 0]
    if (length(hit)) nms[hit[1]] else NA_character_
  }
  has_col <- function(nm) !is.na(nm) && nzchar(nm) && nm %in% names(df)
  getcol  <- function(nm, as_pct = TRUE) {
    if (!has_col(nm)) return(rep(NA_real_, nrow(df)))
    v <- df[[nm]]
    if (as_pct) as_num_perc(v) else safe_num(v)
  }
  
  # % columns (a=NP, b=NN, c=PN, d=PP)
  col_a <- find_col(c("pct_mrdneg_petpos","mrdneg_petpos","pct_mrd_neg_pet_pos")) # NP
  col_b <- find_col(c("pct_mrdneg_petneg","mrdneg_petneg","pct_mrd_neg_pet_neg")) # NN
  col_c <- find_col(c("pct_mrdpos_petneg","mrdpos_petneg","pct_mrd_pos_pet_neg")) # PN
  col_d <- find_col(c("pct_mrdpos_petpos","mrdpos_petpos","pct_mrd_pos_pet_pos")) # PP
  col_petpct <- find_col(c("pet_pos_pct","pet_pos_percent","pct_pet_pos"))
  col_petn   <- find_col(c("pet_pos_n","n_pet_pos"))
  
  pa <- getcol(col_a)  # a% (NP)
  pb <- getcol(col_b)  # b% (NN)
  pc <- getcol(col_c)  # c% (PN)
  pd <- getcol(col_d)  # d% (PP)
  ppet <- getcol(col_petpct)          # PET+ %
  npet <- getcol(col_petn, FALSE)     # PET+ n
  
  prov <- rep("", nrow(df))
  imp  <- rep(FALSE, nrow(df))
  
  # If counts exist, take them (audit later)
  if (all(c("a","b","c","d") %in% names(df))) {
    df$a <- as.integer(round(safe_num(df$a)))
    df$b <- as.integer(round(safe_num(df$b)))
    df$c <- as.integer(round(safe_num(df$c)))
    df$d <- as.integer(round(safe_num(df$d)))
    prov[] <- "counts"
  } else {
    have_all4 <- all(c(!is.na(col_a), !is.na(col_b), !is.na(col_c), !is.na(col_d)))
    
    mk_adj <- function(n, a, b, c, d) {
      vv <- rebalance_counts(n, a, b, c, d)
      c(a = vv$a, b = vv$b, c = vv$c, d = vv$d)
    }
    
    if (strict) {
      if (!have_all4)
        stop("Strict mode needs all four % cells (a,b,c,d) or the four counts.")
      normed <- scale_four_to_100(pa, pb, pc, pd, tol_sum = tol_sum)
      a <- round(df$n * normed$pa/100)
      b <- round(df$n * normed$pb/100)
      c <- round(df$n * normed$pc/100)
      d <- round(df$n * normed$pd/100)
      reb <- mapply(mk_adj, df$n, a, b, c, d, SIMPLIFY = TRUE)
      df$a <- as.integer(reb["a",]); df$b <- as.integer(reb["b",])
      df$c <- as.integer(reb["c",]); df$d <- as.integer(reb["d",])
      prov[] <- "dual_4"; imp[] <- FALSE
    } else {
      # lenient: allow 3/4 + PET+ % or n; else 3/4 + remainder
      a <- if (!is.na(col_a)) as.integer(round(df$n * pa/100)) else rep(NA_integer_, nrow(df))
      b <- if (!is.na(col_b)) as.integer(round(df$n * pb/100)) else rep(NA_integer_, nrow(df))
      d <- if (!is.na(col_d)) as.integer(round(df$n * pd/100)) else rep(NA_integer_, nrow(df))
      
      pet_total <- rep(NA_real_, nrow(df))
      if (any(is.finite(ppet))) pet_total <- round(df$n * ppet/100)
      if (all(!is.finite(pet_total)) && any(is.finite(npet))) pet_total <- as.integer(round(npet))
      
      c_from_pet <- if (any(is.finite(pet_total)) && !is.na(col_a)) as.integer(round(pet_total - a)) else rep(NA_integer_, nrow(df))
      c_from_rem <- as.integer(round(df$n - (a + b + d)))
      use_pet <- is.finite(c_from_pet)
      c_est <- ifelse(use_pet, c_from_pet, c_from_rem)
      
      reb <- mapply(mk_adj, df$n, a, b, c_est, d, SIMPLIFY = TRUE)
      df$a <- as.integer(reb["a",]); df$b <- as.integer(reb["b",])
      df$c <- as.integer(reb["c",]); df$d <- as.integer(reb["d",])
      prov  <- ifelse(use_pet, "three_dual_pct+petpos", "three_dual_pct+remainder")
      imp[] <- TRUE
      
      # If all four % available, prefer that path (deterministic, not imputed)
      if (have_all4) {
        normed <- scale_four_to_100(pa, pb, pc, pd, tol_sum = tol_sum)
        a <- round(df$n * normed$pa/100); b <- round(df$n * normed$pb/100)
        c <- round(df$n * normed$pc/100); d <- round(df$n * normed$pd/100)
        reb <- mapply(mk_adj, df$n, a, b, c, d, SIMPLIFY = TRUE)
        df$a <- as.integer(reb["a",]); df$b <- as.integer(reb["b",])
        df$c <- as.integer(reb["c",]); df$d <- as.integer(reb["d",])
        prov[] <- "dual_4"; imp[] <- FALSE
      }
    }
  }
  
  # final checks
  if (any(df$a < 0 | df$b < 0 | df$c < 0 | df$d < 0, na.rm = TRUE))
    stop("Negative cell counts after reconstruction.")
  if (any((df$a + df$b + df$c + df$d) != df$n, na.rm = TRUE))
    stop("Row sums do not equal n after reconstruction.")
  
  # cross-check against canonical % when available
  assert_canonical_from_perc(df, tol_sum = tol_sum, tol_count = 1L)
  
  # per-study stats
  compute_one <- function(a, b, c, d, n) {
    # table for kappa: rows MRD (neg,pos), cols PET (neg,pos)
    tab <- matrix(c(b, a,  # MRD−: PET−=b, PET+=a
                    c, d), # MRD+: PET−=c, PET+=d
                  nrow = 2, byrow = TRUE)
    
    # Cohen's κ (DescTools if possible)
    kappa <- se <- lo <- hi <- NA_real_
    if (requireNamespace("DescTools", quietly = TRUE)) {
      tmp <- try(DescTools::CohenKappa(tab, conf.level = 0.95, w = "unweighted"), silent = TRUE)
      if (!inherits(tmp, "try-error")) {
        kappa <- suppressWarnings(as.numeric(tmp$kappa))
        ci <- tryCatch(as.numeric(tmp$conf.int),
                       error = function(e) tryCatch(as.numeric(tmp$confint), error=function(e2) c(NA_real_, NA_real_)))
        if (length(ci) == 2 && all(is.finite(ci))) {
          lo <- max(-1, ci[1]); hi <- min(1, ci[2]); se <- (hi - lo)/(2*1.96)
        }
      }
    }
    if (!is.finite(kappa)) {
      # fallback delta method (why: ensure κ even without DescTools)
      Po <- (b + d) / n
      p_mrd_neg <- (a + b) / n; p_mrd_pos <- (c + d) / n
      p_pet_neg <- (b + c) / n; p_pet_pos <- (a + d) / n
      pe  <- p_mrd_neg * p_pet_neg + p_mrd_pos * p_pet_pos
      den <- max(1e-8, 1 - pe)
      kappa <- (Po - pe) / den
      var_k <- (Po * (1 - Po)) / (den^2 * max(1, n))
      se    <- sqrt(var_k); if (!is.finite(se) || se == 0) se <- 1 / sqrt(max(1, n) * 4)
      lo <- max(-1, kappa - 1.96 * se); hi <- min(1, kappa + 1.96 * se)
    }
    
    Po    <- (b + d) / n
    PABAK <- 2*Po - 1
    AC1   <- ac1_bin(a,b,c,d)
    
    # Directional discordance: discordant vs discordant (a vs c)
    dir_logOR <- log((a + 0.5) / (c + 0.5))
    dir_se    <- sqrt(1/(a + 0.5) + 1/(c + 0.5))
    
    list(
      Kappa = kappa, SE = se, lower = lo, upper = hi,
      AC1 = AC1, PABAK = PABAK,
      McNemar_p = mcnemar_p_two_sided(a, c),
      McNemar_midp = mcnemar_midp(a, c),
      dir_logOR = dir_logOR, dir_se = dir_se
    )
  }
  
  a <- as.integer(df$a); b <- as.integer(df$b); c <- as.integer(df$c); d <- as.integer(df$d)
  n_vec <- as.integer(df$n)
  ok <- is.finite(n_vec) & n_vec > 0 & is.finite(a) & is.finite(b) & is.finite(c) & is.finite(d)
  if (!any(ok)) stop("No analysable rows: check a,b,c,d and n.")
  
  a <- a[ok]; b <- b[ok]; c <- c[ok]; d <- d[ok]; n_vec <- n_vec[ok]
  df_ok <- df[ok, , drop = FALSE]
  
  rows_list <- mapply(function(.a,.b,.c,.d,.n) compute_one(.a,.b,.c,.d,.n), a,b,c,d,n_vec, SIMPLIFY = FALSE)
  rows <- dplyr::bind_rows(lapply(rows_list, as.data.frame))
  
  labcol <- intersect(c("study_id","id","authors","title"), names(df_ok))
  study_lab <- if (length(labcol)) as.character(df_ok[[labcol[1]]]) else as.character(seq_len(nrow(df_ok)))
  
  per_study <- dplyr::bind_cols(
    tibble::tibble(
      study_id = study_lab, n = n_vec, a=a,b=b,c=c,d=d,
      pct_agree = (b + d) / n_vec * 100,
      cells_source = ifelse(all(prov == ""), "counts", prov[ok]),
      imputed = ifelse(all(imp == FALSE), FALSE, imp[ok])
    ),
    tibble::as_tibble(rows)
  )
  
  # IV fixed-effect pooled κ
  w <- 1 / (per_study$SE^2); w[!is.finite(w)] <- NA
  if (sum(is.finite(w)) > 0) {
    kappahat <- stats::weighted.mean(per_study$Kappa, w, na.rm = TRUE)
    se_hat   <- sqrt(1 / sum(w, na.rm = TRUE))
    ci_hat   <- kappahat + c(-1.96, 1.96) * se_hat
    ci_hat[1] <- max(-1, ci_hat[1]); ci_hat[2] <- min(1, ci_hat[2])
  } else {
    kappahat <- se_hat <- NA_real_; ci_hat <- c(NA_real_, NA_real_)
  }
  
  list(
    per_study = tibble::as_tibble(per_study),
    pooled = list(estimate = as.numeric(kappahat), se = as.numeric(se_hat),
                  ci_l = as.numeric(ci_hat[1]), ci_u = as.numeric(ci_hat[2]),
                  k = nrow(per_study))
  )
}

# ---------- convenience ----------
concordance_primary_only <- function(conc_per_study) {
  conc_per_study[isFALSE(conc_per_study$imputed) | is.na(conc_per_study$imputed), , drop = FALSE]
}

write_provenance_table <- function(conc_per_study, path = "tables/Provenance_Table2_Counts.csv") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(conc_per_study[, c("study_id","n","a","b","c","d","cells_source","imputed")], path)
  path
}

# ---- survival helpers (unchanged API; tolerant) ----
build_survival_df <- function(mrd_df) {
  has_lower <- "ci_lower" %in% names(mrd_df)
  has_upper <- "ci_upper" %in% names(mrd_df)
  janitor::clean_names(mrd_df) |>
    dplyr::filter(!is.na(pfs_dual)) |>
    dplyr::mutate(
      HR = safe_num(pfs_dual),
      clean_ci = stringr::str_replace_all(as.character(pfs_dual_ci), "[\u2013\u2014\u2212-]", "-"),
      CI_lower = dplyr::coalesce(safe_num(stringr::str_extract(clean_ci, "^[0-9.]+")), if (has_lower) safe_num(ci_lower) else NA_real_),
      CI_upper = dplyr::coalesce(safe_num(stringr::str_extract(clean_ci, "[0-9.]+$")), if (has_upper) safe_num(ci_upper) else NA_real_),
      logHR = log(HR),
      SE = (log(CI_upper) - log(CI_lower)) / (2 * 1.96),
      Study = as.character(study_id)
    ) |>
    dplyr::select(Study, HR, CI_lower, CI_upper, logHR, SE, dplyr::any_of("n")) |>
    dplyr::filter(is.finite(SE), is.finite(logHR))
}

meta_dualneg <- function(surv_df) {
  stopifnot(nrow(surv_df) >= 2)
  ma <- meta::metagen(TE = surv_df$logHR, seTE = surv_df$SE, studlab = surv_df$Study,
                      sm = "HR", method.tau = "DL", random = TRUE, common = TRUE)
  per_study <- tibble::tibble(
    Study = ma$studlab,
    HR = exp(ma$TE), CI_lower = exp(ma$lower), CI_upper = exp(ma$upper), SE = ma$seTE
  )
  pooled <- list(
    HR = exp(ma$TE.random), CI_lower = exp(ma$lower.random), CI_upper = exp(ma$upper.random),
    I2 = ma$I2, tau2 = ma$tau^2, Q = ma$Q
  )
  list(per_study = per_study, pooled = pooled)
}

add_props_for_meta_reg <- function(per_study_df, conc_per_study) {
  conc_min <- conc_per_study |>
    dplyr::select(study_id, n, a, b, c, d)
  per_study_df |>
    dplyr::left_join(conc_min, by = c("Study" = "study_id")) |>
    dplyr::mutate(
      prop_mrdneg_petpos = a / n,  # NP
      prop_dualneg      = b / n    # NN
    )
}

# ---- kappa-standardiser ---------------------------------------------------------
compute_kappa_ci <- function(a, b, c, d, conf.level = 0.95) {
  # continuity corrections only where needed
  cc <- function(z) if (z == 0) z + 0.5 else z
  a1 <- cc(a); b1 <- cc(b); c1 <- cc(c); d1 <- cc(d)
  
  mat <- matrix(c(b1, a1, c1, d1), 2, 2, byrow = TRUE)  # rows MRD-/MRD+, cols PET-/PET+
  ck <- suppressWarnings(try(DescTools::CohenKappa(mat, conf.level = conf.level), silent = TRUE))
  
  if (!inherits(ck, "try-error") && !is.null(ck$overall)) {
    kap <- as.numeric(ck$overall[1, "Kappa"])
    ci  <- as.numeric(ck$overall[1, c("lwr.ci", "upr.ci")])
    se  <- (ci[2] - ci[1]) / (2 * stats::qnorm(1 - (1 - conf.level)/2))
    return(list(kappa = kap, lcl = ci[1], ucl = ci[2], se = se, se_source = "from_descTools_ci"))
  }
  
  # Fallback: large-sample variance via Po/Pe (conservative near bounds)
  n   <- a + b + c + d
  po  <- (a + d) / n
  pe  <- (((a + b) * (a + c)) + ((c + d) * (b + d))) / (n * n)
  kap <- (po - pe) / (1 - pe)
  
  # Variance of kappa (Cohen 1960; Fleiss et al.) — simplified 2x2 form
  q1 <- pe
  q2 <- ((a + b) * (a + c) + (c + d) * (b + d)) / (n^2)
  var_k <- (po * (1 - po) / ((1 - pe)^2 * n)) +
    (2 * (1 - po) * (2 * po * pe - q2) / ((1 - pe)^3 * n)) +
    (((1 - po)^2) * (q1 - 3 * pe^2) / ((1 - pe)^4 * n))
  se <- sqrt(abs(var_k))
  
  z <- stats::qnorm(1 - (1 - conf.level)/2)
  ci <- c(kap - z * se, kap + z * se)
  
  list(kappa = kap, lcl = ci[1], ucl = ci[2], se = se, se_source = "approx_variance")
}

# ---- kappa-pooling --------------------------------------------------------------
pool_kappa <- function(kappa, se, slab = NULL) {
  if (!requireNamespace("metafor", quietly = TRUE)) stop("Install 'metafor'")
  metafor::rma(yi = kappa, sei = se, method = "REML", test = "knha", slab = slab)
}
