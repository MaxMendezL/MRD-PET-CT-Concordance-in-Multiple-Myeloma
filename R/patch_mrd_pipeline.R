# ------------------------------------------------------------
# Canonical 2×2 mapping used everywhere in this pipeline
#   a = MRD− / PET+   (discordant)
#   b = MRD− / PET−   (concordant dual-negative)
#   c = MRD+ / PET−   (discordant)
#   d = MRD+ / PET+   (concordant dual-positive)
#
# Contingency table (rows = MRD status; cols = PET status):
#              PET−     PET+
# MRD−           b        a
# MRD+           c        d
# ------------------------------------------------------------

`%||%` <- function(a,b) if (is.null(a) || length(a)==0 || (length(a)==1 && is.na(a))) b else a

# ---- Validation: enforce canonical mapping & sane counts ---------------------
.assert_cols <- function(df, cols) {
  miss <- setdiff(cols, names(df))
  if (length(miss)) stop("Missing required columns: ", paste(miss, collapse=", "))
  invisible(TRUE)
}

.is_integerish <- function(x, tol = 1e-8) {
  x_ok <- is.finite(x) & x >= 0 & abs(x - round(x)) <= tol
  x_ok | is.na(x)
}

assert_abcd <- function(df) {
  .assert_cols(df, c("a","b","c","d"))
  a <- suppressWarnings(as.numeric(df$a))
  b <- suppressWarnings(as.numeric(df$b))
  c <- suppressWarnings(as.numeric(df$c))
  d <- suppressWarnings(as.numeric(df$d))
  if (any(!(is.finite(a+b+c+d) | is.na(a+b+c+d)))) stop("Non-finite values in a/b/c/d.")
  if (any(!.is_integerish(a)) || any(!.is_integerish(b)) ||
      any(!.is_integerish(c)) || any(!.is_integerish(d))) {
    stop("a/b/c/d must be non-negative integer-like counts.")
  }
  if ("n" %in% names(df)) {
    n <- suppressWarnings(as.numeric(df$n))
    sum_across <- a + b + c + d
    bad <- is.finite(n) & is.finite(sum_across) & (n != sum_across)
    if (any(bad, na.rm = TRUE)) stop("n must equal a+b+c+d for all rows.")
  }
  invisible(TRUE)
}

# ---- Core helpers ------------------------------------------------------------
# Fixed-orientation 2×2 (rows: MRD−,MRD+; cols: PET−,PET+)
.tab2x2 <- function(a,b,c,d) {
  matrix(c(b, a,
           c, d),
         nrow = 2, byrow = TRUE,
         dimnames = list(MRD = c("Negative","Positive"),
                         PET = c("Negative","Positive")))
}

# AC1 (Gwet) for 2×2 with canonical ordering
ac1_bin <- function(a,b,c,d){
  n <- a+b+c+d
  if (!is.finite(n) || n <= 0) return(NA_real_)
  Po <- (b + d) / n
  p_neg_A <- (a + b)/n; p_pos_A <- 1 - p_neg_A            # MRD
  p_neg_B <- (b + c)/n; p_pos_B <- 1 - p_neg_B            # PET
  p_neg <- (p_neg_A + p_neg_B)/2; p_pos <- (p_pos_A + p_pos_B)/2
  Pe <- p_neg*(1-p_neg) + p_pos*(1-p_pos)
  if ((1-Pe) <= 0) return(NA_real_)
  (Po - Pe) / (1 - Pe)
}

# ---- Concordance computations (per-row + pooled) ----------------------------
compute_concordance <- function(df) {
  .assert_cols(df, c("a","b","c","d"))
  if (!requireNamespace("DescTools", quietly = TRUE)) stop("Need {DescTools}")
  
  x <- df
  numify <- function(z) suppressWarnings(as.numeric(z))
  x$a <- numify(x$a); x$b <- numify(x$b); x$c <- numify(x$c); x$d <- numify(x$d)
  x$n <- numify(x$n %||% (x$a + x$b + x$c + x$d))
  
  assert_abcd(x)  # enforce mapping & counts sanity
  
  # Build tables in fixed orientation
  tab_list <- mapply(function(b,a,c,d) .tab2x2(a=a,b=b,c=c,d=d), x$b, x$a, x$c, x$d, SIMPLIFY = FALSE)
  
  # Cohen's kappa + CI; note: DescTools::CohenKappa default is unweighted (nominal)
  ck <- lapply(tab_list, function(T) suppressWarnings(DescTools::CohenKappa(T, conf.level = 0.95)))
  kappa <- vapply(ck, function(z) suppressWarnings(as.numeric(z[["kappa"]])), numeric(1))
  ci_l  <- vapply(ck, function(z){ci <- suppressWarnings(as.numeric(z[["conf.int"]])); if (length(ci)==2) ci[1] else NA_real_}, numeric(1))
  ci_u  <- vapply(ck, function(z){ci <- suppressWarnings(as.numeric(z[["conf.int"]])); if (length(ci)==2) ci[2] else NA_real_}, numeric(1))
  se    <- ifelse(is.finite(ci_l) & is.finite(ci_u), (ci_u - ci_l)/(2*1.96), NA_real_)
  
  # McNemar p (guard when a+c==0)
  mcnemar_p <- vapply(seq_along(tab_list), function(i) {
    T <- tab_list[[i]]
    a <- x$a[i]; c <- x$c[i]
    if (!is.finite(a) || !is.finite(c) || (a + c) == 0) return(NA_real_)
    suppressWarnings(as.numeric(stats::mcnemar.test(T)$p.value))
  }, numeric(1))
  
  # Per-row metrics from canonical mapping
  per <- transform(
    x,
    pct_agree   = 100 * (b + d) / n,
    PABAK       = 2 * (pct_agree/100) - 1,
    AC1         = mapply(ac1_bin, a,b,c,d),
    Kappa       = kappa,
    lower       = ci_l,
    upper       = ci_u,
    SE          = se,
    McNemar_p   = mcnemar_p,
    logOR       = log((a + 0.5) / (c + 0.5)),          # directional discordance a vs c
    se_logOR    = sqrt(1/(a + 0.5) + 1/(c + 0.5)),
    ci_logOR_l  = logOR - 1.96*se_logOR,
    ci_logOR_u  = logOR + 1.96*se_logOR,
    PET_pos_cnt = a + d, PET_neg_cnt = b + c,
    MRD_neg_cnt = a + b, MRD_pos_cnt = c + d
  )
  
  # Pooled kappa (inverse-variance fixed effects on rows with finite SE)
  keep <- is.finite(per$Kappa) & is.finite(per$SE) & per$SE > 0
  pooled <- if (any(keep)) {
    w  <- 1 / (per$SE[keep]^2)
    est <- sum(w * per$Kappa[keep]) / sum(w)
    se_p <- sqrt(1 / sum(w))
    ci   <- est + c(-1.96, 1.96) * se_p
    list(estimate = est, ci_l = max(-1, ci[1]), ci_u = min(1, ci[2]), k = sum(keep))
  } else {
    list(estimate = suppressWarnings(mean(per$Kappa, na.rm = TRUE)), ci_l = NA_real_, ci_u = NA_real_, k = sum(keep))
  }
  
  list(per_study = per, pooled = pooled)
}

add_concordance_derivatives <- function(df){
  .assert_cols(df, c("a","b","c","d"))
  numify <- function(z) suppressWarnings(as.numeric(z))
  out <- dplyr::mutate(
    df,
    a = numify(a), b = numify(b), c = numify(c), d = numify(d),
    n = numify(n %||% (a+b+c+d))
  )
  assert_abcd(out)
  dplyr::mutate(
    out,
    pct_agree   = 100 * (b + d) / n,
    PET_pos_cnt = a + d, PET_neg_cnt = b + c,
    MRD_neg_cnt = a + b, MRD_pos_cnt = c + d,
    logOR       = log((a + 0.5)/(c + 0.5)),
    se_logOR    = sqrt(1/(a + 0.5) + 1/(c + 0.5)),
    PABAK       = 2*(pct_agree/100) - 1,
    AC1         = mapply(ac1_bin, a,b,c,d)
  )
}

mk_table <- function(a,b,c,d){
  stopifnot(all(sapply(list(a,b,c,d), function(z) length(z)==1)))
  .tab2x2(a=a,b=b,c=c,d=d)
}
