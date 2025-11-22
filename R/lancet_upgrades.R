# ======================================================================
# File: R/lancet_upgrades.R   (cleaned + small hardening tweaks)
# ======================================================================

suppressPackageStartupMessages({
  req <- c("metafor","meta","metasens","gt","ggplot2","dplyr","rlang","scales")
  have <- vapply(req, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
  if (!all(have)) {
    stop("Missing packages: ", paste(req[!have], collapse = ", "),
         ". Please install them (recommend using renv::restore()).")
  }
  invisible(lapply(req, function(p) suppressWarnings(suppressMessages(library(p, character.only = TRUE)))))
})

mcnemar_midp <- function(a, c) {
  a <- as.integer(a); c <- as.integer(c)
  if (anyNA(c(a,c)) || a < 0 || c < 0) return(NA_real_)
  n <- a + c; if (n == 0) return(NA_real_)
  k <- min(a, c)
  p_lower <- stats::pbinom(k, n, 0.5)
  p_upper <- stats::pbinom(k - 1, n, 0.5, lower.tail = FALSE)
  p <- 2 * min(p_lower, p_upper) - stats::dbinom(k, n, 0.5)
  max(min(p, 1), 0)
}

ac1_bin_est <- function(a,b,c,d, conf.level = 0.95) {
  a <- as.numeric(a); b <- as.numeric(b); c <- as.numeric(c); d <- as.numeric(d)
  n <- a + b + c + d
  if (anyNA(c(a,b,c,d)) || n <= 0) return(list(est=NA_real_, se=NA_real_, ci=c(NA,NA)))
  Po <- (b + d) / n
  pA_neg <- (a + b) / n; pB_neg <- (b + c) / n
  p_neg <- (pA_neg + pB_neg)/2; p_pos <- 1 - p_neg
  Pe <- p_neg*(1 - p_neg) + p_pos*(1 - p_pos)
  den <- (1 - Pe); if (den <= 0) return(list(est=NA_real_, se=NA_real_, ci=c(NA,NA)))
  ac1 <- (Po - Pe) / den
  dPo <- sqrt(Po * (1 - Po) / n)
  se <- abs(dPo / max(den, 1e-9))
  z <- stats::qnorm(1 - (1 - conf.level)/2)
  ci <- pmin(1, pmax(-1, ac1 + c(-1,1) * z * se))
  list(est=ac1, se=se, ci=ci)
}

pabak_ci <- function(a,b,c,d, conf.level=0.95) {
  a <- as.numeric(a); b <- as.numeric(b); c <- as.numeric(c); d <- as.numeric(d)
  n <- a + b + c + d; agr <- b + d
  if (anyNA(c(a,b,c,d)) || n <= 0) return(c(NA,NA,NA))
  po <- agr / n; pabak <- 2*po - 1
  alpha <- 1 - conf.level
  lo <- if (agr==0) 0 else stats::qbeta(alpha/2, agr, n-agr+1)
  hi <- if (agr==n) 1 else stats::qbeta(1 - alpha/2, agr+1, n-agr)
  ci <- pmin(1, pmax(-1, 2*c(lo,hi) - 1))
  c(est=pabak, lcl=ci[1], ucl=ci[2])
}

i2_ci <- function(Q, df, conf.level=0.95) {
  if (!is.finite(Q) || !is.finite(df) || df <= 0) return(c(NA,NA,NA))
  alpha <- 1 - conf.level
  q_lo <- stats::qchisq(1 - alpha/2, df)
  q_hi <- stats::qchisq(alpha/2, df)
  I2 <- max(0, (Q - df)/Q)
  I2_lo <- max(0, (Q - q_lo)/Q)
  I2_hi <- min(1, (Q - q_hi)/Q)
  c(i2=I2, lcl=I2_lo, ucl=I2_hi)
}

fmt_pi <- function(fit, digits=2) {
  pr <- try(metafor::predict(fit), silent=TRUE)
  if (inherits(pr,"try-error") || any(!is.finite(c(pr$pi.lb, pr$pi.ub)))) return("")
  sprintf("PI = %.*f to %.*f", digits, pr$pi.lb, digits, pr$pi.ub)
}

meta_kappa <- function(kappa, se) {
  keep <- is.finite(kappa) & is.finite(se) & se > 0
  if (sum(keep) < 2) return(invisible(NULL))
  fit <- metafor::rma.uni(yi = kappa[keep], sei = se[keep], method = "REML", test = "knha")
  Q <- fit$QE; df <- fit$k - 1; i2c <- i2_ci(Q, df)
  list(
    fit = fit,
    k = fit$k,
    pooled = c(est = as.numeric(fit$b), lcl = as.numeric(fit$ci.lb), ucl = as.numeric(fit$ci.ub)),
    tau2 = fit$tau2,
    I2 = c(est = fit$I2/100, lcl = i2c[2], ucl = i2c[3]),
    PI = fmt_pi(fit)
  )
}

report_kappa_meta <- function(kappa, se) {
  mk <- meta_kappa(kappa, se)
  if (is.null(mk)) return(invisible(NULL))
  sprintf("κ (REML, KnHa): %.3f (%.3f–%.3f); τ²=%.3f; I²=%.0f%% (%.0f–%.0f%%); %s.",
          mk$pooled["est"], mk$pooled["lcl"], mk$pooled["ucl"],
          mk$tau2, 100*mk$I2["est"], 100*mk$I2["lcl"], 100*mk$I2["ucl"],
          mk$PI)
}

meta_discordance <- function(dat, a="a", c="c", study="study_id", cc = 0.5) {
  stopifnot(all(c(a,c) %in% names(dat)))
  A <- as.numeric(dat[[a]]); C <- as.numeric(dat[[c]])
  S <- if (study %in% names(dat)) as.character(dat[[study]]) else paste0("row_", seq_along(A))
  yi <- log((A + cc)/(C + cc)); vi <- 1/(A + cc) + 1/(C + cc)
  keep <- is.finite(yi) & is.finite(vi) & vi > 0
  if (sum(keep) < 2) return(invisible(NULL))
  fit <- metafor::rma.uni(yi = yi[keep], vi = vi[keep], method = "REML", test = "knha")
  Q <- fit$QE; df <- fit$k - 1
  i2c <- i2_ci(Q, df)
  list(
    fit = fit,
    k = fit$k,
    pooled = c(est = as.numeric(fit$b), lcl = as.numeric(fit$ci.lb), ucl = as.numeric(fit$ci.ub)),
    tau2 = fit$tau2,
    I2 = c(est = fit$I2/100, lcl = i2c[2], ucl = i2c[3]),
    PI = fmt_pi(fit)
  )
}

discordance_cc_report <- function(df, a="a", c="c", grid=c(0,0.1,0.5)) {
  out <- lapply(grid, function(g) {
    md <- meta_discordance(df, a=a, c=c, cc=g)
    if (is.null(md)) return(NA_character_)
    sprintf("cc=%.1f → logOR %.2f (%.2f–%.2f); τ²=%.3f; I²=%.0f%% (%.0f–%.0f%%); %s",
            g, md$pooled["est"], md$pooled["lcl"], md$pooled["ucl"], md$tau2,
            100*md$I2["est"], 100*md$I2["lcl"], 100*md$I2["ucl"], md$PI)
  })
  paste(stats::na.omit(unlist(out)), collapse = " | ")
}

surv_bias_suite <- function(logHR, SE) {
  keep <- is.finite(logHR) & is.finite(SE) & SE > 0
  if (sum(keep) < 2) return(invisible(NULL))
  re <- metafor::rma.uni(yi = logHR[keep], sei = SE[keep], method="REML", test="knha")
  eg <- try(metafor::regtest(re, predictor="sei"), silent=TRUE)
  tf <- try(metafor::trimfill(re), silent=TRUE)
  lm <- try(metasens::limitmeta(meta::metagen(TE=logHR[keep], seTE=SE[keep], sm="HR",
                                              method.tau="REML", random=TRUE, hakn=TRUE)),
            silent=TRUE)
  list(fit=re,
       egger_p = if (!inherits(eg,"try-error")) as.numeric(eg$pval) else NA_real_,
       trimfill = tf,
       limitmeta = lm)
}

theme_lancet <- function(base_size = 9, base_family = "Helvetica") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(face="bold"),
      plot.title = ggplot2::element_text(face="bold"),
      plot.subtitle = ggplot2::element_text(margin=ggplot2::margin(t=2,b=6)),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face="bold")
    )
}

save_lancet <- function(plot, file,
                        layout = c("onecol","twocol"),
                        height_mm = 120,
                        dpi = 600,
                        bg = "white",
                        figure_type = c("line","combo","photo"), # accepted, ignored
                        ...) {
  layout <- match.arg(layout)
  if (!missing(figure_type)) figure_type <- match.arg(figure_type)
  width_mm <- if (layout=="onecol") 90 else 180
  ggplot2::ggsave(filename = file, plot = plot,
                  width = width_mm/25.4, height = height_mm/25.4,
                  units = "in", dpi = dpi, bg = bg, ...)
  invisible(normalizePath(file, mustWork = FALSE))  # <-- invisible
}


gt_lancet <- function(df, title = NULL, subtitle = NULL, note = NULL) {
  tab <- gt::gt(df) |>
    gt::opt_table_lines("none") |>
    gt::tab_options(data_row.padding = gt::px(4),
                    table.font.size = gt::px(10)) |>
    gt::fmt_number(where(is.numeric), decimals = 3)
  if (!is.null(title)) tab <- gt::tab_header(tab, title = gt::md(paste0("**", title, "**")),
                                             subtitle = if (!is.null(subtitle)) subtitle else NULL)
  if (!is.null(note))  tab <- gt::tab_source_note(tab, source_note = gt::md(note))
  tab
}
