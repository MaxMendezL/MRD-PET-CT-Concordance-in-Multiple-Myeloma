# File: R/figures.R
if (!exists("%||%")) {
  `%||%` <- function(a,b) if (is.null(a) || length(a)==0 || (length(a)==1 && is.na(a))) b else a
}

# Save a ggplot to /figs (PNG only)
save_plot <- function(p, filename, width = 6, height = 5, dpi = 600){
  if (!requireNamespace("here", quietly = TRUE)) stop("Package 'here' is required.")
  dir.create(here::here("figs"), recursive = TRUE, showWarnings = FALSE)
  fn <- if (!grepl("[.]png$", filename, ignore.case = TRUE))
    paste0(tools::file_path_sans_ext(filename), ".png") else filename
  path <- here::here("figs", fn)
  ggplot2::ggsave(filename = path, plot = p, width = width, height = height, units = "in", dpi = dpi, bg = "white")
  invisible(path)
}


# ---- lancet-themes -------------------------------------------------------------
# Minimal, house-style base theme (Helvetica; quiet fallback to sans)
theme_lancet_base <- function(base_size = 10,
                              base_family = "Helvetica",
                              legend_position = "right") {
  if (!grepl("Helvetica", base_family) && !capabilities("cairo")) {
    base_family <- "sans"  # fallback if Helvetica/cairo not available
  }
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(hjust = 0, face = "bold", margin = ggplot2::margin(b = 6)),
      plot.subtitle   = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(b = 6)),
      plot.caption    = ggplot2::element_text(hjust = 0, size = base_size - 2, color = "grey30"),
      axis.title      = ggplot2::element_text(size = base_size),
      axis.text       = ggplot2::element_text(size = base_size - 1),
      panel.grid.minor= ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(linewidth = 0.3, colour = "grey85"),
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.3, colour = "grey90"),
      axis.line       = ggplot2::element_line(linewidth = 0.4, colour = "black"),
      legend.position = legend_position,
      legend.title    = ggplot2::element_text(size = base_size - 1),
      legend.text     = ggplot2::element_text(size = base_size - 2),
      strip.background= ggplot2::element_rect(fill = "grey95", colour = NA),
      strip.text      = ggplot2::element_text(face = "bold", size = base_size)
    )
}

# Forest-specific tweaks (no vertical grid; tight y spacing)
theme_lancet_forest <- function(base_size = 10,
                                base_family = "Helvetica",
                                legend_position = "none") {
  theme_lancet_base(base_size = base_size,
                    base_family = base_family,
                    legend_position = legend_position) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      axis.title.y       = ggplot2::element_blank()
    )
}

# ---- figure-dimensions-lancet (unchanged) --------------------------------------
lan_dim <- function(layout = c("onecol", "twocol"), height_mm = 120) {
  layout <- match.arg(layout)
  width_mm  <- if (layout == "onecol") 107 else 175
  height_mm <- min(height_mm, 230)
  list(w_in = width_mm / 25.4, h_in = height_mm / 25.4)
}

strip_titles <- function(p) p

# ---- save-lancet-all (backwards-compatible) ------------------------------------
# Supports BOTH old and new signatures:
#   OLD: save_lancet_all(p, file_png = "figs/Fig_2A.png", layout="onecol", figure_type="line")
#   NEW: save_lancet_all(p, file_base = "figs/Fig_2A", layout="onecol")
# Silently ignores unknown args (e.g., figure_type).
save_lancet_all <- function(p,
                            file_png  = NULL,        # old style: full path with .png
                            file_base = NULL,        # new style: path WITHOUT extension
                            layout    = c("onecol", "twocol"),
                            height_mm = 120,
                            png_dpi   = 600,
                            bg        = "white",
                            ...) {                   # <-- absorbs figure_type or other extras
  layout <- match.arg(layout)
  dims <- lan_dim(layout, height_mm)
  
  # Resolve file_base from file_png if needed
  if (is.null(file_base)) {
    if (is.null(file_png)) stop("save_lancet_all(): provide either file_base or file_png.")
    file_base <- tools::file_path_sans_ext(file_png)
  }
  dir.create(dirname(file_base), recursive = TRUE, showWarnings = FALSE)
  
  # PNG (bitmap for review copies)
  ggplot2::ggsave(paste0(file_base, ".png"), strip_titles(p),
                  width = dims$w_in, height = dims$h_in, dpi = png_dpi, bg = bg)
  
  # PDF (vector)
  grDevices::cairo_pdf(file = paste0(file_base, ".pdf"),
                       width = dims$w_in, height = dims$h_in, family = "Helvetica")
  print(p); grDevices::dev.off()
  
  # EPS (vector)
  grDevices::postscript(file = paste0(file_base, ".eps"),
                        width = dims$w_in, height = dims$h_in,
                        family = "Helvetica", paper = "special",
                        horizontal = FALSE, onefile = FALSE)
  print(p); grDevices::dev.off()
  
  invisible(normalizePath(paste0(file_base, c(".png",".pdf",".eps"))))
}


if (!exists("save_plot_hi")) {
  save_plot_hi <- function(p, file, width = 6.5, height = 5.0, dpi = 600) {
    if (!requireNamespace("here", quietly = TRUE)) stop("Package 'here' is required.")
    dir.create(here::here("figs"), recursive = TRUE, showWarnings = FALSE)
    out <- if (grepl("^figs/", file)) here::here(file) else here::here("figs", file)
    ggplot2::ggsave(out, p, width = width, height = height, dpi = dpi, bg = "white")
    invisible(out)  }
}

# this shim will mask it and keep the figure_type argument working.
if (!exists("save_lancet") || length(formals(save_lancet)) < 5) {
  save_lancet <- function(p, file_png,
                          layout = c("onecol","twocol"),
                          height_mm = 120,
                          figure_type = c("line","combo","photo"),
                          png_dpi = 600,
                          ...) {
    save_lancet_all(
      p = p,
      file_png = file_png,
      layout = layout,
      height_mm = height_mm,
      figure_type = figure_type,
      png_dpi = png_dpi
    )
  }
}

add_red_label_gutter <- function(
    p, text, x_vals = NULL,
    where = c("bottom-right","top-right","bottom-left","top-left"),
    fill = "#374151", color = "white", size = 2.8, ...
){
  where <- match.arg(where)
  p <- p + ggplot2::theme(plot.margin = ggplot2::margin(8, 20, 10, 20))
  
  xr <- NULL
  if (length(x_vals)) xr <- suppressWarnings(range(x_vals, na.rm = TRUE))
  if (is.null(xr) || any(!is.finite(xr))) {
    gb <- ggplot2::ggplot_build(p)
    xr <- tryCatch({
      pp <- gb$layout$panel_params[[1]]
      if (!is.null(pp$x.range)) as.numeric(pp$x.range)
      else if (!is.null(pp$x$range$range)) as.numeric(pp$x$range$range)
      else c(-1, 1)
    }, error = function(...) c(-1, 1))
  }
  span <- diff(xr); if (!is.finite(span) || span <= 0) span <- 1
  
  gb <- ggplot2::ggplot_build(p)
  
  y_levels <- tryCatch({
    pp <- gb$layout$panel_params[[1]]
    if (!is.null(pp$y$range$range)) as.character(pp$y$range$range)
    else if (!is.null(pp$y.range)) as.character(pp$y.range)
    else character(0)
  }, error = function(...) character(0))
  n_y <- if (length(y_levels)) length(y_levels) else 5L
  
  
  x_right <- xr[2] + span * 0.12
  x_left  <- xr[1] - span * 0.12
  y_top   <- n_y + 0.6
  y_bot   <- -0.6
  
  pos <- switch(where,
                "bottom-right" = list(x = x_right, y = y_bot, h = 1, v = 0),
                "top-right"    = list(x = x_right, y = y_top, h = 1, v = 1),
                "bottom-left"  = list(x = x_left,  y = y_bot, h = 0, v = 0),
                "top-left"     = list(x = x_left,  y = y_top, h = 0, v = 1))
  
  p + ggplot2::coord_cartesian(clip = "off") +
    ggplot2::annotate(
      "label",
      x = pos$x, y = pos$y, hjust = pos$h, vjust = pos$v,
      label = text,
      label.size = 0, label.r = grid::unit(6, "pt"),
      size = size, fontface = "bold", fill = fill, color = color
    )
}

rob_cols_quips   <- c("Low"="#2ca25f","Moderate"="#fec44f","High"="#de2d26")
rob_cols_quadas2 <- c("Low"="#2ca25f","Some concerns"="#fec44f","High"="#de2d26")


# ---- Figure 2A ----
fig_concordance_forest_pretty <- function(per_study, pooled = NULL,
                                          out = "Figure_2A_Concordance_Forest_Weighted.png") {
  stopifnot(all(c("study_id","Kappa","SE") %in% names(per_study)))
  
  # base data, clamp CIs to [-1,1]
  df <- per_study %>%
    dplyr::mutate(
      study_id = as.character(.data$study_id),
      Kappa    = as.numeric(.data$Kappa),
      SE       = as.numeric(.data$SE),
      lower    = if (!"lower" %in% names(per_study)) .data$Kappa - 1.96*.data$SE else as.numeric(.data$lower),
      upper    = if (!"upper" %in% names(per_study)) .data$Kappa + 1.96*.data$SE else as.numeric(.data$upper)
    ) %>%
    dplyr::mutate(
      lower = pmax(-1, lower),
      upper = pmin( 1, upper)
    ) %>%
    dplyr::filter(is.finite(Kappa), is.finite(SE), SE > 0,
                  is.finite(lower), is.finite(upper))
  
  if (nrow(df) < 2) return(invisible(NULL))
  
  # pooled κ (REML + Knapp–Hartung)
  need_pool <- is.null(pooled) || !is.list(pooled) ||
    any(!is.finite(c(pooled$estimate %||% NA_real_,
                     pooled$ci_l     %||% NA_real_,
                     pooled$ci_u     %||% NA_real_)))
  if (need_pool && requireNamespace("metafor", quietly = TRUE)) {
    fit <- try(
      metafor::rma(yi = df$Kappa, sei = df$SE, method = "REML", test = "knha"),
      silent = TRUE
    )
    if (!inherits(fit, "try-error")) {
      pooled <- list(
        estimate = as.numeric(fit$b),
        ci_l     = as.numeric(fit$ci.lb),
        ci_u     = as.numeric(fit$ci.ub),
        tau2     = as.numeric(fit$tau2),
        I2       = as.numeric(fit$I2)
      )
    }
  }
  
  if (!is.list(pooled)) pooled <- list()
  
  k_est <- pooled[["estimate"]] %||% NA_real_
  k_l   <- pooled[["ci_l"]]     %||% NA_real_
  k_u   <- pooled[["ci_u"]]     %||% NA_real_
  tau2  <- pooled[["tau2"]]     %||% 0
  I2    <- pooled[["I2"]]       %||% NA_real_
  
  
  # derive n per study if available (on filtered df)
  n_vec <- tryCatch({
    if ("n" %in% names(df)) {
      as.integer(df$n)
    } else if (all(c("a","b","c","d") %in% names(df))) {
      as.integer(df$a + df$b + df$c + df$d)
    } else {
      rep(NA_integer_, nrow(df))
    }
  }, error = function(e) rep(NA_integer_, nrow(df)))
  
  
  # per-study weights (REML; fall back to 1/SE^2)
  wt_raw <- if (is.finite(tau2)) 1 / (df$SE^2 + tau2) else 1 / df$SE^2
  wt_pct <- 100 * wt_raw / sum(wt_raw)
  
  df_k <- df %>%
    dplyr::mutate(
      N          = n_vec,
      w_pct      = wt_pct,
      k_lo       = Kappa - 1.96 * SE,
      k_hi       = Kappa + 1.96 * SE,
      StudyShort = if ("StudyShort" %in% names(per_study)) per_study$StudyShort else study_id
    ) %>%
    dplyr::arrange(dplyr::desc(w_pct)) %>%
    dplyr::mutate(row_id = dplyr::row_number())
  
  # x-axis and label gutters
  xlim   <- c(-1.4, 1.4)
  x_left <- -1.15
  x_right<-  1.35
  
  p <- ggplot2::ggplot(df_k, ggplot2::aes(y = row_id)) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = k_lo, xmax = k_hi),
      height = 0.18, linewidth = 0.5
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = Kappa, size = w_pct),
      shape = 22, stroke = 0.3, fill = "black"
    ) +
    ggplot2::scale_size(range = c(1.8, 6.0), guide = "none") +
    ggplot2::geom_vline(xintercept = 0,       linetype = "dashed", linewidth = 0.4) +
    { if (is.finite(k_est)) ggplot2::geom_vline(xintercept = k_est, linetype = "dashed", linewidth = 0.4) } +
    # left N column
    ggplot2::geom_text(ggplot2::aes(x = x_left, label = ifelse(is.na(N), "", scales::comma(N))),
                       hjust = 1, size = 3) +
    ggplot2::annotate("text", x = x_left,  y = max(df_k$row_id) + 0.65,
                      label = "N", hjust = 1, fontface = "bold", size = 3.3) +
    # right weight + κ[CI] column
    ggplot2::geom_text(
      ggplot2::aes(
        x = x_right,
        label = sprintf("%.1f   %.2f [%.2f, %.2f]", w_pct, Kappa, k_lo, k_hi)
      ),
      hjust = 0, size = 3
    ) +
    ggplot2::annotate("text", x = x_right, y = max(df_k$row_id) + 0.65,
                      label = "Weight (%)   κ [95% CI]",
                      hjust = 0, fontface = "bold", size = 3.3) +
    ggplot2::scale_y_continuous(
      breaks = df_k$row_id,
      labels = df_k$StudyShort,
      expand = ggplot2::expansion(add = c(0.8, 1.0))
    ) +
    ggplot2::scale_x_continuous(
      limits = xlim,
      breaks = seq(-1, 1, by = 0.2),
      name   = "Cohen\u2019s \u03BA (95% CI)"
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    theme_lancet_base(base_size = 9, base_family = "Helvetica") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "grey80", linewidth = 0.3),
      axis.title.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 10, r = 140, b = 10, l = 70)
    )
  
  # optional red pooled κ gutter label, as before
  if (is.finite(k_est)) {
    pooled_lab <- sprintf("Pooled \u03BA = %.3f (%.3f\u2013%.3f); \u03C4\u00B2 = %.3f; I\u00B2 = %.0f%%",
                          k_est, k_l, k_u, tau2, I2)
    p <- add_red_label_gutter(p, text = pooled_lab,
                              x_vals = c(df_k$k_lo, df_k$k_hi, 0, k_est))
  }
  
  save_lancet_all(p, here::here("figs", out),
                  layout = "twocol", height_mm = 120, figure_type = "line")
  invisible(list(plot = p, df = df_k, pooled = pooled))
}


# ---- Figure 2B ----
fig_discordance_asymmetry_pretty <- function(per_study,
                                             out = "Figure_2B_Directional_Discordance.png") {
  stopifnot(all(c("study_id","a","c") %in% names(per_study)))
  
  df <- per_study %>%
    dplyr::transmute(
      study_id = as.character(.data$study_id),
      a = as.numeric(.data$a),
      c = as.numeric(.data$c),
      logOR    = log((.data$a + 0.5) / (.data$c + 0.5)),
      se_logOR = sqrt(1/(.data$a + 0.5) + 1/(.data$c + 0.5)),
      ci_l     = .data$logOR - 1.96 * .data$se_logOR,
      ci_u     = .data$logOR + 1.96 * .data$se_logOR
    ) %>%
    dplyr::filter(is.finite(.data$logOR), is.finite(.data$se_logOR))
  
  if (nrow(df) < 2) return(invisible(NULL))
  
  ma <- metafor::rma(yi = df$logOR, sei = df$se_logOR, method = "REML", test = "knha")
  lor_est <- as.numeric(ma$b)
  lor_l   <- as.numeric(ma$ci.lb)
  lor_u   <- as.numeric(ma$ci.ub)
  tau2    <- ma$tau2
  I2      <- ma$I2
  
  pr <- try(metafor::predict(ma), silent = TRUE)
  pi_txt <- if (!inherits(pr, "try-error") && all(is.finite(c(pr$pi.lb, pr$pi.ub)))) {
    sprintf("; PI = %.2f to %.2f", pr$pi.lb, pr$pi.ub)
  } else ""
  
  p <- ggplot(df, ggplot2::aes(x = .data$logOR, y = reorder(.data$study_id, .data$logOR))) +
    ggplot2::geom_point(shape = 16, size = 2) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$ci_l, xmax = .data$ci_u), height = 0.2) +
    ggplot2::geom_vline(xintercept = 0,       linetype = "dashed") +
    ggplot2::geom_vline(xintercept = lor_est, linetype = "dashed") +
    theme_lancet_base(base_size = 9, base_family = "Helvetica") +
    ggplot2::labs(x = "log-odds", y = NULL) +
    ggplot2::scale_x_continuous(
      name = "log-odds",
      sec.axis = ggplot2::sec_axis(~exp(.), name = "Odds ratio (MRD−/PET+ vs MRD+/PET−)")
    )
  
  lab <- paste0(
    sprintf("Pooled log-odds = %.2f (%.2f – %.2f); \u03C4\u00B2 = %.3f; I\u00B2 = %.0f%%", lor_est, lor_l, lor_u, tau2, I2),
    pi_txt
  )
  p_lab <- add_red_label_gutter(p, text = lab, x_vals = c(df$ci_l, df$ci_u, 0, lor_est))
  
  save_lancet_all(p_lab, here::here("figs", out), layout = "twocol",
                  height_mm = 120, figure_type = "line")
  p_lab
}


# ---- Figure 3 ----
fig_survival_forest_pretty <- function(surv_df,
                                       out = "Figure_3_Forest_DualNeg.png") {
  
  stopifnot(is.data.frame(surv_df),
            all(c("Study", "logHR", "SE") %in% names(surv_df)))
  
  df <- dplyr::filter(surv_df,
                      is.finite(.data$logHR),
                      is.finite(.data$SE))
  if (!nrow(df)) return(invisible(NULL))
  
  # ---------- 1. N vector (safe, no size mismatch) ---------------------------
  N_vec <- if ("N" %in% names(df)) {
    as.numeric(df$N)
  } else if ("n" %in% names(df)) {
    as.numeric(df$n)
  } else {
    rep(NA_real_, nrow(df))
  }
  
  # ---------- 2. Random-effects meta-analysis -------------------------------
  res_re <- if (nrow(df) >= 2) {
    try(metafor::rma(yi = df$logHR,
                     sei = df$SE,
                     method = "REML",
                     test = "knha"),
        silent = TRUE)
  } else {
    structure("skip", class = "try-error")
  }
  
  tau2 <- I2 <- NA_real_
  pooled_logHR <- ci_log_l <- ci_log_u <- NA_real_
  pr <- NULL
  
  if (!inherits(res_re, "try-error")) {
    tau2         <- res_re$tau2
    I2           <- res_re$I2
    pooled_logHR <- as.numeric(res_re$b)
    ci_log_l     <- as.numeric(res_re$ci.lb)
    ci_log_u     <- as.numeric(res_re$ci.ub)
    pr <- try(metafor::predict(res_re, transf = exp), silent = TRUE)
  }
  
  pooled_HR <- if (is.finite(pooled_logHR)) exp(pooled_logHR) else NA_real_
  ci_HR_l   <- if (is.finite(ci_log_l))     exp(ci_log_l)     else NA_real_
  ci_HR_u   <- if (is.finite(ci_log_u))     exp(ci_log_u)     else NA_real_
  
  pi_txt <- if (!inherits(pr, "try-error") &&
                all(is.finite(c(pr$pi.lb, pr$pi.ub)))) {
    sprintf("; PI = %.2f to %.2f", pr$pi.lb, pr$pi.ub)
  } else ""
  
  # ---------- 3. Per-study HR + CI + weights --------------------------------
  per <- df %>%
    dplyr::transmute(
      Study      = .data$Study,
      StudyShort = if ("StudyShort" %in% names(surv_df))
        surv_df$StudyShort else .data$Study,
      logHR      = .data$logHR,
      SE         = .data$SE,
      logHR_lo   = .data$logHR - 1.96 * .data$SE,
      logHR_hi   = .data$logHR + 1.96 * .data$SE,
      HR         = exp(.data$logHR),
      CI_lower   = exp(logHR_lo),
      CI_upper   = exp(logHR_hi),
      N          = N_vec
    ) %>%
    dplyr::filter(.data$HR > 0,
                  .data$CI_lower > 0,
                  .data$CI_upper > 0)
  
  if (!nrow(per)) return(invisible(NULL))
  
  wt_raw <- if (is.finite(tau2)) 1 / (per$SE^2 + tau2) else 1 / per$SE^2
  wt_pct <- 100 * wt_raw / sum(wt_raw)
  
  df_hr <- per %>%
    dplyr::mutate(w_pct = wt_pct) %>%
    dplyr::arrange(dplyr::desc(w_pct)) %>%
    dplyr::mutate(row_id = dplyr::row_number())
  
  # ---------- 4. Axis limits + GUTTERS (same structure as Fig 2B) -----------
  hr_ticks <- c(0.25, 0.5, 1, 2, 4)
  axis_lim <- log(hr_ticks[c(1, length(hr_ticks))])
  xlim     <- c(axis_lim[1] - 0.8, axis_lim[2] + 1.6)
  x_left   <- axis_lim[1] - 0.6    # N column
  x_right  <- axis_lim[2] + 0.6    # Weight/CI column
  
  # show or hide N column depending on availability
  show_N <- any(is.finite(df_hr$N))
  
  # ---------- 5. Plot --------------------------------------------------------
  p <- ggplot2::ggplot(df_hr, ggplot2::aes(y = row_id)) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = logHR_lo, xmax = logHR_hi),
      height = 0.20, linewidth = 0.5
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = logHR, size = w_pct),
      shape = 22, stroke = 0.3, fill = "black"
    ) +
    ggplot2::scale_size(range = c(1.8, 6.0), guide = "none") +
    ggplot2::geom_vline(xintercept = 0,
                        linetype = "dashed", linewidth = 0.4) +
    { if (is.finite(pooled_logHR))
      ggplot2::geom_vline(xintercept = pooled_logHR,
                          linetype = "dashed", linewidth = 0.4) } +
    
    # --- Left N column (only if provided) ---
    { if (show_N)
      ggplot2::geom_text(
        ggplot2::aes(
          x     = x_left,
          label = ifelse(is.finite(N), scales::comma(N), "")
        ),
        hjust = 1, size = 3
      ) } +
    { if (show_N)
      ggplot2::annotate(
        "text", x = x_left, y = max(df_hr$row_id) + 0.65,
        label = "N", hjust = 1, fontface = "bold", size = 3.3
      ) } +
    
    # --- Right weight + HR[CI] ---
    ggplot2::geom_text(
      ggplot2::aes(
        x     = x_right,
        label = sprintf("%.1f   %.2f [%.2f, %.2f]",
                        w_pct, HR, CI_lower, CI_upper)
      ),
      hjust = 0, size = 3
    ) +
    ggplot2::annotate(
      "text", x = x_right, y = max(df_hr$row_id) + 0.65,
      label = "Weight (%)   HR [95% CI]",
      hjust = 0, fontface = "bold", size = 3.3
    ) +
    
    ggplot2::scale_y_continuous(
      breaks = df_hr$row_id,
      labels = df_hr$StudyShort,
      expand = ggplot2::expansion(add = c(0.8, 1.0))
    ) +
    ggplot2::scale_x_continuous(
      limits = xlim,
      breaks = log(hr_ticks),
      labels = hr_ticks,
      name   = "Hazard ratio (PFS): dual-negative vs others (log scale)"
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    theme_lancet_base(base_size = 9, base_family = "Helvetica") +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "grey85", linewidth = 0.3),
      axis.title.y       = ggplot2::element_blank(),
      axis.text.y        = ggplot2::element_text(hjust = 1),
      plot.margin        = ggplot2::margin(t = 10, r = 150, b = 10, l = 80)
    )
  
  # ---------- 6. Save figure (NO internal pooled-HR frame) ------------------
  save_lancet_all(
    p, here::here("figs", out),
    layout = "twocol", height_mm = 120, figure_type = "line"
  )
  
  p
}

#############################

# ---- Figure 3B ----
fig_funnel_pretty <- function(
    surv_df,
    res_model = NULL,
    out = "Figure_3B_Funnel.png",
    label_metric = c("HR","logHR"),
    label_include_ci = FALSE,
    label_digits = 2,
    repel_force = 0.7,
    seed = 123
){
  stopifnot(is.data.frame(surv_df), all(c("Study","logHR","SE") %in% names(surv_df)))
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    stop("Please install ggrepel: install.packages('ggrepel')")
  }
  label_metric <- match.arg(label_metric)
  
  df <- dplyr::filter(surv_df, is.finite(.data$logHR), is.finite(.data$SE))
  if (nrow(df) < 2) return(invisible(NULL))
  
  # --- meta fit (REML + KnHa) ---
  if (is.null(res_model)) {
    res_model <- try(metafor::rma(yi = df$logHR, sei = df$SE,
                                  method = "REML", test = "knha"), silent = TRUE)
    if (inherits(res_model, "try-error"))
      res_model <- try(metafor::rma(yi = df$logHR, sei = df$SE, method = "REML"), silent = TRUE)
  }
  pooled_logHR <- if (!inherits(res_model, "try-error")) as.numeric(res_model$b) else NA_real_
  tau2 <- if (!inherits(res_model, "try-error")) res_model$tau2 else NA_real_
  I2   <- if (!inherits(res_model, "try-error")) res_model$I2   else NA_real_
  
  # Egger (show but caution in caption)
  egger_p <- NA_real_
  if (!inherits(res_model, "try-error") && nrow(df) >= 3) {
    eg <- try(metafor::regtest(res_model, model = "rma", predictor = "sei"), silent = TRUE)
    if (!inherits(eg, "try-error")) egger_p <- suppressWarnings(as.numeric(eg$pval))
  }
  
  # Prediction interval on HR scale for the label (more intuitive)
  pr <- if (!inherits(res_model, "try-error")) try(metafor::predict(res_model, transf = exp), silent = TRUE) else NULL
  
  # Axis helper (funnel cone)
  max_se <- max(df$SE, na.rm = TRUE)
  cone <- NULL
  if (is.finite(pooled_logHR)) {
    cone <- tibble::tibble(
      SE    = seq(0, max_se, length.out = 200),
      upper = pooled_logHR + 1.96 * SE,
      lower = pooled_logHR - 1.96 * SE
    ) %>%
      tidyr::pivot_longer(c(upper, lower), names_to = "side", values_to = "x") %>%
      dplyr::mutate(type = "95% funnel")
  }
  vline_df <- if (is.finite(pooled_logHR)) tibble::tibble(x = pooled_logHR, type = "Pooled effect") else NULL
  
  # Label text (bigger, high contrast, readable)
  if (!"HR" %in% names(df)) df$HR <- exp(df$logHR)
  if (!"CI_lower" %in% names(df) || !"CI_upper" %in% names(df)) {
    df$CI_lower <- ifelse(is.finite(df$SE), exp(df$logHR - 1.96*df$SE), NA_real_)
    df$CI_upper <- ifelse(is.finite(df$SE), exp(df$logHR + 1.96*df$SE), NA_real_)
  }
  metric_val <- if (label_metric == "HR") df$HR else df$logHR
  fmt <- function(x, d = label_digits) ifelse(is.finite(x), formatC(x, format = "f", digits = d), "NA")
  label_text <- if (label_metric == "HR") {
    base <- paste0(df$Study, ": ", fmt(metric_val))
    if (isTRUE(label_include_ci)) paste0(base, " (", fmt(df$CI_lower), "–", fmt(df$CI_upper), ")") else base
  } else {
    base <- paste0(df$Study, ": ", fmt(metric_val))
    if (isTRUE(label_include_ci)) {
      llo <- ifelse(is.finite(df$SE), df$logHR - 1.96*df$SE, NA_real_)
      lhi <- ifelse(is.finite(df$SE), df$logHR + 1.96*df$SE, NA_real_)
      paste0(base, " (", fmt(llo), "–", fmt(lhi), ")")
    } else base
  }
  
  # ---- plot ----
  set.seed(seed)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$logHR, y = .data$SE)) +
    ggplot2::geom_point(size = 2.4, show.legend = FALSE) +
    ggplot2::scale_y_reverse(limits = c(max_se, 0)) +
    ggplot2::labs(x = "log(HR)", y = "SE") +
    theme_lancet_base(base_size = 11, base_family = "Helvetica") +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 11),
      axis.text  = ggplot2::element_text(size = 10)
    )
  
  if (!is.null(vline_df)) {
    p <- p + ggplot2::geom_vline(
      data = vline_df,
      ggplot2::aes(xintercept = .data$x),
      linetype = "dashed", linewidth = 0.7, inherit.aes = FALSE
    )
  }
  if (!is.null(cone)) {
    p <- p + ggplot2::geom_line(
      data = cone,
      ggplot2::aes(x = .data$x, y = .data$SE, linetype = .data$type),
      linewidth = 0.6, inherit.aes = FALSE, show.legend = FALSE
    )
  }
  
  # Readable study labels
  p <- p +
    ggrepel::geom_label_repel(
      ggplot2::aes(label = label_text),
      size = 3.3,            # bigger
      label.padding = grid::unit(0.16, "lines"),
      label.size = 0.3,
      min.segment.length = 0,
      box.padding = 0.25,
      point.padding = 0.2,
      fill = "white",        # high-contrast
      color = "#111111",     # dark text
      max.overlaps = Inf
    )
  
  # --- red gutter label to MATCH Fig. 3A style ---
  pooled_HR <- if (is.finite(pooled_logHR)) exp(pooled_logHR) else NA_real_
  ci_HR_l <- ci_HR_u <- NA_real_
  if (!inherits(res_model, "try-error")) {
    ci_HR_l <- exp(as.numeric(res_model$ci.lb))
    ci_HR_u <- exp(as.numeric(res_model$ci.ub))
  }
  pi_part <- if (!inherits(pr, "try-error") && all(is.finite(c(pr$pi.lb, pr$pi.ub))))
    sprintf("; PI = %.2f–%.2f", pr$pi.lb, pr$pi.ub) else ""
  
  lab <- sprintf(
    "Pooled HR = %s (%s – %s); \u03C4\u00B2 = %s; I\u00B2 = %s%%; Egger p = %s%s",
    if (is.finite(pooled_HR)) formatC(pooled_HR, format="f", digits=3) else "NA",
    if (is.finite(ci_HR_l))   formatC(ci_HR_l,   format="f", digits=3) else "NA",
    if (is.finite(ci_HR_u))   formatC(ci_HR_u,   format="f", digits=3) else "NA",
    if (is.finite(tau2))      formatC(tau2,      format="f", digits=3) else "NA",
    if (is.finite(I2))        formatC(I2,        format="f", digits=0) else "NA",
    if (is.finite(egger_p))   formatC(egger_p,   format="f", digits=3) else "NA",
    pi_part
  )
  
  xr <- range(c(df$logHR, if (!is.null(cone)) cone$x), na.rm = TRUE)
  p <- add_red_label_gutter(p, text = lab, x_vals = xr, where = "bottom-right")
  
  # save & return
  save_lancet_all(p, here::here("figs", out),
                  layout = "twocol", height_mm = 120, figure_type = "combo")
  p
}


# ---- Concordance matrices ----
.concordance_matrix_plot <- function(per_study_conc, subtitle = NULL) {
  stopifnot(all(c("a","b","c","d","n") %in% names(per_study_conc)))
  
  total <- per_study_conc %>%
    dplyr::summarise(
      a = sum(as.numeric(.data$a), na.rm = TRUE),
      b = sum(as.numeric(.data$b), na.rm = TRUE),
      c = sum(as.numeric(.data$c), na.rm = TRUE),
      d = sum(as.numeric(.data$d), na.rm = TRUE)
    )
  
  tot_n <- sum(total$a, total$b, total$c, total$d, na.rm = TRUE)
  if (!is.finite(tot_n) || tot_n <= 0) return(NULL)
  
  mat <- tibble::tibble(
    MRD   = factor(c("MRD−","MRD−","MRD+","MRD+"), levels = c("MRD−","MRD+")),
    PET   = factor(c("PET−","PET+","PET−","PET+"), levels = c("PET−","PET+")),
    count = c(total$b, total$a, total$c, total$d)
  ) %>%
    dplyr::mutate(
      pct = 100 * .data$count / tot_n,
      lab = sprintf("%.1f%%\n(n=%s)", .data$pct, format(.data$count, big.mark=","))
    )
  
  obs_agree <- 100 * (total$b + total$d) / tot_n
  
  p <- ggplot2::ggplot(mat, ggplot2::aes(x = .data$PET, y = .data$MRD, fill = .data$pct)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = .data$lab), size = 3.3, lineheight = 0.95) +
    ggplot2::scale_fill_gradient(low = "#deebf7", high = "#3182bd", name = "% of total") +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = 10),
      legend.text  = ggplot2::element_text(size = 9),
      plot.margin  = ggplot2::margin(8, 8, 10, 8)
    ) +
    ggplot2::labs(subtitle = subtitle)
  
  attr(p, "N")     <- tot_n
  attr(p, "agree") <- obs_agree
  p
}

fig_concordance_matrix_pretty <- function(per_study_conc,
                                          out = "Supplementary_Figure_SF1_Concordance_Matrix.png") {
  if (!requireNamespace("here", quietly = TRUE)) stop("Package 'here' is required.")
  p <- .concordance_matrix_plot(per_study_conc)
  if (is.null(p)) return(invisible(NULL))
  dir.create(here::here("figs"), recursive = TRUE, showWarnings = FALSE)
  out_full <- here::here("figs", out)
  ggplot2::ggsave(out_full, p, width = 5.5, height = 5.2, dpi = 600, bg = "white")
  attr(p, "file") <- out_full
  p
}

fig_concordance_matrix_panel <- function(per_study_primary,
                                         per_study_strict = NULL,
                                         out = "Supplementary_Figure_SF1_Panel_Primary_vs_Strict.png",
                                         caption_inside = TRUE) {
  if (!requireNamespace("here", quietly = TRUE)) stop("Package 'here' is required.")
  if (!requireNamespace("patchwork", quietly = TRUE)) stop("Package 'patchwork' is required.")
  pA <- .concordance_matrix_plot(per_study_primary, subtitle = "Primary")
  if (is.null(pA)) return(invisible(NULL))
  
  cap_primary <- sprintf("Primary: N = %s; observed agreement = %.1f%%",
                         format(attr(pA, "N"), big.mark=","), attr(pA, "agree"))
  
  panel <- pA
  cap_all <- cap_primary
  if (!is.null(per_study_strict) && is.data.frame(per_study_strict) && nrow(per_study_strict)) {
    pB <- .concordance_matrix_plot(per_study_strict, subtitle = "Strict Timepoint")
    if (!is.null(pB)) {
      cap_strict <- sprintf("Strict Timepoint: N = %s; observed agreement = %.1f%%",
                            format(attr(pB, "N"), big.mark=","), attr(pB, "agree"))
      panel <- (pA | pB)
      cap_all <- paste(cap_primary, cap_strict, sep = "    ")
    }
  }
  
  if (caption_inside) {
    panel <- panel + patchwork::plot_annotation(tag_levels = "A", caption = cap_all)
  } else {
    panel <- panel + patchwork::plot_annotation(tag_levels = "A")
    attr(panel, "caption") <- cap_all
  }
  
  dir.create(here::here("figs"), recursive = TRUE, showWarnings = FALSE)
  out_full <- here::here("figs", out)
  ggplot2::ggsave(out_full, panel, width = if (!is.null(per_study_strict)) 11 else 5.5,
                  height = 5.6, dpi = 600, bg = "white")
  attr(panel, "file") <- out_full
  panel
}

# ---- SF2 (STRICT) ----
fig_discordance_strict_pretty <- function(
    df,
    out = "Supplementary_Figure_SF2_Directional_Discordance_STRICT.png",
    caption_inside = FALSE
){
  stopifnot(is.data.frame(df))
  if (!all(c("a","c") %in% names(df))) return(invisible(NULL))
  
  a <- suppressWarnings(as.numeric(df$a))
  c <- suppressWarnings(as.numeric(df$c))
  keep <- is.finite(a) & is.finite(c)
  if (sum(keep) < 2) return(invisible(NULL))
  a <- a[keep]; c <- c[keep]
  
  lab_full <- if ("study_id" %in% names(df)) as.character(df$study_id)
  else if ("Study" %in% names(df)) as.character(df$Study) else rep(NA_character_, nrow(df))
  lab <- lab_full[keep]; bad <- is.na(lab) | lab == ""
  if (any(bad)) lab[bad] <- paste0("row_", which(keep)[bad])
  
  logOR    <- log((a + 0.5)/(c + 0.5))
  se_logOR <- sqrt(1/(a + 0.5) + 1/(c + 0.5))
  ci_l     <- logOR - 1.96*se_logOR
  ci_u     <- logOR + 1.96*se_logOR
  
  ma <- metafor::rma(yi = logOR, sei = se_logOR, method = "REML", test = "knha")
  pooled_est <- as.numeric(ma$b)
  pooled_ci  <- c(as.numeric(ma$ci.lb), as.numeric(ma$ci.ub))
  
  pr <- try(metafor::predict(ma), silent = TRUE)
  have_pi <- !inherits(pr, "try-error") && all(is.finite(c(pr$pi.lb, pr$pi.ub)))
  
  dplot <- data.frame(study = lab, logOR, ci_l, ci_u)
  p <- ggplot2::ggplot(dplot, ggplot2::aes(x = .data$logOR, y = reorder(.data$study, .data$logOR))) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$ci_l, xmax = .data$ci_u), height = 0.25) +
    ggplot2::geom_vline(xintercept = 0,          linetype = "dashed") +
    ggplot2::geom_vline(xintercept = pooled_est, linetype = "dashed") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::labs(x = "log-odds (>0 → MRD−/PET+ predominance)", y = NULL)
  
  xr  <- range(c(dplot$ci_l, dplot$ci_u, 0, pooled_est), na.rm = TRUE)
  pad <- 0.18 * diff(xr)
  p   <- p + ggplot2::coord_cartesian(xlim = c(xr[1], xr[2] + pad), clip = "off")
  
  label_txt <- sprintf(
    "Pooled log-odds = %.2f (%.2f – %.2f)%s",
    pooled_est, pooled_ci[1], pooled_ci[2],
    if (have_pi) sprintf("; PI = %.2f to %.2f", pr$pi.lb, pr$pi.ub) else ""
  )
  
  if (isTRUE(caption_inside)) {
    p <- p +
      ggplot2::annotate("label",
                        x = (xr[2] + pad) - 0.02*(xr[2] + pad - xr[1]), y = -Inf,
                        vjust = -0.5, hjust = 1, label = gsub("; ", "\n", label_txt, fixed = TRUE),
                        color = "red", fill = "white", label.size = 0.6
      ) +
      ggplot2::theme(plot.margin = ggplot2::margin(8, 80, 28, 8))
  } else {
    p <- add_red_label_gutter(p, text = label_txt, x_vals = xr, where = "bottom-right")
  }
  
  dir.create(here::here("figs"), recursive = TRUE, showWarnings = FALSE)
  outfile <- here::here("figs", out)
  ggplot2::ggsave(outfile, p, width = 6.5, height = 5.0, dpi = 600, bg = "white")
  
  caption_text <- sprintf(
    "Strict analysis: pooled log-odds = %.2f (%.2f–%.2f); I² = %.0f%%%s",
    pooled_est, pooled_ci[1], pooled_ci[2], ma$I2,
    if (have_pi) sprintf("; PI = %.2f to %.2f", pr$pi.lb, pr$pi.ub) else ""
  )
  caption_html <- sprintf(
    "<div style='text-align:center;margin-top:6px;font-size:90%%;color:#444'>
     <em>Strict analysis:</em> pooled log-odds = %.2f (%.2f &ndash; %.2f); I&sup2; = %.0f%%%s
     </div>",
    pooled_est, pooled_ci[1], pooled_ci[2], ma$I2,
    if (have_pi) sprintf("; PI = %.2f to %.2f", pr$pi.lb, pr$pi.ub) else ""
  )
  
  invisible(list(
    file = outfile,
    model = ma,
    plot  = p,
    caption = caption_text,
    caption_html = caption_html
  ))
}

# ---- Conceptual pathway ----
fig_conceptual_mm <- function(out = "Figure_5_Conceptual.png",
                              out_dir = here::here("figs"),
                              width_px = 1800, height_px = 1100) {
  if (!requireNamespace("here", quietly = TRUE)) stop("Package 'here' is required.")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out_path <- file.path(out_dir, basename(out))
  
  if (requireNamespace("DiagrammeR", quietly = TRUE)) {
    dot <- 'digraph pathway { graph [rankdir=LR, nodesep=0.45, ranksep=0.6, bgcolor=white, labelloc="t"];
      node  [shape=box, style="rounded,filled", fontname=Helvetica, fontsize=20, color="#444444"];
      edge  [arrowsize=0.7, color="#666666"];
      title [label="Clinical pathway from MRD × PET/CT states", shape=plaintext, fontsize=24];
      mrdlab [label="MRD status", shape=plaintext, fontsize=18];
      petlab [label="PET/CT status", shape=plaintext, fontsize=18];
      s1 [label="MRD− / PET−\\n(Dual negative)\\n• Standard maintenance\\n• Consider de-escalation", fillcolor="#E8F5E9"];
      s2 [label="MRD− / PET+\\n(Metabolic lesions)\\n• Imaging-guided workup\\n• Local/targeted action as indicated", fillcolor="#FFF8E1"];
      s3 [label="MRD+ / PET−\\n(Marrow disease)\\n• Consider intensification\\n• Close marrow monitoring", fillcolor="#FFF8E1"];
      s4 [label="MRD+ / PET+\\n(Dual positive)\\n• Escalation / clinical trial", fillcolor="#FFEBEE"];
      petneg [label="PET−", shape=plaintext]; petpos [label="PET+", shape=plaintext];
      mrdneg [label="MRD−", shape=plaintext]; mrdpos [label="MRD+", shape=plaintext];
      {rank=same; title} {rank=same; petlab} {rank=same; petneg; petpos} {rank=same; s1; s2} {rank=same; s3; s4}
      petlab -> petneg [style=invis]; petlab -> petpos [style=invis]; petneg -> s1 [style=invis]; petpos -> s2 [style=invis];
      mrdneg -> s1 [style=invis]; mrdpos -> s3 [style=invis]; s1 -> s2 [style=invis]; s3 -> s4 [style=invis];
      mrdlab -> mrdneg [style=invis]; mrdlab -> mrdpos [style=invis]; }'
    g <- DiagrammeR::grViz(dot)
    if (knitr::is_html_output()) print(g)
    if (requireNamespace("DiagrammeRsvg", quietly = TRUE) && requireNamespace("rsvg", quietly = TRUE)) {
      svg <- DiagrammeRsvg::export_svg(g)
      rsvg::rsvg_png(charToRaw(svg), file = out_path, width = width_px, height = height_px)
      if (file.exists(out_path)) return(out_path)
    } else if (requireNamespace("htmlwidgets", quietly = TRUE)) {
      htmlwidgets::saveWidget(g, file = sub("\\.png$", ".html", out_path), selfcontained = TRUE)
    }
  }
  
  # fallback
  txt <- function(x, y, label, size = 4.5, face = "plain")
    ggplot2::annotate("text", x = x, y = y, label = label, size = size, fontface = face, lineheight = 0.98)
  
  p <- ggplot2::ggplot() +
    ggplot2::coord_fixed(xlim = c(0, 2), ylim = c(0, 2), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::geom_rect(xmin = 0.02, xmax = 0.98, ymin = 1.02, ymax = 1.98, fill = "#E8F5E9", color = "#444444", linewidth = 0.5) +
    ggplot2::geom_rect(xmin = 1.02, xmax = 1.98, ymin = 1.02, ymax = 1.98, fill = "#FFF8E1", color = "#444444", linewidth = 0.5) +
    ggplot2::geom_rect(xmin = 0.02, xmax = 0.98, ymin = 0.02, ymax = 0.98, fill = "#FFF8E1", color = "#444444", linewidth = 0.5) +
    ggplot2::geom_rect(xmin = 1.02, xmax = 1.98, ymin = 0.02, ymax = 0.98, fill = "#FFEBEE", color = "#444444", linewidth = 0.5) +
    txt(1.0, 1.98 + 0.06, "Clinical pathway from MRD × PET/CT states", size = 6, face = "bold") +
    txt(0.04, 1.5, "MRD−", face = "bold") + txt(0.04, 0.5, "MRD+", face = "bold") +
    txt(0.5, 2.02, "PET−", face = "bold") + txt(1.5, 2.02, "PET+", face = "bold") +
    txt(0.5, 1.5, "MRD− / PET−\n(Dual negative)\n• Standard maintenance\n• Consider de-escalation") +
    txt(1.5, 1.5, "MRD− / PET+\n(Metabolic lesions)\n• Imaging-guided workup\n• Local/targeted action") +
    txt(0.5, 0.5, "MRD+ / PET−\n(Marrow disease)\n• Consider intensification\n• Close marrow monitoring") +
    txt(1.5, 0.5, "MRD+ / PET+\n(Dual positive)\n• Escalation / trial")
  
  ggplot2::ggsave(out_path, p, width = width_px/300, height = height_px/300, dpi = 300, units = "in")
  out_path
}

# ---- Export (PNG + PDF/EPS/TIFF) ----
export_for_lancet <- function(p, base, layout = c("onecol","twocol"),
                              height_mm = 120,
                              make_pdf = TRUE, make_eps = TRUE, make_tiff = TRUE,
                              dpi_line = 1200, dpi_combo = 600,
                              figure_type = c("line","combo","photo")) {
  layout <- match.arg(layout)
  figure_type <- match.arg(figure_type)
  dims <- lan_dim(layout, height_mm)
  if (!requireNamespace("here", quietly = TRUE)) stop("Package 'here' is required.")
  dir.create(here::here("figs"), showWarnings = FALSE, recursive = TRUE)
  
  dpi <- switch(figure_type, line = dpi_line, combo = dpi_combo, photo = 300)
  p <- strip_titles(p)
  
  if (make_pdf) {
    ggplot2::ggsave(filename = here::here("figs", paste0(base, ".pdf")),
                    plot = p, device = grDevices::cairo_pdf,
                    width = dims$w_in, height = dims$h_in, units = "in", dpi = dpi, bg = "white")
  }
  if (make_eps) {
    ggplot2::ggsave(filename = here::here("figs", paste0(base, ".eps")),
                    plot = p, device = grDevices::cairo_ps,
                    width = dims$w_in, height = dims$h_in, units = "in",
                    fallback_resolution = dpi, bg = "white")
  }
  if (make_tiff) {
    if (requireNamespace("ragg", quietly = TRUE)) {
      ragg::agg_tiff(filename = here::here("figs", paste0(base, ".tif")),
                     width = dims$w_in, height = dims$h_in, units = "in",
                     res = dpi, compression = "lzw", background = "white")
      print(p); grDevices::dev.off()
    } else {
      ggplot2::ggsave(filename = here::here("figs", paste0(base, ".tif")),
                      plot = p, device = "tiff",
                      width = dims$w_in, height = dims$h_in, units = "in",
                      dpi = dpi, compression = "lzw", bg = "white")
    }
  }
  invisible(list(width_in = dims$w_in, height_in = dims$h_in, dpi = dpi))
}

# ======================================================================
# File: R/sf3_sim.R   
# ======================================================================

kappa_from_counts <- function(a,b,c,d){
  n <- a+b+c+d
  if (!is.finite(n) || n <= 0) return(NA_real_)
  po <- (b + d) / n
  r1 <- a + b; r2 <- c + d
  c1 <- b + c; c2 <- a + d
  pe <- (r1 * c1 + r2 * c2) / (n^2)
  (po - pe) / max(1 - pe, .Machine$double.eps)
}

counts_from_margins_d <- function(r, s, n, d){
  r1 <- r[1]; r2 <- r[2]; s1 <- s[1]; s2 <- s[2]
  d  <- as.integer(round(d))
  c  <- r2 - d
  b  <- s1 - c
  a  <- r1 - b
  m  <- c(a,b,c,d)
  if (any(m < 0) || sum(m) != n) return(rep(NA_real_, 4))
  as.numeric(m)
}

kappa_bounds <- function(r, s, n){
  r <- as.integer(r); s <- as.integer(s); n <- as.integer(n)
  d_min <- max(0L, r[2] + s[2] - n)
  d_max <- min(r[2], s[2])
  if (d_max < d_min) return(c(NA_real_, NA_real_))
  abcd_min <- counts_from_margins_d(r,s,n,d_min)
  abcd_max <- counts_from_margins_d(r,s,n,d_max)
  r1 <- r[1]; r2 <- r[2]; s1 <- s[1]; s2 <- s[2]
  pe <- ((r1*s1 + r2*s2) / n^2)
  po_min <- if (all(is.finite(abcd_min))) ((abcd_min[2] + abcd_min[4]) / n) else NA_real_
  po_max <- if (all(is.finite(abcd_max))) ((abcd_max[2] + abcd_max[4]) / n) else NA_real_
  k_lo <- (po_min - pe) / max(1 - pe, .Machine$double.eps)
  k_hi <- (po_max - pe) / max(1 - pe, .Machine$double.eps)
  sort(c(k_lo, k_hi))
}

run_sf3_sim <- function(mrd,
                        out_png = "SF3B_Kappa_Bias_vs_True.png",
                        S = 1000L,
                        seed = 1L) {
  if (!requireNamespace("here", quietly = TRUE)) stop("Package 'here' is required.")
  stopifnot(is.data.frame(mrd))
  if (!all(c("a","b","c","d","n") %in% names(mrd)))
    stop("mrd must have a,b,c,d,n")
  
  # restrict to valid rows
  if (!exists("mrd_primary", inherits = TRUE)) {
    mrd_primary <- mrd %>%
      dplyr::filter(
        is.finite(.data$a), is.finite(.data$b),
        is.finite(.data$c), is.finite(.data$d),
        is.finite(.data$n), .data$n > 0,
        .data$a + .data$b + .data$c + .data$d == .data$n
      )
  }
  
  # pooled margins
  pool <- mrd_primary %>%
    dplyr::summarise(
      a = sum(.data$a), b = sum(.data$b),
      c = sum(.data$c), d = sum(.data$d),
      n = sum(.data$n),
      .groups = "drop"
    )
  r_tot <- c(pool$b + pool$a, pool$c + pool$d)  # MRD−, MRD+
  s_tot <- c(pool$b + pool$c, pool$a + pool$d)  # PET−, PET+
  N_tot <- as.numeric(pool$n)
  r_prop <- r_tot / N_tot
  s_prop <- s_tot / N_tot
  
  set.seed(seed)
  S <- as.integer(max(1L, S))
  
  sim <- replicate(S, {
    # per-replicate sample size and margins
    n_i <- as.integer(round(N_tot / S)); if (!is.finite(n_i) || n_i < 4L) n_i <- 4L
    r_i <- as.integer(round(r_prop * n_i)); r_i[2] <- n_i - r_i[1]
    s_i <- as.integer(round(s_prop * n_i)); s_i[2] <- n_i - s_i[1]
    
    # feasible range for d (MRD+/PET+)
    d_min <- max(0L, r_i[2] + s_i[2] - n_i)
    d_max <- min(r_i[2], s_i[2])
    if (d_max < d_min) return(c(NA_real_, NA_real_))
    
    d_star <- sample.int(d_max - d_min + 1L, 1L) + d_min - 1L
    abcd   <- counts_from_margins_d(r_i, s_i, n_i, d_star)
    if (any(!is.finite(abcd))) return(c(NA_real_, NA_real_))
    
    a <- abcd[1]; b <- abcd[2]; c <- abcd[3]; d <- abcd[4]
    
    # true κ
    k_true <- kappa_from_counts(a, b, c, d)
    
    # naïve "marginals-only" κ: lose direction of discordance (either-positive)
    disc_tot <- a + c
    a_naive  <- disc_tot / 2
    c_naive  <- disc_tot / 2
    k_naive  <- kappa_from_counts(a_naive, b, c_naive, d)
    
    c(k_true, k_naive)
  }, simplify = TRUE)
  
  sim <- as.data.frame(t(sim), check.names = FALSE)
  colnames(sim) <- c("k_true", "k_naive")
  sim <- sim[is.finite(sim$k_true) & is.finite(sim$k_naive), , drop = FALSE]
  
  sim$bias <- sim$k_naive - sim$k_true
  
  p <- ggplot2::ggplot(sim, ggplot2::aes(x = .data$k_true, y = .data$bias)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_point(alpha = 0.35, size = 1.1) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, linewidth = 0.6) +
    ggplot2::labs(
      x = "True \u03BA (full 2\u00d72)",
      y = "Bias (na\u00efve marginals-only \u03BA \u2212 true \u03BA)",
      title = "SF3B. Bias of na\u00efve marginals-only \u03BA",
      subtitle = "Zero line = unbiased. Curve below zero = systematic pull toward the null (\u03BA = 0)."
    ) +
    ggplot2::theme_minimal(base_size = 11)
  if (exists("theme_lancet")) p <- p + theme_lancet()
  
  f_out <- here::here("figs", out_png)
  dir.create(dirname(f_out), showWarnings = FALSE, recursive = TRUE)
  if (exists("save_lancet")) {
    save_lancet(p, f_out, layout = "twocol", height_mm = 110, png_dpi = 600)
  } else {
    ggplot2::ggsave(f_out, p, width = 6.8, height = 4.6, dpi = 600, bg = "white")
  }
  
  list(file = f_out, plot = p, sim = sim)
}

# ================================
# Supplementary integration: S1/S2/S3
# Paste these chunks after your existing analysis sections
# ================================

# --- (once) ensure gtools for Dirichlet draws (quietly) ---
try({
  if (!requireNamespace("gtools", quietly = TRUE)) {
    install.packages("gtools", repos = "https://cloud.r-project.org")
  }
}, silent = TRUE)

# ---------
# Helper: Fréchet–Hoeffding κ bounds (no imputation)
# ---------
kappa_bounds_frechet <- function(a=NULL,b=NULL,c=NULL,d=NULL, r1=NULL,r2=NULL,c1=NULL,c2=NULL) {
  if (is.null(r1)) {
    stopifnot(!is.null(a),!is.null(b),!is.null(c),!is.null(d))
    r1 <- a + b; r2 <- c + d; c1 <- b + c; c2 <- a + d
  }
  n  <- r1 + r2
  if (n == 0) return(tibble::tibble(po_min=NA_real_, po_max=NA_real_, kappa_min=NA_real_, kappa_max=NA_real_))
  pe <- (r1/n)*(c1/n) + (r2/n)*(c2/n)
  diag_min_ct <- max(0, r1 + c1 - n) + max(0, r2 + c2 - n)
  diag_max_ct <- min(r1, c1) + min(r2, c2)
  po_min <- diag_min_ct / n
  po_max <- diag_max_ct / n
  kap_min <- if (abs(1 - pe) < 1e-12) NA_real_ else (po_min - pe) / (1 - pe)
  kap_max <- if (abs(1 - pe) < 1e-12) NA_real_ else (po_max - pe) / (1 - pe)
  tibble::tibble(po_min=po_min, po_max=po_max, kappa_min=kap_min, kappa_max=kap_max)
}


# =========================================
# Supplementary Figure S1 — Under-reporting simulation (κ bias)
# =========================================
simulate_underreporting_bias <- function(a,b,c,d, nsim = 5000L, seed = 1L) {
  if (!requireNamespace("gtools", quietly = TRUE)) return(NULL)
  set.seed(seed)
  n <- a+b+c+d
  to_kappa <- function(ct) {
    m <- matrix(c(ct[2], ct[1], ct[3], ct[4]), 2, 2, byrow=TRUE)
    val <- try(DescTools::CohenKappa(m), silent = TRUE)
    suppressWarnings(if (is.list(val) && !is.null(val$overall)) as.numeric(val$overall[1,"Kappa"]) else as.numeric(val)[1])
  }
  mat_true <- matrix(c(b,a,c,d), 2, 2, byrow=TRUE)
  ck_true  <- try(DescTools::CohenKappa(mat_true), silent = TRUE)
  kap_true <- suppressWarnings(if (is.list(ck_true) && !is.null(ck_true$overall)) as.numeric(ck_true$overall[1,"Kappa"]) else as.numeric(ck_true)[1])
  
  p0 <- c(a,b,c,d) / n
  draws <- gtools::rdirichlet(nsim, 1 + 100 * p0)
  
  res <- replicate(nsim, {
    p <- draws[sample.int(nsim,1),]
    ct <- as.integer(round(n * p))
    # reconcile to n
    while (sum(ct) != n) {
      id <- which.max(abs((n * p) - ct))[1]
      ct[id] <- ct[id] + sign((n * p)[id] - ct[id])
    }
    kap_full <- to_kappa(ct)
    ct_a0 <- ct; ct_a0[1] <- 0L; kap_a0 <- to_kappa(ct_a0)  # drop MRD−/PET+
    ct_c0 <- ct; ct_c0[3] <- 0L; kap_c0 <- to_kappa(ct_c0)  # drop MRD+/PET−
    c(kap_full, kap_a0, kap_c0)
  })
  res <- t(res); colnames(res) <- c("kap_full","kap_no_a","kap_no_c")
  tibble::tibble(
    bias_no_a = mean(res[,"kap_no_a"] - kap_true, na.rm=TRUE),
    bias_no_c = mean(res[,"kap_no_c"] - kap_true, na.rm=TRUE),
    rmse_no_a = sqrt(mean((res[,"kap_no_a"] - kap_true)^2, na.rm=TRUE)),
    rmse_no_c = sqrt(mean((res[,"kap_no_c"] - kap_true)^2, na.rm=TRUE))
  )
}

# Figure S1 — Simulation
if (exists("sim_tbl", inherits = TRUE) && is.data.frame(sim_tbl)) {
  sim_long <- sim_tbl %>%
    select(Study, bias_no_a, bias_no_c) %>%
    tidyr::pivot_longer(-Study, names_to = "scenario", values_to = "bias") %>%
    mutate(scenario = recode(scenario,
                             bias_no_a = "Suppress MRD−/PET+ (a=0)",
                             bias_no_c = "Suppress MRD+/PET− (c=0)"
    ))
  print(
    ggplot(sim_long, aes(x = bias, y = scenario)) +
      geom_vline(xintercept = 0, linetype = 2) +
      geom_boxplot(outlier.shape = NA) +
      geom_point(alpha = 0.6, position = position_jitter(height = 0.1, width = 0), size = 1.4) +
      labs(
        title = "Figure S1. Under-reporting simulation — bias in Cohen’s κ",
        x = "Bias in κ vs full 2×2 (simulation median per study)",
        y = NULL
      ) +
      theme_minimal(base_size = 12)
  )
} else {
  cat("*Figure S1 omitted: simulation unavailable.*") }

# =========================================
# Supplementary Figure S2 — Decision-curve analysis (dual-negative rule)
# =========================================
dca_dualneg <- function(p_base = seq(0.05, 0.40, by = 0.01), hr, prev_dn) {
  # Simple net-benefit sketch for de-escalation if dual-negative
  # Treat-all NB and rule NB computed on event risk scale; illustrative as in appendix.
  other_risk <- p_base
  dn_risk <- p_base * hr
  treat_all_nb  <- other_risk - (1 - other_risk) * (p_base / (1 - p_base))
  treat_none_nb <- 0
  rule_nb <- (1 - prev_dn) * (other_risk - (1 - other_risk) * (p_base / (1 - p_base))) +
    prev_dn * (0)  # no treatment when dual-negative
  tibble::tibble(pt = p_base, NB_rule = rule_nb, NB_treat_all = treat_all_nb, NB_treat_none = treat_none_nb)
}

if (exists("dca_tbl", inherits = TRUE) && is.data.frame(dca_tbl)) {
  print(
    ggplot(dca_tbl, aes(pt)) +
      geom_line(aes(y = NB_rule)) +
      geom_line(aes(y = NB_treat_all), linetype = 2) +
      geom_hline(yintercept = 0, linetype = 3) +
      labs(
        title = "Figure S2. Decision-curve analysis — de-escalate if dual-negative",
        x = "Threshold probability", y = "Net benefit"
      ) +
      theme_minimal(base_size = 12)
  )
} else {
  cat("*Figure S2 omitted: DCA inputs unavailable.*")
}



# Create and save a metafor forest plot to a PNG
forest_png <- function(fit, slab = NULL, file, xlab = NULL,
                       width_px = 1800, height_px = 1400, res = 300, ...) {
  if (!requireNamespace("metafor", quietly = TRUE)) stop("Package 'metafor' is required")
  if (!requireNamespace("grDevices", quietly = TRUE)) stop("Package 'grDevices' is required")
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  
  # Open device (prefer ragg for crisp PNG)
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(filename = file, width = width_px, height = height_px,
                  units = "px", res = res, background = "white")
  } else {
    grDevices::png(filename = file, width = width_px, height = height_px,
                   units = "px", res = res, bg = "white", type = "cairo")
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  
  # Pass concrete values to avoid metafor's NSE on `slab`
  args <- list(x = fit, xlab = xlab, ...)
  if (!is.null(slab)) args$slab <- slab
  do.call(metafor::forest, args)
  invisible(file)
}



####################
forest_lancet_bw_hr <- function(
    fit, tab,
    file_base = "Figure3A_HR_Forest_LancetBW_final",
    xlab      = "Hazard ratio (dual-negative vs others)",
    alim_hr   = c(0.25, 4),
    at_hr     = c(0.25, 0.5, 1, 2, 4),
    width_mm  = 107,
    height_mm = NULL,
    cex_body  = 0.8
){
  stopifnot(inherits(fit, "rma.uni"))
  k <- length(stats::weights(fit))
  
  # ---- dynamic height: ~9 mm per row + header ------------------------------
  if (is.null(height_mm)) height_mm <- 18 + 9 * k
  
  # ---- extract per-study info ---------------------------------------------
  # N: prefer an explicit N column; otherwise leave blank (better than guessing wrong)
  n <- if ("N" %in% names(tab)) {
    tab$N
  } else if ("n" %in% names(tab)) {
    tab$n
  } else {
    rep(NA_integer_, k)
  }
  
  study <- if ("StudyShort" %in% names(tab)) {
    tab$StudyShort
  } else if ("Study" %in% names(tab)) {
    tab$Study
  } else {
    as.character(seq_len(k))
  }
  
  # log(HR) variance if needed
  vi <- if ("vi" %in% names(tab)) {
    tab$vi
  } else if ("SE" %in% names(tab)) {
    tab$SE^2
  } else {
    rep(NA_real_, k)
  }
  
  hr     <- if ("HR"    %in% names(tab)) tab$HR    else exp(tab$logHR)
  hr_lo  <- if ("HR_lo" %in% names(tab)) tab$HR_lo else exp(tab$logHR - 1.96 * sqrt(vi))
  hr_hi  <- if ("HR_hi" %in% names(tab)) tab$HR_hi else exp(tab$logHR + 1.96 * sqrt(vi))
  
  est_txt <- sprintf("%.2f [%.2f, %.2f]", hr, hr_lo, hr_hi)
  
  wt      <- stats::weights(fit)
  wt_pct  <- round(100 * wt / sum(wt), 1)
  psize   <- sqrt(wt / max(wt, na.rm = TRUE)) * 1.8
  w_txt   <- sprintf("%.1f", wt_pct)
  
  # ---- axis on log scale ---------------------------------------------------
  alim <- log(alim_hr)
  at   <- log(at_hr)
  
  # gutters (log scale coordinates)
  x_null <- 0                      # log(1)
  x_left <- alim[1] - 0.95         # N column
  x_w    <- alim[2] + 1.35         # Weight (%)
  x_est  <- alim[2] + 2.75         # estimate [CI]
  xlim   <- c(alim[1] - 1.15, alim[2] + 3.1)
  
  draw_once <- function() {
    op <- par(
      mar = c(4, 5, 2, 2),
      cex = cex_body,
      lwd = 0.9,
      xpd = NA,
      family = "Helvetica"
    )
    on.exit(par(op), add = TRUE)
    
    # main forest (squares; no metafor header or RE polygon)
    do.call(metafor::forest, list(
      x          = fit,
      slab       = study,
      atransf    = exp,
      xlab       = paste0(xlab, " — log scale"),
      xlim       = xlim,
      alim       = alim,
      at         = at,
      psize      = psize,
      col        = "black",
      header     = FALSE,
      annotate   = FALSE,
      addfit     = FALSE,
      showweights= FALSE
    ))
    
    # reference line at HR = 1
    abline(v = x_null, lty = 2, col = "black")
    
    # headers
    y_top <- par("usr")[4] - 0.5
    text(x_left, y_top, "N",          cex = 0.85, font = 2, pos = 4)
    text(x_w,    y_top, "Weight (%)", cex = 0.85, font = 2, pos = 2)
    text(x_est,  y_top, "Estimate [95% CI]", cex = 0.85, font = 2, pos = 2)
    
    # rows (1 = bottom in metafor’s forest)
    y_rows  <- k:1
    num_cex <- 0.9 * cex_body
    
    # N (only if at least one non-NA)
    if (any(!is.na(n))) {
      text(x_left, y_rows,
           labels = ifelse(is.na(n), "", format(n, big.mark = ",")),
           pos = 4, cex = num_cex)
    }
    
    text(x_w,   y_rows, w_txt,   pos = 2, cex = num_cex)
    text(x_est, y_rows, est_txt, pos = 2, cex = num_cex)
    
    invisible(TRUE)
  }
  
  # ---- export --------------------------------------------------------------
  dir.create(here::here("figs"), recursive = TRUE, showWarnings = FALSE)
  w_in <- width_mm  / 25.4
  h_in <- height_mm / 25.4
  
  pdf_path <- here::here("figs", paste0(file_base, ".pdf"))
  eps_path <- here::here("figs", paste0(file_base, ".eps"))
  png_path <- here::here("figs", paste0(file_base, ".png"))
  
  grDevices::pdf(pdf_path, width = w_in, height = h_in,
                 family = "Helvetica", useDingbats = FALSE)
  draw_once(); grDevices::dev.off()
  
  grDevices::postscript(eps_path, width = w_in, height = h_in,
                        family = "Helvetica",
                        paper = "special", horizontal = FALSE, onefile = FALSE)
  draw_once(); grDevices::dev.off()
  
  grDevices::png(png_path, width = w_in, height = h_in,
                 units = "in", res = 300, type = "cairo", bg = "white")
  draw_once(); grDevices::dev.off()
  
  invisible(list(pdf = pdf_path, eps = eps_path, png = png_path))
}


########################
prep_discordance_df <- function(per_study) {
  stopifnot(is.data.frame(per_study), all(c("a","c") %in% names(per_study)))
  
  df <- per_study %>%
    dplyr::mutate(
      study_id = as.character(.data$study_id %||% seq_len(dplyr::n())),
      StudyShort = dplyr::if_else("StudyShort" %in% names(per_study), as.character(.data$StudyShort),study_id),
      a = as.numeric(.data$a),
      c = as.numeric(.data$c),
      N = dplyr::case_when(
        "n" %in% names(per_study) ~ as.integer(.data$n),
        all(c("b","d") %in% names(per_study)) ~ as.integer(.data$a + .data$b + .data$c + .data$d),
        TRUE ~ NA_integer_
      ),
      yi  = log((a + 0.5) / (c + 0.5)),
      vi  = 1/(a + 0.5) + 1/(c + 0.5),
      se  = sqrt(vi)
    ) %>%
    dplyr::filter(is.finite(.data$yi), is.finite(.data$se))
  
  if (nrow(df) < 2L) return(NULL)
  df
}

#####################

fig_2B_primary_lancet <- function(per_study_primary,
                                  out_file = here::here("figs","Figure_2B_Directional_Discordance_PRIMARY.png")) {
  df_disc <- prep_discordance_df(per_study_primary)
  if (is.null(df_disc) || nrow(df_disc) < 2L) return(invisible(NULL))
  
  # REML + Knapp–Hartung meta-analysis on log-odds
  res <- metafor::rma(yi = df_disc$yi, vi = df_disc$vi,
                      method = "REML", test = "knha")
  
  forest_2B_lancet(
    d        = df_disc,
    res      = res,
    title    = "B. Directional discordance (primary analysis: log-odds)",
    out_file = out_file
  )
}

####################

forest_2B_lancet <- function(d, res, title = "B. Directional discordance (log-odds)",
                             out_file = here::here("figs","Figure_2B_Directional_Discordance.png"),
                             also_pdf = TRUE) {
  stopifnot(nrow(d) >= 2)
  d2 <- d %>%
    dplyr::mutate(
      w_RE  = 1/(vi + res$tau2),
      w_pct = 100 * w_RE / sum(w_RE),
      yi_lo = yi - 1.96*se,
      yi_hi = yi + 1.96*se
    ) %>%
    dplyr::arrange(dplyr::desc(w_pct)) %>%
    dplyr::mutate(row_id = dplyr::row_number())
  
  # Symmetric x-limits around 0 based on the data, with gutters for N and Weight columns
  x_core <- range(c(d2$yi_lo, d2$yi_hi, as.numeric(res$b), 0), na.rm = TRUE)
  xmax   <- max(abs(x_core))
  xmax   <- min(4, max(1, xmax * 1.05))           # cap at ±4
  x_core <- c(-xmax, xmax)
  pad    <- 0.20 * diff(x_core)                   # extra space for text gutters
  
  # --- X range: keep grid at -6..6, push text outside that frame -------------
  axis_lim <- c(-6, 6)      # where tick marks + grid lines live
  pad      <- 0.8           # extra space on each side for text gutters
  
  # full plotting range (so text is not clipped)
  xlim   <- c(axis_lim[1] - pad, axis_lim[2] + pad)
  
  # N column just to the left of x = -6
  x_left  <- axis_lim[1] - 0.25   # e.g. -6.25
  
  # Weight / Estimate column just to the right of x = 6
  x_right <- axis_lim[2] + 0.25   # e.g.  6.25
  
  
  
  p <- ggplot2::ggplot(d2, ggplot2::aes(y = row_id)) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = yi_lo, xmax = yi_hi),
                            height = 0.18, linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(x = yi, size = w_pct),
                        shape = 22, stroke = 0.3, fill="black") +
    ggplot2::scale_size(range = c(1.4, 4.8), guide = "none") +
    ggplot2::geom_vline(xintercept = 0,linetype = "dashed", linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = as.numeric(res$b), linetype = "dashed", linewidth = 0.5, colour = "#333333") +
    # Left "N" column
    ggplot2::geom_text(ggplot2::aes(x = x_left, label = ifelse(is.finite(N), scales::comma(N), "NA")),
                       hjust = 1, size = 3) +
    ggplot2::annotate("text", x = x_left, y = max(d2$row_id) + 0.65, label = "N",
                      hjust = 1, fontface = "bold", size = 3.3) +
    # Right "Weight   Estimate [95% CI]" column
    ggplot2::geom_text(
      ggplot2::aes(
        x = x_right,
        label = sprintf("%.1f   %.2f [%.2f, %.2f]", w_pct, yi, yi_lo, yi_hi)
      ),
      hjust = 0, size = 3
    ) +
    labs(x = "log-odds (MRD−/PET+ vs MRD+/PET−) (95% CI)\n>0 = MRD−/PET+ predominance; <0 = MRD+/PET− predominance") +
    ggplot2::annotate("text", x = x_right, y = max(d2$row_id) + 0.65,
                      label = "Weight (%)   Estimate [95% CI]",
                      hjust = 0, fontface = "bold", size = 3.3) +
    ggplot2::scale_y_continuous(
      breaks = d2$row_id, labels = d2$StudyShort,
      expand = ggplot2::expansion(add = c(0.8, 1.0))
    ) +
    ggplot2::scale_x_continuous(
      limits = xlim,
      breaks = seq(axis_lim[1], axis_lim[2], by = 2),
      name   = "log-odds (MRD−/PET+ vs MRD+/PET−) (95% CI)"
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(title = title) +
    ggplot2::theme_minimal(base_size = 10,
                           base_family = if (Sys.info()[["sysname"]] %in% c("Darwin","Linux")) "Helvetica" else "Arial") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "grey90", linewidth = 0.25),
      axis.title.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 10, r = 170, b = 10, l = 100),
      legend.position = "none"
    )
  
  print(p)
  
  # Caption + export (mirrors 2A)
  pr <- try(metafor::predict(res), silent = TRUE)
  pi_txt <- if (!inherits(pr, "try-error") && all(is.finite(c(pr$pi.lb, pr$pi.ub))))
    sprintf("; PI = %.2f–%.2f", pr$pi.lb, pr$pi.ub) else ""
  
  cap <- sprintf(
    "B. Directional discordance: log-odds(MRD−/PET+ vs MRD+/PET−). Dotted vertical line = 0; dashed = pooled (REML, Knapp–Hartung). Pooled = %.2f (%.2f–%.2f); \u03C4\u00B2 = %.3f; I\u00B2 = %.0f%%%s.",
    as.numeric(res$b), as.numeric(res$ci.lb), as.numeric(res$ci.ub),
    as.numeric(res$tau2), as.numeric(res$I2), pi_txt
  )
  
  if (requireNamespace("knitr", quietly = TRUE) &&
      requireNamespace("htmltools", quietly = TRUE)) {
    knitr::asis_output(sprintf(
      "<p style='text-align:center'><em>%s</em></p>",
      htmltools::htmlEscape(cap)
    ))
  }
  
  if (requireNamespace("fs", quietly = TRUE)) fs::dir_create(dirname(out_file))
  ggplot2::ggsave(out_file, p, width = 7.5, height = 6, dpi = 600, bg = "white")
  if (capabilities("cairo") && also_pdf) {
    grDevices::cairo_pdf(sub("\\.png$", ".pdf", out_file), width = 7.5, height = 6,
                         family = if (Sys.info()[["sysname"]] %in% c("Darwin","Linux")) "Helvetica" else "Arial")
    print(p); grDevices::dev.off()
  }
  
  invisible(list(plot = p, res = res, df = d2, caption = cap, file = out_file))
}

