# ==============================================
# File: R/utils_io.R
# ==============================================

# Minimal IO helpers and directory bootstrap.
# -------- shared helpers ------------------------------------------------------
if (!exists("%||%")) {
  `%||%` <- function(a, b) {
    if (is.null(a)) return(b)
    if (length(a) == 0) return(b)
    if (length(a) == 1 && is.na(a)) return(b)
    a
  }
}

quiet_message <- function(...) {
  if (isTRUE(getOption("io.verbose", FALSE))) message(...)
}

.path_join <- function(...) {
  if (requireNamespace("here", quietly = TRUE)) do.call(here::here, list(...)) else file.path(...)
}

dir_setup <- function(root = ".") {
  dirs <- c("data", "R", "figs", "tables", "outputs", "manuscript")
  for (d in dirs) {
    p <- .path_join(root, d)
    if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(dirs)
}

# -------- UTF-8 normalisation -------------------------------------------------
.normalize_utf8 <- function(x) {
  if (is.null(x)) return("")
  x  <- as.character(x)
  x2 <- suppressWarnings(iconv(x, from = "", to = "UTF-8", sub = " "))
  bad <- which(is.na(x2))
  if (length(bad)) x2[bad] <- gsub("[^[:print:][:space:]]+", " ", x[bad], useBytes = TRUE)
  x2
}

trySuppress <- function(expr) {
  try(suppressWarnings(suppressMessages(force(expr))), silent = TRUE)
}

# -------- CSV reader with sniffing -------------------------------------------
safe_read_csv <- function(path,
                          na = c("", "NA", "NaN", "null", "NULL", "#N/A"),
                          guess_max = 10000,
                          col_types = NULL) {
  stopifnot(length(path) == 1)
  if (!file.exists(path)) stop(sprintf("File not found: %s", path))
  if (!requireNamespace("readr", quietly = TRUE)) stop("Package 'readr' is required")
  if (!requireNamespace("janitor", quietly = TRUE)) stop("Package 'janitor' is required")
  
  # Heuristics on first non-empty line
  lines <- readLines(path, n = 5, warn = FALSE)
  lines <- lines[nzchar(lines)]
  first <- if (length(lines)) lines[[1]] else ""
  n_comma <- stringr::str_count(first, ",")
  n_semi  <- stringr::str_count(first, ";")
  # choose delimiter by majority on first line
  delim <- if (n_semi > n_comma) ";" else ","
  
  # decimal comma if we see clear 1,23 patterns and semicolon is the delimiter
  dec_comma <- grepl("\\d,\\d", first) && delim == ";"
  
  locale_utf8 <- readr::locale(encoding = "UTF-8", decimal_mark = if (dec_comma) "," else ".")
  locale_l1   <- readr::locale(encoding = "latin1", decimal_mark = if (dec_comma) "," else ".")
  
  quiet_message(sprintf("Reading CSV: %s (delim='%s', dec='%s')", path, delim, if (dec_comma) "," else "."))
  
  df <- trySuppress(
    readr::read_delim(path, delim = delim, locale = locale_utf8,
                      na = na, guess_max = guess_max, col_types = col_types, show_col_types = FALSE)
  )
  if (inherits(df, "try-error")) {
    df <- readr::read_delim(path, delim = delim, locale = locale_l1,
                            na = na, guess_max = guess_max, col_types = col_types, show_col_types = FALSE)
  }
  
  df <- janitor::clean_names(df)
  chr_cols <- names(df)[vapply(df, is.character, logical(1))]
  if (length(chr_cols)) df[chr_cols] <- lapply(df[chr_cols], .normalize_utf8)
  df
}

# -------- XLSX reader ---------------------------------------------------------
safe_read_xlsx <- function(path, sheet = 1,
                           na = c("", "NA", "NaN", "null", "NULL", "#N/A"),
                           col_types = NULL) {
  stopifnot(length(path) == 1)
  if (!file.exists(path)) stop(sprintf("File not found: %s", path))
  if (!requireNamespace("readxl", quietly = TRUE)) stop("Package 'readxl' is required")
  if (!requireNamespace("janitor", quietly = TRUE)) stop("Package 'janitor' is required")
  
  sheets <- trySuppress(readxl::excel_sheets(path))
  if (inherits(sheets, "try-error")) stop(sprintf("Cannot list sheets: %s", path))
  
  if (is.character(sheet)) {
    if (!sheet %in% sheets) stop(sprintf("Sheet '%s' not found. Available: %s", sheet, paste(sheets, collapse = ", ")))
  } else {
    if (sheet < 1 || sheet > length(sheets)) stop(sprintf("Sheet index %d out of range (1..%d).", sheet, length(sheets)))
    sheet <- sheets[[sheet]]
  }
  
  quiet_message(sprintf("Reading XLSX: %s [sheet='%s']", path, sheet))
  
  df <- readxl::read_xlsx(path, sheet = sheet, na = na, col_types = col_types)
  df <- janitor::clean_names(df)
  chr_cols <- names(df)[vapply(df, is.character, logical(1))]
  if (length(chr_cols)) df[chr_cols] <- lapply(df[chr_cols], .normalize_utf8)
  df
}

# -------- Bundle inputs -------------------------------------------------------
read_all_inputs <- function(mrd_path, screen_path, meta_extra_path = NULL, ...) {
  mrd <- safe_read_csv(mrd_path, ...)
  screen <- if (!is.null(screen_path) && file.exists(screen_path)) safe_read_xlsx(screen_path, ...) else NULL
  meta_extra <- if (!is.null(meta_extra_path) && file.exists(meta_extra_path)) safe_read_csv(meta_extra_path, ...) else NULL
  list(mrd = mrd, screen = screen, meta_extra = meta_extra)
}

# -------- Safe writers --------------------------------------------------------
safe_write_csv <- function(df, path, na = "") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_excel_csv(df, file = path, na = na)  # Excel-friendly UTF-8 BOM
  invisible(path)
}

safe_write_xlsx <- function(dfs, path) {
  if (!requireNamespace("writexl", quietly = TRUE)) stop("Install 'writexl'")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  
  if (inherits(dfs, "data.frame")) {
    return(writexl::write_xlsx(dfs, path))
  }
  if (is.list(dfs)) {
    if (is.null(names(dfs)) || any(!nzchar(names(dfs)))) {
      names(dfs) <- paste0("Sheet", seq_along(dfs))
    }
    return(writexl::write_xlsx(dfs, path))
  }
  stop("safe_write_xlsx: 'dfs' must be a data.frame or a named list of data.frames.")
}


show_png <- function(dir, file = NULL, caption = NULL, width = "95%") {
  `%||%` <- function(x, y) if (is.null(x) || !length(x) || (length(x)==1 && is.na(x))) y else x
  path <- if (is.null(file)) dir else file.path(dir, file)
  
  # Resolve via here() if needed
  if (!file.exists(path) && requireNamespace("here", quietly = TRUE)) {
    try_path <- tryCatch(here::here(dir, file), error = function(e) NA_character_)
    if (!is.na(try_path) && file.exists(try_path)) path <- try_path
  }
  if (!file.exists(path)) {
    return(knitr::asis_output(
      sprintf("<p style='color:#a00'><em>Missing image: %s</em></p>", file.path(dir, file %||% ""))))
  }
  
  # Prefer repo-relative if possible
  use_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (requireNamespace("here", quietly = TRUE)) {
    root <- normalizePath(here::here(), winslash = "/", mustWork = TRUE)
    rel  <- sub(paste0("^", root, "/?"), "", use_path)
    if (file.exists(rel)) use_path <- rel
  }
  
  # If knitting self-contained, let knitr embed it for us
  selfc <- isTRUE(knitr::opts_knit$get("self.contained"))
  if (selfc) {
    # Optional caption line (knitr won’t add one here)
    if (!is.null(caption) && nzchar(caption)) {
      knitr::asis_output(
        sprintf("<div style='text-align:center;margin:.3em 0 .6em 0'><em>%s</em></div>",
                htmltools::htmlEscape(caption)))
    }
    # Return include_graphics as the final value so knitr inlines it
    return(knitr::include_graphics(use_path))
  }
  
  # Otherwise: raw HTML (repo-relative link works on GitHub/Pages)
  cap_html <- if (!is.null(caption) && nzchar(caption)) {
    sprintf("<figcaption style='text-align:center'><em>%s</em></figcaption>",
            htmltools::htmlEscape(caption))
  } else ""
  html <- sprintf(
    "<figure style='margin:0 auto; text-align:center'>
       <img src='%s' style='max-width:%s; height:auto;' alt='%s' />
       %s
     </figure>",
    use_path, width, basename(use_path), cap_html
  )
  knitr::asis_output(html)
}


# -------- Conditional source helper ------------------------------------------
source_if <- function(..., local = if (exists("knitr")) knitr::knit_global() else parent.frame()) {
  paths <- unlist(list(...))
  paths <- paths[nzchar(paths)]
  for (p in paths) {
    if (file.exists(p)) {
      base::source(p, local = local, echo = FALSE)
      quiet_message(sprintf("Sourced: %s", p))
      return(TRUE)
    }
  }
  quiet_message(sprintf("Skipped (not found): %s", paste(paths, collapse = ", ")))
  FALSE
}


panel_limits <- function(p) {
  gb <- ggplot2::ggplot_build(p)
  pp <- gb$layout$panel_params[[1]]
  xr <- tryCatch(`%||%`(pp$x.range, `%||%`(pp$x$range$range, c(NA_real_, NA_real_))), error = function(e) c(NA_real_, NA_real_))
  yr <- tryCatch(`%||%`(pp$y.range, `%||%`(pp$y$range$range, c(NA_real_, NA_real_))), error = function(e) c(NA_real_, NA_real_))
  list(x = xr, y = yr)
}


# Sanitize captions for LaTeX output (escape characters that break TeX)
cap_sanitize <- function(x) {
  if (knitr::is_latex_output()) {
    x <- gsub("\\", "\\textbackslash{}", x, fixed = TRUE)
    x <- gsub("%", "\\%", x, fixed = TRUE)
    x <- gsub("_", "\\_", x, fixed = TRUE)
    x <- gsub("#", "\\#", x, fixed = TRUE)
    x <- gsub("&", "\\&", x, fixed = TRUE)
  }
  x
}
