# prisma.R
# Build a PRISMA 2020 study-selection diagram from a screening log.
# Exports PNG + PDF + SVG (editable) to satisfy Lancet's requirement
# for an editable study-selection flow figure in systematic reviews/
# meta-analyses.

# Suggested packages (install if missing):
#   readxl, DiagrammeR, DiagrammeRsvg, rsvg, htmlwidgets

# ---- helpers -------------------------------------------------------------------

.prisma_check_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is required for this function. Please install it.")
  }
}

.count_true <- function(x) sum(isTRUE(as.logical(x)), na.rm = TRUE)

# ---- summarise_prisma_counts() --------------------------------------------------
# Reads an Excel screening log and computes PRISMA counts plus a table of
# full-text exclusion reasons (lowercased, trimmed).
#
# Heuristics accommodate common column names:
#   duplicates: is_duplicate / duplicate
#   screened at title/abstract: title_abstract_screened / screened
#   excluded at title/abstract: ta_exclude / excluded_title_abstract
#   full-text assessed: full_text / full_text_review
#   full-text excluded: ft_exclude / excluded_full_text
#   reason column: exclusion_reason / ft_exclusion_reason / reason
#
# If 'sheet' is NULL, all sheets are read and row-bound.

summarise_prisma_counts <- function(path_xlsx, sheet = NULL) {
  .prisma_check_pkg("readxl")
  if (!file.exists(path_xlsx)) stop("File not found: ", path_xlsx)
  
  sheets <- if (is.null(sheet)) readxl::excel_sheets(path_xlsx) else sheet
  if (!length(sheets)) stop("No sheets found in Excel workbook.")
  
  read_one <- function(sh) {
    x <- readxl::read_excel(path_xlsx, sheet = sh)
    x$..source_sheet.. <- sh
    x
  }
  lst <- lapply(sheets, read_one)
  x <- do.call(rbind, lst)
  
  nm <- tolower(names(x))
  
  # Map likely columns -> canonical fields
  if ("is_duplicate" %in% nm) x$dup <- as.logical(x[[which(nm == "is_duplicate")]])
  if ("duplicate"     %in% nm) x$dup <- as.logical(x[[which(nm == "duplicate")]])
  if (!"dup" %in% names(x)) x$dup <- FALSE
  
  if ("title_abstract_screened" %in% nm) x$screened <- as.logical(x[[which(nm == "title_abstract_screened")]])
  if ("screened"               %in% nm) x$screened <- as.logical(x[[which(nm == "screened")]])
  if (!"screened" %in% names(x)) x$screened <- TRUE
  
  if ("ta_exclude" %in% nm) x$excl_ta <- as.logical(x[[which(nm == "ta_exclude")]])
  if ("excluded_title_abstract" %in% nm) x$excl_ta <- as.logical(x[[which(nm == "excluded_title_abstract")]])
  if (!"excl_ta" %in% names(x)) x$excl_ta <- FALSE
  
  if ("full_text" %in% nm) x$full_text <- as.logical(x[[which(nm == "full_text")]])
  if ("full_text_review" %in% nm) x$full_text <- as.logical(x[[which(nm == "full_text_review")]])
  if (!"full_text" %in% names(x)) x$full_text <- FALSE
  
  if ("ft_exclude" %in% nm) x$excl_ft <- as.logical(x[[which(nm == "ft_exclude")]])
  if ("excluded_full_text" %in% nm) x$excl_ft <- as.logical(x[[which(nm == "excluded_full_text")]])
  if (!"excl_ft" %in% names(x)) x$excl_ft <- FALSE
  
  # Reasons (optional)
  reason_col <- which(nm %in% c("exclusion_reason", "ft_exclusion_reason", "reason"))
  x$reason <- if (length(reason_col) == 1L) as.character(x[[reason_col]]) else NA_character_
  x$reason <- trimws(tolower(x$reason))
  x$reason[!nzchar(x$reason)] <- NA_character_
  
  # PRISMA counts
  identified <- nrow(x)
  after_dup  <- identified - .count_true(x$dup)
  screened   <- .count_true(x$screened)      # usually equals after_dup
  excl_ta    <- .count_true(x$excl_ta)
  full_text  <- .count_true(x$full_text)
  excl_ft    <- .count_true(x$excl_ft)
  included   <- sum(isTRUE(x$full_text) & !isTRUE(x$excl_ft), na.rm = TRUE)
  
  # Reason table for full-text exclusions
  reasons <- stats::na.omit(data.frame(reason = x$reason[x$excl_ft], stringsAsFactors = FALSE))
  if (nrow(reasons)) {
    reasons <- aggregate(list(n = rep(1L, nrow(reasons))), by = list(reason = reasons$reason), FUN = sum)
    reasons <- reasons[order(reasons$n, decreasing = TRUE), , drop = FALSE]
    rownames(reasons) <- NULL
  } else {
    reasons <- data.frame(reason = character(0), n = integer(0))
  }
  
  list(
    counts  = list(identified = identified,
                   after_dup = after_dup,
                   screened = screened,
                   excl_title_abs = excl_ta,
                   full_text = full_text,
                   excl_full_text = excl_ft,
                   included = included),
    reasons = reasons,
    raw     = x
  )
}

# ---- .export_grviz() ------------------------------------------------------------
# Export a DiagrammeR object to SVG + PDF + PNG. SVG is the editable vector source.

.export_grviz <- function(gr, file_base) {
  .prisma_check_pkg("DiagrammeRsvg")
  .prisma_check_pkg("rsvg")
  
  # Export to SVG (vector)
  svg_txt <- DiagrammeRsvg::export_svg(gr)
  dir.create(dirname(file_base), recursive = TRUE, showWarnings = FALSE)
  
  # Write SVG (vector, editable)
  svg_path <- paste0(file_base, ".svg")
  con <- file(svg_path, open = "wb"); writeChar(svg_txt, con, eos = NULL); close(con)
  
  # Render PDF + PNG from the SVG (both will be redrawn in-house; PDF is still vector)
  raw_svg <- charToRaw(svg_txt)
  rsvg::rsvg_pdf(raw_svg, paste0(file_base, ".pdf"))
  rsvg::rsvg_png(raw_svg, paste0(file_base, ".png"), width = 1800, height = 1400)
  
  invisible(list(svg = svg_path,
                 pdf = paste0(file_base, ".pdf"),
                 png = paste0(file_base, ".png")))
}

# ---- make_prisma_2020() ---------------------------------------------------------
# Build a PRISMA 2020 diagram from a named counts list and export to files.

make_prisma_2020 <- function(counts,
                             file_base = file.path(getwd(), "figs", "Figure_1_PRISMA_Diagram")) {
  # Validate counts
  need <- c("identified", "after_dup", "screened", "excl_title_abs",
            "full_text", "excl_full_text", "included")
  miss <- setdiff(need, names(counts))
  if (length(miss)) stop("Counts list missing: ", paste(miss, collapse = ", "))
  
  # Diagram (clean, redraw-friendly labels)
  label <- sprintf(
    'digraph PRISMA {
      graph [rankdir=TB, layout=dot, splines=ortho]
      node  [shape=box, style="rounded,filled", fillcolor="#F8FAFC", fontname="Helvetica"]

      a [label="Records identified (n=%d)"]
      b [label="Records after duplicates removed (n=%d)"]
      c [label="Records screened (n=%d)"]
      d [label="Records excluded (n=%d)"]
      e [label="Full-text articles assessed for eligibility (n=%d)"]
      f [label="Full-text articles excluded (n=%d)"]
      g [label="Studies included in qualitative synthesis (n=%d)"]

      a->b->c->e->g
      c->d
      e->f
    }',
    counts$identified,
    counts$after_dup,
    counts$screened,
    counts$excl_title_abs,
    counts$full_text,
    counts$excl_full_text,
    counts$included
  )
  
  .prisma_check_pkg("DiagrammeR")
  gr <- DiagrammeR::grViz(label)
  
  dir.create(dirname(file_base), recursive = TRUE, showWarnings = FALSE)
  # Also save an interactive HTML for quick internal review
  if (requireNamespace("htmlwidgets", quietly = TRUE)) {
    htmlwidgets::saveWidget(gr, paste0(file_base, ".html"), selfcontained = TRUE)
  }
  
  .export_grviz(gr, file_base)
  invisible(gr)
}

# ---- build_prisma_from_screening_log() ------------------------------------------
# Convenience wrapper: read the Excel log, compute counts, export diagram,
# and write CSVs of counts and exclusion reasons for the appendix.

build_prisma_from_screening_log <- function(path_xlsx,
                                            out_base = file.path(getwd(), "figs", "Figure_1_PRISMA_Diagram"),
                                            sheet = NULL) {
  res <- summarise_prisma_counts(path_xlsx, sheet = sheet)
  make_prisma_2020(res$counts, file_base = out_base)
  
  # Save the audit tables for the appendix
  utils::write.csv(data.frame(name = names(res$counts),
                              value = unlist(res$counts, use.names = FALSE)),
                   paste0(out_base, "_Counts.csv"), row.names = FALSE)
  utils::write.csv(res$reasons, paste0(out_base, "_FullTextExclusions.csv"), row.names = FALSE)
  
  invisible(res)
}
