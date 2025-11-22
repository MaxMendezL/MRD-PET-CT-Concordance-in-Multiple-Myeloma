# ---- meta-diagnostics -----------------------------------------------------------
meta_diagnostics <- function(yi, sei, slab = NULL) {
  if (!requireNamespace("metafor", quietly = TRUE)) stop("Install 'metafor'")
  m <- metafor::rma(yi = yi, sei = sei, method = "REML", test = "knha", slab = slab)
  list(
    influence = metafor::influence(m),
    baujat    = metafor::baujat(m),
    egger     = try(metafor::regtest(m, model = "rma", predictor = "sei"), silent = TRUE),
    model     = m
  )
}

egger_caption_note <- function(k) {
  if (is.na(k) || k >= 10) return("")
  "Note: With <10 studies, funnel/Egger tests are exploratory and have low power."
}
