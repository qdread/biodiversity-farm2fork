# Function definitions for shiny app
# QDR 11 June 2021

# function to determine where to begin and end sampling the color palette for the divergent fill scale so that it is centered at 0 or 1.
# x is the vector of values
scale_begin_end <- function(x, center) {
  r <- range(x, na.rm = TRUE) - center
  out <- 0.5 + 0.5 * (r/max(abs(r)))
  if (any(!is.finite(out))) c(0, 1) else out
}

# Function to join a results dataframe with its own baseline value
get_baseline <- function(dat) {
  dat_base <- dat[scenario_diet == 'baseline' & scenario_waste == 'baseline']
  dat_base[, c('scenario_diet', 'scenario_waste') := NULL]
  flow_cols <- grep('flow', names(dat_base), value = TRUE)
  setnames(dat_base, old = flow_cols, new = paste0(flow_cols, '_baseline'))
  merge(dat, dat_base, all.x = TRUE, all.y = TRUE, by = intersect(names(dat), names(dat_base)))
}
