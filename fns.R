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

### DATA PREPARATION FUNCTION
# This function is called within each of the three tabs to return data to be plotted
prepare_data <- function(input) {
  scale_fn <- ifelse(input[['log_scale']], scale_y_log10, scale_y_continuous)
  flow_type <- input[['flow_type']]
  
  # Define column name to plot, depending on flow direction and flow origin
  col_value <- paste('flow', input[['flow_direction']], input[['flow_origin']], sep = '_')
  col_baseline <- paste(col_value, 'baseline', sep = '_')
  
  # Select data frame to plot.
  # Subset rows based on selected subcategories.
  # Calculate sum of flows by scenarios, summing across all selected subcategories.
  if (flow_type == 'goods') {
    # FIXME Inbound foreign goods will be in units of tonnes.
    
    dat <- copy(county_goods_flow_sums)
    fill_var <- 'ag_good_short_name'
    scale_name <- 'Agricultural goods category'
    y_name <- 'Agricultural goods flow (million USD)'
    dat_plot <- dat[BEA_code %in% input[['goods_subcats']], 
                    lapply(.SD, sum), by = .(ag_good_short_name, scenario_diet, scenario_waste), .SDcols = c(col_value, col_baseline)]
    dat_map <- dat[BEA_code %in% input[['goods_subcats']] & scenario_diet %in% input[['scenario_diet']] & scenario_waste %in% input[['scenario_waste']], 
                   lapply(.SD, sum), by = .(county, ag_good_short_name, scenario_diet, scenario_waste), .SDcols = c(col_value, col_baseline)]
  }
  
  if (flow_type == 'land') {
    dat <- copy(county_land_flow_sums)
    fill_var <- 'land_type'
    scale_name <- 'Land use category'
    y_name <- parse(text = 'Land~flow~(km^2)')
    dat_plot <- dat[land_type %in% input[['land_subcats']], 
                    lapply(.SD, sum), by = .(land_type, scenario_diet, scenario_waste), .SDcols = c(col_value, col_baseline)]
    dat_map <- dat[land_type %in% input[['land_subcats']] & scenario_diet %in% input[['scenario_diet']] & scenario_waste %in% input[['scenario_waste']], 
                   lapply(.SD, sum), by = .(county, land_type, scenario_diet, scenario_waste), .SDcols = c(col_value, col_baseline)]
  }
  
  if (flow_type == 'biodiv') {
    dat <- copy(county_extinction_flow_sums)
    fill_var <- 'taxon'
    scale_name <- 'Taxonomic group'
    y_name <- 'Biodiversity threat flow (potential extinctions)'
    dat_plot <- dat[land_type %in% input[['land_subcats']] & taxon %in% input[['taxa_subcats']], 
                    lapply(.SD, sum), by = .(taxon, scenario_diet, scenario_waste), .SDcols = c(col_value, col_baseline)]
    dat_map <- dat[land_type %in% input[['land_subcats']] & taxon %in% input[['taxa_subcats']] & scenario_diet %in% input[['scenario_diet']] & scenario_waste %in% input[['scenario_waste']], 
                   lapply(.SD, sum), by = .(county, taxon, scenario_diet, scenario_waste), .SDcols = c(col_value, col_baseline)]
  }
  
  # Normalize value by baseline if selected
  if (input[['normalize']]) {
    set(dat_plot, j = col_value, value = dat_plot[[col_value]]/dat_plot[[col_baseline]])
    set(dat_map, j = col_value, value = dat_map[[col_value]]/dat_map[[col_baseline]])
    y_name <- 'Flow relative to baseline scenario'
  }
  
  return(list(data = dat_plot, data_map = dat_map, scale_fn = scale_fn, fill_var = fill_var, scale_name = scale_name, y_name = y_name, col_value = col_value, col_baseline = col_baseline))
}
