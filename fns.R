# Function definitions for shiny app
# QDR 11 June 2021


# Misc functions ----------------------------------------------------------

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

# Functions to check validity of combinations -----------------------------

is_valid_combo <- function(input) {
  any(c(
    input[['flow_type']] == 'goods' & length(input[['goods_subcats']]) > 0,
    input[['flow_type']] == 'land' & length(input[['land_subcats']]) > 0,
    input[['flow_type']] == 'biodiv' & length(input[['taxa_subcats']]) > 0
  ))
  
}

valid_combo_msg <- function(input) {
  x <- switch(input[['flow_type']],
              'land' = 'land use types.',
              'goods' = 'agricultural goods.',
              'biodiv' = 'taxonomic groups.')
  paste('Please select at least one of the', x)
}


# Data preparation function -----------------------------------------------

# This function is called within each of the three tabs to return data to be plotted
prepare_data <- function(input, tab_id) {
  scale_fn <- ifelse(input[['log_scale']] & !input[['normalize']], scale_y_log10, scale_y_continuous)
  scale_label_fn <- ifelse(input[['normalize']], scales::label_percent, scales::label_comma)
  
  # Define column name to plot, depending on tab_id and flow_origin
  col_value <- paste(ifelse(tab_id == 'map', 'flow_outbound', 'flow_inbound'), input[['flow_origin']], sep = '_')
  col_baseline <- paste(col_value, 'baseline', sep = '_')
  
  # Select data frame to plot.
  # Subset rows based on selected subcategories.
  # Calculate sum of flows by scenarios, summing across all selected subcategories if separate_cats is checked.
  if (input[['flow_type']] == 'goods') {
    # FIXME Inbound foreign goods will be in units of tonnes. Currently not supported.
    
    fill_var <- 'ag_good_short_name'
    scale_name <- 'Agricultural goods category'
    y_name <- 'Agricultural goods flow (million USD)'
    fill_name <- if (input[['normalize']]) 'Change in goods footprint' else 'Agricultural goods footprint\n(million USD)'
    
    dat <- copy(county_goods_flow_sums)
    dat <- dat[BEA_code %in% input[['goods_subcats']]]
    if (tab_id == 'map') dat <- dat[scenario_diet %in% input[['scenario_diet']] & scenario_waste %in% input[['scenario_waste']]]
    
    sum_by <- c(if(tab_id == 'map') 'county' else NULL,
                if(input[['separate_cats']]) 'ag_good_short_name' else NULL,
                'scenario_diet', 'scenario_waste')
    
    dat <- dat[, lapply(.SD, sum), by = sum_by, .SDcols = c(col_value, col_baseline)]
    if (!input[['separate_cats']]) dat[, ag_good_short_name := 'total of selected goods']
    
  }
  
  if (input[['flow_type']] == 'land') {
    
    fill_var <- 'land_type'
    scale_name <- 'Land use category'
    y_name <- parse(text = 'Land~footprint~(km^2)')
    fill_name <- if (input[['normalize']]) 'Change in land footprint' else y_name
    
    if (tab_id == 'map' & input[['flow_origin']] == 'foreign') {
      dat <- copy(foreign_land_flow_sums)
      map_group_var <- 'ISO_A3'
    } else {
      dat <- copy(county_land_flow_sums)
      map_group_var <- 'county'
    }
    
    dat <- dat[land_type %in% input[['land_subcats']]]
    if (tab_id == 'map') dat <- dat[scenario_diet %in% input[['scenario_diet']] & scenario_waste %in% input[['scenario_waste']]]
    
    sum_by <- c(if(tab_id == 'map') map_group_var else NULL,
                if(input[['separate_cats']]) 'land_type' else NULL,
                'scenario_diet', 'scenario_waste')
    
    dat <- dat[, lapply(.SD, sum), by = sum_by, .SDcols = c(col_value, col_baseline)]
    if (!input[['separate_cats']]) dat[, land_type := 'total of selected land types']
    
  }
  
  if (input[['flow_type']] == 'biodiv') {
    
    fill_var <- 'taxon'
    scale_name <- 'Taxonomic group'
    y_name <- 'Biodiversity threat footprint (potential extinctions)'
    fill_name <- if (input[['normalize']]) 'Change in biodiversity threat footprint' else 'Biodiversity threat footprint\n(potential extinctions)'
    
    if (tab_id == 'map' & input[['flow_origin']] == 'foreign') {
      dat <- copy(foreign_extinction_flow_sums)
      map_group_var <- 'ISO_A3'
    } else {
      dat <- copy(county_extinction_flow_sums)
      map_group_var <- 'county'
    }
    
    dat <- dat[taxon %in% input[['taxa_subcats']]]
    if (tab_id == 'map') dat <- dat[scenario_diet %in% input[['scenario_diet']] & scenario_waste %in% input[['scenario_waste']]]
    
    sum_by <- c(if(tab_id == 'map') map_group_var else NULL,
                if(input[['separate_cats']]) 'taxon' else NULL,
                'scenario_diet', 'scenario_waste')
    
    dat <- dat[, lapply(.SD, sum), by = sum_by, .SDcols = c(col_value, col_baseline)]
    if (!input[['separate_cats']]) dat[, taxon := 'total of selected taxa']

  }
  
  # Normalize value by baseline if selected
  if (input[['normalize']]) {
    set(dat, j = col_value, value = dat[[col_value]]/dat[[col_baseline]] - 1)
    y_name <- 'Footprint relative to baseline scenario'
  }
  
  return(list(data = dat, scale_fn = scale_fn, scale_label_fn = scale_label_fn, fill_var = fill_var, scale_name = scale_name, y_name = y_name, fill_name = fill_name, col_value = col_value, col_baseline = col_baseline))
}
