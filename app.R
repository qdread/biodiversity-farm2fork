# For issues that need fixing please see github.com/qdread/biodiversity-farm2fork/issues

library(shiny)
library(data.table)
library(sf)
library(ggplot2)
library(gt)

shinyOptions(cache = cachem::cache_mem(max_size = 20e6)) # Set cache size to approx 20 MB.

# Initial processing ------------------------------------------------------

source('fns.R')

# Load data (currently done by simply loading a large .RData with 12 R objects in it)
load('data/all_app_data.RData')

# For each dataset, subset the baseline out, rename the columns, remove scenario columns, then join baseline back to the data
county_goods_flow_sums <- get_baseline(county_goods_flow_sums)
county_land_flow_sums <- get_baseline(county_land_flow_sums)
county_extinction_flow_sums <- get_baseline(county_extinction_flow_sums)
foreign_goods_flow_sums <- get_baseline(foreign_goods_flow_sums)
foreign_land_flow_sums <- get_baseline(foreign_land_flow_sums)
foreign_extinction_flow_sums <- get_baseline(foreign_extinction_flow_sums)

# Join lookup tables with descriptive names to the data where needed.
county_goods_flow_sums <- bea_lookup[county_goods_flow_sums, on = .(BEA_code)]

# Create name value pairs for the flow subcategories menu options
taxa_options <- c('plants', 'amphibians', 'birds', 'mammals', 'reptiles')
goods_options <- setNames(bea_lookup[['BEA_code']][1:10], bea_lookup[['ag_good_short_name']][1:10])
land_options <- c('annual', 'permanent', 'pasture')
names(land_options) <- c('Annual cropland', 'Permanent cropland', 'Pastureland')

# Names of input variables to be used for cache key expression
input_vars <- c('flow_type', 'flow_origin', 'flow_direction', 'scenario_diet', 'scenario_waste', 'normalize', 'log_scale', 'map_type', 'goods_subcats', 'land_subcats', 'taxa_subcats')

# gg stuff
plot_theme <- theme_bw() +
    theme(strip.background = element_blank(),
          legend.position = 'bottom')
map_theme <- plot_theme +
    theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
brewer_cols <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", 
                 "#A6761D", "#666666", "#E41A1C", "#377EB8")

diet_x_labels <- c('Baseline\ndiet', 'USDA\nU.S.-style', 'USDA\nMed.-style', 'USDA\nvegetarian', 'Planetary\nHealth')
diet_long_names <- c('baseline diet', 'USDA U.S.-style', 'USDA Mediterranean-style', 'USDA vegetarian', 'Planetary Health')
waste_long_names <- c('baseline food waste', '50% waste reduction')

# UI ----------------------------------------------------------------------

ui <- fluidPage(

    # Application title
    titlePanel('Biodiversity: Farm to Fork'),

    # Sidebar with options
    sidebarLayout(
        sidebarPanel(
            radioButtons('flow_type',
                         'Flow type',
                         selected = 'land',
                         c('Agricultural goods' = 'goods',
                           'Virtual land transfers' = 'land',
                           'Virtual biodiversity threat transfers' = 'biodiv')),
            radioButtons('flow_direction',
                         'Flow direction',
                         c('Imports (consumption)' = 'inbound',
                           'Exports (production)' = 'outbound')),
            # FIXME it would be nice if this origin option would only appear if flow direction is set to inbound
            radioButtons('flow_origin',
                         'Flow origin (for imports only)',
                         c('Domestic only' = 'domestic',
                           'Foreign only' = 'foreign',
                           'Total (domestic + foreign)' = 'total')),
            # FIXME it would be nice if these map-only options would only appear if map tab is selected ( the next 2 menus)
            selectInput('scenario_diet',
                        'Diet shift scenario (for map only)',
                        c('Baseline diet' = 'baseline',
                          'USDA U.S.-style' = 'usstyle',
                          'USDA Mediterranean-style' = 'medstyle',
                          'USDA vegetarian' = 'vegetarian',
                          'EAT-Lancet Planetary Health' = 'planetaryhealth')),
            selectInput('scenario_waste',
                        'Food waste reduction scenario (for map only)',
                        c('Baseline food waste' = 'baseline',
                          '50% food waste reduction' = 'allavoidable')),
            radioButtons('normalize',
                         'Normalize values relative to baseline?',
                         selected = FALSE,
                         c('Yes' = TRUE,
                           'No' = FALSE)),
            # FIXME it would also be nice if the log-transformation adaptively defaults to a sensible default. (though it should usually be true)
            radioButtons('log_scale',
                         'Log-transform data scale for display?',
                         selected = TRUE,
                         c('Yes' = TRUE,
                           'No' = FALSE)),
            # FIXME the map_type option should only appear if map tab is selected
            radioButtons('map_type',
                         'Which map to display?',
                         c('USA by county' = 'usa',
                           'World by country' = 'world')),
            # FIXME The following input options should only be enabled if the corresponding flow_type is selected.
            # FIXME desired behavior is to have all selected at first, then you can deselect them all with one click to select one. (currently you have to deselect each one individually I think)
            selectInput('goods_subcats',
                        'Which goods to display?',
                        multiple = TRUE,
                        selected = goods_options[1],
                        goods_options
                        ),
            selectInput('land_subcats',
                        'Which land use types to display?',
                        multiple = TRUE,
                        selected = land_options,
                        land_options
            ), 
            selectInput('taxa_subcats',
                        'Which taxonomic groups to display?',
                        multiple = TRUE,
                        selected = taxa_options,
                        taxa_options
            )
        ),

        # Show plots
        mainPanel(
            tabsetPanel(type = 'tabs',
                        tabPanel('Plot', plotOutput('plot')),
                        tabPanel('Table', gt_output('table')),
                        tabPanel('Map', plotOutput('map'))
            )
        )
    )
)

# Data preparation function -----------------------------------------------

# This function is called within each of the three tabs to return data to be plotted
prepare_data <- function(input, tab_id) {
    scale_fn <- ifelse(input[['log_scale']], scale_y_log10, scale_y_continuous)
    scale_label_fn <- ifelse(input[['normalize']], scales::label_percent, scales::label_comma)

    # Define column name to plot, depending on flow direction and flow origin
    col_value <- paste('flow', input[['flow_direction']], input[['flow_origin']], sep = '_')
    col_baseline <- paste(col_value, 'baseline', sep = '_')
    
    # Select data frame to plot.
    # Subset rows based on selected subcategories.
    # Calculate sum of flows by scenarios, summing across all selected subcategories.
    if (input[['flow_type']] == 'goods') {
        # FIXME Inbound foreign goods will be in units of tonnes. Currently not supported.
        
        fill_var <- 'ag_good_short_name'
        scale_name <- 'Agricultural goods category'
        y_name <- 'Agricultural goods flow (million USD)'
        fill_name <- 'Agricultural goods flow\n(million USD)'
        if (tab_id %in% c('plot', 'table')) {
            dat <- copy(county_goods_flow_sums)
            dat <- dat[BEA_code %in% input[['goods_subcats']], 
                       lapply(.SD, sum), by = .(ag_good_short_name, scenario_diet, scenario_waste), .SDcols = c(col_value, col_baseline)]
        } else {
            if (input[['map_type']] == 'usa') {
                dat <- copy(county_goods_flow_sums)
                dat <- dat[BEA_code %in% input[['goods_subcats']] & scenario_diet %in% input[['scenario_diet']] & scenario_waste %in% input[['scenario_waste']], 
                           lapply(.SD, sum), by = .(county, scenario_diet, scenario_waste), .SDcols = c(col_value, col_baseline)]
            } else {
                # FIXME Currently not supported to show foreign goods flows on map.
            }
        }
    }
    
    if (input[['flow_type']] == 'land') {
        
        fill_var <- 'land_type'
        scale_name <- 'Land use category'
        y_name <- parse(text = 'Land~flow~(km^2)')
        fill_name <- y_name
        if (tab_id %in% c('plot', 'table')) {
            dat <- copy(county_land_flow_sums)
            dat <- dat[land_type %in% input[['land_subcats']], 
                       lapply(.SD, sum), by = .(land_type, scenario_diet, scenario_waste), .SDcols = c(col_value, col_baseline)]
        } else {
            if (input[['map_type']] == 'usa') {
                dat <- copy(county_land_flow_sums)
                dat <- dat[land_type %in% input[['land_subcats']] & scenario_diet %in% input[['scenario_diet']] & scenario_waste %in% input[['scenario_waste']], 
                           lapply(.SD, sum), by = .(county, land_type, scenario_diet, scenario_waste), .SDcols = c(col_value, col_baseline)]
            } else {
                # Hardcode value column names for world map.
                col_value <- 'flow_outbound_foreign'
                col_baseline <- 'flow_outbound_foreign_baseline'
                dat <- copy(foreign_land_flow_sums)
                dat <- dat[land_type %in% input[['land_subcats']] & scenario_diet %in% input[['scenario_diet']] & scenario_waste %in% input[['scenario_waste']],
                           lapply(.SD, sum), by = .(ISO_A3, scenario_diet, scenario_waste), .SDcols = c(col_value, col_baseline)]
            }
        }
    }
    
    if (input[['flow_type']] == 'biodiv') {
        
        fill_var <- 'taxon'
        scale_name <- 'Taxonomic group'
        y_name <- 'Biodiversity threat flow (potential extinctions)'
        fill_name <- 'Biodiversity threat flow\n(potential extinctions)'
        if (tab_id %in% c('plot', 'table')) {
            dat <- copy(county_extinction_flow_sums)
            dat <- dat[land_type %in% input[['land_subcats']] & taxon %in% input[['taxa_subcats']], 
                       lapply(.SD, sum), by = .(taxon, scenario_diet, scenario_waste), .SDcols = c(col_value, col_baseline)]
        } else {
            if (input[['map_type']] == 'usa') {
                dat <- copy(county_extinction_flow_sums)
                dat <- dat[land_type %in% input[['land_subcats']] & taxon %in% input[['taxa_subcats']] & scenario_diet %in% input[['scenario_diet']] & scenario_waste %in% input[['scenario_waste']], 
                           lapply(.SD, sum), by = .(county, scenario_diet, scenario_waste), .SDcols = c(col_value, col_baseline)]
            } else {
                # Hardcode value column names for world map.
                col_value <- 'flow_outbound_foreign'
                col_baseline <- 'flow_outbound_foreign_baseline'
                dat <- copy(foreign_extinction_flow_sums)
                dat <- dat[land_type %in% input[['land_subcats']] & taxon %in% input[['taxa_subcats']] & scenario_diet %in% input[['scenario_diet']] & scenario_waste %in% input[['scenario_waste']],
                           lapply(.SD, sum), by = .(ISO_A3, scenario_diet, scenario_waste), .SDcols = c(col_value, col_baseline)]
            }
        }
    }
    
    # Normalize value by baseline if selected
    if (input[['normalize']]) {
        set(dat, j = col_value, value = dat[[col_value]]/dat[[col_baseline]])
        y_name <- 'Flow relative to baseline scenario'
    }
    
    return(list(data = dat, scale_fn = scale_fn, scale_label_fn = scale_label_fn, fill_var = fill_var, scale_name = scale_name, y_name = y_name, fill_name = fill_name, col_value = col_value, col_baseline = col_baseline))
}


# Function to check validity of combinations ------------------------------

is_valid_combo <- function(input) {
    any(c(
        input[['flow_type']] == 'goods' & length(input[['goods_subcats']]) > 0,
        input[['flow_type']] == 'land' & length(input[['land_subcats']]) > 0,
        input[['flow_type']] == 'biodiv' & length(input[['taxa_subcats']]) > 0 & length(input[['land_subcats']]) > 0
    ))
    
}

valid_combo_msg <- function(input) {
    x <- switch(input[['flow_type']],
           'land' = 'land use types.',
           'goods' = 'agricultural goods.',
           'biodiv' = 'taxonomic groups and one of the land use types.')
    paste('Please select at least one of the', x)
}

# Server function (render plots and maps) ---------------------------------

server <- function(input, output) {
    
    output$plot <- renderCachedPlot({
        validate(need(is_valid_combo(input), valid_combo_msg(input)))
        
        tab_data <- prepare_data(input, tab_id = 'plot')
        
        plot_geom <- if(input[['log_scale']]) geom_point(size = 3, position = position_dodge(width = 0.1)) else geom_col(position = 'dodge')

        ggplot(tab_data[['data']], aes_string(x = 'scenario_diet', y = tab_data[['col_value']], fill = tab_data[['fill_var']], color = tab_data[['fill_var']])) +
            facet_wrap(~ scenario_waste, nrow = 1, labeller = labeller(scenario_waste = c('baseline' = 'Baseline food waste',
                                                                                          'allavoidable' = '50% food waste reduction'))) +
            plot_geom +
            tab_data[['scale_fn']](name = tab_data[['y_name']], expand = expansion(mult = c(0, 0.02)), labels = tab_data[['scale_label_fn']](drop0trailing = TRUE)) +
            scale_fill_manual(name = tab_data[['scale_name']], values = brewer_cols) +
            scale_color_manual(name = tab_data[['scale_name']], values = brewer_cols) +
            scale_x_discrete(name = 'Diet scenario', labels = diet_x_labels) +
            plot_theme
        
    },
    cacheKeyExpr = { lapply(input_vars, function(x) input[[x]]) })
    
    output$table <- render_gt({
        validate(need(is_valid_combo(input), valid_combo_msg(input)))
        
        tab_data <- prepare_data(input, tab_id = 'table')
        tab_data[['data']][, grep('baseline', names(tab_data[['data']]), value = TRUE) := NULL]
        
        # Use long names for diet and waste scenarios in table.
        tab_data[['data']][, scenario_diet := factor(scenario_diet, labels = diet_long_names)]
        tab_data[['data']][, scenario_waste := factor(scenario_waste, labels = waste_long_names)]
        
        flow_units <- switch(input[['flow_type']], land = '(hectares)', goods = '(million USD)', biodiv = '(potential extinctions)')
        
        flow_col_name <- ifelse(input[['normalize']], 'Change in flow relative to baseline', paste('Flow', flow_units))
        
        # If scale is divergent, create a palette centered at no change (0 or 1)
        if (input[['normalize']]) {
            # FIXME Uncomment the below line if we want to center at zero, then change to center=0
            # set(tab_data[['data']], j = tab_data[['col_value']], value = tab_data[['data']][[tab_data[['col_value']]]] - 1)
            # Remap scale range so that it is centered at 1.
            fill_scale_range_remap <- scale_begin_end(tab_data[['data']][[tab_data[['col_value']]]], center = 1)
            color_palette <- scico::scico(9, palette = 'vik', begin = fill_scale_range_remap[1], end = fill_scale_range_remap[2])
        } else {
            color_palette <- 'Reds'
        }
        
        gt(tab_data[['data']][order(scenario_diet, scenario_waste)]) %>%
            cols_label(.list = as.list(c(tab_data[['scale_name']], 'Diet scenario', 'Waste scenario', flow_col_name)) %>% setNames(names(tab_data[['data']]))) %>%
            data_color(tab_data[['col_value']], colors = color_palette, alpha = 0.75) %>%
            fmt_number(columns = 4, n_sigfig = 3) %>%
            fmt_percent(columns = ifelse(input[['normalize']], 4, 0), decimals = 1, drop_trailing_zeros = TRUE)
    })

    output$map <- renderCachedPlot({
        
        validate(
            need(is_valid_combo(input), valid_combo_msg(input)),
            need(!(input[['map_type']] == 'world' & input[['flow_direction']] == 'inbound'),
                 'Only outbound flows (production) can be shown for foreign countries.'),
            need(!(input[['map_type']] == 'world' & input[['flow_type']] == 'goods'),
                 'Only land and biodiversity flows can be shown for foreign countries.')
        )
        
        tab_data <- prepare_data(input, tab_id = 'map')
        
        # Get scale to be used across all maps
        vals <- tab_data[['data']][[tab_data[['col_value']]]]
        if (input[['log_scale']]) scale_range <- range(vals[vals > 0], na.rm = TRUE) else scale_range <- range(vals, na.rm = TRUE)
        
        # Use color scale depending on whether divergent or sequential is needed
        if (input[['normalize']]) {
            # FIXME Uncomment the below line if we want to center at zero, then change to center=0
            # set(tab_data[['data']], j = tab_data[['col_value']], value = tab_data[['data']][[tab_data[['col_value']]]] - 1)
            # Remap scale range so that it is centered at 1.
            fill_scale_range_remap <- scale_begin_end(vals, center = 1)
            
            color_scale <- scico::scale_fill_scico(name = tab_data[['fill_name']], trans = ifelse(input[['log_scale']], 'log10', 'identity'), limits = scale_range, palette = 'vik', begin = fill_scale_range_remap[1], end = fill_scale_range_remap[2], labels = tab_data[['scale_label_fn']](drop0trailing = TRUE))
        } else {
            color_scale <- scale_fill_viridis_c(name = tab_data[['fill_name']], trans = ifelse(input[['log_scale']], 'log10', 'identity'), limits = scale_range, labels = tab_data[['scale_label_fn']](drop0trailing = TRUE))
        }
        
        if (input[['map_type']] == 'world') {
            tab_data[['data']] <- merge(global_country_map, tab_data[['data']], by = 'ISO_A3', all.x = TRUE)
        } else {
            tab_data[['data']] <- merge(county_map, tab_data[['data']], by = 'county', all.x = TRUE)
        }
        
        ggplot() +
            geom_sf(data = tab_data[['data']], aes_string(fill = tab_data[['col_value']]), size = 0.25) +
            color_scale +
            map_theme +
            theme(legend.position = 'top', legend.key.width = unit(1.2, 'cm'))
    },
    cacheKeyExpr = { lapply(input_vars, function(x) input[[x]]) })
}

shinyApp(ui = ui, server = server)
