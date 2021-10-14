# For issues that need fixing please see github.com/qdread/biodiversity-farm2fork/issues

library(shiny)
library(data.table)
library(sf)
library(ggplot2)
library(DT)

shinyOptions(cache = cachem::cache_mem(max_size = 50e6)) # Set cache size to approx 50 MB.

# Initial processing ------------------------------------------------------

source('fns.R', local = TRUE)

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
names(land_options) <- c('annual cropland', 'permanent cropland', 'pastureland')

# Names of input variables to be used for cache key expression
input_vars <- c('flow_type', 'flow_origin', 'scenario_diet', 'scenario_waste', 'normalize', 'log_scale', 'separate_cats', 'goods_subcats', 'land_subcats', 'taxa_subcats')

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
                         'Footprint type',
                         selected = 'land',
                         c('Agricultural goods' = 'goods',
                           'Land' = 'land',
                           'Biodiversity threat' = 'biodiv')),
            radioButtons('flow_origin',
                         'Footprint origin',
                         c('Domestic only' = 'domestic',
                           'Foreign only' = 'foreign',
                           'Total (domestic + foreign)' = 'total')),
            # FIXME It would be best to disable the total option if map tab is active.
            selectInput('scenario_diet',
                        'Diet shift scenario (this option only applies to map)',
                        c('Baseline diet' = 'baseline',
                          'USDA U.S.-style' = 'usstyle',
                          'USDA Mediterranean-style' = 'medstyle',
                          'USDA vegetarian' = 'vegetarian',
                          'EAT-Lancet Planetary Health' = 'planetaryhealth')),
            selectInput('scenario_waste',
                        'Food waste reduction scenario (this option only applies to map)',
                        c('Baseline food waste' = 'baseline',
                          '50% food waste reduction' = 'allavoidable')),
            # FIXME The above two menus should only be available if map tab is active.
            checkboxInput('normalize',
                          'Normalize values relative to baseline',
                          value = FALSE),
            # FIXME it would also be nice if the log-transformation adaptively defaults to a sensible default. (though it should usually be true)
            # FIXME also the log transform is entirely ignored if normalize == TRUE. Maybe disable it in that case?
            checkboxInput('log_scale',
                          'Log-transform data scale for display',
                          value = TRUE),
            checkboxInput('separate_cats',
                          'Display categories separately',
                          value = TRUE),
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
                        tabPanel('Table', dataTableOutput('table')),
                        tabPanel('Map', plotOutput('map'))
            )
        )
    )
)

# Server function (render plots and maps) ---------------------------------

server <- function(input, output) {
    
    output$plot <- renderCachedPlot({
        validate(need(is_valid_combo(input), valid_combo_msg(input)))
        
        tab_data <- prepare_data(input, tab_id = 'plot')
        
        # If separate_cats is unchecked, sum the subcategories together
        if (!input[['separate_cats']]) {
            cat_column <- names(tab_data[['data']])[1]
            tab_data[['data']] <- tab_data[['data']][, lapply(.SD, sum), by = .(scenario_diet, scenario_waste), .SDcols = tab_data[['col_value']]]
            tab_data[['data']][, (cat_column) := 'total']
            setcolorder(tab_data[['data']], c(cat_column, 'scenario_diet', 'scenario_waste', tab_data[['col_value']]))
        }

        plot_geom <- if(input[['log_scale']] & !input[['normalize']]) geom_point(size = 3, position = position_dodge(width = 0.1)) else geom_col(position = 'dodge')

        ggplot(tab_data[['data']], aes_string(x = 'scenario_diet', y = tab_data[['col_value']], fill = tab_data[['fill_var']], color = tab_data[['fill_var']])) +
            facet_wrap(~ scenario_waste, nrow = 1, labeller = labeller(scenario_waste = c('baseline' = 'Baseline food waste',
                                                                                          'allavoidable' = '50% food waste reduction'))) +
            plot_geom +
            tab_data[['scale_fn']](name = tab_data[['y_name']], expand = expansion(mult = 0.01), labels = tab_data[['scale_label_fn']](drop0trailing = TRUE)) +
            scale_fill_manual(name = tab_data[['scale_name']], values = brewer_cols) +
            scale_color_manual(name = tab_data[['scale_name']], values = brewer_cols) +
            scale_x_discrete(name = 'Diet scenario', labels = diet_x_labels) +
            plot_theme
        
    },
    cacheKeyExpr = { lapply(input_vars, function(x) input[[x]]) })
    
    output$table <- renderDataTable({
        validate(need(is_valid_combo(input), valid_combo_msg(input)))
        
        tab_data <- prepare_data(input, tab_id = 'table')
        tab_data[['data']][, grep('baseline', names(tab_data[['data']]), value = TRUE) := NULL]
        
        # If separate_cats is unchecked, sum the subcategories together
        if (!input[['separate_cats']]) {
            cat_column <- names(tab_data[['data']])[1]
            tab_data[['data']] <- tab_data[['data']][, lapply(.SD, sum), by = .(scenario_diet, scenario_waste), .SDcols = tab_data[['col_value']]]
            tab_data[['data']][, (cat_column) := 'total']
            setcolorder(tab_data[['data']], c(cat_column, 'scenario_diet', 'scenario_waste', tab_data[['col_value']]))
        }
        
        # Use long names for diet and waste scenarios in table.
        tab_data[['data']][, scenario_diet := factor(scenario_diet, labels = diet_long_names)]
        tab_data[['data']][, scenario_waste := factor(scenario_waste, labels = waste_long_names)]
        
        flow_units <- switch(input[['flow_type']], land = '(hectares)', goods = '(million USD)', biodiv = '(potential extinctions)')
        
        flow_col_name <- ifelse(input[['normalize']], 'Change in footprint relative to baseline', paste('Footprint', flow_units))
        
        # If scale is divergent, create a palette centered at no change
        if (input[['normalize']]) {
            # Remap scale range so that it is centered at 0.
            fill_scale_range_remap <- scale_begin_end(tab_data[['data']][[tab_data[['col_value']]]], center = 0)
            color_palette <- scico::scico(9, palette = 'vik', begin = fill_scale_range_remap[1], end = fill_scale_range_remap[2], alpha = 0.75)
            # If the background is too dark, change text color to white.
            text_colors <- ifelse(rgb2hsv(col2rgb(color_palette))['s',] > 0.9, 'white', 'black')
        } else {
            # Hardcode Reds from RColorBrewer with alpha = 0.75
            color_palette <- c("#FFF5F0BF", "#FEE0D2BF", "#FCBBA1BF", "#FC9272BF", "#FB6A4ABF", 
                               "#EF3B2CBF", "#CB181DBF", "#A50F15BF", "#67000DBF")
            text_colors <- rep(c('black', 'white'), c(7, 2))
        }

        # Manually calculate breaks for color palette
        val_range <- range(tab_data[['data']][[tab_data[['col_value']]]])
        if(input[['log_scale']] & !input[['normalize']]) {
            color_breaks <- exp(seq(log(val_range[1]), log(val_range[2]), length.out = 10)[2:9])
        } else {
            color_breaks <- seq(val_range[1], val_range[2], length.out = 10)[2:9]
        }
        
        set(tab_data[['data']], j = tab_data[['col_value']], value = signif(tab_data[['data']][[tab_data[['col_value']]]], 3))
        
        dt <- datatable(tab_data[['data']][order(scenario_diet, scenario_waste)],
                  colnames = c(tab_data[['scale_name']], 'Diet scenario', 'Waste scenario', flow_col_name),
                  rownames = FALSE,
                  options = list(pageLength = 50)) %>%
            formatStyle(columns = tab_data[['col_value']], 
                        color = styleInterval(color_breaks, text_colors),
                        backgroundColor = styleInterval(color_breaks, color_palette)) %>%
            formatCurrency(columns = tab_data[['col_value']], currency = '') 
        if (input[['normalize']]) dt <- dt %>% formatPercentage(columns = tab_data[['col_value']], digits = 1)
        dt
    })

    output$map <- renderCachedPlot({
        
        validate(
            need(is_valid_combo(input), valid_combo_msg(input)),
            need(!(input[['flow_origin']] == 'foreign' & input[['flow_type']] == 'goods'),
                 'Only land and biodiversity footprints can be shown for foreign countries.'),
            need(input[['flow_origin']] != 'total',
                 'Summed domestic + foreign footprints cannot be shown on the map.')
        )
        
        tab_data <- prepare_data(input, tab_id = 'map')
        
        vals <- tab_data[['data']][[tab_data[['col_value']]]]
        
        # Use color scale depending on whether divergent or sequential is needed
        if (input[['normalize']]) {
            scale_range <- range(vals, na.rm = TRUE)
            fill_scale_range_remap <- scale_begin_end(vals, center = 0)
            
            color_scale <- scico::scale_fill_scico(name = tab_data[['fill_name']], trans = 'identity', limits = scale_range, palette = 'vik', begin = fill_scale_range_remap[1], end = fill_scale_range_remap[2], labels = tab_data[['scale_label_fn']](drop0trailing = TRUE))
        } else {
            if (input[['log_scale']]) scale_range <- range(vals[vals > 0], na.rm = TRUE) else scale_range <- range(vals, na.rm = TRUE)
            color_scale <- scale_fill_viridis_c(name = tab_data[['fill_name']], trans = ifelse(input[['log_scale']], 'log10', 'identity'), limits = scale_range, labels = tab_data[['scale_label_fn']](drop0trailing = TRUE))
        }
        
        if (input[['flow_origin']] == 'foreign') {
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
