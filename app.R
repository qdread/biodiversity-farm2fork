library(shiny)
library(data.table)
library(sf)
library(ggplot2)


# Initial processing ------------------------------------------------------

# Load data (currently done by simply loading a large .RData with 12 R objects in it)
load('data/all_app_data.RData')

# For each dataset, subset the baseline out, rename the columns, and remove scenario columns
# Then join baseline back to the data
get_baseline <- function(dat) {
    dat_base <- dat[scenario_diet == 'baseline' & scenario_waste == 'baseline']
    dat_base[, c('scenario_diet', 'scenario_waste') := NULL]
    flow_cols <- grep('flow', names(dat_base), value = TRUE)
    setnames(dat_base, old = flow_cols, new = paste0(flow_cols, '_baseline'))
    merge(dat, dat_base, all.x = TRUE, all.y = TRUE, by = intersect(names(dat), names(dat_base)))
}

county_goods_flow_sums <- get_baseline(county_goods_flow_sums)
county_land_flow_sums <- get_baseline(county_land_flow_sums)
county_extinction_flow_sums <- get_baseline(county_extinction_flow_sums)
foreign_goods_flow_sums <- get_baseline(foreign_goods_flow_sums)
foreign_land_flow_sums <- get_baseline(foreign_land_flow_sums)
foreign_extinction_flow_sums <- get_baseline(foreign_extinction_flow_sums)

# Join lookup tables with descriptive names to the data where needed.
county_goods_flow_sums <- bea_lookup[county_goods_flow_sums, on = .(BEA_code)]

# Create name value pairs for the flow subcategories menu options
goods_options <- setNames(bea_lookup[['BEA_code']][1:10], bea_lookup[['ag_good_short_name']][1:10])
land_options <- setNames(c('annual', 'pasture', 'permanent'), c('Annual cropland', 'Permanent cropland', 'Pastureland'))
taxa_options <- c('plants', 'amphibians', 'birds', 'mammals', 'reptiles')

# gg stuff
plot_theme <- theme_bw()
brewer_cols <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", 
                 "#A6761D", "#666666", "#E41A1C", "#377EB8")

# UI ----------------------------------------------------------------------

ui <- fluidPage(

    # Application title
    titlePanel('Biodiversity: Farm to Fork'),
    
    # FIXME it might also be nice to have the ability to crosstabulate different variables or display more than one,
    # FIXME instead of stacking within a single variable.

    # Sidebar with options
    sidebarLayout(
        sidebarPanel(
            radioButtons('flow_type',
                         'Flow type',
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
                          'USDA American-style' = 'usstyle',
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
            # FIXME The following input options should only appear at all if the corresponding flow_type is selected.
            # FIXME desired behavior is to have all selected at first, then you can deselect them all with one click to select one.
            # FIXME And if nothing is selected, a sensible error will display on plot or map like "select at least one of whatever"
            selectInput('goods_subcats',
                        'Which goods to display?',
                        multiple = TRUE,
                        selected = goods_options,
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
                        tabPanel('Table', tableOutput('table')),
                        tabPanel('Map', plotOutput('map'))
            )
        )
    )
)


# Server function (render plots and maps) ---------------------------------

server <- function(input, output) {
    
    output$plot <- renderPlot({
        scale_fn <- ifelse(input[['log_scale']], scale_y_log10, scale_y_continuous)
        flow_type <- input[['flow_type']]
        
        # Define column name to plot, depending on flow direction and flow origin
        col_value <- paste('flow', input[['flow_direction']], input[['flow_origin']], sep = '_')
        col_baseline <- paste(col_value, 'baseline', sep = '_')
        
        # Select data frame to plot.
        # Subset rows based on selected subcategories.
        # Calculate sum of flows by scenarios, summing across all selected subcategories.
        if (flow_type == 'goods') {
            dat_plot <- county_goods_flow_sums
            fill_var <- 'ag_good_short_name'
            dat_plot <- dat_plot[BEA_code %in% input[['goods_subcats']], 
                                 lapply(.SD, sum), by = .(ag_good_short_name, scenario_diet, scenario_waste), .SDcols = c(col_value, col_baseline)]
        }
        if (flow_type == 'land') {
            dat_plot <- county_land_flow_sums
            fill_var <- 'land_type'
            dat_plot <- dat_plot[land_type %in% input[['land_subcats']], 
                                 lapply(.SD, sum), by = .(land_type, scenario_diet, scenario_waste), .SDcols = c(col_value, col_baseline)]
        }
        if (flow_type == 'biodiv') {
            dat_plot <- county_extinction_flow_sums
            fill_var <- 'taxon'
            dat_plot <- dat_plot[land_type %in% input[['land_subcats']] & taxon %in% input[['taxa_subcats']], 
                                 lapply(.SD, sum), by = .(taxon, scenario_diet, scenario_waste), .SDcols = c(col_value, col_baseline)]
        }
        
        # Normalize value by baseline if selected
        if (input[['normalize']]) set(dat_plot, j = col_value, value = dat_plot[[col_value]]/dat_plot[[col_baseline]])
        
        dat_plot[, scenario := interaction(scenario_diet, scenario_waste)]
        
        # FIXME Possibly include two plots: a dodged or stacked geom_col so you can see each one separately, and 
        # FIXME a summed geom_col for the total of all categories selected.
        
        # FIXME the log scale is not good for geom_col because there is no bottom of the bar. Maybe use point?
        ggplot(dat_plot, aes_string(x = 'scenario', y = col_value, fill = fill_var)) +
            geom_col(position = 'dodge') +
            scale_fn(expand = expansion(mult = c(0, 0.02))) +
            scale_fill_manual(values = brewer_cols)
        
    })
    
    output$table <- renderTable({
        # PUT STUFF HERE. 
        # Also it might be neat to allow the user to subset the data by county or country in the table, and/or sort it
    })
    
    output$map <- renderPlot({
        # THE MAP SHOULD ONLY SHOW ONE SELECTED SCENARIO
        
    })
}

shinyApp(ui = ui, server = server)
