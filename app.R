library(shiny)
library(data.table)
library(sf)
library(ggplot2)
library(gt)
library(gridExtra)

# FIXME In general, can this all be sped up by running portions of code only when needed? i.e. make more reactive? Not sure.

# Initial processing ------------------------------------------------------

# Load data (currently done by simply loading a large .RData with 12 R objects in it)
load('data/all_app_data.RData')

# For each dataset, subset the baseline out, rename the columns, and remove scenario columns
# Then join baseline back to the data
# In the same function, convert scenario_diet and scenario_waste to ordered factors for plotting.
get_baseline <- function(dat) {
    dat[, scenario_diet := factor(scenario_diet, levels = c('baseline', 'usstyle', 'medstyle', 'vegetarian', 'planetaryhealth'))]
    dat[, scenario_waste := factor(scenario_waste, levels = c('baseline', 'allavoidable'))]
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

# Reorder land types and taxa to ordered factors for plotting
land_options <- c('annual', 'permanent', 'pasture')
taxa_options <- c('plants', 'amphibians', 'birds', 'mammals', 'reptiles')
county_land_flow_sums[, land_type := factor(land_type, levels = land_options)]
foreign_land_flow_sums[, land_type := factor(land_type, levels = land_options)]
county_extinction_flow_sums[, taxon := factor(taxon, levels = taxa_options)]
county_extinction_flow_sums[, land_type := factor(land_type, levels = land_options)]
foreign_extinction_flow_sums[, taxon := factor(taxon, levels = taxa_options)]
foreign_extinction_flow_sums[, land_type := factor(land_type, levels = land_options)]

# Create name value pairs for the flow subcategories menu options
goods_options <- setNames(bea_lookup[['BEA_code']][1:10], bea_lookup[['ag_good_short_name']][1:10])
names(land_options) <- c('Annual cropland', 'Permanent cropland', 'Pastureland')

# gg stuff
plot_theme <- theme_bw() +
    theme(strip.background = element_blank())
map_theme <- plot_theme +
    theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
brewer_cols <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", 
                 "#A6761D", "#666666", "#E41A1C", "#377EB8")

diet_x_labels <- c('Baseline', 'USDA\nAmerican-style', 'USDA\nMed.-style', 'USDA\nvegetarian', 'Planetary\nHealth')

# CRS for AK and HI
ak_crs <- '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs'
hi_crs <- '+proj=aea +lat_1=8 +lat_2=18 +lat_0=3 +lon_0=-157 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs'
# Bounding box set manually to get rid of minor outlying islands
hi_box <- c(xmin = -400000, ymin = 1761000, xmax = 230000, ymax = 2130000)

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
                        selected = goods_options[1],
                        goods_options
                        ),
            selectInput('land_subcats',
                        'Which land use types to display?',
                        multiple = TRUE,
                        selected = land_options[1],
                        land_options
            ), 
            selectInput('taxa_subcats',
                        'Which taxonomic groups to display?',
                        multiple = TRUE,
                        selected = taxa_options[1],
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
    if (input[['normalize']]) set(dat_plot, j = col_value, value = dat_plot[[col_value]]/dat_plot[[col_baseline]])
    
    return(list(data = dat_plot, data_map = dat_map, scale_fn = scale_fn, fill_var = fill_var, scale_name = scale_name, y_name = y_name, col_value = col_value, col_baseline = col_baseline))
}


# Server function (render plots and maps) ---------------------------------

server <- function(input, output) {
    
    output$plot <- renderPlot({
        tab_data <- prepare_data(input)
        
        # FIXME Possibly include two plots: a dodged or stacked geom_col so you can see each one separately, and 
        # FIXME a summed geom_col for the total of all categories selected.
        
        # FIXME the log scale is not good for geom_col because there is no bottom of the bar. Maybe use point?
        
        ggplot(tab_data[['data']], aes_string(x = 'scenario_diet', y = tab_data[['col_value']], fill = tab_data[['fill_var']])) +
            facet_wrap(~ scenario_waste, nrow = 1, labeller = labeller(scenario_waste = c('baseline' = 'Baseline food waste',
                                                                                          'allavoidable' = '50% food waste reduction'))) +
            geom_col(position = 'dodge') +
            tab_data[['scale_fn']](name = tab_data[['y_name']], expand = expansion(mult = c(0, 0.02))) +
            scale_fill_manual(name = tab_data[['scale_name']], values = brewer_cols) +
            scale_x_discrete(name = 'Diet scenario', labels = diet_x_labels) +
            plot_theme
        
    })
    
    output$table <- render_gt({
       tab_data <- prepare_data(input)
       tab_data[['data']][, grep('baseline', names(tab_data[['data']]), value = TRUE) := NULL]
       gt(tab_data[['data']]) %>%
           cols_label(.list = as.list(c('Category', 'Diet scenario', 'Waste scenario', 'Flow')) %>% setNames(names(tab_data[['data']])))  %>%
           data_color(tab_data[['col_value']], 'Reds') %>%
           fmt_number(columns = 4, n_sigfig = 3)
       
       # FIXME Improve column labels (more informative label for Category and include units on Flow)
       # FIXME Any improvements to formatting possible?
       
       # FIXME right now it's the summed up data but maybe we can also show data by county or country?
       # FIXME Also it might be neat to allow the user to subset the data by county or country in the table, and/or sort it
    })
    
    output$map <- renderPlot({
        tab_data <- prepare_data(input)
        
        tab_data[['data_map']] <- merge(county_map, tab_data[['data_map']], by = 'county')
        # FIXME Currently this is only the USA map but later we would need to add in the foreign map for those cases where it's needed

        # Get scale to be used across all three maps
        vals <- tab_data[['data_map']][[tab_data[['col_value']]]]
        if (input[['log_scale']]) vals <- vals[vals > 0]
        scale_range <- range(vals, na.rm = TRUE)
        
        p48 <- ggplot() +
            geom_sf(data = subset(tab_data[['data_map']], !fips_state %in% c('02', '15')), aes_string(fill = tab_data[['col_value']]), size = 0.25) +
            scale_fill_viridis_c(name = tab_data[['y_name']], trans = ifelse(input[['log_scale']], 'log10', 'identity'), limits = scale_range) +
            map_theme +
            theme(legend.position = 'top')
        
        pak <- ggplot() +
            geom_sf(data = subset(tab_data[['data_map']], fips_state %in% '02'), aes_string(fill = tab_data[['col_value']]), size = 0.25) +
            scale_fill_viridis_c(name = tab_data[['y_name']], trans = ifelse(input[['log_scale']], 'log10', 'identity'), limits = scale_range) +
            coord_sf(crs = ak_crs) +
            map_theme +
            theme(legend.position = 'none')
        
        phi <- ggplot() +
            geom_sf(data = subset(tab_data[['data_map']], fips_state %in% '15'), aes_string(fill = tab_data[['col_value']]), size = 0.25) +
            scale_fill_viridis_c(name = tab_data[['y_name']], trans = ifelse(input[['log_scale']], 'log10', 'identity'), limits = scale_range) +
            coord_sf(crs = hi_crs, xlim = hi_box[c('xmin','xmax')], ylim = hi_box[c('ymin','ymax')]) +
            map_theme +
            theme(legend.position = 'none')
        
        # FIXME This layout was done very lazily and needs to be cleaned up. Align panels better and make L48 map bigger and insets smaller.
        grid.arrange(p48, pak, phi, layout_matrix = rbind(c(1,1), c(2,3)), heights = c(2, 1))
        
    })
}

shinyApp(ui = ui, server = server)
