# For issues that need fixing please see github.com/qdread/biodiversity-farm2fork/issues

library(shiny)
library(data.table)
library(sf)
library(ggplot2)
library(DT)
library(shinyjs)

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
scenario_diet_options <- c('Baseline diet' = 'baseline',
                           'USDA U.S.-style' = 'usstyle',
                           'USDA Mediterranean-style' = 'medstyle',
                           'USDA vegetarian' = 'vegetarian',
                           'EAT-Lancet Planetary Health' = 'planetaryhealth')
scenario_waste_options <- c('Baseline food waste' = 'baseline',
                            '50% food waste reduction' = 'allavoidable')

# Create all combinations for foreign land and foreign extinction so that no NA regions appear on map
foreign_land_flow_sums <- foreign_land_flow_sums[CJ(ISO_A3 = global_country_map$ISO_A3, land_type = land_options, scenario_diet = scenario_diet_options, scenario_waste = scenario_waste_options), on = .NATURAL]
foreign_extinction_flow_sums <- foreign_extinction_flow_sums[CJ(ISO_A3 = global_country_map$ISO_A3, land_type = land_options, taxon = taxa_options, scenario_diet = scenario_diet_options, scenario_waste = scenario_waste_options), on = .NATURAL]

foreign_land_flow_sums[is.na(flow_outbound_foreign), flow_outbound_foreign := 0]
foreign_land_flow_sums[is.na(flow_outbound_foreign_baseline), flow_outbound_foreign_baseline := 0]
foreign_extinction_flow_sums[is.na(flow_outbound_foreign), flow_outbound_foreign := 0]
foreign_extinction_flow_sums[is.na(flow_outbound_foreign_baseline), flow_outbound_foreign_baseline := 0]

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
  
  useShinyjs(),  # Set up shinyjs
  
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
      uiOutput("flow_origin"),
      uiOutput("subcats_output"),
      uiOutput("selectall"),
      checkboxInput('normalize',
                    'Normalize values relative to baseline',
                    value = FALSE),
      checkboxInput('log_scale',
                    'Log-transform data scale for display',
                    value = TRUE),
      checkboxInput('separate_cats',
                    'Display categories separately',
                    value = TRUE),
      # map specific options
      conditionalPanel(condition="input.tabselected==3", 
                       selectInput('scenario_diet', 'Diet shift scenario (for map only)', scenario_diet_options),
                       selectInput('scenario_waste','Food waste reduction scenario (for map only)', scenario_waste_options)),
      
    ),
    
    # Show plots
    mainPanel(
      tabsetPanel(type = 'tabs',
                  tabPanel('Plot', value = 1, plotOutput('plot')),
                  tabPanel('Table', value = 2, dataTableOutput('table')),
                  tabPanel('Map', value = 3, plotOutput('map')),
                  id = 'tabselected'
      )
    )
  )
)

# Server function (render plots and maps) ---------------------------------

server <- function(input, output, session) {
  
  # turn off log scale option for normalize
  observe({ 
    toggleState("log_scale", condition = input$normalize == FALSE)
  })
  
  # flow origin UI based on selected tab
  output$flow_origin <- renderUI({
    switch(input$tabselected,
           "3" = radioButtons('flow_origin','Footprint origin', c('Domestic only' = 'domestic','Foreign only' = 'foreign'), selected = input$flow_origin),
           "2" = radioButtons('flow_origin','Footprint origin', c('Domestic only' = 'domestic','Foreign only' = 'foreign','Total (domestic + foreign)' = 'total'), selected = input$flow_origin),
           "1" = radioButtons('flow_origin','Footprint origin', c('Domestic only' = 'domestic','Foreign only' = 'foreign','Total (domestic + foreign)' = 'total'), selected = input$flow_origin)
    )
  })
  
  # subcategory selection based on flow type
  output$subcats_output <- renderUI({
    switch(input$flow_type, 
           "goods" = checkboxGroupInput('goods_subcats',
                                        'Which goods to display?',
                                        selected = goods_options,
                                        choices = goods_options),
           "land" = checkboxGroupInput('land_subcats',
                                       'Which land use types to display?',
                                       selected = land_options,
                                       choices = land_options),
           "biodiv" = checkboxGroupInput('taxa_subcats',
                                         'Which taxonomic groups to display?',
                                         selected = taxa_options,
                                         choices = taxa_options)
    )
  })
  
  # select all button and math
  output$selectall <- renderUI({
    switch(input$flow_type, 
           "goods" = actionButton("selectall_goods", label="Select/Deselect all goods"),
           "land" = actionButton("selectall_land", label="Select/Deselect all land use types"),
           "biodiv" =  actionButton("selectall_taxa", label="Select/Deselect all taxa")
    )
  })
  observe({
    req(input$selectall_goods)
    if (input$selectall_goods > 0) {
      if (input$selectall_goods %% 2 == 0){
        updateCheckboxGroupInput(session=session, inputId="goods_subcats",
                                 choices = goods_options,
                                 selected = goods_options)}
      else {
        updateCheckboxGroupInput(session=session, inputId="goods_subcats",
                                 choices = goods_options,
                                 selected = c())}}
  })
  
  observe({
    req(input$selectall_land)
    
    if (input$selectall_land > 0) {
      if (input$selectall_land %% 2 == 0){
        updateCheckboxGroupInput(session=session, inputId="land_subcats",
                                 choices = land_options,
                                 selected = land_options)}
      else {
        updateCheckboxGroupInput(session=session, inputId="land_subcats",
                                 choices = land_options,
                                 selected = c())}}
  })
  observe({
    req(input$selectall_taxa)
    
    if (input$selectall_taxa > 0) {
      if (input$selectall_taxa %% 2 == 0){
        updateCheckboxGroupInput(session=session, inputId="taxa_subcats",
                                 choices = taxa_options,
                                 selected = taxa_options)}
      else {
        updateCheckboxGroupInput(session=session, inputId="taxa_subcats",
                                 choices = taxa_options,
                                 selected = c())}}
  })
  
  output$plot <- renderCachedPlot({
    validate(need(is_valid_combo(input), valid_combo_msg(input)), 
             need(!(input[['flow_origin']] %in% c('foreign', 'total') & input[['flow_type']] == 'goods'),
                  'Only land and biodiversity footprints can be shown for foreign countries.'))
    
    tab_data <- prepare_data(input, tab_id = 'plot')
    
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
    validate(need(is_valid_combo(input), valid_combo_msg(input)),
             need(!(input[['flow_origin']] %in% c('foreign', 'total') & input[['flow_type']] == 'goods'),
                  'Only land and biodiversity footprints can be shown for foreign countries.'))
    
    tab_data <- prepare_data(input, tab_id = 'table')
    tab_data[['data']][, grep('baseline', names(tab_data[['data']]), value = TRUE) := NULL]
    
    setcolorder(tab_data[['data']], c(switch(input[['flow_type']], land = 'land_type', goods = 'ag_good_short_name', biodiv = 'taxon'), 
                                      'scenario_diet', 'scenario_waste', tab_data[['col_value']]))
    
    # Use long names for diet and waste scenarios in table.
    tab_data[['data']][, scenario_diet := factor(scenario_diet, labels = diet_long_names)]
    tab_data[['data']][, scenario_waste := factor(scenario_waste, labels = waste_long_names)]
    
    flow_units <- switch(input[['flow_type']], land = '(km<sup>2</sup>)', goods = '(million USD)', biodiv = '(potential extinctions)')
    
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
    
    dt <- datatable(tab_data[['data']][order(scenario_diet, scenario_waste)], escape = FALSE,
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
    
    category_name <- switch(input[['flow_type']], land = 'land_type', goods = 'ag_good_short_name', biodiv = 'taxon')
    
    vals <- tab_data[['data']][[tab_data[['col_value']]]]
    
    # Use color scale depending on whether divergent or sequential is needed
    if (input[['normalize']]) {
      scale_range <- range(vals, na.rm = TRUE)
      fill_scale_range_remap <- scale_begin_end(vals, center = 0)
      
      color_scale <- scico::scale_fill_scico(name = tab_data[['fill_name']], trans = 'identity', limits = scale_range, palette = 'vik', begin = fill_scale_range_remap[1], end = fill_scale_range_remap[2], labels = tab_data[['scale_label_fn']](drop0trailing = TRUE))
    } else {
      if (input[['log_scale']]) scale_range <- range(vals[vals > 1e-6], na.rm = TRUE) else scale_range <- range(vals, na.rm = TRUE) # Note: here I round extremely small values to 0 for log scale.
      color_scale <- scale_fill_viridis_c(name = tab_data[['fill_name']], trans = ifelse(input[['log_scale']], 'log10', 'identity'), limits = scale_range, labels = tab_data[['scale_label_fn']](drop0trailing = TRUE))
    }
    
    if (input[['flow_origin']] == 'foreign') {
      tab_data[['data']] <- merge(global_country_map, tab_data[['data']], by = 'ISO_A3', all.x = TRUE)
    } else {
      tab_data[['data']] <- merge(county_map, tab_data[['data']], by = 'county', all.x = TRUE)
    }
    
    # FIXME How can we make the map maximize the available space better?
    ggplot() +
      geom_sf(data = tab_data[['data']], aes_string(fill = tab_data[['col_value']]), color = NA) +
      (if (input[['separate_cats']]) facet_wrap(as.formula(paste('~', category_name))) else NULL) +
      color_scale +
      map_theme +
      theme(legend.position = 'top', legend.key.width = unit(1.2, 'cm'))
  },
  cacheKeyExpr = { lapply(input_vars, function(x) input[[x]]) })
}

shinyApp(ui = ui, server = server)
