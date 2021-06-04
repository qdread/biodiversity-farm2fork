library(shiny)

ui <- fluidPage(

    # Application title
    titlePanel('Biodiversity: Farm to Fork'),

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
            # FIXME it would be nice if these map-only options would only appear if map tab is selected
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
            radioButtons('log_scale',
                         'Log-transform data scale for display?',
                         selected = TRUE,
                         c('Yes' = TRUE,
                           'No' = FALSE))
            ### FIXME MORE INPUT OPTIONS SHOULD APPEAR HERE allowing the user to select one or more categories for ag goods, 
            ### FIXME land use types, or taxa, or the total, depending on what flow_type is selected.
            ### FIXME it would also be nice if the log-transformation adaptively defaults to a sensible default. (though it should usually be true)
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

server <- function(input, output) {

    output$plot <- renderPlot({
        # THE PLOT SHOULD COMPARE ALL 10 SCENARIOS
        
        # Dummy plot
        x <- faithful[, 2]
        hist(x, breaks = 5, col = 'darkgray', border = 'white')
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
