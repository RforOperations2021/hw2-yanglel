library(shiny)
library(tidyverse)
library(ggplot2)
library(tools)
library(shinydashboard)
library(plotly)

# load data
load("PA_M_2006_2018_full.RData")
pop_max <- max(PA_M_2006_2018_full$Population)
PA_M_2006_2018_full$deficit <- factor(PA_M_2006_2018_full$deficit, label = c("No", "Yes"))
year_min <- min(PA_M_2006_2018_full$Reporting_Year)
year_max <- max(PA_M_2006_2018_full$Reporting_Year)
county <- unique(c(PA_M_2006_2018_full$county_name, PA_M_2006_2018_full$county_name_2))
county <- county[!is.na(county)]


# Avoid plotly issues 
pdf(NULL)

# Define UI
ui <- dashboardPage(
    # change dashboard color
    skin = "black",
    
    # create header
    dashboardHeader(
        title = "PA Municipalities Public Finance Data (2006-2018)",
        titleWidth = 500
        ),
    
    # create sidebar
    dashboardSidebar(
        sidebarMenu(
            menuItem("Surplus/Deficit", tabName = "SD", icon = icon("money-bill-wave")),
            
            menuItem("External Revenue Dependency", tabName = "Ext", icon = icon("landmark")),
            
            menuItem("Debt", tabName = "Debt", icon = icon("credit-card")),
            
            # create filters
            sliderInput(
                inputId = "year",
                label = "Max Year (Scatterplot & Bar Chart) & Year Range (Line Chart & Data Table)",
                min = year_min,
                max = year_max,
                step = 1,
                value = c(year_min, year_max),
                dragRange = T,
                sep = "",
                ticks = F
            ),
            
            # check box for municipal type
            checkboxGroupInput(
                inputId = "type",
                label = "Municipality Type",
                choices = c("City",
                            "Borough",
                            "First Class Township",
                            "Second Class Township"
                ),
                selected = c("City",
                             "Borough",
                             "First Class Township",
                             "Second Class Township"
                )
            ),
            
            # create county comparison filters
            selectInput(
                inputId = "county",
                label = "County (Type to search, backspace to delete)",
                choices = county,
                selectize = T ,
                multiple =  T
            ),
            
            # download button
            downloadButton(
                outputId = "download", 
                label = "Download Data Table"
            )
            
        )
        ),
    
    # create body
    dashboardBody(
        tabItems(
            # first tab
            tabItem(tabName = "SD",
                    h2("Is Revenue Enough To Cover Expenditure?"),
                    
                    fluidRow(
                        tabBox(
                            width = 12,
                            tabPanel("Tab1", "First tab content"),
                            tabPanel("Tab2", "Tab content 2")
                        )
                    )
                    ),
        
            tabItem(tabName = "Ext",
                    h2("Is The Municipality Too Dependent On Other Governments For Revenue?")
                    ),
            
            tabItem(tabName = "Debt",
                    h2("Is There Too Much Debt?")
                    )
        )
        
        
        
    )
) 
    
    
    
    
#     fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )

# Define server logic required to draw a histogram
server <- function(input, output) {

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
