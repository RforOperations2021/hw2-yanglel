library(shiny)
library(tidyverse)
library(ggplot2)
library(tools)
library(shinydashboard)
library(plotly)

# load data
load("PA_M_2006_2018_full.RData")

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
            
            menuItem("Debt", tabName = "Debt", icon = icon("credit-card"))
        )
        ),
    
    # create body
    dashboardBody(
        tabItems(
            tabItem(tabName = "SD",
                    h2("Is Revenue Enough To Cover Expenditure?")
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
