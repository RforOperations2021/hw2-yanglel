library(shiny)
library(tidyverse)
library(ggplot2)
library(tools)
library(shinydashboard)
library(plotly)

# load data
load("PA_M_2006_2018_full.RData")
year_min <- min(PA_M_2006_2018_full$Reporting_Year)
year_max <- max(PA_M_2006_2018_full$Reporting_Year)
county <- unique(c(PA_M_2006_2018_full$county_name, PA_M_2006_2018_full$county_name_2a))

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
                inputId = "county_1",
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
                                    valueBoxOutput("YearOfDeficit", width = 6 ),
                                    valueBoxOutput("Numberofdeficits", width = 6),
                                    valueBoxOutput("DeficitSize", width = 6),
                                    valueBoxOutput("surplusSize", width = 6)
                                    
                                    ),
                                
                             fluidRow(box(
                                 plotlyOutput("scatter"), 
                                 width = 12, 
                                 "Above line (y = x) means presence of surplus and below line means presence of deficit.",
                                 br(),
                                 "Outliers in both axes are removed to assist to visualization",
                                 br(),
                                 "Median is used due to large outliers.")
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




    

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # reactive subsetting for scatterplot and bar chart
    
    PA_subset <- reactive({
        req(input$year)
        req(input$type)
        a <- PA_M_2006_2018_full %>% 
            filter((Reporting_Year == max(input$year)) & (Municipality_Type %in% input$type))
        if (length(input$county_1) > 0) {
            a <- a %>% 
               filter((county_name %in% input$county_1) | (county_name_2a %in% input$county_1)) 
        }
        return(a)
    })
    
 
    # Create scatterplot
    output$scatter <- renderPlotly({
        # identify outliers
        outliers_x <- (boxplot(PA_subset()$Expenditures_Per_Capita))$out
        outliers_y <- (boxplot(PA_subset()$Revenues_Per_Capita))$out
        
        ggplotly(
        PA_subset() %>%
            filter(!(Expenditures_Per_Capita %in% outliers_x)) %>%
            filter(!(Revenues_Per_Capita %in% outliers_y)) %>%
            ggplot(aes(x = Expenditures_Per_Capita, y = Revenues_Per_Capita, text = paste(full_municipal_name,"\n", "RevPC:", Revenues_Per_Capita, "\n", "ExpPC:", Expenditures_Per_Capita) )) +
            geom_point(aes(color = Municipality_Type)) +
            geom_abline(intercept =0 , slope = 1, size = 1, color = "red",
                        linetype = "dashed") +
            geom_text(aes(x=150, label="Deficit Zone", y= 0), colour= "#D55E00")+
            geom_text(aes(x=150, label="Surplus Zone", y=500), colour= "#0072B2") +
            xlab("Expenditure per Capita($)") + ylab("Revenue Per Capita($)") +
            labs(title = paste("Revenue Per Capita vs Expenditure per Capita",
                "In",max(input$year))) + 
            scale_color_discrete(name = "Municipality Type"),
        tooltip = "text"
        )
     })
    
    # value boxes for for surplus/deficit tab
    output$YearOfDeficit <- renderValueBox({
        valueBox(round(mean(PA_subset()$no_of_deficit_year, na.rm = T),0), paste("Average Years of Deficits As Of", max(input$year) , "(out of", max(input$year) - year_min, ")"), icon = icon("calendar-minus"), color = "purple")
        })
        
        output$Numberofdeficits <- renderValueBox({
            valueBox(round(sum(PA_subset()$deficit, na.rm = T),0), paste("Municipality Facing Deficits In", max(input$year) , "(out of", nrow(PA_subset()), ")"), icon = icon("city"), color = "blue")
        })
        
        output$DeficitSize <- renderValueBox({
           a <- PA_subset() %>% 
               filter(deficit == 1)
            valueBox(paste("$", (scales::comma_format()(round(median(a$Revenues_Over_Expenditures, na.rm = T),0)))), paste("Median Deficit Size In", max(input$year)) , icon = icon("minus-circle"), color = "orange")
        })
        
        output$surplusSize <- renderValueBox({
            a <- PA_subset() %>% 
                filter(deficit == 0)
            valueBox(paste("$", (scales::comma_format()(round(median(a$Revenues_Over_Expenditures, na.rm = T),0)))), paste("Median Surplus Size In", max(input$year)) , icon = icon("plus-circle"), color = "green")
        })
        
        

    
}
    
    

# Run the application 
shinyApp(ui = ui, server = server)
