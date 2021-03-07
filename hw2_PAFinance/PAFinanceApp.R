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
# get list of county names
county <- unique(c(PA_M_2006_2018_full$county_name, PA_M_2006_2018_full$county_name_2a))
# create date variable
PA_M_2006_2018_full$date <- as.Date(
    paste(PA_M_2006_2018_full$Reporting_Year, 1, 1, sep="-"))

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
            menuItem("Surplus/Deficit", 
                     tabName = "SD", 
                     icon = icon("money-bill-wave")),
            
            menuItem("External Revenue Dependency", 
                     tabName = "Ext", 
                     icon = icon("landmark")),
            
            menuItem("Debt", 
                     tabName = "Debt", 
                     icon = icon("credit-card")),
            
            # create filters
            sliderInput(
                inputId = "year",
                label = "Year",
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
    
    # create body--------------------------------
    dashboardBody(
        tabItems(
            # first page------------------------
            tabItem(tabName = "SD",
                    h2("Is Revenue Enough To Cover Expenditure?"),
                    fluidRow(
                        valueBoxOutput("YearOfDeficit", width = 3 ),
                        valueBoxOutput("Numberofdeficits", width = 3),
                        valueBoxOutput("DeficitSize", width = 3),
                        valueBoxOutput("surplusSize", width = 3)
                    ),
                    
                    fluidRow(
                        box(plotlyOutput("scatter"), 
                            width = 12, 
                            "Above line (y = x) means presence of surplus and below line means presence of deficit.",
                            br(),
                            "Outliers in both axes are removed to assist visualization",
                            br(),
                            "Median is used due to large outliers.")
                    )
            ),
            
            # second page---------------------------------
            tabItem(tabName = "Ext",
                    h2("Is The Municipality Too Dependent On Other Governments For Revenue?"),
                    fluidRow(
                        infoBoxOutput("ExtPer", width = 6),
                        infoBoxOutput("Extsize", width = 6)
                    ),
                    
                    fluidRow(
                        box(plotlyOutput("line"), 
                            height= "450px",
                            "Fund balance is the accumulated financial resources that is still unused over the years.", 
                            br(), 
                            "Median is used due to large outliers."),
                        
                        box(plotlyOutput("bar"),
                            height= "450px"),
                    )
            ),
            
            # Third page---------------------
            tabItem(tabName = "Debt",
                    h2("Is There Too Much Debt?"),
                    fluidRow(
                        valueBoxOutput("debtpc", width = 3),
                        valueBoxOutput("debtshare", width = 3),
                        valueBoxOutput("debtservicepc", width = 3),
                        valueBoxOutput("debtserviceshare", width = 3)
                    ),
                    
                    fluidRow(
                        box(plotlyOutput("histo"), 
                            width = 12,
                            "The maximum per capita value is removed to help in vizualization."
                        )
                    )
            )
        )
    )
)


# Define server logic-----------------------------
server <- function(input, output) {
    
    # subset data -----------------------------------
    # reactive subsetting for single year charts 
    PA_subset <- reactive({
        req(input$year)
        req(input$type)
        a <- PA_M_2006_2018_full %>% 
            filter((Reporting_Year == max(input$year)) & 
                       (Municipality_Type %in% input$type))
        
        if (length(input$county_1) > 0) {
            a <- a %>% 
                filter((county_name %in% input$county_1) | 
                           (county_name_2a %in% input$county_1)) 
        }
        return(a)
    })
    
    # create reative subset for mutiple years data
    PA_subset_time <- reactive({
        req(input$year)
        req(input$type) 
        a <- PA_M_2006_2018_full %>% 
            filter((Reporting_Year <= max(input$year)) &
                       (Reporting_Year >= min(input$year)) &
                       (Municipality_Type %in% input$type))
        if (length(input$county_1) > 0) {
            a <- a %>% 
                filter((county_name %in% input$county_1) | 
                           (county_name_2a %in% input$county_1)) 
        }
        return(a)
    })  
    
    
    
    # 1st page: Surplus/deficit------------------------------------
    # Create scatterplot
    output$scatter <- renderPlotly({
        # identify outliers
        outliers_x <- (boxplot(PA_subset()$Expenditures_Per_Capita))$out
        outliers_y <- (boxplot(PA_subset()$Revenues_Per_Capita))$out
        
        ggplotly(
            PA_subset() %>%
                filter(!(Expenditures_Per_Capita %in% outliers_x)) %>%
                filter(!(Revenues_Per_Capita %in% outliers_y)) %>%
                ggplot(aes(x = Expenditures_Per_Capita, 
                           y = Revenues_Per_Capita, 
                           text = paste(full_municipal_name,"\n", "RevPC:",
                                        Revenues_Per_Capita, "\n", "ExpPC:", 
                                        Expenditures_Per_Capita))) +
                geom_point(aes(color = Municipality_Type)) +
                geom_abline(intercept =0 , slope = 1, size = 1, color = "red",
                            linetype = "dashed") +
                geom_text(aes(x=150, label="Deficit Zone", y= 0), 
                          colour= "#D55E00")+
                geom_text(aes(x=150, label="Surplus Zone", y=500), 
                          colour= "#0072B2") +
                xlab("Expenditure per Capita($)") + 
                ylab("Revenue Per Capita($)") +
                labs(title = 
                         paste("Revenue Per Capita vs Expenditure per Capita",
                               "In",max(input$year))) + 
                scale_color_discrete(name = "Municipality Type"),
            tooltip = "text"
        )
    })
    
    # value boxes for surplus/deficit tab ----------------------
    output$YearOfDeficit <- renderValueBox({
        valueBox(round(mean(PA_subset()$no_of_deficit_year, na.rm = T),0), 
                 paste("Average Years of Deficits As Of", max(input$year) , "(out of", max(input$year) - year_min, ")"), 
                 icon = icon("calendar-minus"), 
                 color = "purple")
    })
    
    output$Numberofdeficits <- renderValueBox({
        valueBox(round(sum(PA_subset()$deficit, na.rm = T),0), 
                 paste("Municipality Facing Deficits In", max(input$year) , "(out of", nrow(PA_subset()), ")"), 
                 icon = icon("city"), 
                 color = "blue")
    })
    
    output$DeficitSize <- renderValueBox({
        a <- PA_subset() %>% 
            filter(deficit == 1)
        
        valueBox(paste("$", (scales::comma_format()(round(median(a$Revenues_Over_Expenditures, na.rm = T),0)))), 
                 paste("Median Deficit Size In", max(input$year)), 
                 icon = icon("minus-circle"), 
                 color = "orange")
    })
    
    output$surplusSize <- renderValueBox({
        a <- PA_subset() %>% 
            filter(deficit == 0)
        
        valueBox(paste("$", (scales::comma_format()(round(median(a$Revenues_Over_Expenditures, na.rm = T),0)))), 
                 paste("Median Surplus Size In", max(input$year)), 
                 icon = icon("plus-circle"), 
                 color = "green")
    })
    
    # Second page: External revenue -----------------------
    
    # charts for second page------------------------------
    # line charts
    output$line <- renderPlotly({
        # create table of median
        med_subset_time_1 <- PA_subset_time() %>%
            group_by(Municipality_Type, date) %>%
            summarize(median = median(ext_revenue_over_revenue, na.rm = T)*100)
        
        ggplotly(
            ggplot(data = med_subset_time_1, aes(x = date, y = median)) +
                geom_line(aes(color = Municipality_Type)) +
                geom_point(aes(color = Municipality_Type)) +
                scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
                xlab("Year") +
                ylab("Median Share of Total Revenue(%)") +
                labs(title = paste(
                    "Reliance on Intergovernmental Revenue", "\nFrom",min(input$year), "To", max(input$year)))+ 
                scale_color_discrete(name = "Municipality Type"),
            tooltip = c("x", "y")
        )
        })
    
    # bar chart---------------------------
    output$bar <- renderPlotly({
        # create table of median percentage share
        med_subset_1 <- PA_subset() %>%
            mutate(
                federal_share =
                          (Intergovernmental_Revenues_Federal_Government/total_intergovernmental_Revenues),
                state_share = Intergovernmental_Revenues_State_Government/total_intergovernmental_Revenues,
                local_share = Intergovernmental_Revenues_Local_Government/total_intergovernmental_Revenues) %>% 
            group_by(Reporting_Year, Municipality_Type) %>% 
            summarize(
                federal = round(((median(federal_share, na.rm = T))*100),0),
                state = round(((median(state_share, na.rm = T))*100),0),
                local = round(((median(local_share, na.rm = T))*100),0)
            ) %>% 
            pivot_longer(c("federal","state","local"), names_to = "level", values_to = "share")
        
        ggplotly(
            ggplot(data = med_subset_1, aes(x = level, y = share, fill = Municipality_Type)) +
                geom_bar(stat = "identity", position = "dodge") +
                xlab("Source Government Level") + 
                ylab("Median Share of Total Intergovernmental Revenue") +
                labs(title = (paste("Sources of Intergovernmental Revenue In", max(input$year))))+ 
                scale_fill_discrete(name = "Municipality Type"),
            tooltip = c("y")
            
        )
     })
    
    # info boxes for second page -------------------------------
    output$ExtPer <- renderInfoBox({
        infoBox(paste("Median Intergovernmental Revenue Per Capita In", max(input$year)),
                paste("$", (scales::comma_format()(round(median(PA_subset()$intergovernmental_per_capita, na.rm = T),0)))),
                icon = icon("external-link-alt"), 
                color = "purple")
    })
    
    output$Extsize <- renderInfoBox({
        infoBox(paste("Median Revenue Share of Intergovernmental Revenue In", max(input$year)), 
                paste((round((median(PA_subset()$ext_revenue_over_revenue, na.rm = T))*100,0)), "%"),
                icon = icon("hands-helping"), 
                color = "blue")
    })
    
    # Third page: debt ----------------------------------
    
    # boxplot-----------------------
    output$histo <- renderPlotly({

        # remove max value
        a <- PA_subset() %>% 
            filter(debt_per_capita < max(debt_per_capita))
        
        # plot
            ggplotly(
                ggplot(data = a, aes(x = Municipality_Type, y = debt_per_capita)) +
                    geom_boxplot(aes(color = Municipality_Type)) +
                    xlab("Municipality Type") + 
                    ylab("Debt Per Capita") +
                    labs(title = (paste("Debt Per Capita In", max(input$year)))) +
                    theme(legend.position = "none")
            )
        
    })
    
    # value boxes for debt tab ----------------------
    output$debtpc <- renderValueBox({
        valueBox(paste("$", round(mean(PA_subset()$debt_per_capita, na.rm = T),0)), 
                 paste("Mean Debt Per Capita In", max(input$year)), 
                 icon = icon("piggy-bank"), 
                 color = "purple")
    })
    
    output$debtshare <- renderValueBox({
        valueBox(paste(round((mean(PA_subset()$debt_over_revenue, na.rm = T))*100,0), "%"), 
                 paste("Mean Debt Share of Total Revenue In", max(input$year)), 
                 icon = icon("money-check-alt"), 
                 color = "blue")
    })
    
    output$debtservicepc <- renderValueBox({
        valueBox(paste("$", round(mean(PA_subset()$Debt_Service_capita, na.rm = T),0)), 
                 paste("Mean Annual Debt Payments Per Capita In", max(input$year)), 
                 icon = icon("money-check"), 
                 color = "orange")
    })
    
    output$debtserviceshare <- renderValueBox({
        valueBox(paste(round((mean(PA_subset()$debt_service_over_exp, na.rm = T))*100,0), "%"), 
                 paste("Mean Annual Debt Payment Share of Expenditure In", max(input$year)), 
                 icon = icon("money-bill-alt"), 
                 color = "green")
    })
    
}
    
    

# Run the application 
shinyApp(ui = ui, server = server)
