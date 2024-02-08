library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(ggplot2)
library(forcats)



# Define the dashboard structure
header <- dashboardHeader(title = "Victoria Crime Statistics")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Statistics", tabName = "stats", icon = icon("chart-bar")),
    menuItem("Relevant Variables", tabName = "variables", icon = icon("pie-chart")),
    menuItem("Further Comparison", tabName = "comparison", icon = icon("database"))
    
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "stats",
            h2("Crime Statistics in Victoria"),
            p("Represents the information that enlightens the information regarding the causes of arises crime in Victoria"),
            
            fluidRow(
              valueBoxOutput("average_incidents"),
              valueBoxOutput("average_victim_reports"),
              valueBoxOutput("maximum_victim_subdiv_reports")
            ),
            fluidRow(
              box(width = 12,
                  selectInput("yearInput", "Select Year:", choices = unique(crime_incidents_data$Year)),
                  plotlyOutput("pieChart")
                  )
            ),
            fluidRow(
              column(width=6, plotlyOutput("top10_offences")),
              column(width=6, plotlyOutput("top10_crime_location"))
            )
            
           
    ),
    tabItem(tabName = "variables",
            h2("Location - Sex - Age"),
            p("Variables involved in the possibility of how crime rates spike"),
            fluidRow(
              column(width = 6, 
                  selectInput("level", "Level of Detail", choices = c("Location Division", "Location Group")),
                  plotlyOutput("line_location_crime_plot")),
              column(width = 6,
                  selectInput("view_type", "View type:", choices = c("Sex", "Age Group")),
                  plotlyOutput("histogram")
              )
            ),
            h2("Observation"),
            box(width = 12,
                h3("Location"),
                p("Here you can see that the most prone location is the", tags$b("residential")," place, in which if to be dived into specific it is in", tags$b("houses"), "area"),
                h3("Sex and Age-group"),
                p("It seems that the alleged offenders are likely to be", tags$b("male-dominated")," which are around the age of ",tags$b("18-24 years old")),
                h3("Area"),
                p("The most prone area observed as stated in the treemap below is ", tags$b("Melbourne"))
            ),
            h2("Local Government Area"),
            p("Represents the crime incidents that happened across areas in Victoria"),
            fluidRow(
              box(width = 12,
                  plotlyOutput("treemap")
              )
            )
    ),
    
    tabItem(tabName = "comparison",
            h2("Offences vs Victims"),
            fluidRow(
              box(width = 12,
                  selectInput("selected_year", "Select a year:", choices = unique(merged_data_victims_offences$Year)),
                  plotlyOutput("bubble_chart")
              )
            ),
            h2("Criminal Incidents Recorded - Alleged Offenders - Victims"),
            fluidRow(
              box(width = 12,
                  plotlyOutput("parallelcoordinatesplot")
              )
            ),
            h2("Possible External Factor"),
            fluidRow(
              box(width = 12,
                  plotlyOutput("external")
              )
            )
            
    )
)
)

ui <- dashboardPage(header,sidebar, body)

# Define the server logic
server <- function(input, output) {
  
  # Calculate the maximum victim reports per Offence Subdivision
  output$maximum_victim_subdiv_reports <- renderValueBox({
    data_filtered <- victims_data
    data_filtered$`Victim.Reports` <- as.numeric(gsub(",", "", data_filtered$`Victim.Reports`))
    
    # Calculate maximum victim reports per Offence Subdivision
    max_victim_reports <- max(data_filtered$`Victim.Reports`)
    max_offence_subdivision <- data_filtered$`Offence.Subdivision`[which.max(data_filtered$`Victim.Reports`)]
    
    # Create valueBox to display maximum victim reports per Offence Subdivision
    valueBox(max_victim_reports, 
             subtitle = paste("Highest Victims Reported is ", max_offence_subdivision), 
             color = "green", 
             icon = icon("user-check"))
  })
  
  # Calculate the average victim reports per year
  output$average_victim_reports <- renderValueBox({
    data_filtered <- victims_data
    data_filtered$`Victim.Reports` <- as.numeric(gsub(",", "", data_filtered$`Victim.Reports`))
    
    # Calculate average victim reports per year
    avg_victim_reports <- round(mean(data_filtered$`Victim.Reports`))
    
    # Create valueBox to display average victim reports per year
    valueBox(avg_victim_reports, 
             subtitle = "Average Victims Reported per Year", 
             color = "red", 
             icon = icon("user-check"))
  })
  
  # Calculate the average incidents recorded per year
  output$average_incidents <- renderValueBox({
    data_filtered <- crime_incidents_data
    data_filtered$`Incidents.Recorded` <- as.numeric(gsub(",", "", data_filtered$`Incidents.Recorded`))
    
    # Calculate average incidents per year
    avg_incidents <- round(mean(data_filtered$`Incidents.Recorded`))
    
    # Create valueBox to display average incidents per year
    valueBox(avg_incidents, 
             subtitle = "Average Crime Incidents Recorded per Year", 
             color = "purple", 
             icon = icon("gavel"))
  })
  
  output$top10_crime_location <- renderPlotly({
    # Ensure that Incidents Recorded is numeric
    location_crime_data$`Incidents.Recorded` <- as.numeric(gsub(",", "", location_crime_data$`Incidents.Recorded`))
    
    # Group by Location Group and summarise the total incidents
    data_summarised <- location_crime_data %>%
      group_by(`Location.Group`) %>%
      summarise(Total_Incidents = sum(`Incidents.Recorded`)) %>%
      arrange(-Total_Incidents)
    
    # Filter for top 10
    data_summarised_top10 <- data_summarised %>%
      slice_max(Total_Incidents, n = 10)
    
    # Create bar chart
    plot_ly(data_summarised_top10, 
            x = ~Total_Incidents, 
            y = ~reorder(`Location.Group`, Total_Incidents), 
            type = 'bar', 
            orientation = 'h') %>%
      layout(title = "Top 10 Locations with Highest Incidents Recorded",
             xaxis = list(title = "Total Incidents Recorded"),
             yaxis = list(title = "Location Group"))
  })

  output$top10_offences <- renderPlotly({
    df_grouped <- NotDrugs %>%
      mutate(`Offence.Count` = as.numeric(gsub(",", "", `Offence.Count`))) %>%
      group_by(`Offence.Subgroup`) %>%
      summarise(Total_Offences = sum(`Offence.Count`)) %>%
      arrange(Total_Offences) %>%
      slice_max(Total_Offences, n = 10) %>%
      plot_ly(x = ~Total_Offences, y = ~fct_rev(`Offence.Subgroup`), type = 'bar', orientation = 'h') %>%
      layout(title = "Top 10 Offences",
             xaxis = list(title = "Total Offences", autorange = "reversed"),
             yaxis = list(title = "Offence Subgroup"))
  })
  
  output$bubble_chart <- renderPlotly({
    selected_data <- merged_data_victims_offences[merged_data_victims_offences$Year == input$selected_year,]
    
    p <- selected_data %>%
      plot_ly(
        x = ~Total_Victim_Reports, # use total victims reported on the x-axis
        y = ~Total_Offence_Count,
        size = ~Total_LGA_Rate*2,  # Scale circle sizes by 2
        sizes = c(100, 1000),  # This sets the range of circle sizes. Adjust as needed
        color = ~`Local.Government.Area`,
        hoverinfo = "text",
        text = ~paste('Year: ', Year, '<br>',
                      'Local Government Area: ', `Local.Government.Area`, '<br>',
                      'Total Offence Count: ', Total_Offence_Count, '<br>',
                      'Total LGA Rate per 100,000 Population: ', Total_LGA_Rate, '<br>',
                      'Total Victims Reported: ', Total_Victim_Reports),
        type = "scatter",
        mode = "markers"
      ) %>%
      layout(
        title = "Bubble Chart of Total Offence Count, LGA Rate per 100,000 Population, and Total Victims Reported",
        xaxis = list(title = "Total Victims Reported"),
        yaxis = list(title = "Total Offence Count"),
        showlegend = TRUE
      )
    
    p
  })
  
  
  output$pieChart <- renderPlotly({
    # Filter data based on selected year
    data_filtered <- crime_incidents_data[crime_incidents_data$Year == input$yearInput,]
    data_filtered$`Incidents.Recorded` <- as.numeric(gsub(",", "", data_filtered$`Incidents.Recorded`))
    
    # Exclude "C Drug Offences"
    data_filtered <- data_filtered[data_filtered$`Offence.Division` != "C Drug Offences", ]
    
    # Summarize data by Offence Division
    data_summarized <- aggregate(data_filtered$`Incidents.Recorded`, by = list(Category = data_filtered$`Offence.Division`), FUN = sum)
    
    # Create pie chart using plotly
    p <- plot_ly(data_summarized, labels = ~Category, values = ~x, type = 'pie', textinfo = 'label+percent', hoverinfo = 'label+value') %>%
      layout(title = paste("Crime Incidents Recorded in", input$yearInput, "by Offence Division"),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    p
  })
  
  
  output$treemap <- renderPlotly({
    LGA_Crime_Incidents <- LGA_Crime_Incidents[order(-LGA_Crime_Incidents$Total), ]
    
    p <- plot_ly(LGA_Crime_Incidents, 
                 type = "treemap",
                 labels = ~`Local.Government.Area`,
                 parents = "",
                 values = ~Total,
                 hoverinfo = "label+value+percent entry",
                 textinfo = "label+value")
    p <- p %>% layout(title = "Crime Rates by Local Government Area")
    ggplotly(p)
  })
  
  # Data processing
  sex_alleged$`Alleged.Offender.Incidents` <- as.numeric(gsub(",", "", sex_alleged$`Alleged.Offender.Incidents`))
  age_alleged$`Alleged.Offender.Incidents` <- as.numeric(gsub(",", "", age_alleged$`Alleged.Offender.Incidents`))
  
  output$histogram <- renderPlotly({
    histogram_data <- data.frame()
    
    if(input$view_type == "Sex"){
      # Group by Sex and Year and calculate total incidents
      histogram_data <- sex_alleged %>%
        group_by(Sex, Year) %>%
        summarise(Total = sum(`Alleged.Offender.Incidents`, na.rm = TRUE)) %>%
        ungroup()
      
      # Convert Year to factor
      histogram_data$Year <- as.factor(histogram_data$Year)
      
      p <- ggplot(histogram_data, aes(x = Year, y = Total, fill = Sex)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Total Alleged Offenders by Sex per Year") +
        theme(plot.title = element_text(hjust = 0.5))
    } else if(input$view_type == "Age Group"){
      # Group by Age Group and Year and calculate total incidents
      histogram_data <- age_alleged %>%
        group_by(`Age.Group`, Year) %>%
        summarise(Total = sum(`Alleged.Offender.Incidents`, na.rm = TRUE)) %>%
        ungroup()
      
      # Convert Year to factor
      histogram_data$Year <- as.factor(histogram_data$Year)
      
      p <- ggplot(histogram_data, aes(x = Year, y = Total, fill = `Age.Group`)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Total Alleged Offenders by Sex per Year") +
        theme(plot.title = element_text(hjust = 0.5))
    }
    
    ggplotly(p)
  })

  output$line_location_crime_plot <- renderPlotly({
    if (input$level == "Location Division") {
      # Aggregate data by year and location division
      df_sum <- location_crime_data %>%
        group_by(Year, `Location.Division`) %>%
        summarise(Total.Incidents = sum(Incidents.Recorded))
      # Create plot
      fig <- df_sum %>%
        plot_ly(x = ~Year, y = ~Total.Incidents, color = ~`Location.Division`, type = 'scatter', mode = 'lines+markers') %>%
        layout(title = "Total incidents per year for each Location Division",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Total Incidents"))
    } else {
      # Aggregate data by year and location group
      df_sum <- location_crime_data %>%
        group_by(Year, `Location.Group`) %>%
        summarise(Total.Incidents = sum(Incidents.Recorded))
      # Create plot
      fig <- df_sum %>%
        plot_ly(x = ~Year, y = ~Total.Incidents, color = ~`Location.Group`, type = 'scatter', mode = 'lines+markers') %>%
        layout(title = "Total incidents per year for each Location Group",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Total Incidents"))
    }
    
    fig
  })
  
  output$parallelcoordinatesplot <- renderPlotly({
    
    merged_data_victims_alleged_crime <- merged_data_victims_alleged_crime %>% mutate(Local.Government.Area = as.factor(Local.Government.Area))
    
    fig <- plot_ly(
      type = 'parcoords',
      line = list(color = merged_data_victims_alleged_crime$Total_Crime_Recorded,
                  colorscale = 'Jet',
                  showscale = TRUE,
                  cmin = min(merged_data_victims_alleged_crime$Total_Crime_Recorded),
                  cmax = max(merged_data_victims_alleged_crime$Total_Crime_Recorded)),
      dimensions = list(
        list(range = c(min(merged_data_victims_alleged_crime$Year), max(merged_data_victims_alleged_crime$Year)),
             label = 'Year', values = merged_data_victims_alleged_crime$Year),
        list(range = c(min(merged_data_victims_alleged_crime$Total_Victim_Reports), max(merged_data_victims_alleged_crime$Total_Victim_Reports)),
             label = 'Total Victim Reports', values = merged_data_victims_alleged_crime$Total_Victim_Reports),
        list(range = c(min(merged_data_victims_alleged_crime$Total_Alleged_Offender_Incidents), max(merged_data_victims_alleged_crime$Total_Alleged_Offender_Incidents)),
             label = 'Total Alleged Offender Incidents', values = merged_data_victims_alleged_crime$Total_Alleged_Offender_Incidents),
        list(range = c(min(merged_data_victims_alleged_crime$Total_Crime_Recorded), max(merged_data_victims_alleged_crime$Total_Crime_Recorded)),
             label = 'Total Crime Recorded', values = merged_data_victims_alleged_crime$Total_Crime_Recorded)
      )
    )
    
    fig <- fig %>%
      layout(
        title = "Parallel Coordinates Plot of Crime Data"
      )
    
    fig
  })
  
  output$external <- renderPlotly({
    
    fig <- plot_ly(unemployment_data, x = ~Year, y = ~Monthly.Hours.Worked, name = 'Monthly Hours Worked', type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~Unemployed.Individuals, name = 'Unemployed Individuals', mode = 'lines') %>%
      layout(title = "Monthly Hours Worked vs Unemployed Individuals Over the Years",
             xaxis = list(title = "Year"), 
             yaxis = list(title = "Value"))
    
    fig
  })
  
}

shinyApp(ui, server)
