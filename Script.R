setwd("/Users/alvin.gra/Desktop/FIT5147 - Data Visualisation and Exploration/Narrative Visualisation")


# Load the 'plotly' and 'dplyr' packages
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(ggplot2)
library(forcats)

# Read datas into a data frame
df <- read.csv('LGA_RATE_PER100K.csv')
victims_data <- read.csv('VICTIMS_DATA.csv')
crime_incidents_data <- read.csv("LGA_CRIMINAL_INCIDENTS.csv") 
location_crime_data <- read.csv("Location_Criminal.csv")
sex_alleged <- read.csv("LGA_Sex_AllegedOff.csv")
age_alleged <- read.csv("LGA_Age_AllegedOff.csv")
lga_alleged <- read.csv("LGA_Alleged.csv")
unemployment_data <- read.csv("Unemployed_And_Monthly_Hours_Worked.csv")

# Filter out 'C Drug offences'
NotDrugs <- df %>%
  filter(`Offence.Division` != 'C Drug offences')
lga_alleged <- lga_alleged %>%
  filter(`Offence.Division` != 'C Drug offences')

# Convert to numeric
NotDrugs$`Offence.Count` <- as.numeric(gsub(",", "", NotDrugs$`Offence.Count`))
location_crime_data$`Incidents.Recorded` <- as.numeric(gsub(",", "", location_crime_data$`Incidents.Recorded`))
victims_data$`Victim.Reports` <- as.numeric(gsub(",", "", victims_data$`Victim.Reports`))
NotDrugs$`LGA.Rate.per.100.000.population` <- as.numeric(gsub(",", "", NotDrugs$`LGA.Rate.per.100.000.population`))
crime_incidents_data$`LGA.Rate.per.100.000.population` <- as.numeric(gsub(",", "", crime_incidents_data$`LGA.Rate.per.100.000.population`))
crime_incidents_data$`Incidents.Recorded` <- as.numeric(gsub(",", "", crime_incidents_data$`Incidents.Recorded`))
lga_alleged$`Alleged.Offender.Incidents` <- as.numeric(gsub(",", "", lga_alleged$`Alleged.Offender.Incidents`))

# Summarize data
summarized_victims <- victims_data %>%
  group_by(Year, `Local.Government.Area`) %>%
  summarise(Total_Victim_Reports = sum(Victim.Reports))

# Group by Year and Local Government Area to calculate total 'Offence Count' and 'LGA Rate per 100,000 population' per year per LGA
df_grouped <- NotDrugs %>%
  group_by(Year, `Local.Government.Area`) %>%
  summarise(Total_Offence_Count = sum(`Offence.Count`, na.rm = TRUE),
            Total_LGA_Rate = sum(`LGA.Rate.per.100.000.population`, na.rm = TRUE), .groups = 'drop')

LGA_Crime_Incidents <- crime_incidents_data %>%
  filter(Offence.Division != 'C Drug offences') %>%
  group_by(`Local.Government.Area`) %>%
  summarize(Total = sum(`LGA.Rate.per.100.000.population`, na.rm = TRUE))

# Group by Year and Local Government Area to visualise 3 categorical
victims_grouped <- victims_data %>%
  group_by(Year, `Local.Government.Area`) %>%
  summarise(Total_Victim_Reports = sum(`Victim.Reports`, na.rm = TRUE))

alleged_grouped <- lga_alleged %>%
  group_by(Year, `Local.Government.Area`) %>%
  summarise(Total_Alleged_Offender_Incidents = sum(`Alleged.Offender.Incidents`, na.rm = TRUE))

crime_grouped <- crime_incidents_data %>%
  filter(Offence.Division != 'C Drug offences') %>%
  group_by(Year, `Local.Government.Area`) %>%
  summarize(Total_Crime_Recorded = sum(`Incidents.Recorded`, na.rm = TRUE))

# Merge the two datasets based on the 'Local Government Area' column
merged_data_victims_offences <- merge(df_grouped, summarized_victims, by = c("Year", "Local.Government.Area"))
merged_data_victims_alleged_crime <- merge(victims_grouped, alleged_grouped, by = c("Year", "Local.Government.Area"))
merged_data_victims_alleged_crime <- merge(merged_data_victims_alleged_crime, crime_grouped, by = c("Year", "Local.Government.Area"))


merged_data_victims_alleged_crime <- merged_data_victims_alleged_crime %>% mutate(Local.Government.Area = as.factor(Local.Government.Area))



