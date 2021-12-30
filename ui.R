
##Covid19 dataset

#Import necessary libraries
library(tidyverse)
library(maps)
library(shiny)
library(shinythemes)
library(ggplot2)
library(data.table)





#Import dataset
df<-read.csv("data/worldometer_coronavirus_summary_data.csv", header = TRUE, sep = ',')
head(df)

#Check the structure of the dataset
str(df)

##Feature engineering of the dataset

#Check for missing value
sum(is.na (df))

#Copy df
df_or<-data.frame(df)


##Remove columns that aren't useful for this analysis
df= subset(df, select = -c(serious_or_critical,total_cases_per_1m_population,
                           total_deaths_per_1m_population,
                           total_tests_per_1m_population, continent))
head(df)

#replace missing values with 0
df[is.na(df)]<-0
head(df)

##Import world map dataset

mapdata<- map_data("world")
#View(mapdata)

##Join mapdata and df together

#rename df country column to region
names(df)[1]<-"region"
colnames(df)

new_df<- left_join(mapdata, df, by= "region")
#View(new_df)

##Filter out N/A
new_df<- new_df %>% filter(!is.na(new_df$total_confirmed))
#View(new_df)

##Save the new_df as CSV in the working directory file
write.csv(new_df, "new_df.csv")



##Part two of the Dataset
#calculate sum by group (Continent). Just to make our visualization smart
df2<-data.frame(df_or)
#View(df2)

##Remove columns that aren't useful for this analysis
df2= subset(df2, select = -c(serious_or_critical,total_cases_per_1m_population,
                             total_deaths_per_1m_population,
                             total_tests_per_1m_population, country))
#View(df2)

#Remove the null values
df2[is.na(df2)]<-0
#View(df2)

setDT(df2)
df2= df2[ ,list(Total_Confirmed= sum(total_confirmed),
                Total_Deaths= sum(total_deaths),
                Total_Recovered= sum(total_recovered),
                Total_Active_cases= sum(active_cases),
                Total_Tests= sum(total_tests),
                Total_Popuplation= sum(population)), by= continent]
#View(df2)

##Save the df2 as CSV in the working directory file
write.csv(df2, "df2.csv")



##UI interphase
shinyUI(fluidPage(
    
    theme = shinytheme("sandstone"), themeSelector(),#Select your preferred viewing theme
    
    titlePanel("Covid_19 Map Visualization"),
   
    
    sidebarLayout(
        sidebarPanel(
            
            helpText("Creating a Covid_19 global map summary from 15/02/2020 to 30-07-2021"),
            
            selectInput("var",
                        label = "Select one category",
                        choices = c("Total Confirmed Cases", "Total Deaths",
                                    "Total Recovered", "Active Cases",
                                    "Total Tests", "Population"),
                        selected = "Total Confirmed Cases"),
            
            sliderInput("range",
                        label = "Range of interest",
                        min = 0, max = 100, value = c(0,100)),
            
            
        ),
        
        mainPanel(
            textOutput("text1"),
            textOutput("text2"),
            plotOutput("map"),
            tableOutput("table")
        )
    )
))



