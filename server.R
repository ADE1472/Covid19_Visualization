
library(shiny)

shinyServer(function(input, output) {
  
    output$text1<- renderText({paste("You have selected: ", input$var)})
  
    output$text2<- renderText({paste("You have selected a range that goes from: ", 
                                     input$range[1], "to", input$range[2],
                                     sep = " ")})
    
    output$table<- renderTable(df2)
    
  
    output$map <- renderPlot({
        data<- switch(input$var,
                      "Total Confirmed Cases" = new_df$total_confirmed, 
                      "Total Deaths" = new_df$total_deaths,
                      "Total Recovered" = new_df$total_recovered, 
                      "Active Cases" = new_df$active_cases,
                      "Total Tests" = new_df$total_tests, 
                      "Population" = new_df$population)
        
        Color<- switch(input$var,
                       "Total Confirmed Cases" = "darkblue", 
                       "Total Deaths" = "darkred",
                       "Total Recovered" = "darkgreen", 
                       "Active Cases" = "yellow",
                       "Total Tests" = "orange", 
                       "Population" = "black")
        
        legend<- switch(input$var,
                        "Total Confirmed Cases" = "% Total Cases", 
                        "Total Deaths" = "% Total Deaths",
                        "Total Recovered" = "% Total Recovered", 
                        "Active Cases" = "% Active Cases",
                        "Total Tests" = "% Total Test", 
                        "Population" = "% Population")
        
        mp<- ggplot(new_df, aes(x= long, y= lat, group=group))+
            geom_polygon(aes(fill= data), color= Color,
                         max = input$range[2],
                         min = input$range[1]
            )
            
        mp <- mp + guides(fill=guide_legend(legend))
        
        
        mp<- mp + scale_fill_gradient(name= legend,
                                      low = "darkblue", high = "darkred",
                                            na.value = "black")+
            theme(axis.text.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title.x = element_blank(),
                  axis.text.y = element_blank(),
                  rect = element_blank())
        mp
        
    })
    
})

