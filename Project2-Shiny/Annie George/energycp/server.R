

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

energy <- read.csv("Energy Census and Economic Data US 2010-2014.csv",
                   stringsAsFactors = FALSE, header = TRUE)
df <- energy[,1:136]
df <- df[, c(-26:-41)]
df <- gather(df, key= "key", value = "value", -StateCodes, - Division, -Region, -Coast, -Great.Lakes)
df <- separate(df,key, into=c("fuel", "year"), sep = -5)


shinyServer(function(input, output) {
  
  output$barplotC <- renderPlot({
      df <-force(df)
      fuel_C <- paste0(input$fuel, 'C')
      df1 <- df %>%
        filter(year== input$year & fuel == fuel_C & !is.na(value) & (StateCodes != 'US'))%>%
        arrange(desc(value))
      
    # draw the bar graph based on fuel type
    p <- ggplot(data=df1, aes(x=df1$StateCodes, y=desc(df1$value), fill=df1$StateCodes)) +
         geom_bar(stat="identity") + coord_flip() +
         ylab("Consumption in ?") +
         xlab("States")
    print(p)
  })

})
