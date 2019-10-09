#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(readxl)

negatives = read_excel("negative_code.xlsx")

negatives %>% filter(variant != "/") %>% 
  mutate(variant_y = ifelse(variant=="D",1,0)) -> negatives

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("DO Support in Corpus of Early English Correspondance"),
   
   # Sidebar with a slider input for number of bins 
   #Modi
   sidebarLayout(
      sidebarPanel(
      
          selectInput("authorChoice","Author: ", choices = unique(sort(negatives$author))) 
      ),
      # Show a plot of the generated distribution
      # Show a plot of the data, fit a logistic regression and plot it. 
      mainPanel(
         plotOutput("plot1"), 
         tableOutput("showTable")
      )
   )
)

# plot the output 
# plot a table
server <- function(input, output) {
  library (ggplot2)
  library(scales)
   output$plot1 = renderPlot({
     dispData = negatives %>% filter(author == input$authorChoice) %>%
       mutate(variant = recode(variant, I = "Inversion", D="Do Support"))
     
     
     ggplot(dispData) +  
       geom_bar(aes(x=variant, y=..prop.., group=1), stat="count") + theme_bw () +
       ylab("Proportion") + ggtitle(paste0("Percent Use for ", input$authorChoice))+
       scale_y_continuous(labels=percent_format(), limits=c(0,1))
       
   })
  output$showTable = renderTable({
      negatives %>% filter(author == input$authorChoice) %>%
      mutate(variant = recode(variant, I = "Inversion", D="Do Support")) %>%
      group_by(variant) %>% summarise(N = n())
    
    
  })  
}

# Run the application 
shinyApp(ui = ui, server = server)

