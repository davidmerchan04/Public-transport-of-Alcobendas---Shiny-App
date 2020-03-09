#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(httr)
library(rlist)
library(jsonlite)
library(dplyr)
library(tidyverse)
library(shinyjs)
library(magrittr)

###### The data is get from datos.gob.es
### We get de API dato using de JSON format

resp=GET("http://datos.gob.es/apidata/catalog/dataset/l01280066-transporte-publico-autobus-historico1.json")

http_type(resp)
http_error(resp)

jsonRespText<-content(resp,as="text") 
jsonRespText

jsonRespParsed<-content(resp,as="parsed") 

url=jsonRespParsed$result$items[[1]]$distribution[[4]]$accessURL

######

data=read.csv(url)
head(data)
View(data)

jsonRespParsed

names(data)

names(data)[1]="Line"
names(data)[2]="Year"
names(data)[4]="Número.anual.de.pasajeros"
names(data)[5]="Expediciones.por.dia.laborable"
names(data)[6]="Viajeros.por.dia"
names(data)[7]="Viajeros.por.expedicion"
names(data)[8]="Kilometros.anuales.realizados" 

data$Tipo.de.transporte=gsub("Ãº","u",data$Tipo.de.transporte)

###################################################################

###################################################################

########
##App 

data %<>% mutate_at(c("Line", "Year"), as.factor)

data_line = levels(data$Line)
data_year = levels(data$Year)

headrow = div(id="header", useShinyjs(),
              selectInput("selecline", 
                          label="Select the bus line", 
                          multiple = TRUE,
                          choices=data_line,
                          selected=head(data_line,3)),
              selectInput("selecyear", 
                          label="Select the year", 
                          multiple = TRUE,
                          choices=data_year,
                          selected=head(data_year,2)) 
              )


dataPanel = tabPanel("Data",tableOutput("dataTable"),downloadButton("report", "Generate report"))

histPanel = tabPanel("Histograms",
                     fluidPage( 
                         sidebarLayout(
                             sidebarPanel(
                                 sliderInput("bins",
                                             "Number of bins:",
                                             min = 1,
                                             max = 50,
                                             value = 30)
                             ), # sidebarPanel
                             mainPanel(
                                 plotOutput("distPlot")
                             ) # mainPanel
                         ) # sidebarLayout
                     ) # fluidPage
)


# Define UI for application that draws a histogram
ui <- navbarPage("Shiny App", dataPanel, histPanel, id="navBar", header=headrow)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    data_filtered <- reactive({
        req(input$selecline)
        req(input$selecyear)
        data %>% filter(Year %in% input$selecyear, 
                             Line %in% input$selecline)
    })
    
    
    
    output$dataTable = renderTable({
        data_filtered()
    })
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        Viajeros.por.dia    <- faithful[, 2]
        bins <- seq(min(Viajeros.por.dia), max(Viajeros.por.dia), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(Viajeros.por.dia, breaks = bins, col = 'darkgray', border = 'white')
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
