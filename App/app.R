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

url1=jsonRespParsed$result$items[[1]]$distribution[[5]]$accessURL

url="https://datos.alcobendas.org/dataset/9cc894a1-8cfb-4dfe-a29f-fb197aa03ae0/resource/eff1bb9c-110e-4962-8370-d78589f987c2/download/uso-de-autobuses.csv"
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

data$Número.anual.de.pasajeros=as.numeric(data$Número.anual.de.pasajeros)
data$Expediciones.por.dia.laborable=as.numeric(data$Expediciones.por.dia.laborable)
data$Viajeros.por.dia=as.numeric(data$Viajeros.por.dia)
data$Kilometros.anuales.realizados=as.numeric(data$Kilometros.anuales.realizados)


data2=data[4:8]
str(data2)




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


headrow2 = div(id="header2", useShinyjs(),
              selectInput("var1", 
                          label="Select the variable:", 
                          multiple = FALSE,
                          choices=c("Número.anual.de.pasajeros"=1,"Viajeros.por.dia"=2, "Expediciones.por.dia.laborable"=3,"Viajeros.por.expedicion"=4,"Kilometros.anuales.realizados"=5),
                          selected=1),
              )



dataPanel = tabPanel("Data", fluidPage(headrow), 
                     tableOutput("dataTable"),
                     downloadButton("report", "Generate report"))

histPanel = tabPanel("Histograms",
                     fluidPage(headrow2, 
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


Coorpanel = tabPanel("Corr", fluidPage(
    fluidRow(
        column(width = 4,
               plotOutput("plot1", height = 300,
                          # Equivalent to: click = clickOpts(id = "plot_click")
                          click = "plot1_click",
                          brush = brushOpts(
                              id = "plot1_brush"
                          )
               )
        )
    ),
    fluidRow(
        column(width = 6,
               h4("Points near click"),
               verbatimTextOutput("click_info")
        ),
        column(width = 6,
               h4("Brushed points"),
               verbatimTextOutput("brush_info")
        )
    )
))


# Define UI for application that draws a histogram
ui <- navbarPage("Shiny App", dataPanel, histPanel,Coorpanel)


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
    
###############
## Histogram
###############        
    output$distPlot <- renderPlot({
        colm= as.numeric(input$var1)
        # generate bins based on input$bins from ui.R
        #Viajeros.por.dia    <- faithful[, 2]
        bins <- seq(0, max(data2[,colm]), length.out = input$bins + 1)
        # draw the histogram with the specified number of bins
        hist(data2[,colm], breaks = bins, col = 'darkgray', border = 'white', xlab = names(data2[colm]), main="Histrogram")
    })
    
#################
## Correlation plot
#################        
    output$plot1 <- renderPlot({
        ggplot(data, aes(Número.anual.de.pasajeros, Kilometros.anuales.realizados)) + geom_point()
    })
    
    output$click_info <- renderPrint({
        # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
        # were a base graphics plot, we'd need those.
        nearPoints(data, input$plot1_click, addDist = TRUE)
    })
    
    output$brush_info <- renderPrint({
        brushedPoints(data, input$plot1_brush)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
