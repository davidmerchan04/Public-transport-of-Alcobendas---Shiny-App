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
names(data)[4]="Numero.anual.de.pasajeros"
names(data)[5]="Expediciones.por.dia.laborable"
names(data)[6]="Viajeros.por.dia"
names(data)[7]="Viajeros.por.expedicion"
names(data)[8]="Kilometros.anuales.realizados" 

data$Tipo.de.transporte=gsub("Ãº","u",data$Tipo.de.transporte)

###################################################################

###################################################################

########
##App 

data %<>% mutate_at(c("Line", "Year","Tipo.de.transporte"), as.factor)

data_line = levels(data$Line)
data_year = levels(data$Year)
data_type = levels(data$Tipo.de.transporte)

data$Numero.anual.de.pasajeros=as.numeric(data$Numero.anual.de.pasajeros)
data$Expediciones.por.dia.laborable=as.numeric(data$Expediciones.por.dia.laborable)
data$Viajeros.por.dia=as.numeric(data$Viajeros.por.dia)
data$Kilometros.anuales.realizados=as.numeric(data$Kilometros.anuales.realizados)


data2=data[4:8]
str(data2)


namev = function(vec){
       tmp=as.list(vec)
       names(tmp)=as.character(unlist(vec))
       tmp
}


# "boxplot"=geom_boxplot()
# "density"= geom_density(alpha=.75)
# "bar"=geom_bar(position="dodge")


#######################################

####################
# Line and year headrow
###################

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

###################
# Variable headrow 
###################
headrow2 = div(id="header2", useShinyjs(),
              selectInput("var1", 
                          label="Select the variable:", 
                          multiple = FALSE,
                          choices=c("Numero.anual.de.pasajeros"=1,"Viajeros.por.dia"=3, "Expediciones.por.dia.laborable"=2,"Viajeros.por.expedicion"=4,"Kilometros.anuales.realizados"=5),
                          selected=1),
              )


headrow3 = div(id="header3", useShinyjs(),
               selectInput("var2", 
                           label="Select the variable:", 
                           multiple = FALSE,
                           choices=c("Numero.anual.de.pasajeros"=1,"Viajeros.por.dia"=3, "Expediciones.por.dia.laborable"=2,"Viajeros.por.expedicion"=4,"Kilometros.anuales.realizados"=5),
                           selected=3),
)

####################
### FIRST PANEL  ###
####################

dataPanel = tabPanel("Data", fluidPage(headrow), 
                     tableOutput("dataTable"),
                     downloadButton("report", "Generate report"))

####################
### HISTOGRAM PANEL#
####################

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

####################
# CORRELATION PLOT #
####################

Coorpanel = tabPanel("Correlation graph", fluidPage(
    fluidRow(
        column(width = 7,
               plotOutput("plot1", height = 400,
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

##############
# PLOTS (DENSITY and BAR)
##############

Plots=tabPanel("Other plots",
               pageWithSidebar( 
               headerPanel("Select Options"),
               sidebarPanel(headrow3,
               selectInput("plot.type", "Plot Type:",
                                              c(Density="density", Barchart="Barchart")
                           )),
               mainPanel(
                   h3(textOutput("caption")),
                   plotOutput("plot")
               )
               ))


# Define UI for application that draws a histogram
ui <- navbarPage("Shiny App", dataPanel, histPanel,Coorpanel,Plots)


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
        bins <- seq(0, max(data[,4:8][,colm]), length.out = input$bins + 1)
        # draw the histogram with the specified number of bins
        hist(data[,4:8][,colm], breaks = bins, col = 'darkgray', border = 'white', xlab = names(data[,4:8][colm]), main="Histrogram")
    })
    
#################
## Correlation plot
#################        
    output$plot1 <- renderPlot({
        ggplot(data, aes(Numero.anual.de.pasajeros, Kilometros.anuales.realizados)) + geom_point()
    })
    
    output$click_info <- renderPrint({
        # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
        # were a base graphics plot, we'd need those.
        nearPoints(data, input$plot1_click, addDist = TRUE)
    })
    
    output$brush_info <- renderPrint({
        brushedPoints(data, input$plot1_brush)
    })

##############
## plots
##############

 
output$var1=renderUI({
     input$var1
     })

# output$group= renderUI({
#     selectInput("group","Groups:", data_type)
# })
    
output$caption=renderText({
    switch(input$plot.type,
           "density" 	=	"Density plot",
           "Barchart" 		=	"Bar chart")
})

# output$plot = renderUI({
#     input$var1
#     plotOutput("p")
# })

output$plot = renderPlot({
    plot.type=switch(input$plot.type,
                     "density"  = geom_density(alpha=.75),
                     "Barchart"  = geom_bar(position=position_dodge2(preserve = "total"))
                     )
    colm1= as.numeric(input$var2) 
    ggplot(data,
             aes( 
                 x = data[,4:8][,colm1],
                 fill = data[,3],
                 group = data[,3]
                 )
             ) + labs(x=names(data[,4:8][,colm1]), fill=data[,3], y="") + plot.type
})



####################
## Generate report #
####################

output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(
            selecline = isolate(input$selecline),
            selecyear = isolate(input$selecyear),
                   )
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
    }
)





}




# Run the application 
shinyApp(ui = ui, server = server)
