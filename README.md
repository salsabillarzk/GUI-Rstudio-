# GUI RSTUDIO Principal Componen Analysis (PCA)

## Graphic User Interface (GUI) Principal Componen Analysis

```
library(shiny)
library(shinythemes)
library(DT)
library(REdaS)
library(psych)
library(GPArotation)
```

```
shinyUI <- fluidPage(theme=shinytheme("flatly"),
                     #Application title
                     titlePanel("Analisis Faktor"),
                     h4("Principal Component Analysis"),
                     h5("Oleh : Kelompok 3"),
                     navbarPage("Tugas Komputasi Lanjut",
                                tabPanel("PETUNJUK",
                                         mainPanel(
                                           h4("Petunjuk Penggunaan Aplikasi"),
                                           h5("1. Aplikasi digunakan hanya untuk analisis faktor kompunen utama (PCA)"),
                                           h5("2. Pastikan data sudah berskala interval"),
                                           h5("3.Data.csv"),
                                           h5("4. Masukkan data pada tempat yang sudah disediakan"),
                                           h5("5. Sesuaikan Header,Quote, dan Separator sesuai dengan data"),
                                           h5("6. Buka Tab Uji Asumsi untuk melihat hasil KMO MSA dan Bartlett test"),
                                           h5("7. Buka Tab Principal Component Analyze untuk melihat hasil PCA"),
                                           h5("8. Sesuaikan angka dengan jumlah faktor yang terbentuk dengan melihat Jumlah Faktor maupun ScaterPlot"),
                                           h5("9. Buka bagian Faktor Utama, lalu lakukan analisis"),
                                           h5("10. Klik ikon refresh untuk memulai analisis baru atau close untuk menutup GUI"),
                                         )),
                                tabPanel("DATA",
                                         sidebarLayout(
                                           sidebarPanel(
                                             fileInput("data","Choose CSV File",
                                                       multiple=FALSE,
                                                       accept=c("text/csv",
                                                                "text/comma-separated-values,text/plain",
                                                                ".csv")),
                                             #Horizontal line
                                             tags$hr(),
                                             #Input Checkbox if file has header
                                             checkboxInput("header","Header",TRUE),
                                             #Input:select separator
                                             radioButtons("sep","Separator",
                                                          choices= c(Comma=",",
                                                                     semicolon=";",
                                                                     Tab="\t"),
                                                          selected=";"),
                                             #Input:select quotes
                                             radioButtons("quote","Quote",
                                                          choices=c(None="",
                                                                    "Double Quote"="",
                                                                    "Single Quote"=""),
                                                          selected="")
                                           ),
                                           mainPanel(mainPanel(DTOutput("data")),
                                                     position="left"
                                           )
                                         )),
                                tabPanel("Uji Asumsi",
                                         mainPanel(tabsetPanel(type="tabs",
                                                               id = "navbar",
                                                               tabPanel("Uji KMO MSA",
                                                                        verbatimTextOutput("KMO"),
                                                                        value="KMO"),
                                                               tabPanel("Uji Bartlett",
                                                                        verbatimTextOutput("bartlett"),
                                                                        value="Bartlett")))
                                ),
                                tabPanel("Principal Component Analyze",
                                         sidebarPanel(
                                           sliderInput("jumlah",
                                                       "Jumlah Faktor:",
                                                       min=1,
                                                       max=10,
                                                       value=4)
                                         ),
                                         mainPanel(tabsetPanel(type="tabs",
                                                               id="navbar",
                                                               tabPanel("Jumlah Faktor",
                                                                        verbatimTextOutput("faktor"),
                                                                        value="Jumlah Faktor"),
                                                               tabPanel("Scree Plot",
                                                                        plotOutput("plot"),
                                                                        value="Scree Plot"),
                                                               tabPanel("Faktor Utama",
                                                                        verbatimTextOutput("faktoru"),
                                                                        value="Faktor Utama"))))
                     )
)
```

```
#Define server logic required to draw a histogram
shinyServer <- function(input, output){
  #Input Data
  data.input <- reactive({
    in.file <- input$data
    if(is.null(in.file)){
      return(NULL)
    }else
      df <- read.csv(input$data$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote) 
  })
  #Print Data
  output$data = renderDT({
    data = data.input()
  })
  
  ##Uji Asumsi
  #Uji KMO MSA
  output$KMO = renderPrint({
    data = data.input()
    KMOS(data)
  })
  
  #Uji Barlett
  output$bartlett = renderPrint({
    data = data.input()
    bart_spher(data)
  })
  
  #tabPanel Principal Component Analyze
  #Jumlah Faktor
  output$faktor = renderPrint({
    data = data.input()
    PCA <- principal(data)
    PCA[["values"]]
  })
  
  #Scree Plot
  output$plot = renderPlot({
    data = data.input()
    scree(data)
  })
  
  #Faktor Utama
  output$faktoru = renderPrint({
    data = data.input()
    PCARotasi <- principal(data,nfactors = input$jumlah, rotate = "varimax")
    PCARotasi
  })
}
shinyApp(ui = shinyUI, server = shinyServer)
```
