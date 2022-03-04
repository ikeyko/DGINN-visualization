library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyalert)
library(V8)
library(openxlsx)
library(tools)
library(gplots)
source("fig1.R")
source("fig2.R")
source("utils.R")

# include JS code into app for reset function
jsResetCode <- "shinyjs.resetCode = function() {history.go(0);}"

ui <- fluidPage(
  
  titlePanel("Visualization of DGINN results"),
  theme = shinytheme("darkly"),
  useShinyjs(),
  extendShinyjs(text = jsResetCode, functions = c("resetCode")),
  
  sidebarLayout(
    
    sidebarPanel(
      # input results file
      tags$div(
        title = "Tab-separated file, as produced by the parseResults script. Accepted formats: txt/tsv/xlsx",
        fileInput("resFile" , "Results file (mandatory)", placeholder = "No file selected")
        ),
      
      # input coverage file
      tags$div(
        title = "Tab-separated file, as produced by the parseResults script. Accepted formats: tab/txt/tsv",
        fileInput("covFile" , "Coverage file (optional)", placeholder = "No file selected")
      ),
      
      # choose nb of gene per page for figure 2 (PS profiles)
      sliderInput("nbGene", "Number of genes represented per page (Figure 2):", min = 10, max = 30, value = 15),
      
      actionButton("submit", "Submit", icon("upload"), class = "btn-warning"),
      actionButton('resetData', 'Reset', icon("refresh")),
      downloadButton('dlFigs', 'Download Figures'),
      
      br(),br(),
      h6(
        "Development by Lea Picard & the",
        a(href = "http://ciri.inserm.fr/service-bioinformatique-et-biostatistique-bibs/", 
          "CIRI-BIBS Service.")
      ),
      h6(
        "Find all the documentation on running DGINN on the",
        a(href = "https://github.com/leapicard/DGINN",
          "Github"),
        "page."
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Input data", icon = icon("list-alt"), dataTableOutput("inData")),
        tabPanel("Figure 1", icon = icon("bar-chart-o"), plotOutput("fig1")),
        tabPanel("Figure 2", icon = icon("bar-chart-o"), plotOutput("fig2"))
      )
    )
  )
)
