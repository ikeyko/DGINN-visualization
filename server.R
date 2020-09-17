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

server <- function(input, output) {
  
  # maximum size allowed (notably for coverage file)
  options(shiny.maxRequestSize = 3e2*1024^2)
  
  # reset event
  observeEvent(input$resetData, {
    js$reset()
  })
  
  # get data when user submits
  observeEvent(input$submit,{
    
    covFile <- input$covFile
    if(!is.null(input$covFile)) {
      cov  <- read.table(covFile$datapath, header = F, fill = T, sep = "\t")
    }
    else{
      cov <- NA
    }
    
    resFile <- input$resFile 
    df  <- read.table(resFile$datapath, header = T, sep = "\t")
    
    # stop app if no results file uploaded
    if(is.null(resFile)) {
      showModal(
        modalDialog(
          title = "No data uploaded",
          "Upload your data file before pressing the 'Submit' button.",
          footer = modalButton("OK"),
          size = "s"
        )
      )
      return()
    }
    
    # stop app if over 100 genes in the results file
    if(nrow(df) > 100) {
      showModal(
        modalDialog(
          title = "Too many genes in your data file",
          "Your data file contains more than 100 genes. Please run the app locally (refer to the Github documentation for explanations).",
          footer = modalButton("OK"),
          size = "s"
        )
      )
      return()
    }
    
    ################################### VISUALIZE DATA
    
    res = cleandf(df)

    # show a quick overview of the results file so the user can check
    colDisplay <- c("Name", "Gene", "GeneSize", "NbSpecies", "BUSTED", "BppM1M2", "BppM7M8", "codemlM1M2", "codemlM7M8")
    dfDisplay <- res[ , colDisplay]
    output$inData <- renderDataTable({
      dfDisplay
      })

    # order genes (nb methods pos, perc PSS)
    resOrd = orderdf(res)

    ################################### FIGURE 1 - RANKING

    output$fig1 <- renderPlot({
      makeFig1(resOrd)
      })

    ################################### FIGURE 2 - PROFILES

    output$fig2 <- renderPlot({
      makeFig2(resOrd, input$nbGene, cov)
    })
    
    ################################### CREATE OUTPUT FILE & DOWNLOAD
    inName = file_path_sans_ext(input$resFile$name)
    pdfName = paste(inName,"_FIGURES_", Sys.Date(), ".pdf", sep="")
      
    pdf(file = pdfName, paper = 'a4' , h = 0 , w = 0)
    makeFig1(resOrd)
    makeFig2(resOrd, input$nbGene, cov)
    dev.off()
    
    output$dlFigs <- downloadHandler(
      # name output file
      filename = pdfName,
      # create temporary file
      content = function(tmpFile) {
        file.copy(pdfName, tmpFile)
      },
      contentType = "application/pdf"
    )
  })

}