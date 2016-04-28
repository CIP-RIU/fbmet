library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyFiles)
library(DT)
library(agricolae)
library(dplyr)
library(fbmet)
library(fbhelp)

shinyApp(
  ui = dashboardPage( skin = "yellow",
    dashboardHeader(title = "MET explorer"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      tabBox( width = 12, selected = "Plots",
        tabPanel("Data",
                 prepareMetDataUI("data")
                 ),
        tabPanel("Plots",
                linkedBiplotUI("met")
                ),
        tabPanel("Report"),
        tabPanel("Help",
                 helpPanel(fbhelp::list_tutorials("fbmet")[[1]])
                ),
        tabPanel("About",
                 helpPanel(fbhelp::list_tutorials("fbmet", name = "about")[[1]],
                           center = TRUE)
        )
      )
    )
  ),

  server = function(input, output, session) {

    volumes <- getVolumes(c("(E:)", "Page File (F:)"))
    #print(volumes)
    shinyFileChoose(input, 'file', roots=volumes, session=session)

    metFiles <- reactive({
      mf = parseFilePaths(volumes, input$file)$datapath
      mf = as.character(mf)
      mf
    })
    output$filepaths <- renderPrint({metFiles()})

    met_raw <- reactive({
      bks = metFiles()
      if(length(bks)==0) return(NULL)
      dat <- combine_books(bks)
      for(i in 4:ncol(dat)){
        dat[, i] = round(dat[, i], 5)
      }
      dat
    })

    output$met_raw <- DT::renderDataTable({
      met_raw()
    }
    ,#filter = "top",
     options = list(searching = TRUE,
                      lengthMenu = list(c(5, 10, 15, 25, -1),
                                        c('5', '10','15', '25', 'All')),
                      pageLength = 10)
    )

    get_met_traits <- reactive({
      dat = metFiles()
      if(length(dat)==0) return(NULL)
      names(met_raw())[4:ncol(met_raw())]
    })

    output$met_traits <- renderUI({
      traits <- get_met_traits()
      selectInput("met_trait", label = "Trait:", choices = traits)
    })

    output$met_elston_neg <- renderUI({
      traits <- get_met_traits()
      selectizeInput("met_en", label = "Negative traits:", choices = traits,
                     multiple = TRUE)
    })

    ei_ng <- reactive({
      out = input$met_en
      print(out)
    })

    output$met_elston_pos <- renderUI({
      traits <- get_met_traits()
      x = ei_ng()
      selectizeInput("met_ps", label = "Positive traits:", choices = traits,
                     multiple = TRUE)
    })


    plrv =  loadRData((system.file("data/plrv.rda", package="agricolae")))

    model<- with(plrv, AMMI(Locality, Genotype, Rep, Yield, console = FALSE))
    ndat <- dplyr::summarise(group_by(plrv, Genotype, Locality), Yield = mean(Yield))
    withProgress(message = "Generating plots", {
      metsel = callModule(met_selected, "met", plrv, model, ndat)
    }
    )

  }

)
