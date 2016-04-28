library(shiny)
library(shinydashboard)
library(shinyBS)
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
        tabPanel("Data"),
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
    plrv =  loadRData((system.file("data/plrv.rda", package="agricolae")))
    model<- with(plrv, AMMI(Locality, Genotype, Rep, Yield, console=FALSE))
    ndat <- dplyr::summarise(group_by(plrv, Genotype, Locality), Yield = mean(Yield))
    withProgress(message = "Generating plots", {
      metsel = callModule(met_selected, "met", plrv, model, ndat)
    }
    )

  }

)
