library(shiny)
library(shinydashboard)
library(agricolae)
library(dplyr)
library(fbmet)

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
        tabPanel("Help")
      )
    )
  ),

  server = function(input, output, session) {
    plrv =  loadRData((system.file("data/plrv.rda", package="agricolae")))
    model<- with(plrv, AMMI(Locality, Genotype, Rep, Yield, console=FALSE))
    ndat <- plrv %>% group_by(Genotype, Locality) %>% summarise(Yield = mean(Yield))

    metsel = callModule(met_selected, "met", model, ndat)
  }
)
