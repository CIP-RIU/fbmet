library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyFiles)
library(DT)
library(agricolae)
library(dplyr)
library(fbmet)
library(fbhelp)

library(brapiUI)

is_server <- function() {
  return( !Sys.info()["sysname"] %in%
            c("Windows",
              "Darwin") )
}


shinyApp(
  ui = dashboardPage( skin = "yellow",
    dashboardHeader(title = "MET explorer"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      fbmet::fbmet_ui()
    )
  ),

  server = function(input, output, session, values) {
    fbmet::fbmet_sv(input, output, session, values)
  }
)
