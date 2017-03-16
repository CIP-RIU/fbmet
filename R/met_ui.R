
#' fbmet_ui
#'
#' @param title string for caption
#'
#' @return shiny tagList
#' @export
fbmet_ui <- function(title = "MET analytical graphics"){
  shinydashboard::tabItem(tabName = title,

  h2("MET analytical graphics"),
  fluidRow(
    column(width = 12,

      tabBox(width = 12,  selected = "Data",
          tabPanel("Data",
                   prepareMetDataUI("data")
          ),
          tabPanel("Plots",
                   linkedBiplotUI("met")
          )
          #,
          # tabPanel("Report",
          #          uiOutput("metReport")
          # ),
          # tabPanel("Help",
          #          helpPanel(fbhelp::list_tutorials("fbmet")[[1]])
          # )
          # ,
          # tabPanel("About",
          #          helpPanel(fbhelp::list_tutorials("fbmet", name = "about")[[1]],
          #                    center = TRUE)
          # )
  ))
  )
  )
}
