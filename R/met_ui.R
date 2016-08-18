
#' met_ui
#'
#' @param title string for caption
#'
#' @return
#' @export
met_ui <- function(title = "MET explorer"){
  tabBox( width = 12, selected = "Data",
          tabPanel("Data",
                   prepareMetDataUI("data")
          ),
          tabPanel("Plots",
                   linkedBiplotUI("met")
          ),
          # tabPanel("Report",
          #          uiOutput("metReport")
          # ),
          tabPanel("Help",
                   helpPanel(fbhelp::list_tutorials("fbmet")[[1]])
          ),
          tabPanel("About",
                   helpPanel(fbhelp::list_tutorials("fbmet", name = "about")[[1]],
                             center = TRUE)
          )
  )
}
