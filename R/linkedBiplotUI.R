#' linkedBiplotUI
#'
#' @param id shiny
#' @import fbhelp
#' @author Reinhard Simon
#' @return fluidRow (shiny fragment)
#' @export
linkedBiplotUI <- function(id){
  ns <- shiny::NS(id)

  shiny::fluidRow(
    column(width = 7,
           fbhelp::ui_tips("plotTitle", "fbmet"),
           plotOutput(ns("plot"), height=400,
                      click = clickOpts(id = ns("plot_click")),  # Equiv, to click=clickOpts(id="plot_click")
                      dblclick = ns("plot_dblclick"),
                      brush = brushOpts(id = ns("plot_brush"))
           ),
           HTML("<center>"),
           actionButton(ns("exclude_reset"), "Reset"),
           HTML("<center/>"),
           ui_tips("tableTitle", "fbmet"),
           dataTableOutput(ns("plot_brushedpoints"))
    ),
    column(width = 5,
           ui_tips("lineTitle", "fbmet"),
           plotOutput(ns("line"), height = 400),
           ui_tips("histTitle", "fbmet"),
           plotOutput(ns("hist"), height = 300)

    )
  )

}
