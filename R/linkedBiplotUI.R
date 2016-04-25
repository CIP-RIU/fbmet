
#' linkedBiplotUI
#'
#' @param id shiny
#' @import shiny
#' @export
linkedBiplotUI <- function(id){
  ns <- shiny::NS(id)

  shiny::fluidRow(
    column(width = 7,
           ui_help("plotTitle", "fbmet"),
           # uiOutput(ns("plotTitle")),
           # bsTooltip(ns("plotTitle"), paste0(
           #   "This biplot has dots sized by Yield and colored by type - ",
           #   "either genotype or environment. Drawing a rectangle over ",
           #   "a set of points will select them and update the other charts ",
           #   "and table."),
           #   "bottom"),
           plotOutput(ns("plot"), height=400,
                      click = clickOpts(id = ns("plot_click")),  # Equiv, to click=clickOpts(id="plot_click")
                      dblclick = ns("plot_dblclick"),
                      #hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
                      brush = brushOpts(id = ns("plot_brush"))
           ),
           HTML("<center>"),
           #actionButton("exclude_toggle", "Select/unselect genotypes"),
           actionButton(ns("exclude_reset"), "Reset"),
           HTML("<center/>"),
           #h4("Brushed points"),
           #uiOutput(ns("tableTitle")),
           #ui_help(ns("tableTitle")),
           ui_help("tableTitle", "fbmet"),
           dataTableOutput(ns("plot_brushedpoints"))
    ),
    column(width = 5,
           #center(h4("Compare genotypes")),
           #uiOutput(ns("lineTitle")),
           ui_help("lineTitle", "fbmet"),
           plotOutput(ns("line"), height = 400),
           ui_help("histTitle", "fbmet"),
           plotOutput(ns("hist"), height = 300)

    )
  )

}
