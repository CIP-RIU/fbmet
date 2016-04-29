
#' prepareMetDataUI
#'
#' @param id shiny id
#' @import fbhelp
#' @return shiny fluid row
#' @export
prepareMetDataUI <- function(id){
  #ns <- shiny::NS(id)

  shiny::fluidRow(
    column(width = 3,
           #ui_tips("metFileTitle", "fbmet"),
           selectInput("met_crop", "Crop", c("potato", "sweetpotato")),
           radioButtons("met_source", "Data source:",
                        choices = c("File", "Previous", "Cache","BRAPI DB"),
                        selected = "File", inline = TRUE),
           conditionalPanel(
             condition = "input.met_source == 'File'",
              shinyFiles::shinyFilesButton('file', 'File select',
                'Please select a file', TRUE
              )#,
             #verbatimTextOutput('filepaths')
            ),

           # radioButtons("metTraitOrIndex", "Variable to analyze:",
           #              choices = c("trait", "Elston"),
           #              selected = "trait",
           #              inline = TRUE),
           # conditionalPanel(
           #   condition = "input.metTraitOrIndex == 'trait'",
             uiOutput("met_traits"),
           verbatimTextOutput('logs')
           # ),
           #
           # conditionalPanel(
           #   condition = "input.metTraitOrIndex == 'Elston'",
           #  uiOutput("met_elston_neg"),
           #  uiOutput("met_elston_pos")
           # )
           ),
    column(width = 9,
           DT::dataTableOutput("met_raw")
           )
  )


}
