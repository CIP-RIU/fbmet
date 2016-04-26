#' metAddin
#'
#' Creates an Rstudio Addin.
#' @import agricolae
#' @import dplyr
#' @return exit status
#'
#' @export
metAddin <- function(){
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("MET assist"),
    miniUI::miniContentPanel(
      linkedBiplotUI("met")
    )
  )

  server <- function(input, output, session) {
    #library(agricolae)
    #library(dplyr)

    data("plrv", envir = environment())
    model<- AMMI(plrv$Locality, plrv$Genotype, plrv$Rep, plrv$Yield,
                 console=FALSE)
    ndat <- plrv %>% dplyr::group_by(Genotype, Locality) %>%
      dplyr::summarise(Yield = mean(Yield))

    metsel = callModule(met_selected, "met", model, ndat)


    observeEvent(input$done, {
      stopApp()
    })
  }

  viewer <- paneViewer(300)
  runGadget(ui, server, viewer = viewer)

}
