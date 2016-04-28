#' metAddin
#'
#' Creates an Rstudio Addin.
#' @import dplyr
#' @import agricolae
#' @import shiny
#' @import miniUI
#' @return exit status
#'
#' @export
metAddin <- function(){

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("MET explorer"),
    miniUI::miniContentPanel(
      linkedBiplotUI("met")
    )
  )

  server <- function(input, output, session) {
    plrv =  loadRData((system.file("data/plrv.rda", package="agricolae")))

    model<- with(plrv, AMMI(Locality, Genotype, Rep, Yield,
                 console=FALSE))
    ##ndat <- dplyr::group_by(plrv, "Genotype", "Locality")
    ndat <- with(plrv, dplyr::summarise(group_by(plrv, Genotype, Locality),
                             Yield = mean(Yield))
    )

    metsel = callModule(met_selected, "met", plrv, model, ndat)

    observeEvent(input$done, {
      stopApp()
    })
  }

  viewer <- paneViewer(300)
  runGadget(ui, server, viewer = viewer)

}
