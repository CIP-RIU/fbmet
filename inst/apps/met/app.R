library(shiny)
library(fbmet)

library(agricolae)
# Example 1
data(plrv)

model<- with(plrv, AMMI(Locality, Genotype, Rep, Yield, console=FALSE))

library(dplyr)
ndat <- plrv %>% group_by(Genotype, Locality) %>% summarise(Yield = mean(Yield))


library(ggplot2)
library(ggrepel)
library(shinyBS)
library(fbmet)

shinyApp(
  ui = basicPage(
    linkedBiplotUI("met")
  ),

  server = function(input, output, session) {
    metsel = callModule(met_selected, "met", model, ndat)

    # output$plot_brushedpoints <- DT::renderDataTable({
    #   res = NULL
    #   if(is.data.frame( metsel)){
    #     res = metsel
    #   }
    #   res
    # })

  }
)
