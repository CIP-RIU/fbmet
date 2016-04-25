#' ui_help
#'
#' @param id shiny id
#' @import shiny
#' @import shinyBS
#' @return tagList
#' @export
ui_help <- function(id){

  tagList(
    HTML("<center><h4>"),
    div(id = id, "Table MET"),
    HTML("</h4></center>"),
    bsTooltip(id, paste0(
      "This biplot has dots sized by Yield and colored by type - ",
      "either genotype or environment. Drawing a rectangle over ",
      "a set of points will select them and update the other charts ",
      "and table."),
      "bottom")
  )

}
