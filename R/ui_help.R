#' ui_help
#'
#' @param idHlp a tag id
#' @param pkg the package where the tips are
#'
#' @import shiny
#' @import shinyBS
#' @import yaml
#' @return tagList
#' @export
ui_help <- function(idHlp, pkg){
  alang = "en"
  afile = paste0("context_", alang, ".yaml")
  apath = file.path("tips", afile)
  ahelp = system.file(apath, package = pkg)
  actxt = yaml::yaml.load_file(ahelp)

  idLbl = paste0(idHlp, "_label")
  idLbl = actxt[[idLbl]]
  idHvr = paste0(idHlp, "_hover")
  idHvr = actxt[[idHvr]]


  tagList(
    HTML("<center><h4>"),
    div(id = idHlp, idLbl),
    HTML("</h4></center>"),
    bsTooltip(idHlp, idHvr, "bottom")
  )

}
