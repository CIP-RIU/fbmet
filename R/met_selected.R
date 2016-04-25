
#' met_selected
#'
#' Select interactively genotypes from a multi-environmental dataset.
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param model AMMI model
#' @param ndat average data by genotype and environment
#' @import agricolae
#' @import shiny
#' @export
met_selected <- function(input, output, session, model, ndat){

  mm = round(model$biplot[, c(2:ncol(model$biplot))], 5)
  model$biplot[, c(2:ncol(model$biplot)) ] = mm


  ranges <- reactiveValues(x = NULL, y = NULL)


  dat = model

  data <- reactive({
    model
  })


  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(dat$biplot))
  )


  brush <- reactive({
    brushedPoints(data()$biplot, input$plot_brush, "PC1", "PC2")
  })

  mdata <- reactive({
    unmarked    <- data()$biplot[ vals$keeprows, , drop = FALSE]
    marked      <- data()$biplot[!vals$keeprows, , drop = FALSE]
    marked$type <- as.character(marked$type)
    if(nrow(marked) > 0){
      for(i in 1:nrow(marked)){
        if(marked$type[i] == "GEN") {
          marked$type[i] <- "GENSEL"
        }
      }
    }

    unmarked$type <- as.character(unmarked$type)

    df <- rbind(marked, unmarked)
    df$type <- as.factor(df$type)
    #print(df)
    df
  })


  output$plot <- renderPlot({
    mm = mdata()
    model$biplot = mm
    gg_biplot(model, ranges = ranges)
  })

  output$plotTitle <- renderUI(HTML("<center><h4>Biplot explorer</h4></center>"))

  # shinyBS::addPopover(session, "plotTitle", "Biplot", content = paste0(
  #   "<p>This biplot has dots sized by Yield and colored by type -
  #   either 'genotype' or 'environment'", placement = "bottom"
  # ) )

  output$lineTitle <- renderUI(HTML("<center><h4>Genotypes in environments</h4></center>"))

  flt <- reactive({
    res <- brush()

    res2 <- data()$biplot[!vals$keeprows, , drop = FALSE]
    if(nrow(res2)!=0){
      flt = res2
      res = flt
    }

    if(nrow(res) == 0) {
      d <- data()$biplot
      #d <- vals
      gt <- sort(unique(rownames(d)))
      flt <- gt[stringr::str_detect(gt, "[A-Z]{1,1}[a-z]{2,3}")]
    }
    if(nrow(res) > 0){
      flt = unique(rownames(res))
    }
    flt
  })

  output$line <- renderPlot({
    gg_env_plot(ndat, flt())
  })

  output$hist <- renderPlot({
    # res <- brush()
    # if(nrow(res) == 0){
    #   res <- model$biplot[!vals$keeprows, , drop = FALSE]
    # }
    # print(res)
    gg_hist(model, flt())
  })

  fltDat <- reactive({
    res <- brush()
    if(nrow(res)==0){
      res <- data()$biplot[!vals$keeprows, , drop = FALSE]
    }

    # res <- mdata()
    # res <- res[res$type == "GENSEL", ]
    #print(res)
    if (nrow(res) == 0 )
      #return()
      return(data()$biplot[, c(2:5)])

    #if(nrow(res) == 0) res = resb
    res[, c(2:5)]
  })

  output$plot_brushedpoints <- renderDataTable({
    fltDat()
  }, options = list(filter = "top"))


  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })



  observeEvent(input$plot_click, {
    #print(input$plot_click)
    res <- shiny::nearPoints(data()$biplot, input$plot_click,
                             "PC1", "PC2", allRows = TRUE)
    #print(res)
    vals$keeprows <- xor(vals$keeprows, res$selected_)
    #print(vals$keeprows)
  })

  # Toggle points that are brushed, when button is clicked
  # observeEvent(input$exclude_toggle, {
  #   res <- brushedPoints(dat$biplot, input$plot_brush, allRows = TRUE)
  #
  #   vals$keeprows <- xor(vals$keeprows, res$selected_)
  # })

  # Reset all points
  observeEvent(input$exclude_reset, {
    vals$keeprows <- rep(TRUE, nrow(dat$biplot))
  })


  # metsel = shiny::reactive({
  #   res <- brush()
  #   if(nrow(res)==0){
  #     res <- dat$biplot[!vals$keeprows, , drop = FALSE]
  #   }
  #
  #   if (nrow(res) == 0 )
  #     #return()
  #     return(data()$biplot[, c(2:5)])
  #
  #   res[, c(2:5)]
  #
  # })
  #
  return(fltDat)

}
