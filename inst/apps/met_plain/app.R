
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
    fluidRow(
      column(width = 7,
             uiOutput("plotTitle"),
             bsTooltip("plotTitle", paste0(
               "This biplot has dots sized by Yield and colored by type - ",
               "either genotype or environment. Drawing a rectangle over ",
               "a set of points will select them and update the other charts ",
               "and table."),
               "bottom"),
             plotOutput("plot", height=400,
                        click = clickOpts(id = "plot_click"),  # Equiv, to click=clickOpts(id="plot_click")
                        dblclick = "plot_dblclick",
                        #hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
                        brush = brushOpts(id = "plot_brush")
             ),
             HTML("<center>"),
             #actionButton("exclude_toggle", "Select/unselect genotypes"),
             actionButton("exclude_reset", "Reset"),
             HTML("<center/>"),
             #h4("Brushed points"),
             dataTableOutput("plot_brushedpoints")
      ),
      column(width = 5,
             #center(h4("Compare genotypes")),
             uiOutput("lineTitle"),
             plotOutput("line", height = 400),
             plotOutput("hist", height = 300)

      )
    )
  ),
  server = function(input, output, session) {

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

    output$plot_brushedpoints <- renderDataTable({
      res <- brush()
      res <- brush()
      dat = data()$biplot
      if(nrow(res)==0){
        res <- dat[!vals$keeprows, , drop = FALSE]
      }
      nn = min(5, ncol(dat))
      if (nrow(res) == 0 )
        return(dat[, c(2:nn)])
      res[, c(2:nn)]
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

  }
)





