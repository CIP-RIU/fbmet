library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyFiles)
library(DT)
library(agricolae)
library(dplyr)
library(fbmet)
library(fbhelp)

shinyApp(
  ui = dashboardPage( skin = "yellow",
    dashboardHeader(title = "MET explorer"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      tabBox( width = 12, selected = "Data",
        tabPanel("Data",
                 prepareMetDataUI("data")
                 ),
        tabPanel("Plots",
                linkedBiplotUI("met")
                ),
        tabPanel("Report"),
        tabPanel("Help",
                 helpPanel(fbhelp::list_tutorials("fbmet")[[1]])
                ),
        tabPanel("About",
                 helpPanel(fbhelp::list_tutorials("fbmet", name = "about")[[1]],
                           center = TRUE)
        )
      )
    )
  ),

  server = function(input, output, session) {

    volumes <- getVolumes(c("(E:)", "Page File (F:)"))
    #print(volumes)
    shinyFileChoose(input, 'file', roots=volumes, session=session)

    metFiles <- reactive({
      mf = parseFilePaths(volumes, input$file)$datapath
      mf = as.character(mf)
      mf
    })
    output$filepaths <- renderPrint({metFiles()})

    met_raw <- reactive({
      bks = metFiles()
      if(length(bks)==0) return(NULL)
      dat <- combine_books(bks)
      for(i in 4:ncol(dat)){
        dat[, i] = round(dat[, i], 5)
      }
      dat
    })

    met_active <- reactive({
      out = NULL
      #if(input$metTraitOrIndex == "trait"){
        act_trait = input$met_trait
        #print(str(act_trait))
        if(length(act_trait) == 0) return(met_raw()[, 1:3])
        if(is.null(act_trait)) return(met_raw()[, 1:3])
        if(act_trait == "") return(met_raw()[, 1:3])
        #print(paste0("active ", act_trait))
        nm = c(names(met_raw())[1:3], act_trait)
        #print(str(nm))
        out = met_raw()[, nm]
      #}
      # if(input$metTraitOrIndex == "Elston"){
      #   act_en = input$met_en
      #   print(act_en)
      #   act_ep = input$met_ep
      #   print(act_ep)
      #   if(length(act_en) == 0 & length(act_ep) == 0){
      #     return(met_raw()[, 1:3])
      #   }
      #   if(length(act_en) > 0 & length(act_ep) == 0){
      #     x = met_raw()[, act_en]
      #     x = x * -1
      #     if(is.vector(x)){
      #       out = cbind(met_raw()[, 1:3], x)
      #       names(out)[4] = act_en
      #     } else{
      #       out = cbind(met_raw()[, 1:3], x[act_en])
      #     }
      #
      #   }
      #   if(length(act_en) == 0 & length(act_ep) > 0) {
      #     x = met_raw()[, act_ep]
      #     if(is.vector(x)){
      #       out = cbind(met_raw()[, 1:3], x)
      #       names(out)[4] = act_ep
      #     } else{
      #       out = cbind(met_raw()[, 1:3], x[act_ep])
      #     }
      #   }
      #   if(length(act_en) > 0 & length(act_ep) > 0) {
      #     x = met_raw()[, act_en]
      #     x = x * -1
      #     y = met_raw()[, act_ep]
      #     #out = cbind(met_raw()[, 1:3], x[act_en], y[act_ep])
      #     if(is.vector(x)){
      #       out = cbind(met_raw()[, 1:3], x)
      #       names(out)[4] = act_en
      #     } else{
      #       out = cbind(met_raw()[, 1:3], x[act_en])
      #     }
      #     if(is.vector(y)){
      #       out = cbind(out, y)
      #       names(out)[ncol(out)] = act_ep
      #     } else {
      #       out = cbind(out, y[act_ep])
      #     }
      #
      #     if(ncol(out) > 3){
      #       trt = names(out)[3:ncol(out)]
      #       els = st4gi::elston(trt, names(out)[2], names(out)[1], names(out)[3],
      #                           data = out)
      #       #out = cbind(out, Elston = els)
      #       #out = merge(out, els$)
      #     }
      #
      #   }


      #}

      #write.csv(out, file = "out.csv")
      out
    })

    output$met_raw <- DT::renderDataTable({
      met_active()
    }
    ,#filter = "top",
     options = list(searching = TRUE,
                      lengthMenu = list(c(5, 10, 15, 25, -1),
                                        c('5', '10','15', '25', 'All')),
                      pageLength = 10)
    )

    get_met_traits <- reactive({
      dat = metFiles()
      if(length(dat)==0) return(NULL)
      names(met_raw())[4:ncol(met_raw())]
    })

    output$met_traits <- renderUI({
      traits <- get_met_traits()
      if(!is.null(traits)) {
        last_trait = last(traits)
      } else {
        last_trait = NULL
      }
      selectInput("met_trait", label = "Trait:", choices = traits,
                  selected = last_trait)
    })

    output$met_elston_neg <- renderUI({
      traits <- get_met_traits()
      selectizeInput("met_en", label = "Negative traits:", choices = traits,
                     multiple = TRUE)
    })

    # plrv =  loadRData((system.file("data/plrv.rda", package="agricolae")))
    # model<- with(plrv, AMMI(Locality, Genotype, Rep, Yield, console = FALSE))
    # ndat <- dplyr::summarise(group_by(plrv, Genotype, Locality), Yield = mean(Yield))

    mdl <- reactive({
      dat = met_active()

      if(is.null(dat)) return(NULL)
      trt = input$met_trait
      if(length(trt) == 0) return(NULL)
      if(trt == "") return(NULL)
      dat = dat[, c(2, 1, 3, 4)]
      model <- AMMI(dat$Locality, dat$Genotype, dat$Rep, dat[, trt], console = FALSE)
      names(model$biplot)[2] = trt
      #ndat <- dplyr::summarise(group_by(dat, Genotype, Locality), Yield = mean(trt))
      ndat = model$means
      names(ndat)[1:3] = c("Locality", "Genotype", trt)
      ndat = ndat[, c(2,1,3)]
       list(dat, model, ndat)
    })

  observe({

    if(!is.null(mdl())) {
      withProgress(message = "Preparing plots", {
      callModule(met_selected, "met",
                 reactive(mdl()[[1]]),
                 reactive(mdl()[[2]]),
                 reactive(mdl()[[3]])
      )
      })
    }
    })
}
)
