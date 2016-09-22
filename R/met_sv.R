# TODO move this to fbglobal!
extract_params <- function(cn) {

  gti= which(stringr::str_detect(cn, "CODE|INSTN|GENOTYPE|GENO|GERMPLASMNAME|CIPNUMBER"))[1]
  bki= which(stringr::str_detect(cn, "BLOCK|BLK|BLOC" ))[1]
  rpi= which(stringr::str_detect(cn, "REP|REPL|REPLICATION" ))[1]
  pti= which(stringr::str_detect(cn, "PLOT|PLT" ))[1]
  lti= which(stringr::str_detect(cn, "LOCALITY" ))[1]
  ci = 1:length(cn)
  tti= ci[!ci %in% c(gti, bki, rpi, pti, lti)]
  tn = cn[tti] %>% sort(decreasing = TRUE)
  list(tn = tn,tti = tti, gti = gti,bki = bki, rpi = rpi, pti= pti, lti = lti, ci = ci)
}



#' met_sv
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @importFrom shinyFiles shinyFileChoose getVolumes parseFilePaths shinyFilesButton
#' @param values shiny values
#' @export
met_sv <- function(input, output, session, values){
  volumes <- getVolumes(c("(E:)", "Page File (F:)"))
  #print(volumes)
  shinyFileChoose(input, 'dc_met_fieldbook', roots=volumes, session=session,
                  filetypes = c('', 'xls', 'xlsx'))

  metFiles <- reactive({
    req(input$dc_met_fieldbook)
    mf = parseFilePaths(volumes, input$dc_met_fieldbook)$datapath
    mf = as.character(mf)
    mf
  })

  output$filepaths <- renderPrint({metFiles()})

  met_bks <- reactive({
    bks = metFiles()
    #print(bks)
    if(length(bks) <= 2) return(NULL)
    dat <- withProgress({
      combine_books(bks)
    }, message = "Combining fieldbooks")

    if(is.null(dat)) return(NULL)
    names(dat) <- names(dat) %>% toupper()
     dat
  })

  colNms <- reactive({
    req(met_bks())
    names(met_bks()) #%>% toupper()
  })

  ep <- reactive({
    cn = colNms()
    extract_params(cn)
  })

  output$ui_met_env <- renderUI({
    req(met_bks())
    req(ep())
    #mi = min(ep()$ci, 4)
    mi  = 4
    cn = colNms()
    shiny::selectInput("met_env", label = "Environment", cn[1:mi], cn[ep()$lti])
  })

  output$ui_met_plt <- renderUI({
    req(met_bks())
    req(ep())
    print(ep())
    # message(ep())
    # message(colNms())
    #mi = min(ep()$ci, 4)
    mi = 4
    cn = colNms()
    shiny::selectInput("met_plt", label = "Plot", cn[1:mi], cn[ep()$pti])
  })

  output$ui_met_rep <- renderUI({
    req(met_bks())
    req(ep())
    #mi = min(ep()$ci, 4)
    mi = 4
    cn = colNms()
    shiny::selectInput("met_rep", label = "Replication", cn[1:mi], cn[ep()$rpi])
  })


  output$ui_met_gen <- renderUI({
    req(met_bks())
    req(ep())
    #mi = min(ep()$ci, 5)
    mi = 4
    cn = colNms()
    shiny::selectInput("met_gen", label = "Genotype", cn[1:mi], cn[ep()$gti])
  })

  output$ui_met_trt <- renderUI({
    req(met_bks())
    req(ep())
    #mi = min(ep()$ci, 5)
    shiny::selectInput("met_trt", label = "Trait", ep()$tn, multiple = FALSE) # TODO multiple for Sel. Ind.
  })

  # met_dat <- reactive({
  #   req(met_bks())
  #   req(ep())
  #   dat = met_bks()[, c(input$met_env, input$met_plt, input$met_rep,
  #                 input$met_gen, input$met_trt[1])]
  #   dat[, input$met_env] = sapply(dat[, input$met_env], as.factor)
  #   dat[, input$met_gen] = sapply(dat[, input$met_gen], as.factor)
  #   dat
  # })

  met_data <- reactive({
    req(met_bks())
    #req(ep())
    req(input$met_env)
    req(input$met_plt)
    req(input$met_rep)
    req(input$met_gen)
    req(input$met_trt)
    dat = met_bks()[, c(input$met_env, input$met_plt, input$met_rep,
                        input$met_gen, input$met_trt)]
    dat[, input$met_env] = sapply(dat[, input$met_env], as.factor)
    dat[, input$met_gen] = sapply(dat[, input$met_gen], as.factor)
    dat
  })


  output$met_raw <- DT::renderDataTable({
    #met_active()
    met_data()
  }
  ,#filter = "top",
  options = list(searching = TRUE,
                 lengthMenu = list(c(5, 10, 15, 25, -1),
                                   c('5', '10','15', '25', 'All')),
                 pageLength = 10)
  )

  # get_met_traits <- reactive({
  #   dat = metFiles()
  #   if(length(dat)==0) return(NULL)
  #   names(met_raw())[4:ncol(met_raw())]
  # })
  #
  # output$met_traits <- renderUI({
  #   req(input$dc_fieldbook)
  #   traits <- get_met_traits()
  #   if(!is.null(traits)) {
  #     last_trait = last(traits)
  #   } else {
  #     last_trait = NULL
  #   }
  #   selectInput("met_trait", label = "Trait:", choices = traits,
  #               selected = last_trait)
  # })
  #
  # output$met_elston_neg <- renderUI({
  #   traits <- get_met_traits()
  #   selectizeInput("met_en", label = "Negative traits:", choices = traits,
  #                  multiple = TRUE)
  # })
  #
  # # plrv =  loadRData((system.file("data/plrv.rda", package="agricolae")))
  # # model<- with(plrv, AMMI(Locality, Genotype, Rep, Yield, console = FALSE))
  # # ndat <- dplyr::summarise(group_by(plrv, Genotype, Locality), Yield = mean(Yield))
  #
  mdl <- reactive({
    #dat = met_dat()
    req(met_bks())
    #req(ep())
    req(input$met_env)
    req(input$met_plt)
    req(input$met_rep)
    req(input$met_gen)
    req(input$met_trt)
    dat = met_data()

    #withProgress(message = "Calculating ...", {
    if(is.null(dat)) return(NULL)
    trt = input$met_trt[1]
    gen = input$met_gen
    rep = input$met_rep
    env = input$met_env
    if(length(trt) == 0) return(NULL)
    if(trt == "") return(NULL)
    #dat = dat[, c(2, 1, 3, 4)]
    #model <- AMMI(dat$Locality, dat$Genotype, dat$Rep, dat[, trt], console = FALSE)
    model <- AMMI(dat[, env], dat[, gen], dat[, rep], dat[, trt], console = FALSE)
    names(model$biplot)[2] = trt
    #ndat <- dplyr::summarise(group_by(dat, Genotype, Locality), Yield = mean(trt))
    ndat = model$means
    names(ndat)[1:3] = c("Locality", "Genotype", trt)
    ndat = ndat[, c(2,1,3)]
    #})
    out = list(dat, model, ndat)

    #print(out)

    out

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

  output$metReport <- renderUI({
    req(input$met_trt)
  withProgress({
    try({
    dirfiles <- system.file(package = "pepa")
    fileRmd <- paste(dirfiles, "/rmd/met.Rmd", sep = "")
    # #fileURL <- paste(dirfiles, "/rmd/met.html", sep = "")
    dat = met_data()

    filePath = rmarkdown::render(fileRmd, params = list(traits = input$met_trt,
                                             geno = input$met_gen,
                                             env = input$met_env,
                                             rep = input$met_rep,
                                             data = dat,
                                             maxp = .1,
                                             title = "Automatic report for a MET with a RCBD",
                                             subtitle = NULL,
                                             #format = "html_document",
                                             author = "CIP"))
    tags$div(style = "max-width: 800px;",
             includeHTML(
               filePath
             )
    )
    })
  }, message = "Creating Multi Enviroment Report...")
  })


}

