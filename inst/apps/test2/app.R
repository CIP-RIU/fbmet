library(shiny)
library(shinyFiles)
library(fbmet)

ui = shiny::shinyUI(pageWithSidebar(
  headerPanel(
    'Selections with shinyFiles',
    'shinyFiles example'
  ),
  sidebarPanel(),
  mainPanel(
shiny::fluidRow(
  column(width = 8,
         # shinyFiles::shinyFilesButton('file', 'File select',
         #                              'Please select a file', TRUE)
         #prepareMetDataUI('metdata')
         fluidRow(
           column(width = 9,
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

                  radioButtons("metTraitOrIndex", "Variable to analyze:",
                               choices = c("trait", "Elston"),
                               selected = "trait",
                               inline = TRUE),

                  conditionalPanel(
                    condition = "input.metTraitOrIndex == 'trait'",
                    uiOutput("met_traits")
                  ),

                  conditionalPanel(
                    condition = "input.metTraitOrIndex == 'Elston'",
                    uiOutput("met_elston_neg"),
                    uiOutput("met_elston_pos")
                  )
           ),
           column(width = 3,
                  DT::dataTableOutput("met_raw")
           )
         )
  ),
  column(width = 4)
)
)
)
)

sv = shinyServer(function(input, output, session) {
  #volumes <- c('R Installation'=R.home())
  volumes <- getVolumes(c("(E:)", "Page File (F:)"))
  #print(volumes)
  shinyFileChoose(input, 'file', roots=volumes, session=session)

  metFiles <- reactive({
    mf = parseFilePaths(volumes, input$file)$datapath
    mf = as.character(mf)
    mf
  })

  output$met_raw <- DT::renderDataTable({
    #print(metFiles())
    bks = metFiles()
    #print(str(bks))
    if(length(bks)==0) return(NULL)
    dat <- combine_books(bks)
    #print(head(dat))
    #data = cbind(rownames(dat), dat)
    #names(dat)[1] = "Genotype"
    dat
  })

  get_met_traits <- reactive({
    dat = metFiles()
    if(length(dat)==0) return(NULL)
    names(met_raw())[4:ncol(met_raw())]
  })


  output$met_traits <- renderUI({
    traits <- get_met_traits()
    selectInput("met_trait", label = "Trait:", choices = traits)
  })

  output$met_elston_neg <- renderUI({
    traits <- get_met_traits()
    selectizeInput("met_en", label = "Negative traits:", choices = traits)
  })

  ei_ng <- reactive({
    out = input$met_en
    print(out)
  })

  output$met_elston_pos <- renderUI({
    traits <- get_met_traits()
    x = ei_ng()
    selectizeInput("met_ps", label = "Positive traits:", choices = traits)
  })



})

shinyApp(ui, sv)
