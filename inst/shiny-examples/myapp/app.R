library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyMatrix)

library(dplyr)
library(ggplot2)
library(visNetwork)

options(spinner.color = "#518CE2", spinner.color.background = "000000", spinner.size = 2)

ui <- navbarPage(title = "TMT interference optimization",
                 id = "navBar",
                 theme = "paper.css", # file in www
                 collapsible = TRUE, # usefull when viewing on smaller screen
                 inverse = FALSE, # true: use a dark background and light text for the navigation bar
                 windowTitle = "tmt_opt", # just name of tab
                 position = "fixed-top",
                 header = tagList(
                   shinyWidgets::useShinydashboard(),      # allow to render the boxes from shinydashboard
                   tags$style("body {padding-top: 75px;}")
                 ),

                 tabPanelBody(value = "main",
                              shinyjs::useShinyjs(),
                   shiny::HTML("<h1>Background</h1><br>"),
                   shiny::HTML("<h5>When using TMT labeling, ion interference can occur due to mainly the carbon isotopes,
                               but also nitrogen isotopes and now deuterium with TMT32 and TMT35.
                               It has been shown that this can have a strong impact on the quality of the data produced,
                               inducing noise and false positive between channels (<a href='https://www.mcponline.org/article/S1535-9476(20)31525-5/fulltext'>
                               Brenes et al., Molecular & Cellular Proteomics 2019</a>). The goal of this app is to help you
                               find the optimal experiment design (the one that induce the less interference)
                               considering your number of conditions and replicates in order to lower the false positive rate as much as possible.</h5>"),
                   tags$hr(),

                   shiny::HTML("<h2>Your design</h2><br>"),
                   tags$style(type = 'text/css',
                              '#modal1_exampletmtinter .modal-dialog { width: fit-content !important; overflow-x: initial !important}
                               #modal1_exampletmtinter .modal-body { width: 150vh; overflow-x: auto;}'
                              ),
                   tags$style(type = 'text/css',
                              '#modal2_tmtinter .modal-dialog { width: fit-content !important; overflow-x: initial !important}
                               #modal2_tmtinter .modal-body { width: 150vh; overflow-x: auto;}'
                              ),
                   fluidRow(column(6, radioButtons("noise_datatype", "",
                                                    choiceNames = list(HTML("<b>Use default TMT intereference data</b>"),
                                                                    HTML("<b>Use your own TMT intereference data</b>")
                                                                    ),
                                                   choiceValues = list("def", "own"),
                                                    selected = "def", inline = TRUE, width = "100%")),
                            conditionalPanel(condition = "input.noise_datatype == 'own'",
                                             column(6, fileInput("noise_datafile", "Upload the reporter ion isotopic distribution of the kit you are using (xlsx, txt, csv or pdf)",
                                                                 accept = c("txt", "xlsx", "csv", "pdf"), width = "90%"),
                                                    textOutput("diag_readinter"),
                                                    column(3, actionButton("tmtinter_example", "View example file")),
                                                    column(3, actionButton("own_tmtinter", "View uploaded file"))
                                                    )
                                             ),
                            conditionalPanel(condition = "input.noise_datatype == 'def'",
                                             column(6, selectInput("nb_channels", "How many TMT channels do you want to use ?",
                                                                   choices = c("10", "11", "16", "18", "32", "35"), selected = "16", width = "90%")
                                                    )
                                             )
                            ),
                   tags$br(),
                   fluidRow(column(4, numericInput("nb_cond", "How many condition do you have ?",
                                                   min = 1, step = 1, value = 5, width = "80%")),
                            column(4, checkboxInput("same_nb_rep", "Keep the same number of replicate for all conditions", TRUE, width = "80%"),
                                   conditionalPanel(condition = "input.same_nb_rep",
                                                    numericInput("nb_rep", "How many replicates do you have ?",
                                                                 min = 1, step = 1, value = 3, width = "80%")
                                                    ),
                                   conditionalPanel(condition = "!input.same_nb_rep",
                                                    uiOutput("diff_nb_rep_ui", width = "80%"),
                                                    htmlOutput("nbrep_matrix_check")
                                                    )
                                   ),
                            conditionalPanel(condition = "output.is_mix",
                                             column(4, selectInput("nb_mix", "How many carrier channels will you add ?
                                                                            If 0, then consider it's an empty channel (i.e. no interference)",
                                                                   choices = c(0), selected = 0, width = "80%"))
                                             )
                            ),
                   tags$hr(),

                   shiny::HTML("<h1>Optimize the interferences</h1><br>"),
                   shiny::HTML("<h5>Some optimal designs have already been computed and you can see them directly.
                               If it's not the case it will compute the optimal one. The more iteration you do
                               the more likely your are to find a global minimum but it will of course take more
                               time.<br>
                               When NA is shown, it means that the channel is empty and so will not produce any
                               interferences. When using carrier channels (labelled Mix), those will induce interferences to other
                               channels but since it's not used for the quantification, interferences they get from the other channels
                               are not taken into account.</h5>"),
                   tags$hr(),

                   shinyjs::useShinyjs(),
                   fluidRow(conditionalPanel(condition = "!output.is_already_compted",
                                             conditionalPanel(condition = "input.nb_channels == '10' | input.nb_channels == '11'",
                                                              column(4, checkboxInput("get_exact", "Compute exact optimum (quite long)", FALSE, width = "100%"))
                                                              ),
                                             conditionalPanel(condition = "!input.get_exact",
                                                              column(4, numericInput("nb_iter", "Choose a maximum number of iterations",
                                                                                     min = 1, step = 5, value = 10, width = "100%")),
                                                              )
                                             ),
                            column(4, actionButton("plot_opt", "Plot optimal design", class = "btn-success btn-lg"))
                            ),
                   tags$hr(),
                   textOutput("diag_opt"),
                   conditionalPanel(condition = "output.opt_des_up",
                                    shinycssloaders::withSpinner(plotOutput("opt_des", height = "400px"), type = 6),
                                    tags$br(),
                                    fluidRow(column(3, downloadButton("down_design", "Download plot", class = "btn-primary btn-lg")),
                                             column(3, downloadButton("down_designTab", "Download possible optimal designs (xlsx)", class = "btn-primary btn-lg")),
                                             column(3, textOutput("diag_downdesTab"))
                                             )
                                    ),
                   tags$hr(),

                   shiny::HTML("<h1>Plot and compute noise from your design</h1><br>"),
                   uiOutput("cond_name_ui"),
                   shiny::HTML("<br><h2>Assign your channels to your conditons</h2>"),
                   tableOutput("design_table"),
                   conditionalPanel(condition = "output.tmt_greater18",
                                    tableOutput("design_table_32")
                                    ),

                   conditionalPanel(condition = "!output.design_ok",
                                    htmlOutput("warning_design")
                                    ),
                   conditionalPanel(condition = "output.design_ok",
                                    fluidRow(column(3, actionButton("plot_design", "Plot your design", class = "btn-success btn-lg")),
                                             column(3, downloadButton("down_designTabNamed", "Download possible designs (xlsx)", class = "btn-primary btn-lg")),
                                             column(3, textOutput("diag_downdesTabNamed"))
                                             )
                                    ),

                   tags$hr(),
                   conditionalPanel(condition = "output.design_ready",
                                    fluidRow(column(6, selectInput("swap", "Choose conditions positions to swap",
                                                                   multiple = TRUE, choices = NULL, selected = NULL,
                                                                   width = "100%")),
                                             column(6, actionButton("go_swap", "Swap selected positions", class = "btn-success btn-lg"))),
                                    shinycssloaders::withSpinner(visNetworkOutput("own_design", height = "400px"), type = 6)
                                    ),
                   tags$hr(),

                   shiny::HTML("<br><br><em>Author: Marc-Antoine GERAULT<em><br><em>Contact: <a href='mailto:marc.antoine.gerault@ki.se'>marc.antoine.gerault@ki.se</a><br><br>")
                   )
                 )

server <- function(input, output, session){
  shinyjs::hide(selector = "#navBar li a[data-value=main]")

  ### handle own reporter ion isotopic distribution
  uploaded_tmtinter <- reactive({
    if(input$noise_datatype == "own"){
      File <- input$noise_datafile
      if (is.null(File)){
        return(NULL)
      }

      withCallingHandlers({
        shinyjs::html("diag_readinter", "")
        message("Reading...")
        df <- read_tmtinterference(File$datapath)
        message("File successfully uploaded !")
      },
      message = function(m) {
        m <- m$message
        if(grepl("Warning|Error", m)){
          m_ <- paste0("<span style='color:red;'>", m, "</span><br>")
        }
        else{
          m_ <- paste(m, "<br>")
        }
        shinyjs::html(id = "diag_readinter", html = m_, add = TRUE)
      }
      )

      return(df)
    }
    else if(input$noise_datatype == "def"){
      return(NULL)
    }
  })


  observeEvent(input$tmtinter_example,{
    showModal(tags$div(id="modal1_exampletmtinter", modalDialog(
      shiny::HTML("<h1>PDF file example</h1><br>
                   <br><img src='example_TMT_pdf.png' alt='pdf example', width='720' height='1080'><br>
                   <br>As you can see you can upload any pdf from ThermoFisher's product data sheet containing the
                   TMT channel leakage data. Note that this pdf should contains the data for all channel; so
                   if you pplan to use TMT18, this pdf should contain 18 lines. If you can't find such pdf, you'll have
                   to write the data in an xlsx, csv or txt (tab delimited) file. To gain time, you can upload the
                   TMT16 data sheet, download the obtained file and add the last two lines containing the data for the
                   TMT18 channels.<br>
                   <br><h1>Tab delimited file examples</h1><br>
                   <img src='example_TMT_xlsx.png' alt='pdf example', width='1280' height='360'><br>
                   From these examples, you can see that the column names does not matter. However, it should necessarily
                   contain 3 columns, one containing the TMT tags (either 10, 11, 16 or 18) and the C leakage -1 and +1.
                   These leakage data have to be in percentage and can be written in the three manner depicted in the
                   example above.<br>
                   Although the example show xlsx files, you can upload as well csv and txt (tab delimited) files that
                   also contains these columns.<br>
                    "),
      footer = NULL,
      easyClose = TRUE
    )))
  })

  observeEvent(input$own_tmtinter, {
    if(!is.null(uploaded_tmtinter())){
      showModal(tags$div(id="modal2_tmtinter", modalDialog(
        DT::renderDataTable({DT::datatable(uploaded_tmtinter(),
                                           caption = htmltools::tags$caption(
                                             style = 'caption-side: top; text-align: left;',
                                             htmltools::strong("Uploaded TMT interference")
                                           ),
                                           rownames = FALSE,
                                           options = list(lengthMenu = c(10,20,30), pageLength = 10,
                                                          scrollX = TRUE))
        }),
        fluidRow(column(3, downloadButton("down_TMTinterference", "Download data", class = "btn-primary btn-lg")),
                 column(3, selectInput("tmtinterference_format", "Select a format", choices = c("xlsx", "csv", "txt"), selected = "xlsx"))
                 ),
        footer = NULL,
        easyClose = TRUE
      )))
    }
    else{
      showNotification("The data are currently NULL, try to refresh.", type = "error")
    }
  })

  output$down_TMTinterference <- downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%y%m%d_%H%M_"), "TMT", nrow(uploaded_tmtinter()), "_interference.", input$tmtinterference_format)
    },
    content = function(file){
      if(input$tmtinterference_format == "xlsx"){
        openxlsx::write.xlsx(uploaded_tmtinter(), file)
      }
      else if(input$tmtinterference_format == "csv"){
        readr::write_csv(uploaded_tmtinter(), file, progress = FALSE)
      }
      else if(input$tmtinterference_format == "txt"){
        readr::write_tsv(uploaded_tmtinter(), file, progress = FALSE)
      }
    }
  )

  ### handling numbers of channels
  output$diff_nb_rep_ui <- renderUI({
    m_rep <- matrix(1, ncol = 1, nrow = input$nb_cond, byrow = TRUE)
    colnames(m_rep) <- "Nb replicate"
    rownames(m_rep) <- paste("Condition", 1:input$nb_cond)

    matrixInput("diff_nb_rep", "Type the number of replicate per condition",
                value = m_rep,
                rows = list(names = TRUE), cols = list(names = TRUE)
                )
  })
  observe({
    m_rep <- matrix(1, ncol = 1, nrow = input$nb_cond, byrow = TRUE)
    colnames(m_rep) <- "Nb replicate"
    rownames(m_rep) <- paste("Condition", 1:input$nb_cond)

    updateMatrixInput(session, "diff_nb_rep", value = m_rep)
  })

  nbchan_react <- reactive({
    if(input$noise_datatype == "own"){
      if(!is.null(uploaded_tmtinter())){
        return(as.character(nrow(uploaded_tmtinter())))
      }
      else{
        return(input$nb_channels)
      }
    }
    else{
      return(input$nb_channels)
    }
  })

  nbrep_matrix_check <- reactiveValues(
    x = TRUE,
    t = NULL
  )
  observe({
    mat_nbrep <- input$diff_nb_rep[,1]
    mat_nbrep <- as.numeric(mat_nbrep)
    if(length(mat_nbrep)){
      if(any(is.na(mat_nbrep))){
        nbrep_matrix_check$x <- FALSE
        nbrep_matrix_check$t <- "No character are allowed !"
      }
      else if(any(mat_nbrep <= 0)){
        nbrep_matrix_check$x <- FALSE
        nbrep_matrix_check$t <- "Only positive non null value are allowed !"
      }
      else if(sum(mat_nbrep) > as.numeric(nbchan_react())){
        nbrep_matrix_check$x <- FALSE
        nbrep_matrix_check$t <- "The sum of replicates is greater than the number of channels !"
      }
      else{
        nbrep_matrix_check$x <- TRUE
        nbrep_matrix_check$t <- NULL
      }
    }
  })

  output$nbrep_matrix_check <- renderText({
    paste0("<span style='color:red;'>", nbrep_matrix_check$t, "</span><br>")
  })


  observe({
    if(input$same_nb_rep | !nbrep_matrix_check$x){
      max_cond <- as.numeric(nbchan_react())%/%input$nb_rep
    }
    else{
      max_cond <- as.numeric(nbchan_react())
    }
    max_rep <- as.numeric(nbchan_react())%/%input$nb_cond

    updateNumericInput(session, "nb_cond", max = max_cond)
    updateNumericInput(session, "nb_rep", max = max_rep)

    if(!is.na(input$nb_rep) & !is.na(input$nb_cond)){
      if(input$nb_rep > max_rep){
        updateNumericInput(session, "nb_rep", value = max_rep)
      }
      if(input$nb_cond > max_cond){
        updateNumericInput(session, "nb_cond", value = max_cond)
      }
    }

    if(input$same_nb_rep | !nbrep_matrix_check$x){
      nb_mix <- as.numeric(nbchan_react()) - input$nb_rep*input$nb_cond
    }
    else{
      nb_mix <- as.numeric(nbchan_react()) - sum(as.numeric(input$diff_nb_rep[,1]))
    }

    if(!is.na(nb_mix)){
      updateSelectInput(session, "nb_mix", choices = 0:nb_mix)
    }
  })

  output$is_mix <- reactive({
    if(input$same_nb_rep | !nbrep_matrix_check$x){
      nb <- input$nb_rep*input$nb_cond
    }
    else{
      nb <- sum(as.numeric(input$diff_nb_rep[,1]))
    }

    nb <- nb < as.numeric(nbchan_react())
    if(!nb){
      updateSelectInput(session, "nb_mix", selected = 0)
    }
    return(nb)
  })
  outputOptions(output, "is_mix", suspendWhenHidden = FALSE)



  ### compute and plot optimal design
  output$is_already_compted <- reactive({
    opt <- FALSE
    if(input$nb_mix == "0" & input$same_nb_rep){
      if(input$nb_cond == 3 & input$nb_rep == 3){
        if(nbchan_react() == "10" | nbchan_react() == "11"){
          opt <- TRUE
        }
      }
      else if(input$nb_cond == 2 & input$nb_rep == 5){
        if(nbchan_react() == "10"){
          opt <- TRUE
        }
      }
      else if(input$nb_cond == 4 & input$nb_rep == 4){
        if(nbchan_react() == "16" | nbchan_react() == "18"){
          opt <- TRUE
        }
      }
      else if(input$nb_cond == 5 & input$nb_rep == 3){
        if(nbchan_react() == "16" | nbchan_react() == "18"){
          opt <- TRUE
        }
      }
    }

    if(opt){
      updateActionButton(session, "plot_opt", label = "Plot optimal design")
    }
    else {
      updateActionButton(session, "plot_opt", label = "Compute and plot optimal design")
    }

    return(opt)
  })
  outputOptions(output, "is_already_compted", suspendWhenHidden = FALSE)

  observe({
    if(nbchan_react() == "16" | nbchan_react() == "18"){
      updateCheckboxInput(session, "get_exact", value = FALSE)
    }
  })

  opt_design <- reactiveValues(
    plt = NULL,
    des = NULL
  )
  opt_design_ <- reactive({
    TMTion <- list("10" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131"),
                   "11" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C"),
                   "16" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N"),
                   "18" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N", "134C", "135N"),
                   "32" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N",
                            "127D", "128ND", "128CD", "129ND", "129CD", "130ND", "130CD", "131ND", "131CD", "132ND", "132CD", "133ND", "133CD", "134ND", "134CD", "135ND"),
                   "35" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N", "134C", "135N",
                            "127D", "128ND", "128CD", "129ND", "129CD", "130ND", "130CD", "131ND", "131CD", "132ND", "132CD", "133ND", "133CD", "134ND", "134CD", "135ND", "135CD")
                   )

    design <- NULL

    if(input$nb_mix == "0" & input$same_nb_rep){
      if(input$nb_cond == 3 & input$nb_rep == 3){
        if(nbchan_react() == "10"){
          design <- c(rep(1:2, 3), NA, rep(3, 3))
        }
        else if(nbchan_react() == "11"){
          design <- c(rep(1:2, 3), NA, NA, rep(3, 3))
        }
      }
      else if(input$nb_cond == 2 & input$nb_rep == 5){
        if(nbchan_react() == "10"){
          design <- rep(1:2, 5)
        }
      }
      else if(input$nb_cond == 4 & input$nb_rep == 4){
        if(nbchan_react() == "16"){
          design <- c(rep(1:2, 4), rep(3:4, 4))
        }
        else if(nbchan_react() == "18"){
          design <- c(rep(1:2, 4), NA, NA, rep(3:4, 4))
        }
      }
      else if(input$nb_cond == 5 & input$nb_rep == 3){
        if(nbchan_react() == "16"){
          design <- c(rep(1:2, 3), 3, NA, rep(3:4, 2), 5, 4, 5, 5)
        }
        else if(nbchan_react() == "18"){
          design <- c(rep(1, 3), NA, NA, rep(2:3, 3), NA, rep(4:5, 3))
        }
      }
    }

    if(is.null(design)){
      withCallingHandlers({
        shinyjs::html("diag_opt", "")
        if(input$noise_datatype == "own"){
          tmtinter <- uploaded_tmtinter()
        }
        else{
          tmtinter <- NULL
        }

        if(input$same_nb_rep | !nbrep_matrix_check$x){
          replicates <- input$nb_rep
        }
        else{
          replicates <- as.numeric(input$diff_nb_rep[,1])
        }

        design <- tmt_optimal(nbchan_react(), ncond = input$nb_cond,
                              tmt_correction = tmtinter,
                              rep = replicates, nmix = as.numeric(input$nb_mix),
                              maxiter = input$nb_iter, exact = input$get_exact)
      },
      message = function(m) {
        shinyjs::html(id = "diag_opt", html = paste(m$message, "<br>", sep = ""), add = FALSE)
        }
      )

      design <- unlist(c(design[nrow(design),]))[-ncol(design)]
    }

    names(design) <- TMTion[[nbchan_react()]]

    design
  })
  observeEvent(input$plot_opt, {
    if(input$noise_datatype == "own"){
      tmtinter <- uploaded_tmtinter()
    }
    else{
      tmtinter <- NULL
    }
    opt_design$plt <- draw_design(opt_design_(), tmtinter)
    opt_design$des <- opt_design_()
  })
  output$opt_des <- renderPlot({
    opt_design$plt
  })

  output$opt_des_up <- reactive({
    return(!is.null(opt_design$plt))
  })
  outputOptions(output, "opt_des_up", suspendWhenHidden = FALSE)

  output$down_design <- downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%y%m%d_%H%M_"), "optimal_design.png")
    },
    content = function(file){
      ggsave(file, plot = opt_design$plt, device = "png", units = "in",
             height = 2.5, width = 10)
    }
  )

  output$down_designTab <- downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%y%m%d_%H%M_"), "possible_optimal_designs.xlsx")
    },
    content = function(file){
      withCallingHandlers({
        shinyjs::html("diag_downdesTab", "")
        message("Computing possible designs...")
        d <- opt_design$des
        d <- possible_designs(d)
        openxlsx::write.xlsx(d, file)
        message("")
      },
      message = function(m) {
        shinyjs::html(id = "diag_downdesTab", html = paste(m$message, "<br>", sep = ""), add = FALSE)
      })
    }
  )

  ### plot and compute noise of design
  output$cond_name_ui <- renderUI({
    m_cond <- matrix(LETTERS[1:input$nb_cond], nrow = 1, ncol = input$nb_cond)
    colnames(m_cond) <- paste("Condition", 1:input$nb_cond)
    m <- m_cond

    if(input$nb_mix != 0){
      if(input$nb_mix == 1){
        m_mix <- matrix("Mix", nrow = 1, ncol = 1)
        colnames(m_mix) <- "Mix channel"
      }
      else{
        m_mix <- matrix(paste("Mix", 1:input$nb_mix), nrow = 1, ncol = as.numeric(input$nb_mix))
        colnames(m_mix) <- paste("Mix channel", 1:as.numeric(input$nb_mix))
      }
      m <- cbind(m, m_mix)
    }

    matrixInput("cond_name", "Type names for your conditions (optional)",
                value = m,
                rows = list(names = FALSE),
                cols = list(names = TRUE)
    )
  })


  ### way to select design
  output$design_table <- renderTable({
    TMTion <- list("10" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131"),
                   "11" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C"),
                   "16" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N"),
                   "18" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N", "134C", "135N"),
                   "32" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N"),
                   "35" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N", "134C", "135N")
                   )
    nb_chan <- as.numeric(nbchan_react())

    all_cond <- input$cond_name[1,]
    if(input$same_nb_rep | !nbrep_matrix_check$x){
      does_miss <- nb_chan - (input$nb_cond*input$nb_rep + as.numeric(input$nb_mix))
    }
    else{
      does_miss <- nb_chan - (sum(as.numeric(input$diff_nb_rep[,1])) + as.numeric(input$nb_mix))
    }

    if(does_miss > 0){
      all_cond <- c(all_cond, "NA")
    }

    if(nb_chan == 32){
      nb_chan <- 16
    }
    else if(nb_chan == 35){
      nb_chan <- 18
    }

    channels <- paste0("<select id='chan_", 1:nb_chan,
                       "' style='width: 100%'>",
                       paste0("<option value='", all_cond, "'>", all_cond, "</option>", collapse = " "),
                       "</select>"
                       )
    count = 1
    count_rep = 1
    for(i in 1:nb_chan){
      if(!input$same_nb_rep &  nbrep_matrix_check$x){
        if(i > sum(as.numeric(input$diff_nb_rep[,1])[1:ifelse(count_rep > input$nb_cond, input$nb_cond, count_rep)], na.rm = TRUE)){
          count = count + 1
          count_rep = count_rep + 1
        }
      }

      channels[i] <- sub(paste0(">", all_cond[count], "</option"),
                         paste0(" selected>", all_cond[count], "</option"),
                         channels[i])

      if(input$same_nb_rep | !nbrep_matrix_check$x){
        if(i %% input$nb_rep == 0){
          count = count + 1
        }
      }
    }

    channels <- t(data.frame(channels))
    colnames(channels) <- TMTion[[nbchan_react()]]

    # if computed opt design, change default selection to it
    if(!is.null(opt_design$des)){
      if(as.numeric(nbchan_react()) == length(opt_design$des)){
        if(as.numeric(nbchan_react()) <= 18){
          name_chan <- names(opt_design$des)
        }
        else{
          name_chan <- TMTion[[nbchan_react()]]
        }

        channels <- mapply(function(ch, sel){
          sel <- sub(" selected", "", sel)

          if(!is.na(opt_design$des[[ch]])){
            if(opt_design$des[[ch]] == "Mix"){
              if(sum(grepl("Mix", opt_design$des)) == 1){
                cd <- input$cond_name[[grep(paste0("^Mix channel$"), colnames(input$cond_name))]]
              }
              else{
                # get order of mix channels
                cd <- which(opt_design$des == "Mix")
                cd <- sapply(1:length(cd), function(x){
                  y <- cd[x]
                  y[[1]] <- x;
                  y
                })
                cd <- input$cond_name[[grep(paste0("^Mix channel ", cd[[ch]], "$"), colnames(input$cond_name))]]
              }
              sel <- sub(paste0(" value='", cd, "'>"),
                         paste0(" value='", cd, "' selected>"),
                         sel)
            }
            else{
              cd <- input$cond_name[[grep(paste0("^Condition ", opt_design$des[[ch]], "$"), colnames(input$cond_name))]]

              sel <- sub(paste0(" value='", cd, "'>"),
                         paste0(" value='", cd, "' selected>"),
                         sel)
            }
          }
          else{
            sel <- sub(paste0(" value='NA'>"),
                       paste0(" value='NA' selected>"),
                       sel)
          }

          return(sel)
        },
        name_chan, channels)

        channels <- t(data.frame(channels))
        colnames(channels) <- TMTion[[nbchan_react()]]
      }
    }

    channels
  },
  sanitize.text.function = function(x) x)

  output$tmt_greater18 <- reactive({
    return(as.numeric(nbchan_react()) > 18)
  })
  outputOptions(output, "tmt_greater18", suspendWhenHidden = FALSE)

  output$design_table_32 <- renderTable({
    TMTion <- list("32" = c("127D", "128ND", "128CD", "129ND", "129CD", "130ND", "130CD", "131ND", "131CD", "132ND", "132CD", "133ND", "133CD", "134ND", "134CD", "135ND"),
                   "35" = c("127D", "128ND", "128CD", "129ND", "129CD", "130ND", "130CD", "131ND", "131CD", "132ND", "132CD", "133ND", "133CD", "134ND", "134CD", "135ND", "135CD")
    )
    nb_chan <- as.numeric(nbchan_react())

    if(nb_chan <= 18){
      return(NULL)
    }
    else{
      all_cond <- input$cond_name[1,]
      if(input$same_nb_rep | !nbrep_matrix_check$x){
        does_miss <- nb_chan - (input$nb_cond*input$nb_rep + as.numeric(input$nb_mix))
      }
      else{
        does_miss <- nb_chan - (sum(as.numeric(input$diff_nb_rep[,1])) + as.numeric(input$nb_mix))
      }

      if(does_miss > 0){
        all_cond <- c(all_cond, "NA")
      }

      nb_chan_start <- ifelse(nb_chan == 32, 17, 19)
      channels <- paste0("<select id='chan_", nb_chan_start:nb_chan,
                         "' style='width: 100%'>",
                         paste0("<option value='", all_cond, "'>", all_cond, "</option>", collapse = " "),
                         "</select>"
      )
      count = 1
      count_rep = 1
      for(i in 1:(length(TMTion[[nbchan_react()]]))){
        if(!input$same_nb_rep &  nbrep_matrix_check$x){
          if(i > sum(as.numeric(input$diff_nb_rep[,1])[1:ifelse(count_rep > input$nb_cond, input$nb_cond, count_rep)], na.rm = TRUE)){
            count = count + 1
            count_rep = count_rep + 1
          }
        }

        channels[i] <- sub(paste0(">", all_cond[count], "</option"),
                           paste0(" selected>", all_cond[count], "</option"),
                           channels[i])

        if(input$same_nb_rep | !nbrep_matrix_check$x){
          if(i %% input$nb_rep == 0){
            count = count + 1
          }
        }
      }

      channels <- t(data.frame(channels))
      colnames(channels) <- TMTion[[nbchan_react()]]

      # if computed opt design, change default selection to it
      if(!is.null(opt_design$des)){
        if(as.numeric(nbchan_react()) == length(opt_design$des)){

          channels <- mapply(function(ch, sel){
            sel <- sub(" selected", "", sel)

            if(!is.na(opt_design$des[[ch]])){
              if(opt_design$des[[ch]] == "Mix"){
                if(sum(grepl("Mix", opt_design$des)) == 1){
                  cd <- input$cond_name[[grep(paste0("^Mix channel$"), colnames(input$cond_name))]]
                }
                else{
                  # get order of mix channels
                  cd <- which(opt_design$des == "Mix")
                  cd <- sapply(1:length(cd), function(x){
                    y <- cd[x]
                    y[[1]] <- x;
                    y
                  })
                  cd <- input$cond_name[[grep(paste0("^Mix channel ", cd[[ch]], "$"), colnames(input$cond_name))]]
                }
                sel <- sub(paste0(" value='", cd, "'>"),
                           paste0(" value='", cd, "' selected>"),
                           sel)
              }
              else{
                cd <- input$cond_name[[grep(paste0("^Condition ", opt_design$des[[ch]], "$"), colnames(input$cond_name))]]

                sel <- sub(paste0(" value='", cd, "'>"),
                           paste0(" value='", cd, "' selected>"),
                           sel)
              }
            }
            else{
              sel <- sub(paste0(" value='NA'>"),
                         paste0(" value='NA' selected>"),
                         sel)
            }

            return(sel)
          },
          TMTion[[nbchan_react()]], channels)

          channels <- t(data.frame(channels))
          colnames(channels) <- TMTion[[nbchan_react()]]
        }
      }

      return(channels)
    }
  },
  sanitize.text.function = function(x) x)

  own_design <- reactiveValues(
    d = NULL,
    is_ok = FALSE
  )
  observe({
    TMTion <- list("10" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131"),
                   "11" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C"),
                   "16" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N"),
                   "18" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N", "134C", "135N"),
                   "32" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N",
                            "127D", "128ND", "128CD", "129ND", "129CD", "130ND", "130CD", "131ND", "131CD", "132ND", "132CD", "133ND", "133CD", "134ND", "134CD", "135ND"),
                   "35" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N", "134C", "135N",
                            "127D", "128ND", "128CD", "129ND", "129CD", "130ND", "130CD", "131ND", "131CD", "132ND", "132CD", "133ND", "133CD", "134ND", "134CD", "135ND", "135CD")
                   )
    nb_chan <- as.numeric(nbchan_react())

    design <- reactiveValuesToList(input)
    design <- design[paste0("chan_", 1:nb_chan)]
    if(length(design)){
      names(design) <- TMTion[[nbchan_react()]]
      design <- unlist(design)

      own_design$d <- design

      if(ifelse(input$same_nb_rep | !nbrep_matrix_check$x, input$nb_cond*input$nb_rep, sum(as.numeric(input$diff_nb_rep[,1]))) < nb_chan){
        if(input$nb_mix != "0"){
          mix_name <- as.character(input$cond_name[,grep("Mix",
                                                         colnames(input$cond_name),
                                                         value = TRUE)])

          design <- design[-grep(paste0("^", mix_name, "$", collapse = "|"), design)]
        }

        if(ifelse(input$same_nb_rep | !nbrep_matrix_check$x, input$nb_cond*input$nb_rep, sum(as.numeric(input$diff_nb_rep[,1]))) + as.numeric(input$nb_mix) < nb_chan){
          design <- design[-grep(paste0("^NA$"), design)]
        }
      }

      # make sure it's in the right order
      design <- table(design)[as.character(input$cond_name[,grep("Condition",
                                                                 colnames(input$cond_name),
                                                                 value = TRUE)])]
      design <- ifelse(input$same_nb_rep | !nbrep_matrix_check$x, all(design == input$nb_rep), all(design == as.numeric(input$diff_nb_rep[,1])))
      if(is.na(design)){
        design <- FALSE
      }
      own_design$is_ok <- design
    }
  })
  output$design_ok <- reactive({
    return(own_design$is_ok)
  })
  outputOptions(output, "design_ok", suspendWhenHidden = FALSE)

  output$warning_design <- renderText({
    if(own_design$is_ok){
      t <- ""
    }
    else{
      t <- paste0("<span style='color:red;'>",
                  "Your design doesn't the fit the number of replicates you selected !", "</span><br>")
    }
  })


  ## plot design using visnetwork
  own_design_ <- reactiveValues(
    x = NULL,
    nodes = NULL
  )
  nb_edges <- reactiveValues(
    x = 1
  )
  own_design_rec <- reactive({
    TMTion <- list("10" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131"),
                   "11" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C"),
                   "16" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N"),
                   "18" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N", "134C", "135N"),
                   "32" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N",
                            "127D", "128ND", "128CD", "129ND", "129CD", "130ND", "130CD", "131ND", "131CD", "132ND", "132CD", "133ND", "133CD", "134ND", "134CD", "135ND"),
                   "35" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N", "134C", "135N",
                            "127D", "128ND", "128CD", "129ND", "129CD", "130ND", "130CD", "131ND", "131CD", "132ND", "132CD", "133ND", "133CD", "134ND", "134CD", "135ND", "135CD")
                   )

    nb_chan <- as.numeric(nbchan_react())

    color = grDevices::colors()[grep('gr(a|e)y|white|black|wheat|beige|ivory', grDevices::colors(), invert = T)]
    color = color[c(1:18)*20 + 9]
    color = t(grDevices::col2rgb(color))
    color = apply(color, 1, function(x) grDevices::rgb(x[1], x[2], x[3], maxColorValue = 255))

    if(own_design$is_ok){
      labs_cond <- own_design$d

      nd_color <- rep("", length(labs_cond))

      if(any(grepl("^NA$", labs_cond))){
        cd <- unique(labs_cond[-which(labs_cond == "NA")])
        nd_color[which(labs_cond == "NA")] <- "#A5A3A3"
        ci = 1
        for(i in cd){
          nd_color[which(labs_cond == i)] <- color[ci]
          ci = ci + 1
        }
      }
      else{
        cd <- unique(labs_cond)
        ci = 1
        for(i in cd){
          nd_color[which(labs_cond == i)] <- color[ci]
          ci = ci + 1
        }
      }
    }
    else{
      if(input$same_nb_rep | !nbrep_matrix_check$x){
        nbrep <- input$nb_rep
      }
      else{
        nbrep <- as.numeric(input$diff_nb_rep[,1])
      }
      labs_cond <-  unlist(mapply(rep, as.character(input$cond_name[,grep("Condition",
                                                                          colnames(input$cond_name),
                                                                          value = TRUE)]),
                                  nbrep, SIMPLIFY = FALSE)
                           )
      labs_cond <- c(labs_cond,
                     as.character(input$cond_name[,grep("Mix",
                                                        colnames(input$cond_name),
                                                        value = TRUE)])
                     )

      does_miss <- nb_chan - length(labs_cond)

      nd_color <- rep("", length(labs_cond))

      ci = 1
      for(i in unique(labs_cond)){
        nd_color[which(labs_cond == i)] <- color[ci]
        ci = ci + 1
      }

      if(does_miss > 0){
        labs_cond <- c(labs_cond, rep("NA", does_miss))
        nd_color[which(labs_cond == "NA")] <- "#A5A3A3"
      }
    }

    min_dist <- max(stringr::str_length(labs_cond))*20 + nb_chan*40
    good_font_size <- optTMT:::norm_font_size(labs_cond)


    nodes <- data.frame(id = 1:(2*nb_chan),
                        label = c(TMTion[[nbchan_react()]],
                                  labs_cond),
                        shape = c(rep("dot", nb_chan), rep("circle", nb_chan)),
                        y = 1,
                        x = rep(seq(-min_dist, min_dist, (min_dist*2)/(nb_chan - 1)), 2),
                        color = c(rep("#FFFFFF00", nb_chan), nd_color),
                        font.size = c(rep(max(good_font_size) - 4, nb_chan),
                                      good_font_size)
    )

    # get design to compute interferences
    design_info <- nodes %>% dplyr::group_by(x) %>%
      dplyr::summarise(channel = label[1], cond = label[2])
    design <- design_info %>%
      dplyr::select(-x) %>%
      as.data.frame()
    rownames(design) <- design$channel
    design <- t(design)["cond",]
    if(any(design == "NA")){
      design[design == "NA"] <- NA
    }

    mix_name <- as.character(input$cond_name[,grep("Mix",
                                                   colnames(input$cond_name),
                                                   value = TRUE)])
    if(length(mix_name)){
      design[which(design %in% mix_name)] <- paste0("Mix", design[which(design %in% mix_name)])
    }

    if(input$noise_datatype == "own"){
      tmtinter <- uploaded_tmtinter()
    }
    else{
      tmtinter <- NULL
    }
    noise <- tmt_interference_noise(design, tmtinter)
    edges <- tmt_interference_links(design, tmtinter)

    if(!is.null(edges)){
      edges_info <-  nodes[(nb_chan + 1):(2*nb_chan), c("id", "label", "x")]
      edges_info <- merge(design_info, edges_info,
                          by = "x")
      edges_info <- edges_info[,c("channel", "id")]

      edges <- merge(merge(edges, edges_info,
                           by.x = "from", by.y = "channel"
      ),
      edges_info,
      by.x = "to", by.y = "channel"
      )
      edges <- edges[,3:4]
      colnames(edges) <- c("from", "to")

      edges$color <- "#FF000091"
      edges$arrows <- "to"
      edges$id <- 1:nrow(edges)

      nb_edges$x <- nrow(edges)
    }
    else{
      nb_edges$x <- 0
    }

    net_vis <- visNetwork(nodes, edges, width = "100%",
                          main =  paste0("Noise of ", noise, "%")) %>%
      visNodes(fixed = TRUE) %>%
      visEdges(smooth = list(enabled = TRUE, type = "curvedCW", roundness = 0.6)) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visExport(type = "png", name = paste0(format(Sys.time(), "%y%m%d_%H%M_"), "TMTdesign"),
                float = "left", label = paste("Save network as", 'png'),
                background = "#FFFFFF00", style= "")

    return(list("nodes" = nodes, "visNetwork" = net_vis))
  })
  observeEvent(input$plot_design, {
    own_design_$x <- own_design_rec()[["visNetwork"]]
    own_design_$nodes <- own_design_rec()[["nodes"]]
  })
  output$own_design <- renderVisNetwork({
    own_design_$x
  })

  output$design_ready <- reactive({
    return(!is.null(own_design_$x))
  })
  outputOptions(output, "design_ready", suspendWhenHidden = FALSE)


  output$down_designTabNamed <- downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%y%m%d_%H%M_"), "possible_named_designs.xlsx")
    },
    content = function(file){
      withCallingHandlers({
        shinyjs::html("diag_downdesTabNamed", "")
        message("Computing possible designs...")
        d <- own_design$d
        d <- possible_designs(d)
        openxlsx::write.xlsx(d, file)
        message("")
      },
      message = function(m) {
        shinyjs::html(id = "diag_downdesTabNamed", html = paste(m$message, "<br>", sep = ""), add = FALSE)
      })
    }
  )


  ## all possible swaps
  observe({
    TMTion <- list("10" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131"),
                   "11" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C"),
                   "16" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N"),
                   "18" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N", "134C", "135N"),
                   "32" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N",
                            "127D", "128ND", "128CD", "129ND", "129CD", "130ND", "130CD", "131ND", "131CD", "132ND", "132CD", "133ND", "133CD", "134ND", "134CD", "135ND"),
                   "35" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N", "134C", "135N",
                            "127D", "128ND", "128CD", "129ND", "129CD", "130ND", "130CD", "131ND", "131CD", "132ND", "132CD", "133ND", "133CD", "134ND", "134CD", "135ND", "135CD")
                   )
    vec <- TMTion[[nbchan_react()]]
    all_swaps <- c()
    for(i in 1:(length(vec) - 1)){
      all_swaps <- c(all_swaps, paste(vec[i], "to", vec[(i+1):length(vec)]))
    }
    updateSelectInput(session, "swap", choices = all_swaps)
  })


  ### compute interactive swaping
  observeEvent(input$go_swap, {
    nb_chan <- as.numeric(nbchan_react())

    if(!is.null(input$own_design_nodes)){
      nodes_info <- input$own_design_nodes
      nodes_info <- lapply(input$own_design_nodes, unlist)
      nodes_info <- as.data.frame(Reduce(rbind, nodes_info))
      rownames(nodes_info) <- NULL

      if(any(!(nodes_info$label %in% own_design_$nodes$label))){
        nodes_info <- own_design_$nodes
      }
      else{
        nodes_info$id <- as.numeric(nodes_info$id)
        nodes_info$x <- as.numeric(nodes_info$x)
        nodes_info$y <- as.numeric(nodes_info$y)
      }
    }
    else{
      nodes_info <- own_design_$nodes
    }

    if(!is.null(nodes_info) & !is.null(input$swap)){
      for(s in input$swap){
        ids <- strsplit(s, " to ")[[1]]

        id1 <- nodes_info$id[which(nodes_info$label == ids[1])] + nb_chan
        id2 <- nodes_info$id[which(nodes_info$label == ids[2])] + nb_chan

        nodes_info$label <- optTMT:::swapPositions(nodes_info$label, id1, id2)
        nodes_info$color <- optTMT:::swapPositions(nodes_info$color, id1, id2)
        nodes_info$font.size <- as.numeric(optTMT:::swapPositions(nodes_info$font.size, id1, id2))

        visNetworkProxy("own_design") %>%
          visUpdateNodes(nodes = nodes_info, updateOptions = FALSE)
      }

      design_info <- nodes_info[,c("label", "x")]
      # visnetwork change coordinate and can't have addcoordinates set to TRUE since it doesn't continue dot modify so set manually good factors
      design_info$x[(nb_chan + 1):(nb_chan*2)] <- sapply(design_info$x[(nb_chan + 1):(nb_chan*2)],
                              function(x){
                                if(!(x %in% design_info$x[1:nb_chan])){
                                  x <- design_info$x[1:nb_chan][which.min((design_info$x[1:nb_chan] - x)**2)]
                                };
                                x
                              })
      design <- design_info %>%
        dplyr::group_by(x) %>%
        dplyr::summarise(channel = label[1], cond = label[2]) %>%
        dplyr::select(-x) %>%
        as.data.frame()
      rownames(design) <- design$channel
      design <- t(design)["cond",]
      if(any(design == "NA")){
        design[design == "NA"] <- NA
      }

      if(input$noise_datatype == "own"){
        tmtinter <- uploaded_tmtinter()
      }
      else{
        tmtinter <- NULL
      }
      noise <- tmt_interference_noise(design, tmtinter)
      edges <- tmt_interference_links(design, tmtinter)

      if(!is.null(edges)){
        design_info <- merge(data.frame(channel = design_info$label[1:nb_chan],
                                        x = design_info$x[1:nb_chan]
                                        ),
                             data.frame(cond = design_info$label[(nb_chan + 1):(nb_chan*2)],
                                        x = design_info$x[(nb_chan + 1):(nb_chan*2)]
                                        )
                             )

        edges_info <-  nodes_info[(nb_chan + 1):(nb_chan*2), c("id", "label", "x")]
        edges_info$x <- sapply(edges_info$x,
                               function(x){
                                 if(!(x %in% design_info$x)){
                                   x <- design_info$x[which.min((design_info$x - x)**2)]
                                 };
                                 x
                               })
        edges_info <- merge(design_info, edges_info,
                            by = "x")
        edges_info <- edges_info[,c("channel", "id")]

        edges <- merge(merge(edges, edges_info,
                             by.x = "from", by.y = "channel"
                             ),
                       edges_info,
                       by.x = "to", by.y = "channel"
                       )
        edges <- edges[,3:4]
        colnames(edges) <- c("from", "to")
        edges$color <- "#FF000091"
        edges$arrows <- "to"
        edges$id <- 1:nrow(edges)
      }

      # update design visualization
      if(!is.null(nb_edges$x)){
        visNetworkProxy("own_design") %>%
          visRemoveEdges(id = 1:nb_edges$x)
      }
      visNetworkProxy("own_design") %>%
        visUpdateEdges(edges = edges) %>%
        visEdges(smooth = list(enabled = TRUE, type = "curvedCW", roundness = 0.6))
      visNetworkProxy("own_design") %>%
        visSetTitle(main = list(text = paste0("Noise of ", noise, "%")))

      nb_edges$x <- nrow(edges)
    }

    visNetworkProxy("own_design") %>%
      visGetNodes()
  })

  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)
