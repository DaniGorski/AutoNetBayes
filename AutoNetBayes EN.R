###########################################################################################################
#       Elaborated by: Daniela Gorski  Corrected and reviewed by: Fernanda Tonin and Eric Domingos        #
###########################################################################################################
# Guidelines for the Code:                                                                                #
#                                                                                                         #
#   *To use this code, you need to install JAGS, which can be found on the website:                       #
#   https://sourceforge.net/projects/mcmc-jags/files/.                                                    #
#                                                                                                         #
#   *For the first run, use the “# import AutoNetBayes” step. If a new run is required, use               #
#   the “# run AutoNetBayes” step.                                                                        #
#                                                                                                         #
#   *Import data from a CSV or XLSX file in the patterns described for the code that contains             #
#   the minimum requirements for the analysis (study, treatment, sampleSize, and data on  the             #
#   outcome to be analyzed).                                                                              #
#                                                                                                         #
#   *For cross-over studies, name the different periods with equal numbers followed by different          #
#    letters (e.g., 1A and 1B).                                                                           #
#                                                                                                         #
#   *Each result is displayed in a specific tab. After each analysis, the user must manually              # 
#   select the tab corresponding to the desired result. The convergence and Gelman-Rubin  plots           #
#   may take a few seconds to display after the initial selection of the appropriate tab.                 #
#                                                                                                         #
#   *This code performs a Bayesian network meta-analysis without the use of priors.                       #
#                                                                                                         #
#   *cite: package gemtc (DOI: 10.32614/CRAN.package.gemtc), package (DOI:10.32614/CRAN.package.network)  #
#    and geometric analysis of the network (DOI: 10.1371/journal.pone.0212650).                           #
###########################################################################################################
a
# import AutoNetBayes --------------------------------------------------
{packages <- c("shinyjs", "shiny", "rio", "gemtc", "dplyr", "purrr", "tidyr", "openxlsx",
               "rsvg", "svglite", "network", "ggplot2", "stringr", "igraph",
               "DT", "tibble", "rjags")
for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  } else {
    library(package, character.only = TRUE)
  }
}

rename_columns_ui <- function(missing_columns, colnames_data) {
  lapply(missing_columns, function(column) {
    selectInput(paste0("column_", column), 
                label = paste("select the column for", 
                              column),
                choices = colnames_data) 
  })
}

ui <- fluidPage(
  useShinyjs(),
  titlePanel("AutoNetBayes"),
  tags$head(
    tags$style(HTML("
      /* General fund*/
      body {
        background-color: #d9ecf2 !important;  /* Light blue */
        color: #000000 !important;  /* black text */
      }

      /* Sidebar */
      #sidebar {
        height: 85vh;  
        overflow-y: auto;
        background-color: #aacbe3 !important;  /* dark blue */
        color: black !important;
        font-size: 14px;
        padding: 10px;
        border-radius: 8px;
      }

      /* Main painel */
      #main-panel {
        height: 85vh;
        overflow-y: auto;
        background-color: #ffffff !important;
        padding: 10px;
        border-radius: 8px;
        box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.1);
      }
      
      /* Internal panels */
      .panel {
        background-color: #ffffff !important;  
        border: 1px solid #88aacc !important;
        border-radius: 8px;
        box-shadow: 2px 2px 10px rgba(0,0,0,0.1);
      }

      .panel-heading {
        background-color: #88aacc !important;
        color: #000000 !important;
        font-weight: bold;
      }

      /* Buttons */
      .btn {
        background-color: #d6d6d6 !important;  /* Light gray */
        color: black !important;
        border-radius: 6px;
        border: none;
        padding: 8px 12px;
      }

      .btn:hover {
        background-color: #b0b0b0 !important;  /* Dark gray when hovering */
      }

      /* Style of the main panel tabs */
      .nav-tabs > li > a {
        background-color: #ffffff !important;  /* White background */
        color: black !important;
        border: 1px solid #88aacc !important;
      }

      /* Active tab in the main panel */
      .nav-tabs > li.active > a, 
      .nav-tabs > li.active > a:focus, 
      .nav-tabs > li.active > a:hover {
        background-color: #88aacc !important;  /* Dark blue */
        color: white !important;
        border: 1px solid #88aacc !important;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      id = "sidebar",  
      width = 3, 
      tags$div(
        class = "panel panel-default",
        tags$div(class = "panel-heading",
                 tags$h4(class = "panel-title",
                         tags$a("Importing Data", `data-toggle` = "collapse", href = "#collapseImporting")
                 )
        ),
        tags$div(id = "collapseImporting", class = "panel-collapse collapse",
                 tags$div(class = "panel-body",
                          actionButton("import", "Import File"),
                          selectInput("outcome", "Which outcome is analyzed?", 
                                      choices = c("dichotomous", "survival", "continuous")),
                          actionButton("process", "Checking the data"),
                          hr(),
                          uiOutput("columns_ui"), 
                          actionButton("confirm", "Rename", disabled = TRUE),
                          hr(),
                          actionButton("Check_connection", "Check network connection"),
                          textInput("name_outcome", "Name of the outcome being analyzed:", value = "outcome"),
                 )
        )
      ),
      tags$div(
        class = "panel panel-default",
        tags$div(class = "panel-heading",
                 tags$h4(class = "panel-title",
                         tags$a("Network graph", `data-toggle` = "collapse", href = "#collapseCustomization")
                 )
        ),
        tags$div(id = "collapseCustomization", class = "panel-collapse collapse",
                 tags$div(class = "panel-body",
                          selectInput("color_vertex", "Color of the nodes:",
                                      choices = c("Orange" = "orange", "Light blue" = "lightblue",
                                                  "Dark blue" = "darkblue", "Green" = "lightgreen",
                                                  "Red" = "red", "Gray" = "gray", "Purple" = "#553582",
                                                  "Black" = "black", "Yellow" = "yellow",
                                                  "Pink" = "lightpink", "Aqua Blue" = "#00ffff")),
                          
                          selectInput("edge_color", "Color of the edges:",
                                      choices = c("Black" = "black","Orange" = "orange", "Light blue" = "lightblue",
                                                  "Dark blue" = "darkblue", "Green" = "lightgreen",
                                                  "Red" = "red", "Gray" = "gray", "Purple" = "#553582",
                                                  "Yellow" = "yellow",
                                                  "Pink" = "lightpink", "Aqua Blue" = "#00ffff")),
                          
                          numericInput("label_cex", "Label size:", value = 1, min = 0.1, step = 0.1),
                          
                          selectInput("pos_label", "Label position:",
                                      choices = c("External" = 0, "Below" = 1, "Left" = 2,
                                                  "Above" = 3, "Right" = 4, "Center" = 5)),
                          
                          numericInput("limit_x", "Graph area size:", value = 1, min = 0.1, step = 0.1),
                          
                          checkboxInput("boxed_labels", "Use backgrounds on labels??", value = FALSE),
                          
                          conditionalPanel(
                            condition = "input.boxed_labels == true",
                            selectInput("cor_bg_label", "Background color of labels:",
                                        choices = c("Orange" = "orange", "Light blue" = "lightblue",
                                                    "Dark blue" = "darkblue", "Green" = "lightgreen",
                                                    "Red" = "red", "Gray" = "gray", "Purple" = "#553582",
                                                    "White" = "white", "Yellow" = "yellow",
                                                    "Pink" = "lightpink", "Aqua Blue" = "#00ffff")),
                            numericInput("label_pad", "Size of label background:", value = 0.5, min = 0.1, step = 0.1)
                          ),
                          hr(),
                          h4("Graph export"),
                          numericInput("export_width", "Width:", value = 4500, min = 100, step = 100),
                          numericInput("export_height", "Height:", value = 3000, min = 100, step = 100),
                          numericInput("export_res", "Resolution (DPI):", value = 300, min = 10, step = 10),
                          
                          downloadButton("export_svg", "Export as SVG"),
                          br(), br(),
                          downloadButton("export_tiff", "Export as TIFF"),
                          br(), br(),
                          actionButton("geo_analysis", "Geometric analysis"),
                          br(), br(),
                          downloadButton("export_geo", "Export geometric analysis (.xlsx)")
                 )
        )
      ),
      tags$div(
        class = "panel panel-default",
        tags$div(class = "panel-heading",
                 tags$h4(class = "panel-title",
                         tags$a("Analysis", `data-toggle` = "collapse", href = "#collapseModel")
                 )
        ),
        tags$div(id = "collapseModel", class = "panel-collapse collapse",
                 tags$div(class = "panel-body",
                          tags$h4("Configure model"),
                          selectInput("outcome_type", "Select the type of result:",
                                      choices = c("Odds Ratio", "Risk Ratio")), 
                          numericInput("adapt", "Number of Adaptations:", value = 1000, min = 1000),
                          numericInput("thin", "Thinning Factor:", value = 25, min = 5),
                          numericInput("iter", "Number of Iterations:", value = 5000, min = 1000),
                          actionButton("run_analysis", "Run analysis"),
                          hr(),
                          tags$h4("Export results"),
                          downloadButton("download_summary", "Export model summary to .txt"),
                          br(), br(),
                          downloadButton("download_plot", "Download Traceplot in TIFF"),
                          br(), br(),
                          downloadButton("download_densplot", "Download densplot in TIFF"),
                          br(), br(),
                          downloadButton("download_gelman", "Download Gelman-Rubin (.xlsx)"),
                          br(), br(),
                          downloadButton("download_gelman_plot", "Download Gelman-Rubin graphs in ZIP"),
                          br(), br(),
                          downloadButton("export_node", "Download node splitting results (.xlsx)"),
                          br(), br(),
                          downloadButton("download_tabela", "Download League table (.xlsx)"),
                          hr(),
                          tags$h4("SUCRA analysis"),
                          selectInput("direction_SUCRA", "Which direction of the outcome is analyzed?", 
                                      choices = c("Beneficial"=1, "Harmful"=-1)),
                          numericInput("text_size", "text size:", value = 22, min = 0.1, step = 0.1),
                          actionButton("run_SUCRA", "Generate SUCRA"),
                          br(), br(),
                          downloadButton("download_SUCRA_xlsx", "Download SUCRA (.xlsx)"),
                          br(), br(),
                          downloadButton("download_SUCRA_tiff", "Download SUCRA graph (.tiff)"),
                 )
        )
      ),
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data", 
                 DT::dataTableOutput("data_preview")),
        tabPanel("Network Graph", plotOutput("networkPlot")),
        tabPanel("Geometric Analysis", DT::dataTableOutput("data_geo")),
        tabPanel("Model", verbatimTextOutput("analysis_output")),
        tabPanel("Convergence", 
                 fluidRow(
                   column(12, plotOutput("trace_plot")),
                   column(12, plotOutput("dens_plot"))
                 )),
        tabPanel("Gelman-Rubin Plots", uiOutput("gelman_plots_ui")),
        tabPanel("Tabela Gelman-Rubin", tableOutput("gelman_table")),
        tabPanel("Node splitting",
                 div(id = "Node splitting", uiOutput("mensagem_node")), 
                 DTOutput("Data_node")
        ),
        tabPanel("League table",DTOutput("rend_league_table")),
        tabPanel("SUCRA plot",plotOutput("SUCRA_plot")),
        tabPanel("SUCRA table",dataTableOutput("SUCRA_table"))
      ),
      textOutput("status_output"),
      verbatimTextOutput("error_output")
    )
  )
)
define_required_columns <- function(outcome_options) {
  switch(outcome_options,
         "dichotomous" = c("study", "sampleSize", "treatment", "responders"),
         "survival" = c("study", "sampleSize", "treatment", "responders", "exposure"),
         "continuous" = c("study", "sampleSize", "treatment", "mean", "std.dev"))
}
rename_columns <- function(data, required_columns, new_name_columns) {
  for (i in 1:length(required_columns)) {
    new_name <- new_name_columns[[i]]
    if (!is.null(new_name) && new_name != "") {
      colnames(data)[colnames(data) == new_name] <- required_columns[i]
    }
  }
  return(data)
}

server <- function(input, output, session) {
  
  data <- reactiveVal(NULL)  
  
  observeEvent(input$import, {
    req(input$import)
    
    file_path <- tryCatch({
      file.choose()
    }, error = function(e) {
      showNotification("No files selected.", type = "warning")
      return(NULL)
    })
    
    req(file_path)
    
    imported_date <- tryCatch({
      import(file_path)
    }, error = function(e) {
      showNotification("Error uploading the file. Check the format.", type = "error")
      return(NULL)
    })
    
    req(imported_date)
    
    data(imported_date)
  })
  
  output$data_preview <- renderDT({
    req(data())
    datatable(data(),
              options = list(pageLength = 50, autoWidth = TRUE),
              rownames = FALSE) 
  })
  
  
  
  observeEvent(input$process, {
    req(data())
    
    required_columns <- define_required_columns(input$outcome)
    
    missing_columns <- setdiff(required_columns, colnames(data()))
    
    if (length(missing_columns) > 0) {
      output$columns_ui <- renderUI({
        rename_columns_ui(missing_columns, setdiff(colnames(data()), required_columns))
      })
      
      shinyjs::enable("confirm")
      
    } else {output$columns_ui <- renderUI({
      h3()
    })
    
    showModal(modalDialog(
      title = div(
        icon("check-circle", class = "fa-lg", style = "color: #5cb85c;"), 
        "Check completed"
      ),
      div(
        style = "color: #5cb85c; font-weight: bold; font-size: 16px; text-align: center; padding: 10px;",
        "All columns are correct!"
      ),
      footer = tags$div(
        style = "text-align: center;",
        tags$button(
          "close",
          class = "btn btn-success",
          onclick = "$('#shiny-modal').modal('hide');",
          style = "background-color: #5cb85c; color: white; border-radius: 5px; border: none; padding: 8px 16px;"
        )
      ),
      easyClose = TRUE,
      fade = TRUE
    ))
    
    shinyjs::disable("confirm")
    }
  })
  
  observeEvent(input$confirm, {
    req(data())
    
    required_columns <- define_required_columns(input$outcome)
    
    new_name_columns <- lapply(required_columns, function(column) {
      input[[paste0("column_", column)]]
    })
    
    updated_data <- rename_columns(data(), required_columns, new_name_columns)
    
    data(updated_data)
    
    output$data_preview <- renderDT({
      req(data())
      datatable(data(),
                options = list(pageLength = 50, autoWidth = TRUE),
                rownames = FALSE)  
    })
    
    showModal(modalDialog(
      title = div(
        icon("check-circle", class = "fa-lg", style = "color: #5cb85c;"),  
        "Renaming completed"
      ),
      div(
        style = "color: green; font-weight: bold; font-size: 16px; text-align: center; padding: 10px;",
        "Columns have been renamed successfully!"
      ),
      footer = tagList(
        actionButton("close_modal_rename", "close", 
                     style = "background-color: #5cb85c; color: white; border-radius: 5px; border: none; padding: 8px 16px;"),
        tags$script(HTML("
      $('#close_modal_rename').on('click', function() {
        $('#shiny-modal').modal('hide');
      });
    "))
      ),
      easyClose = TRUE,
      fade = TRUE
    ))
    
    
    output$resultado <- renderPrint({
      paste("renamed columns:", paste(required_columns, collapse = ", "))
    })
    
    shinyjs::disable("confirm")
  })
  observeEvent(input$Check_connection, {  
    req(data())
    data_rede <- data() 
    
    data_rede$study <- as.numeric(gsub("[A-Za-z].*", "", data_rede$study))
    
    nodes <- data_rede %>%
      group_by(treatment) %>%
      summarise(total_sampleSize = sum(sampleSize, na.rm = TRUE)) %>%
      mutate(id = treatment, label = treatment) %>%
      arrange(label)  
    
    nodes$label <- as.character(nodes$label) 
    
    min_size <- 1  
    max_size <- 3   
    
    nodes <- nodes %>%
      mutate(
        normalized_size = (total_sampleSize - min(total_sampleSize, na.rm = TRUE)) / 
          (max(total_sampleSize, na.rm = TRUE) - min(total_sampleSize, na.rm = TRUE)),
        vertex_size = normalized_size * (max_size - min_size) + min_size  
      )
    
    edges <- data_rede %>% 
      select(study, treatment) %>% 
      group_by(study) %>% 
      summarise(treatments = list(unique(treatment)), .groups = "drop") %>% 
      mutate(treatments_pairs = map(treatments, ~ combn(.x, 2, simplify = FALSE))) %>% 
      unnest(treatments_pairs) %>% 
      mutate(
        from = pmin(sapply(treatments_pairs, `[[`, 1), sapply(treatments_pairs, `[[`, 2)),
        to = pmax(sapply(treatments_pairs, `[[`, 1), sapply(treatments_pairs, `[[`, 2))
      ) %>% 
      filter(from != to) %>% 
      group_by(from, to) %>% 
      summarise(weight = n(), .groups = 'drop') %>% 
      ungroup() %>%
      arrange(from, to)  
    
    EDGES <- as.data.frame(edges)
    
    network <- as.network(EDGES, directed = FALSE, multiple = TRUE)
    network %v% "label" <- nodes$label[match(network %v% "vertex.names", nodes$id)]
    network %v% "vertex_size" <- nodes$vertex_size[match(network %v% "vertex.names", nodes$id)]
    g <- graph_from_data_frame(EDGES, directed = FALSE, vertices = nodes)
    library(igraph)
    
    if (!is_connected(g)) {
      componentes <- components(g)
      maior_componente <- which.max(componentes$csize)  
      nos_desconectados <- V(g)[componentes$membership != maior_componente]$name  
      mensagem_erro <- if (length(nos_desconectados) > 0) {
        paste("⚠ There are disconnected interventions on the network:", paste(nos_desconectados, collapse = ", "), 
              ". Check the data before proceeding with the analysis.")
      } else {
        "⚠ There are disconnected interventions on the network. Check the data before proceeding with the analysis."
      }
      
      showModal(modalDialog(
        title = div(
          icon("exclamation-triangle", class = "fa-lg", style = "color: red;"), 
          "Atenção"
        ),
        div(
          style = "color: red; font-weight: bold; font-size: 16px; text-align: center; padding: 10px;",
          mensagem_erro
        ),
        footer = tagList(
          actionButton("close_modal", "close", 
                       style = "background-color: #d9534f; color: white; border-radius: 5px; border: none; padding: 8px 16px;"),
          tags$script(HTML("
        $('#close_modal').on('click', function() {
          $('#shiny-modal').modal('hide');
        });
      "))
        ),
        easyClose = TRUE,
        fade = TRUE
      ))
      
      
    } else {
      showModal(modalDialog(
        title = div(
          icon("check-circle", class = "fa-lg", style = "color: #5cb85c;"),  
          "Coxexão da rede verificada"
        ),
        div(
          style = "color: green; font-weight: bold; font-size: 16px; text-align: center; padding: 10px;",
          "All interventions are connected to the network! You can continue with the analysis."
        ),
        footer = tagList(
          actionButton("close_modal_success", "close", 
                       style = "background-color: #5cb85c; color: white; border-radius: 5px; border: none; padding: 8px 16px;"),
          tags$script(HTML("
      $('#close_modal_success').on('click', function() {
        $('#shiny-modal').modal('hide');
      });
    "))
        ),
        easyClose = TRUE,
        fade = TRUE
      ))
      
    }
  })
  
  rede_reactive <- reactive({
    req(data()) 
    
    data_rede <- data() 
    
    data_rede$study <- as.numeric(gsub("[A-Za-z].*", "", data_rede$study))
    
    nodes <- data_rede %>%
      group_by(treatment) %>%
      summarise(total_sampleSize = sum(sampleSize, na.rm = TRUE)) %>%
      mutate(id = treatment, label = treatment) %>%
      arrange(label)  
    
    nodes$label <- as.character(nodes$label) 
    
    min_size <- 1  
    max_size <- 3   
    
    nodes <- nodes %>%
      mutate(
        normalized_size = (total_sampleSize - min(total_sampleSize, na.rm = TRUE)) / 
          (max(total_sampleSize, na.rm = TRUE) - min(total_sampleSize, na.rm = TRUE)),
        vertex_size = normalized_size * (max_size - min_size) + min_size  
      )
    
    edges <- data_rede %>% 
      select(study, treatment) %>% 
      group_by(study) %>% 
      summarise(treatments = list(unique(treatment)), .groups = "drop") %>% 
      mutate(treatments_pairs = map(treatments, ~ combn(.x, 2, simplify = FALSE))) %>% 
      unnest(treatments_pairs) %>% 
      mutate(
        from = pmin(sapply(treatments_pairs, `[[`, 1), sapply(treatments_pairs, `[[`, 2)),
        to = pmax(sapply(treatments_pairs, `[[`, 1), sapply(treatments_pairs, `[[`, 2))
      ) %>% 
      filter(from != to) %>% 
      group_by(from, to) %>% 
      summarise(weight = n(), .groups = 'drop') %>% 
      ungroup() %>%
      arrange(from, to)  
    
    EDGES <- as.data.frame(edges)
    
    network <- as.network(edges, directed = FALSE)
    network %v% "label" <- nodes$label[match(network %v% "vertex.names", nodes$id)]
    network %v% "vertex_size" <- nodes$vertex_size[match(network %v% "vertex.names", nodes$id)]
    
    list(network = network, EDGES = edges)
  })
  
  output$networkPlot <- renderPlot({
    req(rede_reactive())  
    
    cor_bg_label <- if (input$boxed_labels) input$cor_bg_label else FALSE
    label_pad <- if (input$boxed_labels) input$label_pad else FALSE
    set.seed(197)
    par(mar = c(4, 4, 4, 4))
    plot.network(rede_reactive()$network,
                 displaylabels = TRUE,
                 label = rede_reactive()$network %v% "label",
                 label.pos = as.numeric(input$pos_label),
                 edge.col = input$edge_color,
                 vertex.col = input$color_vertex,
                 label.pad = label_pad,
                 vertex.cex = rede_reactive()$network %v% "vertex_size",
                 edge.lwd = log(rede_reactive()$EDGES$weight + 1) / log(max(rede_reactive()$EDGES$weight) + 1) * 6,
                 label.cex = input$label_cex,
                 label.bg = cor_bg_label,
                 boxed.labels = input$boxed_labels,
                 mode = "circle",
                 pad = 0.05,
                 xlim = c(-input$limit_x, input$limit_x),
                 ylim = c(-input$limit_x, input$limit_x),
                 edge.label.cex = 50)
  })
  
  output$export_svg <- downloadHandler(
    filename = function() { paste(input$name_outcome,"_network", Sys.Date(), "svg", sep = ".") },
    content = function(file) {
      svg(filename = file, width = input$export_width / 1000, height = input$export_height / 1000, pointsize = input$label_cex * 10)
      par(mar = c(4, 4, 4, 4), cex = input$label_cex)
      plot.network(rede_reactive()$network,
                   displaylabels = TRUE,
                   label = rede_reactive()$network %v% "label",
                   label.pos = as.numeric(input$pos_label),
                   edge.col = input$edge_color,
                   vertex.col = input$color_vertex,
                   label.pad = if (input$boxed_labels) input$label_pad else FALSE,
                   vertex.cex = rede_reactive()$network %v% "vertex_size",
                   edge.lwd = log(rede_reactive()$EDGES$weight + 1) / log(max(rede_reactive()$EDGES$weight) + 1) * 6,
                   label.cex = input$label_cex,
                   label.bg = if (input$boxed_labels) input$cor_bg_label else FALSE,
                   boxed.labels = input$boxed_labels,
                   mode = "circle",
                   pad = 0.05,
                   xlim = c(-input$limit_x, input$limit_x),
                   ylim = c(-input$limit_x, input$limit_x),
                   edge.label.cex = 50)
      dev.off()
    }
  )
  
  output$export_tiff <- downloadHandler(
    filename = function() { paste(input$name_outcome,"network", Sys.Date(), "tiff", sep = ".") },
    content = function(file) {
      tiff(filename = file, width = input$export_width, height = input$export_height, res = input$export_res)
      par(mar = c(4, 4, 4, 4))
      plot.network(rede_reactive()$network,
                   displaylabels = TRUE,
                   label = rede_reactive()$network %v% "label",
                   label.pos = as.numeric(input$pos_label),
                   edge.col = input$edge_color,
                   vertex.col = input$color_vertex,
                   label.pad = if (input$boxed_labels) input$label_pad else FALSE,
                   vertex.cex = rede_reactive()$network %v% "vertex_size",
                   edge.lwd = log(rede_reactive()$EDGES$weight + 1) / log(max(rede_reactive()$EDGES$weight) + 1) * 6,
                   label.cex = input$label_cex,
                   label.bg = if (input$boxed_labels) input$cor_bg_label else FALSE,
                   boxed.labels = input$boxed_labels,
                   mode = "circle",
                   pad = 0.05,
                   xlim = c(-input$limit_x, input$limit_x),
                   ylim = c(-input$limit_x, input$limit_x),
                   edge.label.cex = 50)
      dev.off()
    }
  )
  geo_analysis <- reactiveVal(NULL)
  
  observeEvent(input$geo_analysis, {
    req(data())
    data_rede <- data() 
    
    nodes <- data_rede %>%
      group_by(treatment) %>%
      summarise(total_sampleSize = sum(sampleSize, na.rm = TRUE)) %>%
      mutate(id = treatment, label = treatment) %>%
      arrange(label)  
    
    edges <- data_rede %>% 
      select(study, treatment) %>% 
      group_by(study) %>% 
      summarise(treatments = list(unique(treatment)), .groups = "drop") %>% 
      mutate(treatments_pairs = map(treatments, ~ combn(.x, 2, simplify = FALSE))) %>% 
      unnest(treatments_pairs) %>% 
      mutate(
        from = pmin(sapply(treatments_pairs, `[[`, 1), sapply(treatments_pairs, `[[`, 2)),
        to = pmax(sapply(treatments_pairs, `[[`, 1), sapply(treatments_pairs, `[[`, 2))
      ) %>% 
      filter(from != to) %>% 
      group_by(from, to) %>% 
      summarise(weight = n(), .groups = 'drop') %>% 
      arrange(from, to)  
    
    EDGES <- as.data.frame(edges)
    
    n_nodes <- nodes %>%
      distinct(treatment) %>%
      nrow()
    
    n_combinações <- choose(n_nodes, 2)
    
    n_edges <- nrow(EDGES)
    
    n_studies <- data_rede %>%
      distinct(study) %>%
      nrow()
    
    nós_com_mais_de_um_edge <- edges %>%
      pivot_longer(cols = c(from, to), names_to = "direction", values_to = "node") %>%
      count(node) %>%
      filter(n > 1)
    
    n_edges_peso_maior_1 <- sum(EDGES$weight > 1)
    
    density <- n_edges / n_combinações
    
    Mean_thickness <- n_studies / n_edges
    
    Common_comparator <- (nrow(nós_com_mais_de_um_edge) / n_nodes) * 100
    
    strong_edges <- (n_edges_peso_maior_1 / n_edges) * 100
    
    geo_analysis(data.frame(
      Nodes = n_nodes,
      Edges = n_edges,
      Studies = n_studies,
      "Edges_more_than_1study" = n_edges_peso_maior_1,
      density = density,
      "Mean_thickness" = Mean_thickness,
      "Common_comparator" = Common_comparator,
      "strong edges" = strong_edges
    ))
    
    output$export_geo <- downloadHandler(
      filename = function() {
        paste(input$name_outcome, "geo_analysis", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        write.xlsx(geo_analysis(), file)
      }
    )
  })
  
  output$data_geo <- renderDT({
    req(geo_analysis())
    datatable(geo_analysis(), 
              options = list(pageLength = 50, autoWidth = TRUE), 
              caption = 'Resumo da Geometric Analysis')
  })
  
  
  observeEvent(input$outcome, {
    if (input$outcome == "dichotomous") {
      updateSelectInput(session, "outcome_type", choices = c("Odds Ratio", "Risk Ratio"))
    } else if (input$outcome == "survival") {
      updateSelectInput(session, "outcome_type", choices = c("Hazard Ratio", "Risk Ratio"))
    } else if (input$outcome == "continuous") {
      updateSelectInput(session, "outcome_type", choices = c("Mean difference"))
    }
  })
  
  observeEvent(input$run_analysis, {
    req(data())
    
    Data <- data()
    if (any(grepl("[^A-Za-z0-9_]", Data$treatment))) {
      temp_Data <- Data
      temp_Data$treatment <- gsub("[^A-Za-z0-9_]", "_", temp_Data$treatment)
      temp_Data$treatment <- gsub(" ", "", temp_Data$treatment)
      Data <-temp_Data
      removeModal()}
    
    output$error_output <- NULL
    tryCatch({withProgress(message = 'Analysing:', value = 0, {
      incProgress(1/10, detail = "Defining network data...")
      net.Data <- mtc.network(data = Data)
      
      incProgress(1/10, detail = "Defining the type of result...")
      
      outcome_type <- switch(input$outcome,
                             "dichotomous" = ifelse(input$outcome_type == "Odds Ratio", "logit", "log"),
                             "survival" = ifelse(input$outcome_type == "Hazard Ratio", "cloglog", "log"),
                             "identity")
      
      likelihood <- switch(input$outcome,
                           "dichotomous" = "binom",
                           "survival" = "poisson",
                           "normal")
      
      incProgress(1/10, detail = "Configuring the network model...")
      network_Data <- mtc.model(net.Data, 
                                n.chain = 4, 
                                type = "consistency", 
                                likelihood = likelihood, 
                                link = outcome_type,
                                linearModel = "random")
      
      incProgress(1/10, detail = "Run analysis...")
      mcmc_b_bin_fe <- mtc.run(network_Data, 
                               n.adapt = input$adapt, 
                               n.iter = input$iter, 
                               thin = input$thin)
      
      
      output$analysis_output <- renderPrint({
        summary(mcmc_b_bin_fe)
      })
      
      incProgress(1/10, detail = "Generating trace plots...")
      generate_traceplot <- function() {
        if (exists("mcmc_b_bin_fe")) {
          mcmc_list <- as.mcmc.list(mcmc_b_bin_fe)  
          trace_plot <- traceplot(mcmc_list)  
          return(trace_plot)
        } else {
          stop("The object ‘mcmc_b_bin_fe’ does not exist in the environment!")
        }
      }
      
      incProgress(1/10, detail = "Generating dens plot...")
      generate_densplot <- function() {
        if (exists("mcmc_b_bin_fe")) {
          mcmc_list <- as.mcmc.list(mcmc_b_bin_fe)  
          dens_plot <- densplot(mcmc_list)  
          return(dens_plot)
        } else {
          stop("The object ‘mcmc_b_bin_fe’ does not exist in the environment!")
        }
      }
      incProgress(1/10, detail = "Generating gelman rubin...")
      generate_gelman_diag <- function() {
        if (!exists("mcmc_b_bin_fe")) stop("The object ‘mcmc_b_bin_fe’ does not exist in the environment!")
        
        gelman <- gelman.diag(mcmc_b_bin_fe)
        num_params <- ncol(mcmc_b_bin_fe$samples[[1]])
        
        plot_list <- lapply(1:num_params, function(param_idx) {
          local({
            function() {
              par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))
              gelman.plot(lapply(mcmc_b_bin_fe$samples, function(chain) chain[, param_idx]), auto.layout = FALSE)
              title(main = paste(input$name_outcome, "_", param_idx, "_Gelman-Rubin"), cex.main = 0.8)
            }
          })
        })
        
        return(list(gelman = as.data.frame(gelman$psrf), plots = plot_list))
      }
      
      output$trace_plot <- renderPlot({
        generate_traceplot()
      })
      
      output$dens_plot <- renderPlot({
        generate_densplot()
      })
      
      output$gelman_plots_ui <- renderUI({
        plot_list <- generate_gelman_diag()$plots
        tagList(lapply(seq_along(plot_list), function(i) plotOutput(paste("plot", i, sep = "_"), height = "400px", width = "100%")))
      })
      
      observe({
        plot_list <- generate_gelman_diag()$plots
        lapply(seq_along(plot_list), function(i) {
          output[[paste("plot", i, sep = "_")]] <- renderPlot({ plot_list[[i]]() })
        })
      })
      
      output$gelman_table <- renderTable({
        if (!exists("mcmc_b_bin_fe")) return(data.frame(Erro = "The object ‘mcmc_b_bin_fe’ does not exist in the environment!"))
        generate_gelman_diag()$gelman
      }, rownames = TRUE)
      
      
      
      
      
      output$download_gelman <- downloadHandler(
        filename = function() paste0(input$name_outcome, "_gelman_diag", Sys.Date(), ".xlsx"),
        content = function(file) {
          gelman_df <- generate_gelman_diag()$gelman
          openxlsx::write.xlsx(gelman_df, file, rowNames = TRUE)
        }
      )
      
      output$download_gelman_plot <- downloadHandler(
        filename = function() {
          paste0(input$name_outcome, "_gelman_diag_plots.zip")
        },
        content = function(file) {
          temp_dir <- tempfile()
          dir.create(temp_dir)
          
          tiff_files <- c()
          
          num_chains <- length(mcmc_b_bin_fe$samples)  
          num_params <- ncol(mcmc_b_bin_fe$samples[[1]])  
          param_names <- colnames(mcmc_b_bin_fe$samples[[1]]) 
          if (is.null(param_names)) {
            param_names <- paste0("param_", 1:num_params)
          }
          
          file_counter <- 1
          
          for (param_idx in seq(from = 1, to = num_params, by = 9)) {
            tiff_path <- file.path(temp_dir, paste0(input$name_outcome, "_", file_counter, "_gelman_diag.tiff"))
            
            tiff(tiff_path, width = 9, height = 9, units = "in", res = 300)  
            par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))  
            
            for (i in 0:8) {
              current_param_idx <- param_idx + i
              if (current_param_idx <= num_params) {  
                gelman.plot(lapply(mcmc_b_bin_fe$samples, function(chain) chain[, current_param_idx]), 
                            auto.layout = FALSE)  
                title(main = paste(input$name_outcome, "_", current_param_idx, "_Gelman-Rubin"))
              }
            }
            
            dev.off()  
            tiff_files <- c(tiff_files, tiff_path)  
            file_counter <- file_counter + 1
          }
          
          zipfile_path <- file.path(tempdir(), paste0(input$name_outcome, "_gelman_diag_plots.zip"))
          zip(zipfile_path, files = tiff_files, flags = "-j")  
          file.copy(zipfile_path, file)
        }
      )
      node_splitting_Data <- reactiveVal(NULL)
      incProgress(1/10, detail = "Generating node splitting...")
      
      req(net.Data)
      
      comp <- mtc.nodesplit.comparisons(net.Data)
      
      if (nrow(comp) > 0) { 
        node_splitting <- mtc.nodesplit(net.Data, comparisons = comp, linearModel = "random", 
                                        n.adapt = input$adapt, n.iter = input$iter, thin = input$thin)
        node_splitting <- summary(node_splitting)
        
        node_splitting_print <- print (node_splitting)
        
        node_splitting_df <- as.data.frame(node_splitting_print)
        node_splitting_df[is.na(node_splitting_df)] <- "-"
        
        node_splitting_Data(node_splitting_df)  
        output$Data_node <- renderDT({  
          req(node_splitting_Data())  
          datatable(node_splitting_Data(), 
                    options = list(pageLength = 50, autoWidth = TRUE))
        })
        output$mensagem_node <- renderUI(NULL)
      } else {
        output$mensagem_node <- renderUI({
          div(
            style = "color: #f0ad4e; font-weight: bold; font-size: 16px; text-align: center; padding: 10px; 
                   border: 1px solid #f0ad4e; background-color: #fff3cd; border-radius: 5px;",
            icon("exclamation-circle", class = "fa-lg", style = "color: #f0ad4e; margin-right: 5px;"),
            "No loop comparison with more than one study found. It was not possible to perform Node Splitting."
          )
        })  
        
      }
      
      
      output$export_node <- downloadHandler(
        filename = function() {
          paste(input$name_outcome, "_node_splitting", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
          req(node_splitting_Data())  
          write.xlsx(node_splitting_Data(), file)
        }
      )
      tabela_gerada <- reactiveVal(NULL)
      SUCRA_gerado <- reactiveVal(NULL)
      SUCRA_plot <- reactiveVal(NULL)
      
      req(mcmc_b_bin_fe)
      incProgress(1/10, detail = "Generating league table...")
      
      tipo_de_dado_escolhida <- switch(input$outcome,
                                       "dichotomous" = 1,
                                       "survival" = 2,
                                       "continuous" = 3)
      
      if (tipo_de_dado_escolhida == 1 || tipo_de_dado_escolhida == 2) {
        conv_exp <- function(x) {
          parts <- strsplit(x, split = "[ \\(\\),]")[[1]][c(1, 3, 5)]
          num_parts <- suppressWarnings(as.numeric(parts))
          if (any(is.na(num_parts))) return(NA)
          
          exp_parts <- exp(num_parts)
          formatted <- formatC(exp_parts, digits = 2, format = "f")
          
          
          paste0(formatted[1], " (", formatted[2], ", ", formatted[3], ")")
        }
      } else {
        conv_round <- function(x) {
          parts <- strsplit(x, split = "[ \\(\\),]")[[1]][c(1, 3, 5)]
          numeric_parts <- suppressWarnings(as.numeric(parts))
          if (any(is.na(numeric_parts))) return(NA)
          
          rounded_values <- round(numeric_parts, digits = 2)
          formatted_values <- formatC(rounded_values, digits = 2, format = "f")
          paste0(formatted_values[1], " (", formatted_values[2], ", ", formatted_values[3], ")")
        }
      }
      
      league.tab <- relative.effect.table(mcmc_b_bin_fe)
      
      rownames(league.tab) <- rownames(relative.effect.table(mcmc_b_bin_fe))
      colnames(league.tab) <- colnames(relative.effect.table(mcmc_b_bin_fe))
      
      league.tab <- as.data.frame(league.tab)
      
      league.tab2 <- if (tipo_de_dado_escolhida == 3) {
        as.data.frame(apply(league.tab, MARGIN = c(1, 2), FUN = conv_round))
      } else {
        as.data.frame(apply(league.tab, MARGIN = c(1, 2), FUN = conv_exp))
      }
      
      rownames(league.tab2) <- rownames(league.tab)
      colnames(league.tab2) <- colnames(league.tab)
      league.tab2[sapply(league.tab2, is.factor)] <- lapply(league.tab2[sapply(league.tab2, is.factor)], as.character)
      
      trt.names <- colnames(league.tab2)
      for(i in 1:length(trt.names)) {
        league.tab2[i, i] <- trt.names[i]
      }
      
      tabela_gerada(league.tab2)
      
      league_tab_xlsx <- tempfile(fileext = ".xlsx") 
      
      write.xlsx(league.tab2, league_tab_xlsx, rowNames = TRUE)  
      
      output$download_tabela <- downloadHandler(
        filename = function() {
          paste0(input$name_outcome, "_league_table_", Sys.Date(), ".xlsx")  
        },
        content = function(file) {
          req(tabela_gerada())
          
          ws <- createWorkbook()
          addWorksheet(ws, "LEAGUETB")
          writeData(ws, "LEAGUETB", tabela_gerada(), rowNames = FALSE, colNames = FALSE)
          saveWorkbook(ws, file, overwrite = TRUE)  
        }
      )
      
      SUCRA_tabela <- reactiveVal(NULL)
      observeEvent(input$run_SUCRA, {
        
        req(mcmc_b_bin_fe)
        
        direction_SUCRA <- as.numeric(input$direction_SUCRA)
        
        ranks <- rank.probability(mcmc_b_bin_fe, preferredDirection = direction_SUCRA)
        ranks1 <- as.data.frame(print(ranks))
        cumulative_probs <- t(apply(ranks1, 1, cumsum))
        
        cumulative_probs <- as.data.frame(cumulative_probs)
        
        colnames(cumulative_probs) <- gsub("V", "", colnames(cumulative_probs))
        
        cumulative_probs[, -1] <- cumulative_probs[, -1] * 100
        
        cumulative_probs$treatments <- rownames(cumulative_probs)
        
        cumulative_probs <- cumulative_probs[, c(ncol(cumulative_probs), 1:(ncol(cumulative_probs)-1))]
        
        cumulative_probs_long <- reshape2::melt(cumulative_probs, id.vars = "treatments", variable.name = "Rank", value.name = "Probability")
        
        p <- ggplot(cumulative_probs_long, 
                    aes(x = Rank, y = Probability, color = treatments, group = treatments)) + 
          geom_line(linewidth = 1) + 
          labs(x = "Rank", y = "Cumulative probability") + 
          theme_minimal() + 
          theme(
            legend.position = "right", 
            legend.text = element_text(size = input$text_size),  
            legend.title = element_text(size = input$text_size),
            axis.text.x = element_text(size = input$text_size),
            axis.text.y = element_text(size = input$text_size),                # opcional, para consistência
            axis.title.x = element_text(size = input$text_size, face = "bold"), # aumenta título do eixo x
            axis.title.y = element_text(size = input$text_size, face = "bold")  # aumenta título do eixo y
          )
        
        SUCRA_xlsx <- paste0("SUCRA_", Sys.Date(), ".xlsx")
        write.xlsx(as.data.frame(print(sucra(ranks))), SUCRA_xlsx, rowNames = TRUE, colNames = TRUE)
        
        SUCRA_gerado(SUCRA_xlsx)
        SUCRA_plot(p)
        
        
        
        output$download_SUCRA_tiff <- downloadHandler(
          filename = function() paste0(input$name_outcome,"_SUCRA_", Sys.Date(), ".tiff"),
          content = function(file) {
            ggsave(file, plot = p, device = "tiff", width = 20, height = 17, units = "in", dpi = 300)
          }
        )
        SUCRA <- as.data.frame(sucra(ranks))
        SUCRA <- round(SUCRA * 100, 2)
        SUCRA <- rownames_to_column(SUCRA, var = "Treatment")  
        colnames(SUCRA)[2] <- "%SUCRA"
        SUCRA_tabela(SUCRA)  
      })
      
      
      output$SUCRA_table <- renderDataTable({
        req(SUCRA_tabela())  
        datatable(SUCRA_tabela(), rownames = FALSE)
        
      })
      output$download_SUCRA_xlsx <- downloadHandler(
        filename = function() {
          paste0(input$name_outcome, "_SUCRA_", Sys.Date(), ".xlsx")  
        },
        content = function(file) {
          req(SUCRA_tabela())  
          
          ws <- createWorkbook()
          addWorksheet(ws, "SUCRA")
          writeData(ws, "SUCRA", SUCRA_tabela(), rowNames = FALSE, colNames = TRUE)
          saveWorkbook(ws, file, overwrite = TRUE)  
        }
      )
      
      
      output$rend_league_table <- renderDT({
        req(league.tab2)
        datatable(league.tab2, escape = FALSE, options = list(pageLength = 100, autoWidth = TRUE))
      })
      
      output$SUCRA_plot <- renderPlot({
        req(SUCRA_plot())
        SUCRA_plot()
      })
      
      incProgress(1/10, detail = "Finalising...")
      showModal(modalDialog(
        title = div(
          icon("check-circle", class = "fa-lg", style = "color: #5cb85c;"),  
          "Analysis completed"
        ),
        div(
          style = "color: #5cb85c; font-weight: bold; font-size: 16px; text-align: center; padding: 10px;",
          "Analysis successfully completed!"
        ),
        footer = tags$div(
          style = "text-align: center;",
          tags$button(
            "close",
            class = "btn btn-success",
            onclick = "$('#shiny-modal').modal('hide');",
            style = "background-color: #5cb85c; color: white; border-radius: 5px; border: none; padding: 8px 16px;"
          )
        ),
        easyClose = TRUE,
        fade = TRUE
      ))
      
      output$download_plot <- downloadHandler(
        filename = function() {
          paste0(input$name_outcome, "_trace_plot", Sys.Date(), ".tiff")
        },
        content = function(file) {
          # Salva o Graph como TIFF
          tiff(file, width = 4500, height = 3000, res = 300)
          print(generate_traceplot())  # Gera o Graph
          dev.off()
        }
      )
      output$download_densplot <- downloadHandler(
        filename = function() {
          paste0(input$name_outcome, "_densplot", Sys.Date(),".tiff")
        },
        content = function(file) {
          tiff(file, width = 4500, height = 3000, res = 300)
          print(generate_densplot())  # Gera o Graph
          dev.off()
        }
      )
      output$download_summary <- downloadHandler(
        filename = function() {
          paste(input$name_outcome, "_resum_analyze_", Sys.Date(), ".txt", sep = "")
        },
        content = function(file) {
          writeLines(capture.output(summary(mcmc_b_bin_fe)), file)
        }
      )
      output$error_output <- NULL
    })
    }, error = function(e) {
      output$error_output <- renderText({
        paste("Error in execution: ", e$message)
      })
      showNotification("Error when running the analysis. Check the data and try again.", type = "error")
    })
  })
  
}

shinyApp(ui = ui, server = server)}

# executar AutoNetBayes --------------------------------------------------
shinyApp(ui = ui, server = server)

