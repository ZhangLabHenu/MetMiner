#' import from tbl data of UI
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyjqui jqui_resizable
#' @importFrom shinyFiles shinyDirButton
#' @importFrom shinyWidgets materialSwitch
#' @importFrom DT dataTableOutput
#' @noRd


data_overview_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    useShinyjs(),
    title = 'Overview',
    icon = icon("scale-balanced"),
    sidebarLayout(
      div(id = ns('Sidebar'),
          #### sidebarPanel ======
          sidebarPanel(width = 2,
                       tags$h3("Check batch effect",style = 'color: #008080'),
                       hr_main(),
                       radioButtons(
                         inputId = ns('data_clean_col_key'),
                         label = "Boxplot colored by",
                         choices = c("batch","injection order"),
                         selected = 'injection order'
                       ),
                       fileInput(
                         inputId = ns('re_upload_sample_info'),
                         label = "re-upload sample info (if needed) ",multiple = F,
                         accept = '.csv'
                       ),
                       tags$h3("Summary of missing values",style = 'color: #008080'),
                       hr_main(),
                       selectInput(
                         inputId = ns("mv_color_by"),label = "color by",
                         choices = c("class","group","..."),selected = "class",
                         multiple = F
                       ),
                       selectInput(
                         inputId = ns("mv_order_by"),label = "order by",
                         choices = c("sample id","injection oreder","batch","..."),selected = "injection oreder",
                         multiple = F
                       ),
          )# sidebarPanel
      ),# div
      #### MainPanel ========
      mainPanel(
        fluidPage(
          actionButton(ns('toggleSidebar'),"Toggle sidebar"),
          actionButton(inputId = ns('data_clean_start'),label = "Start",icon = icon("play")),
          actionButton(inputId = ns('data_clean_reupload_si'),"Update sample information",icon = icon("angles-up")),
          hr_head(),
          materialSwitch(inputId = ns("data_clean_plt_format"),label = "Interactive plot", status = "primary"),
          tabsetPanel(
            tabPanel(
              title = 'Positive',height = '500px',width = "100%",
              icon = icon('plus'),
              tags$h3("",style = 'color: #008080'),
              hr_main(),
              jqui_resizable(
                uiOutput(ns("data_clean_batch_plt.pos"),fill = T)
              ),
              textInput(inputId = ns("widthPart2.1.1"),
                        label = "width",
                        value = 10),
              textInput(inputId = ns("heightPart2.1.1"),
                        label = "height",
                        value = 10),
              actionButton(ns("adjustPart2.1.1"),"Set fig size"),
              downloadButton(ns("downfigPart2.1.1"),"Download"),
              tags$h3("Summary of missing values in all samples",style = 'color: #008080'),
              hr_main(),
              jqui_resizable(
                uiOutput(ns("data_clean_mv_plt.pos"),fill = T)
              ),
              textInput(inputId = ns("widthPart2.1.2"),
                        label = "width",
                        value = 10),
              textInput(inputId = ns("heightPart2.1.2"),
                        label = "height",
                        value = 10),
              actionButton(ns("adjustPart2.1.2"),"Set fig size"),
              downloadButton(ns("downfigPart2.1.2"),"Download"),
            ),

            tabPanel(
              title = 'Negative',height = '500px',width = "100%",
              icon = icon('minus'),
              tags$h3("Feature accumulation profile of QC samples",style = 'color: #008080'),
              hr_main(),
              jqui_resizable(
                uiOutput(ns("data_clean_batch_plt.neg"),fill = T)
              ),
              textInput(inputId = ns("widthPart2.1.3"),
                        label = "width",
                        value = 10),
              textInput(inputId = ns("heightPart2.1.3"),
                        label = "height",
                        value = 10),
              actionButton(ns("adjustPart2.1.3"),"Set fig size"),
              downloadButton(ns("downfigPart2.1.3"),"Download"),
              tags$h3("Summary of missing values in all samples",style = 'color: #008080'),
              hr_main(),
              jqui_resizable(
                uiOutput(ns("data_clean_mv_plt.neg"))
              ),
              textInput(inputId = ns("widthPart2.1.4"),
                        label = "width",
                        value = 10),
              textInput(inputId = ns("heightPart2.1.4"),
                        label = "height",
                        value = 10),
              actionButton(ns("adjustPart2.1.4"),"Set fig size"),
              downloadButton(ns("downfigPart2.1.4"),"Download"),
            )
          )
        )
      )
    )
  )

}


#' import from tbl data of server
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs toggle runjs useShinyjs
#' @importFrom dplyr select left_join
#' @importFrom massdataset activate_mass_dataset
#' @importFrom plotly renderPlotly plotlyOutput
#' @param id module of server
#' @param volumes shinyFiles volumes
#' @param prj_init use project init variables.
#' @param data_import_rv reactivevalues mass_dataset export
#' @param data_clean_rv reactivevalues p2 dataclean
#' @param downloads download module
#' @noRd


data_overview_server <- function(id,volumes,prj_init,data_import_rv,data_clean_rv,downloads) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observeEvent(input$toggleSidebar, {
      shinyjs::toggle(id = "Sidebar")
    })
    #> reupload sample info
    update_sample_info <- reactive({
      file1 <- input$re_upload_sample_info
      if(is.null(file1)){return()}
      read.csv(file = file1$datapath,
               sep=",",header = T,stringsAsFactors = F)
    })

    p2_dataclean <- reactiveValues(data = NULL)

    observe({
      updateSelectInput(session, "mv_color_by",choices = colnames(prj_init$sample_info),selected = colnames(prj_init$sample_info)[3])
      updateSelectInput(session, "mv_order_by",choices = colnames(prj_init$sample_info),selected = colnames(prj_init$sample_info)[2])
    })


    observeEvent(
      input$data_clean_start,
      {
        #> Init object pos and neg
        if(!is.null(data_import_rv$object_pos)) {
          p2_dataclean$object_pos = data_import_rv$object_pos
          p2_dataclean$object_neg = data_import_rv$object_neg
        } else {
          return()
        }

        p2_dataclean$object_neg =
          p2_dataclean$object_neg %>%
          activate_mass_dataset('sample_info') %>%
          dplyr::select(sample_id) %>%
          dplyr::left_join(prj_init$sample_info)

        p2_dataclean$object_pos =
          p2_dataclean$object_pos %>%
          activate_mass_dataset('sample_info') %>%
          dplyr::select(sample_id) %>%
          dplyr::left_join(prj_init$sample_info)

        ##> update sample info
        observeEvent(
          input$data_clean_reupload_si,
          {
            if(is.null(input$re_upload_sample_info)){return()}
            if(is.null(prj_init$sample_info)){return()}
            p2_dataclean$temp_sample_info =prj_init$sample_info%>% as.data.frame()

            p2_dataclean$object_pos <-
              p2_dataclean$object_pos %>%
              activate_mass_dataset('sample_info') %>%
              select(sample_id) %>%
              left_join(p2_dataclean$temp_sample_info)

            p2_dataclean$object_neg <-
              p2_dataclean$object_neg %>%
              activate_mass_dataset('sample_info') %>%
              select(sample_id) %>%
              left_join(p2_dataclean$temp_sample_info)
          }
        )

        #> color key of missing value
        p2_dataclean$color_key_batch = input$data_clean_col_key
        p2_dataclean$colby = input$mv_color_by
        p2_dataclean$orderby = input$mv_order_by

        #> batch effect
        #> plot.pos
        output$data_clean_batch_plt.pos <- renderUI({
          plot_type <- input$data_clean_plt_format

          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_plot_checkbatch.pos"))
          } else {
            plotOutput(outputId = ns("plot_checkbatch.pos"))
          }
        })
        output$plot_checkbatch.pos <- renderPlot({
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_pos)){return()}
          QC_boxplot(object = p2_dataclean$object_pos,colby = p2_dataclean$color_key_batch,type = 'plot')
        })


        output$plotly_plot_checkbatch.pos <- renderPlotly({
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_pos)){return()}
          QC_boxplot(object = p2_dataclean$object_pos,colby = p2_dataclean$color_key_batch,type = 'plotly')
        })



        #> plot.neg
        output$data_clean_batch_plt.neg <- renderUI({
          plot_type <- input$data_clean_plt_format

          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_plot_checkbatch.neg"))
          } else {
            plotOutput(outputId = ns("plot_checkbatch.neg"))
          }
        })

        output$plot_checkbatch.neg <- renderPlot({
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_neg)){return()}
          QC_boxplot(object = p2_dataclean$object_neg,colby = p2_dataclean$color_key_batch,type = 'plot')
        })

        output$plotly_plot_checkbatch.neg <- renderPlotly({
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_neg)){return()}
          QC_boxplot(object = p2_dataclean$object_neg,colby = p2_dataclean$color_key_batch,type = 'plotly')
        })


        ##> MV plots
        #> plot.pos
        output$data_clean_mv_plt.pos <- renderUI({
          plot_type <- input$data_clean_plt_format

          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_plot_checkmv.pos"))
          } else {
            plotOutput(outputId = ns("plot_checkmv.pos"))
          }
        })

        output$plot_checkmv.pos <- renderPlot({
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_pos)){return()}
          check_mv(object = p2_dataclean$object_pos,colby = p2_dataclean$colby,orderby = p2_dataclean$orderby,type = 'plot')
        })

        output$plotly_plot_checkmv.pos <- renderPlotly({
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_pos)){return()}
          check_mv(object = p2_dataclean$object_pos,colby = p2_dataclean$colby,orderby = p2_dataclean$orderby,type = 'plot')
        })



        #> plot.neg
        output$data_clean_mv_plt.neg <- renderUI({
          plot_type <- input$data_clean_plt_format

          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_plot_checkmv.neg"))
          } else {
            plotOutput(outputId = ns("plot_checkmv.neg"))
          }
        })

        output$plot_checkmv.neg <- renderPlot({
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_neg)){return()}
          check_mv(object = p2_dataclean$object_neg,colby = p2_dataclean$colby,orderby = p2_dataclean$orderby,type = 'plot')
        })

        output$plotly_plot_checkmv.neg <- renderPlotly({
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_neg)){return()}
          check_mv(object = p2_dataclean$object_neg,colby = p2_dataclean$colby,orderby = p2_dataclean$orderby,type = 'plotly')
        })

        data_clean_rv$object_neg <- p2_dataclean$object_neg
        data_clean_rv$object_pos <- p2_dataclean$object_pos


        # download ----------------------------------------------------------------
        #> pos qc data
        observeEvent(
          input$adjustPart2.1.1,
          {
            downloads$widthPart2.1.1 <- as.numeric(input$widthPart2.1.1)
            downloads$heightPart2.1.1 <-  as.numeric(input$heightPart2.1.1)
          }
        )
        output$downfigPart2.1.1 = downloadHandler(
          filename = function() {
            "Part2.1.1_check_batch_pos.pdf"
          },
          content = function(file) {
            ggsave(plot = QC_boxplot(object = p2_dataclean$object_pos,colby = p2_dataclean$color_key_batch,type = 'plot'),
                   filename = file,
                   width = downloads$widthPart2.1.1,
                   height = downloads$heightPart2.1.1)
          }
        )

        #> neg qc data
        observeEvent(
          input$adjustPart2.1.3,
          {
            downloads$widthPart2.1.3 <- as.numeric(input$widthPart2.1.3)
            downloads$heightPart2.1.3 <-  as.numeric(input$heightPart2.1.3)
          }
        )
        output$downfigPart2.1.3 = downloadHandler(
          filename = function() {
            "Part2.1.3_check_batch_neg.pdf"
          },
          content = function(file) {
            ggsave(plot = QC_boxplot(object = p2_dataclean$object_neg,colby = p2_dataclean$color_key_batch,type = 'plot'),
                   filename = file,
                   width = downloads$widthPart2.1.3,
                   height = downloads$heightPart2.1.3)
          }
        )

        #> download pos
        observeEvent(
          input$adjustPart2.1.2,
          {
            downloads$widthPart2.1.2 <- as.numeric(input$widthPart2.1.2)
            downloads$heightPart2.1.2 <-  as.numeric(input$heightPart2.1.2)
          }
        )
        output$downfigPart2.1.2 = downloadHandler(
          filename = function() {
            "Part2.1.2_check_mv_pos.pdf"
          },
          content = function(file) {
            ggsave(plot = check_mv(object = p2_dataclean$object_pos,colby = p2_dataclean$colby,orderby = p2_dataclean$orderby,type = 'plot'),
                   filename = file,
                   width = downloads$widthPart2.1.2,
                   height = downloads$heightPart2.1.2)
          }
        )

        observeEvent(
          input$adjustPart2.1.4,
          {
            downloads$widthPart2.1.4 <- as.numeric(input$widthPart2.1.4)
            downloads$heightPart2.1.4 <-  as.numeric(input$heightPart2.1.4)
          }
        )
        output$downfigPart2.1.4 = downloadHandler(
          filename = function() {
            "Part2.1.4_check_mv_neg.pdf"
          },
          content = function(file) {
            ggsave(plot = check_mv(object = p2_dataclean$object_neg,colby = p2_dataclean$colby,orderby = p2_dataclean$orderby,type = 'plot'),
                   filename = file,
                   width = downloads$widthPart2.1.4,
                   height = downloads$heightPart2.1.4)
          }
        )

      }
    )
  })
}

