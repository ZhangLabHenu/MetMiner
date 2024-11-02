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
              textInput(inputId = ns("fig1_width"),
                        label = "width",
                        value = 10),
              textInput(inputId = ns("fig1_height"),
                        label = "height",
                        value = 10),
              selectInput(
                inputId = ns("fig1_format"),label = "format",
                choices = c("jpg","pdf","png","tiff"),
                selected = "pdf",selectize = F
              ),
              downloadButton(ns("fig1_download"),"Download"),
              tags$h3("Summary of missing values in all samples",style = 'color: #008080'),
              hr_main(),
              jqui_resizable(
                uiOutput(ns("data_clean_mv_plt.pos"),fill = T)
              ),
              textInput(inputId = ns("fig2_width"),
                        label = "width",
                        value = 10),
              textInput(inputId = ns("fig2_height"),
                        label = "height",
                        value = 10),
              selectInput(
                inputId = ns("fig2_format"),label = "format",
                choices = c("jpg","pdf","png","tiff"),
                selected = "pdf",selectize = F
              ),
              downloadButton(ns("fig2_download"),"Download"),
            ),
            tabPanel(
              title = 'Negative',height = '500px',width = "100%",
              icon = icon('minus'),
              tags$h3("Feature accumulation profile of QC samples",style = 'color: #008080'),
              hr_main(),
              jqui_resizable(
                uiOutput(ns("data_clean_batch_plt.neg"),fill = T)
              ),
              textInput(inputId = ns("fig3_width"),
                        label = "width",
                        value = 10),
              textInput(inputId = ns("fig3_height"),
                        label = "height",
                        value = 10),
              selectInput(
                inputId = ns("fig3_format"),label = "format",
                choices = c("jpg","pdf","png","tiff"),
                selected = "pdf",selectize = F
              ),
              downloadButton(ns("fig3_download"),"Download"),
              tags$h3("Summary of missing values in all samples",style = 'color: #008080'),
              hr_main(),
              jqui_resizable(
                uiOutput(ns("data_clean_mv_plt.neg"))
              ),
              textInput(inputId = ns("fig4_width"),
                        label = "width",
                        value = 10),
              textInput(inputId = ns("fig4_height"),
                        label = "height",
                        value = 10),
              selectInput(
                inputId = ns("fig4_format"),label = "format",
                choices = c("jpg","pdf","png","tiff"),
                selected = "pdf",selectize = F
              ),
              downloadButton(ns("fig4_download"),"Download")
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
    #> parameters
    ##> download parameters ================
    download_para = reactive({
      list(
        ##> fig1
        fig1_width = as.numeric(input$fig1_width),
        fig1_height = as.numeric(input$fig1_height),
        fig1_format = as.character(input$fig1_format),
        ##> fig2
        fig2_width = as.numeric(input$fig2_width),
        fig2_height = as.numeric(input$fig2_height),
        fig2_format = as.character(input$fig2_format),
        ##> fig3
        fig3_width = as.numeric(input$fig3_width),
        fig3_height = as.numeric(input$fig3_height),
        fig3_format = as.character(input$fig3_format),
        ##> fig4
        fig4_width = as.numeric(input$fig4_width),
        fig4_height = as.numeric(input$fig4_height),
        fig4_format = as.character(input$fig4_format)
      )
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

      }
    )


    # download ----------------------------------------------------------------
    ###> fig1 =====
    output$fig1_download = downloadHandler(
      filename = function() {
        paste0("01.boxplot-pos.", download_para()$fig1_format)
      },
      content = function(file) {
        # extract parameters
        para_d <- download_para()
        # draw condition
        p = QC_boxplot(object = p2_dataclean$object_pos,colby = p2_dataclean$color_key_batch,type = 'plot')

        # save plot
        ggsave(
          filename = file,
          plot = p,
          width = para_d$fig1_width,
          height = para_d$fig1_height,
          device = para_d$fig1_format
        )
      }
    )
    ###> fig2 ====
    output$fig2_download = downloadHandler(
      filename = function() {
        paste0("02.mv_plot-pos.", download_para()$fig2_format)
      },
      content = function(file) {
        # extract parameters
        para_d <- download_para()

        # draw condition
        p = check_mv(object = p2_dataclean$object_pos,colby = p2_dataclean$colby,orderby = p2_dataclean$orderby,type = 'plot')

        # save plot

        ggsave(
          filename = file,
          plot = p,
          width = para_d$fig2_width,
          height = para_d$fig2_height,
          device = para_d$fig2_format
        )
      }
    )
    ###> fig3 =====
    output$fig3_download = downloadHandler(
      filename = function() {
        paste0("01.boxplot-neg.", download_para()$fig3_format)
      },
      content = function(file) {
        # extract parameters
        para_d <- download_para()

        # draw condition
        p = QC_boxplot(object = p2_dataclean$object_neg,colby = p2_dataclean$color_key_batch,type = 'plot')

        # save plot
        ggsave(
          filename = file,
          plot = p,
          width = para_d$fig3_width,
          height = para_d$fig3_height,
          device = para_d$fig3_format
        )
      }
    )
    ###> fig4 ====
    output$fig4_download = downloadHandler(
      filename = function() {
        paste0("02.mv_plot-neg.", download_para()$fig4_format)
      },
      content = function(file) {
        # extract parameters
        para_d <- download_para()

        # draw condition
        p = check_mv(object = p2_dataclean$object_neg,colby = p2_dataclean$colby,orderby = p2_dataclean$orderby,type = 'plot')

        # save plot

        ggsave(
          filename = file,
          plot = p,
          width = para_d$fig4_width,
          height = para_d$fig4_height,
          device = para_d$fig4_format
        )
      }
    )

  })
}

