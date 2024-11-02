#' remove outliers
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyjqui jqui_resizable
#' @importFrom shinyWidgets materialSwitch
#' @importFrom DT dataTableOutput
#' @noRd


data_rm_outlier_ui <- function(id) {
  ns <- NS(id)
  ### Part1.4.2 Remove noisy features -----------------------------------------

  tabPanel(
    useShinyjs(),
    title = 'Remove outliers',
    icon = icon("trash-can"),
    sidebarLayout(
      div(id = ns('Sidebar'),
          #### sidebarPanel ======
          sidebarPanel(width = 2,
                       tags$h3("Parameters",style = 'color: #008080'),
                       hr_main(),
                       sliderInput(
                         inputId = ns('na_percentage_cutoff'),
                         label = "na_percentage_cutoff",
                         min = 0,
                         max = 1,
                         value = 0.5
                       ),
                       textInput(
                         inputId = ns('sd_fold_change'),
                         label = "sd_fold_change",value = 6
                       ),
                       textInput(
                         inputId = ns('mad_fold_change'),
                         label = "mad_fold_change",value = 6
                       ),
                       textInput(
                         inputId = ns('dist_p_cutoff'),
                         label = "dist_p_cutoff",value = 0.05
                       ),
                       tags$h3("Summary of missing values",style = 'color: #008080'),
                       hr_main(),
                       selectInput(
                         inputId = ns("outlier_color_by"),label = "color by",
                         choices = c("class","group","..."),selected = "class",
                         multiple = F
                       ),
                       selectInput(
                         inputId = ns("outlier_order_by"),label = "order by",
                         choices = c("sample id","injection oreder","batch","..."),selected = "injection oreder",
                         multiple = F
                       ),
          )# sidebarPanel
      ),# div
      #### MainPanel ========
      mainPanel(
        fluidPage(
          actionButton(ns('toggleSidebar'),"Toggle sidebar"),
          actionButton(inputId = ns('outlier_start'),label = "Find outliers",icon = icon("play")),
          actionButton(inputId = ns('rm_outliers'),"Remove and update",icon = icon("angles-up")),
          hr_head(),
          materialSwitch(inputId = ns("outlier_plt_format"),label = "Interactive plot", status = "primary"),
          tabsetPanel(
            tabPanel(
              title = 'Positive',height = '500px',width = "100%",
              icon = icon('plus'),
              tags$h3("Outliers",style = 'color: #008080'),
              hr_main(),
              HTML(
                "
              Detect outlier samples. See more here: <a href='https://privefl.github.io/blog/detecting-outlier-samples-in-pca/'>Detecting outlier samples in PCA</a> <br>
              In your experiment, if there is heterogeneity among the test samples, meaning they may come from different species, organs, or tissues, then there may be significant differences in the overall metabolic composition among these samples. Exercise caution when identifying outlier samples and refer to the following figure to determine whether outlier samples are caused by experimental errors or sampling errors (such as sample contamination or air bubbles in sample vials). If 80% of features cannot be detected in samples from the same tissue (organ), consider analyzing samples from that tissue separately.
              "
              ),
              hr_bar(),
              DT::dataTableOutput(ns("outlier.pos")),
              tags$h3("Missing value",style = 'color: #008080'),
              hr_main(),
              jqui_resizable(
                uiOutput(ns("outlier.pos_plt"),fill = T)
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
              tags$h3("Pick outliers",style = 'color: #008080'),
              hr_main(),
              selectInput(inputId = ns('out_vari_pos'),label = "Select sample_id",choices = "none",selected = "none",multiple = T),
              tags$h3("Status",style = 'color: #008080'),
              hr_main(),
              verbatimTextOutput(ns("object_pos.outlier"))
            ),

            tabPanel(
              title = 'Negative',height = '500px',width = "100%",
              icon = icon('minus'),
              tags$h3("Outliers",style = 'color: #008080'),
              hr_main(),
              DT::dataTableOutput(ns("outlier.neg")),
              tags$h3("Missing value",style = 'color: #008080'),
              hr_main(),
              jqui_resizable(
                uiOutput(ns("outlier.neg_plt"),fill = T)
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
              tags$h3("Pick outliers",style = 'color: #008080'),
              hr_main(),
              selectInput(inputId = ns('out_vari_neg'),label = "Select sample_id",choices = "none",selected = "none",multiple = T),
              tags$h3("Status",style = 'color: #008080'),
              hr_main(),
              verbatimTextOutput(ns("object_neg.outlier"))
            )
          )
        )
      )
    )
  )

}


#' remove outliers
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs toggle
#' @importFrom dplyr select all_of left_join
#' @importFrom tibble rownames_to_column
#' @importFrom massdataset activate_mass_dataset
#' @importFrom ggplot2 ggtitle
#' @importFrom masscleaner detect_outlier extract_outlier_table
#' @importFrom plotly renderPlotly plotlyOutput
#' @param id module of server
#' @param volumes shinyFiles volumes
#' @param prj_init use project init variables.
#' @param data_clean_rv reactivevalues p2 dataclean
#' @noRd


data_rm_outlier_server <- function(id,volumes,prj_init,data_clean_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$toggleSidebar, {
      shinyjs::toggle(id = "Sidebar")
    })

    #> remove outlier
    p2_mv_outlier <- reactiveValues(data = NULL)

    observe({
      updateSelectInput(session, "outlier_color_by",choices = colnames(prj_init$sample_info),selected = colnames(prj_init$sample_info)[3])
      updateSelectInput(session, "outlier_order_by",choices = colnames(prj_init$sample_info),selected = colnames(prj_init$sample_info)[2])
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
        fig2_format = as.character(input$fig2_format)
      )
    })

    observeEvent(
      input$outlier_start,
      {
        if(is.null(prj_init$sample_info)) {return()}
        if(!is.null(prj_init$object_negative.init) & !is.null(prj_init$object_positive.init) & prj_init$steps == "Remove outlier"){
          p2_mv_outlier$object_neg.mv = prj_init$object_negative.init %>%
            activate_mass_dataset('sample_info') %>%
            dplyr::select('sample_id') %>%
            dplyr::left_join(prj_init$sample_info)
          p2_mv_outlier$object_pos.mv = prj_init$object_positive.init %>%
            activate_mass_dataset('sample_info') %>%
            dplyr::select('sample_id') %>%
            dplyr::left_join(prj_init$sample_info)
        } else {
          if(is.null(data_clean_rv$object_pos.mv)) {return()}
          if(is.null(data_clean_rv$object_neg.mv)) {return()}
          p2_mv_outlier$object_neg.mv = data_clean_rv$object_neg.mv
          p2_mv_outlier$object_pos.mv = data_clean_rv$object_pos.mv
        }

        p2_mv_outlier$colby = input$outlier_color_by %>% as.character()
        p2_mv_outlier$orderby = input$outlier_order_by %>% as.character()

        #> parameters

        temp_na_percentage_cutoff = input$na_percentage_cutoff %>% as.numeric()
        temp_sd_fold_change = input$sd_fold_change %>% as.numeric()
        temp_mad_fold_change = input$mad_fold_change %>% as.numeric()
        temp_dist_p_cutoff = input$dist_p_cutoff %>% as.numeric()


        p2_mv_outlier$outlier_samples.pos <-
          p2_mv_outlier$object_pos.mv %>%
          `+`(1) %>%
          log(2) %>%
          scale() %>%
          detect_outlier(na_percentage_cutoff = temp_na_percentage_cutoff,
                         sd_fold_change = temp_sd_fold_change,
                         mad_fold_change = temp_sd_fold_change,
                         dist_p_cutoff = temp_dist_p_cutoff) %>%
          extract_outlier_table() %>%
          rownames_to_column('sample_id')

        p2_mv_outlier$s_id.pos <- c("none", p2_mv_outlier$outlier_samples.pos$sample_id)


        output$outlier.pos = renderDataTable_formated(
          actions = input$outlier_start,
          condition1 = p2_mv_outlier$outlier_samples.pos,
          tbl = p2_mv_outlier$outlier_samples.pos,
          filename.a = "3.6.3.outliers.pos"
        )

        p2_mv_outlier$outlier_samples.neg <-
          p2_mv_outlier$object_neg.mv %>%
          `+`(1) %>%
          log(2) %>%
          scale() %>%
          detect_outlier(na_percentage_cutoff = temp_na_percentage_cutoff,
                         sd_fold_change = temp_sd_fold_change,
                         mad_fold_change = temp_sd_fold_change,
                         dist_p_cutoff = temp_dist_p_cutoff) %>%
          extract_outlier_table() %>%
          rownames_to_column('sample_id')

        p2_mv_outlier$s_id.neg <- c("none", p2_mv_outlier$outlier_samples.neg$sample_id)
        output$outlier.neg = renderDataTable_formated(
          actions = input$outlier_start,
          condition1 = p2_mv_outlier$outlier_samples.neg,
          tbl = p2_mv_outlier$outlier_samples.neg,
          filename.a = "3.6.3.outliers.neg"
        )


        #> mv plot original pos
        output$outlier.pos_plt <- renderUI({
          plot_type <- input$outlier_plt_format
          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_outlier.pos"))
          } else {
            plotOutput(outputId = ns("plot_outlier.pos"))
          }
        })
        print(p2_mv_outlier$orderby)

        p2_mv_outlier$plt_mv_ori.pos = check_mv(object = p2_mv_outlier$object_pos.mv,
                                                colby = p2_mv_outlier$colby,
                                                orderby = p2_mv_outlier$orderby,
                                                type = 'plot')

        output$plot_outlier.pos <- renderPlot({
          if(is.null(input$outlier_start)){return()}
          if(is.null(p2_mv_outlier$plt_mv_ori.pos)){return()}
          p2_mv_outlier$plt_mv_ori.pos
        })


        output$plotly_outlier.pos <- renderPlotly({
          if(is.null(input$outlier_start)){return()}
          if(is.null(p2_mv_outlier$plt_mv_ori.pos)){return()}
          plotly::ggplotly(p2_mv_outlier$plt_mv_ori.pos)
        })


        #> mv plot original neg
        output$outlier.neg_plt <- renderUI({
          plot_type <- input$outlier_plt_format

          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_outlier.neg"))
          } else {
            plotOutput(outputId = ns("plot_outlier.neg"))
          }
        })
        print(p2_mv_outlier$orderby)

        p2_mv_outlier$plt_mv_ori.neg = check_mv(object = p2_mv_outlier$object_neg.mv,
                                                colby = p2_mv_outlier$colby,
                                                orderby = p2_mv_outlier$orderby,
                                                type = 'plot')

        output$plot_outlier.neg <- renderPlot({
          if(is.null(input$outlier_start)){return()}
          if(is.null(p2_mv_outlier$plt_mv_ori.neg)){return()}
          p2_mv_outlier$plt_mv_ori.neg
        })

        output$plotly_outlier.neg <- renderPlotly({
          if(is.null(input$outlier_start)){return()}
          if(is.null(p2_mv_outlier$plt_mv_ori.neg)){return()}
          plotly::ggplotly(p2_mv_outlier$plt_mv_ori.neg)
        })
      }
    )

    observe({
      updateSelectInput(session, "out_vari_pos",choices = p2_mv_outlier$s_id.pos,selected = p2_mv_outlier$s_id.pos[1])
      updateSelectInput(session, "out_vari_neg",choices = p2_mv_outlier$s_id.neg,selected = p2_mv_outlier$s_id.neg[1])
    })

    observeEvent(
      input$rm_outliers,
      {
        if(is.null(p2_mv_outlier$outlier_samples.neg)) {return()}
        if(is.null(p2_mv_outlier$outlier_samples.pos)) {return()}
        p2_mv_outlier$pos_out_sid = input$out_vari_pos %>% as.character()
        p2_mv_outlier$neg_out_sid = input$out_vari_neg %>% as.character()


        #> remove outlier in pos
        if(length(p2_mv_outlier$pos_out_sid) == 1 & "none" %in% p2_mv_outlier$pos_out_sid) {
          p2_mv_outlier$object_pos.outlier = p2_mv_outlier$object_pos.mv
          p2_mv_outlier$merge.pos = p2_mv_outlier$plt_mv_ori.pos
        } else {
          temp_pos_sid = p2_mv_outlier$pos_out_sid[p2_mv_outlier$pos_out_sid != "none"]
          p2_mv_outlier$object_pos.outlier <-
            p2_mv_outlier$object_pos.mv %>%
            activate_mass_dataset('expression_data') %>%
            select(-all_of(temp_pos_sid))
          p2_mv_outlier$plt_mv_ori.pos.after = check_mv(object = p2_mv_outlier$object_pos.outlier,
                                                        colby = p2_mv_outlier$colby,
                                                        orderby = p2_mv_outlier$orderby,
                                                        type = 'plot')
          p2_mv_outlier$merge.pos = ((p2_mv_outlier$plt_mv_ori.pos+ggtitle('remove noise (before)'))/(p2_mv_outlier$plt_mv_ori.pos.after+ggtitle("remove outlier (after)")))
        }


        #> remove outlier in neg
        if(length(p2_mv_outlier$neg_out_sid) == 1 & "none" %in% p2_mv_outlier$neg_out_sid) {
          p2_mv_outlier$object_neg.outlier = p2_mv_outlier$object_neg.mv
          p2_mv_outlier$merge.neg = p2_mv_outlier$plt_mv_ori.neg
        } else {
          temp_neg_sid = p2_mv_outlier$neg_out_sid[p2_mv_outlier$neg_out_sid != "none"]
          p2_mv_outlier$object_neg.outlier <-
            p2_mv_outlier$object_neg.mv %>%
            activate_mass_dataset('expression_data') %>%
            select(-all_of(temp_neg_sid))
          p2_mv_outlier$plt_mv_ori.neg.after = check_mv(object = p2_mv_outlier$object_neg.outlier,colby = p2_mv_outlier$colby,orderby = p2_mv_outlier$orderby,type = 'plot')
          p2_mv_outlier$merge.neg = ((p2_mv_outlier$plt_mv_ori.neg+ggtitle('remove noise (before)'))/(p2_mv_outlier$plt_mv_ori.neg.after+ggtitle("remove outlier (after)")))
        }

        # print("check2")
        #> update plot
        output$outlier.pos_plt <- renderUI({
          plot_type <- input$outlier_plt_format
          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_outlier.pos"))
          } else {
            plotOutput(outputId = ns("plot_outlier.pos"))
          }
        })

        output$plot_outlier.pos <- renderPlot({
          if(is.null(input$rm_outliers)){return()}
          if(is.null(p2_mv_outlier$merge.pos)){return()}
          p2_mv_outlier$merge.pos
        })

        output$plotly_outlier.pos <- renderPlotly({
          if(is.null(input$rm_outliers)){return()}
          if(is.null(p2_mv_outlier$merge.pos)){return()}
          plotly::ggplotly(p2_mv_outlier$merge.pos)
        })

        #> mv plot original neg
        output$outlier.neg_plt <- renderUI({
          plot_type <- input$outlier_plt_format

          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_outlier.neg"))
          } else {
            plotOutput(outputId = ns("plot_outlier.neg"))
          }
        })

        output$plot_outlier.neg <- renderPlot({
          if(is.null(input$rm_outliers)){return()}
          if(is.null(p2_mv_outlier$merge.neg)){return()}
          p2_mv_outlier$merge.neg
        })

        output$plotly_outlier.neg <- renderPlotly({
          if(is.null(input$rm_outliers)){return()}
          if(is.null(p2_mv_outlier$merge.neg)){return()}
          plotly::ggplotly(p2_mv_outlier$merge.neg)
        })

        #>

        save_massobj(
          polarity = 'positive',
          file_path = paste0(prj_init$wd,"/Result/POS/Objects/"),
          stage = 'outlier',
          obj = p2_mv_outlier$object_pos.outlier)

        save_massobj(
          polarity = 'negative',
          file_path = paste0(prj_init$wd,"/Result/NEG/Objects/"),
          stage = 'outlier',
          obj = p2_mv_outlier$object_neg.outlier)

        #> information of mass datasets
        output$object_pos.outlier = renderPrint({
          print(p2_mv_outlier$object_pos.outlier)
        })
        output$object_neg.outlier = renderPrint({
          print(p2_mv_outlier$object_neg.outlier)
        })
        data_clean_rv$object_pos.outlier <- p2_mv_outlier$object_pos.outlier
        data_clean_rv$object_neg.outlier <- p2_mv_outlier$object_neg.outlier
      }
    )

    # download ----------------------------------------------------------------
    ###> fig1 =====
    output$fig1_download = downloadHandler(
      filename = function() {
        paste0("01.mv_remove_outliers-pos.", download_para()$fig1_format)
      },
      content = function(file) {
        # extract parameters
        para <- plot1_para()
        para_d <- download_para()

        # draw condition
        p = p2_mv_outlier$plt_mv_ori.pos

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
        paste0("01.mv_remove_outliers-neg.", download_para()$fig2_format)
      },
      content = function(file) {
        # extract parameters
        para <- plot2_para()
        para_d <- download_para()

        # draw condition
        p = p2_mv_outlier$plt_mv_ori.neg

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
  })
}

