#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
  # Call module server functions
  if(Sys.info()["sysname"] == "Windows") {
    volumes = getVolumes_win()
  } else {
    volumes = shinyFiles::getVolumes()()
  }
  #> project init
  prj_init <- reactiveValues(data = NULL) # project init
  project_init_server(id = "project_init_id",volumes = volumes,prj_init)
  #> download reactive value
  downloads <- reactiveValues(data = NULL)
  #> Data import
  ##> from raw data
  data_import_rv <- reactiveValues(data = NULL)
  data_import_raw_server(
    id = "data_import_raw_id",
    volumes = volumes,
    prj_init = prj_init,
    data_import_rv = data_import_rv
  )
  ##> from peak picking table
  data_import_tbl_server(
    id = "data_import_tbl_id",
    volumes = volumes,
    prj_init = prj_init,
    data_import_rv = data_import_rv
  )
  ##> from mass_dataset
  data_import_massdataset_server(
    id = "data_import_massdataset_id",
    volumes = volumes,
    prj_init = prj_init,
    data_import_rv = data_import_rv
  )
  #> Data clean
  data_clean_rv <- reactiveValues(data = NULL)
  data_overview_server(
    id = "data_overview_id",
    volumes = volumes,
    prj_init = prj_init,
    data_import_rv = data_import_rv,
    data_clean_rv = data_clean_rv,
    downloads = downloads
  )
  ##> remove noise
  data_rm_noise_server(
    id = "data_rm_noise_id",
    volumes = volumes,
    prj_init = prj_init,
    data_clean_rv = data_clean_rv
  )
  ##> detected outlier
  data_rm_outlier_server(
    id = "data_rm_outlier_id",
    volumes = volumes,
    prj_init = prj_init,
    data_clean_rv = data_clean_rv
  )
  ##> missing value imputation
  data_mv_impute_server(
    id = "data_mv_impute_id",
    volumes = volumes,
    prj_init = prj_init,
    data_clean_rv = data_clean_rv
  )
  ##> data normalization
  data_normalize_server(
    id = "data_normalize_id",
    volumes = volumes,
    prj_init = prj_init,
    data_clean_rv = data_clean_rv
  )
  #> annotation server
  ##> annotation
  annotation_server(
    id = "annotation_id",
    volumes = volumes,
    prj_init = prj_init,
    data_clean_rv = data_clean_rv
  )
  ##> annotation filter
  annotation_filter_server(
    id = "annotation_filter_id",
    volumes = volumes,
    prj_init = prj_init,
    data_clean_rv = data_clean_rv
  )
  ##> data merge
  ##> annotation filter
  data_merge_server(
    id = "data_merge_id",
    volumes = volumes,
    prj_init = prj_init,
    data_clean_rv = data_clean_rv
  )
  #> metabolite marker picking
  ##> DAM analysis
  data_class_server(
    id = "data_class_id",
    volumes = volumes,
    prj_init = prj_init,
    data_clean_rv = data_clean_rv
  )
  ##> DAM analysis
  dam_res_server(
    id = "dam_res_id",
    volumes = volumes,
    prj_init = prj_init,
    data_clean_rv = data_clean_rv
  )
  ##> DAM analysis
  data_enrich_server(
    id = "data_enrich_id",
    volumes = volumes,
    prj_init = prj_init,
    data_clean_rv = data_clean_rv
  )




}
