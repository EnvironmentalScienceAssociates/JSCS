#' Get Fulcrum trap general comments and tables
#'
#'
#' @md
#' @param table       Fulcrum table: main, water_quality
#'
#' @export
#'

get_gct <- function(table = c("main", "water_quality")){
  gct_tables = c("main" = "JSCS General Comments and Transport 2024",
                 "water_quality" = "JSCS General Comments and Transport 2024/transport_water_quality")
  fulcrumr::fulcrum_table(gct_tables[[table]])
}


#' Prepare Fulcrum general comments and transport main table
#'
#'
#' @md
#' @param gct_main_raw     Unprocessed comments and transport main table
#'
#' @export
#'

prep_gct_main <- function(gct_main_raw){
  gct_main_raw |>
    dplyr::rename(gct_main_id = `_record_id`) |>
    dplyr::select(!dplyr::starts_with("_") & !dplyr::contains("photos")) |>
    dplyr::relocate(gct_main_id, .after = dplyr::last_col()) |>
    dplyr::arrange(desc(date))
}

#' Prepare Fulcrum general comments table
#'
#'
#' @md
#' @param gct_main     Processed comments and transport main table
#'
#' @export
#'

prep_gct_gc <- function(gct_main){
  gct_main |>
    dplyr::select(date, general_comments) |>
    dplyr::arrange(desc(date))
}

#' Prepare Fulcrum transport main table
#'
#'
#' @md
#' @param gct_main     Processed comments and transport main table
#'
#' @export
#'

prep_gct_transport <- function(gct_main){
  gct_main |>
    dplyr::select(-general_comments) |>
    dplyr::arrange(desc(date))
}

#' Prepare Fulcrum transport water quality table
#'
#'
#' @md
#' @param gct_wq_raw       Unprocessed transport water quality table
#' @param gct_main         Processed transport  main table
#'
#' @export
#'

prep_gct_transport_wq <- function(gct_wq_raw, gct_transport){
  gct_wq_raw |>
    dplyr::rename(gct_wq_id = `_child_record_id`, gct_main_id = `_parent_id`) |>
    dplyr::left_join(dplyr::select(gct_transport, gct_main_id, date)) |>
    dplyr::mutate(datetime = prep_dt_fulcrum(date, time)) |>
    dplyr::select(date, datetime, location = water_quality_location,
                  temp_c, do_sat, do_mgl, gct_wq_id, gct_main_id) |>
    dplyr::arrange(desc(datetime))
}


