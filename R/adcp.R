#' Get Fulcrum ADCP tables
#'
#'
#' @md
#' @param table       Fulcrum table: main, transect
#'
#' @export
#'

get_adcp <- function(table = c("main", "transect")){
  adcp_tables = c("main" = "JSCS ADCP 2024",
                  "transect" = "JSCS ADCP 2024/transects")
  fulcrumr::fulcrum_table(adcp_tables[[table]])
}

#' Prepare Fulcrum ADCP main table
#'
#'
#' @md
#' @param adcp_main_raw     Unprocessed ADCP main table
#'
#' @export
#'

prep_adcp_main <- function(adcp_main_raw){
  adcp_main_raw |>
    dplyr::rename(adcp_main_id = `_record_id`) |>
    dplyr::select(!dplyr::starts_with("_") & !dplyr::contains("photos")) |>
    dplyr::mutate(adcp_clock_time = prep_time_fulcrum(adcp_clock_time)) |>
    dplyr::relocate(adcp_main_id, .after = dplyr::last_col())  |>
    dplyr::arrange(desc(date))
}

#' Prepare Fulcrum ADCP transect table
#'
#'
#' @md
#' @param adcp_transect_raw          Unprocessed ADCP transect table
#' @param adcp_main                  Processed ADCP main table
#'
#' @export
#'

prep_adcp_transect <- function(adcp_transect_raw, adcp_main){
  adcp_transect_raw |>
    dplyr::rename(adcp_transect_id = `_child_record_id`, adcp_main_id = `_parent_id`) |>
    dplyr::select(!dplyr::starts_with("_")) |>
    dplyr::left_join(dplyr::select(adcp_main, adcp_main_id, date)) |>
    dplyr::relocate(date, .before = adcp_transect_id) |>
    dplyr::relocate(adcp_transect_id, adcp_main_id, .after = dplyr::last_col()) |>
    dplyr::arrange(desc(date))
}
