#' Get Fulcrum trap efficiency transport and tempering tables
#'
#'
#' @md
#' @param table       Fulcrum table: main, transport, transport_details, tempering
#'
#' @export
#'

get_ett <- function(table = c("main", "transport", "transport_details", "tempering")){
  ett_tables = c("main" = "JSCS Efficiency Transport and Tempering 2024",
                 "transport" = "JSCS Efficiency Transport and Tempering 2024/transport",
                 "transport_details" = "JSCS Efficiency Transport and Tempering 2024/transport_details",
                 "tempering" = "JSCS Efficiency Transport and Tempering 2024/tempering")
  fulcrumr::fulcrum_table(ett_tables[[table]])
}

#' Prepare Fulcrum efficiency transport and tempering main table
#'
#'
#' @md
#' @param ett_main_raw     Unprocessed transport and tempering main table
#'
#' @export
#'

prep_ett_main <- function(ett_main_raw){
  ett_main_raw |>
    dplyr::rename(ett_main_id = `_record_id`) |>
    dplyr::select(!dplyr::starts_with("_") & !dplyr::contains("photos")) |>
    dplyr::relocate(ett_main_id, .after = dplyr::last_col()) |>
    dplyr::arrange(dplyr::desc(date))
}

#' Prepare Fulcrum efficiency transport table
#'
#'
#' @md
#' @param ett_transport_raw     Unprocessed efficiency transport table
#' @param ett_td_raw            Unprocessed efficiency transport details table
#' @param ett_main              Processed transport and tempering main table
#'
#' @export
#'

prep_ett_transport <- function(ett_transport_raw, ett_td_raw, ett_main){
  trans = ett_transport_raw |>
    dplyr::rename(ett_transport_id = `_child_record_id`, ett_main_id = `_parent_id`) |>
    dplyr::select(!starts_with("_"))

  td = ett_td_raw |>
    dplyr::rename(ett_td_id = `_child_record_id`, ett_transport_id = `_parent_id`) |>
    dplyr::select(!starts_with("_"))

  td |>
    dplyr::left_join(trans) |>
    dplyr::left_join(dplyr::select(ett_main, ett_main_id, date)) |>
    dplyr::mutate(datetime = prep_dt_fulcrum(date, time)) |>
    dplyr::select(-time) |>
    dplyr::relocate(date, datetime, .before = ett_td_id) |>
    dplyr::relocate(ett_td_id, ett_transport_id, ett_main_id, .after = dplyr::last_col()) |>
    dplyr::arrange(dplyr::desc(datetime))
}

#' Prepare Fulcrum efficiency tempering table
#'
#'
#' @md
#' @param ett_tempering_raw     Unprocessed efficiency tempering table
#' @param ett_main              Processed transport and tempering main table
#'
#' @export
#'

prep_ett_tempering <- function(ett_tempering_raw, ett_main){
  ett_tempering_raw |>
    dplyr::rename(ett_transport_id = `_child_record_id`, ett_main_id = `_parent_id`) |>
    dplyr::left_join(dplyr::select(ett_main, ett_main_id, date)) |>
    dplyr::mutate(datetime = prep_dt_fulcrum(date, tempering_time)) |>
    dplyr::select(date, datetime, release, location = tempering_location,
                  fish_condition = tempering_fish_condition, temp_c = tempering_temp_c,
                  do_sat = tempering_do_sat, do_mgl = tempering_do_mgl,
                  notes = tempering_notes, ett_transport_id, ett_main_id) |>
    dplyr::arrange(dplyr::desc(datetime))
}
