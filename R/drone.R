#' Get Fulcrum drone tables
#'
#'
#' @md
#' @param table       Fulcrum table: main, flight
#'
#' @export
#'

get_drone <- function(table = c("main", "flight")){
  drone_tables = c("main" = "JSCS Drone 2024",
                   "flight" = "JSCS Drone 2024/flights")
  fulcrumr::fulcrum_table(drone_tables[[table]])
}


#' Prepare Fulcrum drone main table
#'
#'
#' @md
#' @param drone_main_raw     Unprocessed drone main table
#'
#' @export
#'

prep_drone_main <- function(drone_main_raw){
  drone_main_raw |>
    dplyr::rename(drone_main_id = `_record_id`) |>
    dplyr::select(!dplyr::starts_with("_") & !dplyr::contains("photos")) |>
    dplyr::mutate(uav_clock_time = prep_time_fulcrum(uav_clock_time)) |>
    dplyr::relocate(drone_main_id, .after = dplyr::last_col())  |>
    dplyr::arrange(dplyr::desc(date))
}

#' Prepare Fulcrum drone flight table
#'
#'
#' @md
#' @param drone_flight_raw           Unprocessed drone flight table
#' @param drone_main                 Processed drone main table
#'
#' @export
#'

prep_drone_flight <- function(drone_flight_raw, drone_main){
  drone_flight_raw |>
    dplyr::rename(drone_transect_id = `_child_record_id`, drone_main_id = `_parent_id`) |>
    dplyr::select(!dplyr::starts_with("_")) |>
    dplyr::left_join(dplyr::select(drone_main, drone_main_id, date)) |>
    dplyr::mutate(start_time = prep_time_fulcrum(start_time),
                  end_time = prep_time_fulcrum(end_time)) |>
    dplyr::relocate(date, .before = drone_transect_id) |>
    dplyr::relocate(drone_transect_id, drone_main_id, .after = dplyr::last_col()) |>
    dplyr::arrange(dplyr::desc(date))
}
