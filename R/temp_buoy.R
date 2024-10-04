#' Get Fulcrum temperature buoy tables
#'
#'
#' @md
#' @param table       Fulcrum table: main, buoy, sensor
#'
#' @export
#'

get_tb <- function(table = c("main", "buoy", "sensor")){
  tb_tables = c("main" = "JSCS Temperature Buoy 2024",
                "buoy" = "JSCS Temperature Buoy 2024/buoy_data",
                "sensor" = "JSCS Temperature Buoy 2024/sensor_data")
  fulcrumr::fulcrum_table(tb_tables[[table]])
}

#' Prepare Fulcrum temperature buoy main table
#'
#'
#' @md
#' @param tb_main_raw     Unprocessed temperature buoy main table
#'
#' @export
#'

prep_tb_main <- function(tb_main_raw){
  tb_main_raw |>
    dplyr::rename(tb_main_id = `_record_id`) |>
    dplyr::select(!dplyr::starts_with("_") & !dplyr::contains("photos")) |>
    dplyr::mutate(start_time = prep_dt_fulcrum(date, start_time)) |>
    dplyr::relocate(tb_main_id, .after = dplyr::last_col())  |>
    dplyr::arrange(dplyr::desc(date))
}

#' Prepare Fulcrum temperature buoy table
#'
#'
#' @md
#' @param tb_buoy_raw            Unprocessed temperature buoy table
#' @param tb_main                Processed temperature buoy main table
#'
#' @export
#'

prep_tb_buoy <- function(tb_buoy_raw, tb_main){
  tb_buoy_raw |>
    dplyr::rename(tb_buoy_id = `_child_record_id`, tb_main_id = `_parent_id`) |>
    dplyr::select(!dplyr::starts_with("_")) |>
    dplyr::left_join(dplyr::select(tb_main, tb_main_id, date)) |>
    dplyr::mutate(time_retrieved_pst = prep_time_fulcrum(time_retrieved_pst),
                  time_deployed_pst = prep_time_fulcrum(time_deployed_pst)) |>
    dplyr::relocate(date, .before = tb_buoy_id) |>
    dplyr::relocate(tb_buoy_id, tb_main_id, .after = dplyr::last_col()) |>
    dplyr::arrange(dplyr::desc(date))
}

#' Prepare Fulcrum temperature sensor table
#'
#'
#' @md
#' @param tb_sensor_raw          Unprocessed temperature sensor table
#' @param tb_buoy                Processed temperature buoy table
#'
#' @export
#'

prep_tb_sensor <- function(tb_sensor_raw, tb_buoy){
  tb_sensor_raw |>
    dplyr::rename(tb_sensor_id = `_child_record_id`, tb_buoy_id = `_parent_id`) |>
    dplyr::select(!dplyr::starts_with("_")) |>
    dplyr::left_join(dplyr::select(tb_buoy, tb_buoy_id, date, location)) |>
    dplyr::relocate(date, location, .before = tb_sensor_id) |>
    dplyr::relocate(tb_sensor_id, tb_buoy_id, .after = dplyr::last_col()) |>
    dplyr::arrange(dplyr::desc(date))
}
