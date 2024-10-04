#' Get Fulcrum trap platform tables
#'
#'
#' @md
#' @param table       Fulcrum table: main, perf_config, velocity, water_quality
#'
#' @export
#'

get_tp <- function(table = c("main", "perf_config", "velocity", "water_quality")){
  tp_tables = c("main" = "JSCS Trap Platform Activities 2024",
                "perf_config" = "JSCS Trap Platform Activities 2024/performance_and_configuration",
                "velocity" = "JSCS Trap Platform Activities 2024/velocity_data",
                "water_quality" = "JSCS Trap Platform Activities 2024/water_quality_data")
  fulcrumr::fulcrum_table(tp_tables[[table]])
}

#' Prepare Fulcrum trap platform main table
#'
#'
#' @md
#' @param tp_main_raw     Unprocessed trap platform main table
#'
#' @export
#'

prep_tp_main <- function(tp_main_raw){
  tp_main_raw |>
    dplyr::rename(tp_main_id = `_record_id`) |>
    dplyr::select(!dplyr::starts_with("_") & !dplyr::contains("photos")) |>
    dplyr::mutate(shift_start = prep_dt_fulcrum(date, shift_start),
                  shift_end = prep_dt_fulcrum(date, shift_end),
                  shift_hrs = round(as.numeric(difftime(shift_end, shift_start, units = "hours")), 2),
                  trap_close = prep_dt_fulcrum(date, trap_close),
                  trap_open = prep_dt_fulcrum(date, trap_open),
                  open_hrs = round(as.numeric(difftime(trap_open, trap_close, units = "hours")), 2)) |>
    dplyr::relocate(shift_hrs, .after = shift_end) |>
    dplyr::relocate(open_hrs, .after = trap_open) |>
    dplyr::relocate(tp_main_id, .after = dplyr::last_col())  |>
    dplyr::arrange(dplyr::desc(date))
}

#' Prepare Fulcrum performance and configuration table
#'
#'
#' @md
#' @param tp_perf_config_raw     Unprocessed performance and configuration table
#' @param tp_main                Processed trap platform main table
#'
#' @export
#'

prep_tp_perf_config <- function(tp_perf_config_raw, tp_main){
  tp_perf_config_raw |>
    dplyr::rename(tp_perf_config_id = `_child_record_id`, tp_main_id = `_parent_id`) |>
    dplyr::select(!dplyr::starts_with("_")) |>
    dplyr::left_join(dplyr::select(tp_main, tp_main_id, date, trap_check)) |>
    dplyr::relocate(date, trap_check, .before = tp_perf_config_id) |>
    dplyr::relocate(tp_perf_config_id, tp_main_id, .after = dplyr::last_col()) |>
    dplyr::arrange(dplyr::desc(date))
}

#' Prepare Fulcrum velocity table
#'
#'
#' @md
#' @param tp_velocity_raw     Unprocessed velocity table
#' @param tp_main             Processed trap platform main table
#'
#' @export
#'

prep_tp_velocity <- function(tp_velocity_raw, tp_main){
  tp_velocity_raw |>
    dplyr::rename(tp_velocity_id = `_child_record_id`, tp_main_id = `_parent_id`) |>
    dplyr::select(!starts_with("_"))  |>
    dplyr::mutate(velocity_fps = round(sqrt(u_fps^2 + v_fps^2 + w_fps^2), 2)) |>
    dplyr::left_join(dplyr::select(tp_main, tp_main_id, date, trap_check, debris_loading, trap_cleaned)) |>
    dplyr::relocate(date, trap_check, debris_loading, trap_cleaned, .before = tp_velocity_id) |>
    dplyr::relocate(tp_velocity_id, tp_main_id, .after = dplyr::last_col()) |>
    dplyr::arrange(dplyr::desc(date))
}

#' Prepare Fulcrum velocity table
#'
#'
#' @md
#' @param tp_wq_raw           Unprocessed water quality table
#' @param tp_main             Processed trap platform main table
#'
#' @export
#'

prep_tp_wq <- function(tp_wq_raw, tp_main){
  tp_wq_raw |>
    dplyr::rename(tp_wq_id = `_child_record_id`, tp_main_id = `_parent_id`) |>
    dplyr::select(!starts_with("_")) |>
    dplyr::left_join(dplyr::select(tp_main, tp_main_id, date, trap_check, debris_loading, trap_cleaned)) |>
    dplyr::relocate(date, trap_check, debris_loading, trap_cleaned, .before = tp_wq_id) |>
    dplyr::relocate(tp_wq_id, tp_main_id, .after = dplyr::last_col()) |>
    dplyr::arrange(dplyr::desc(date))
}
