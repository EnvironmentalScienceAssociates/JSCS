#' Get Fulcrum YSI tables
#'
#'
#' @md
#' @param table       Fulcrum table: main, sample, depth
#'
#' @export
#'

get_ysi <- function(table = c("main", "sample", "depth")){
  ysi_tables = c("main" = "JSCS YSI Profiles 2024",
                 "sample" = "JSCS YSI Profiles 2024/ysi_samples",
                 "depth" = "JSCS YSI Profiles 2024/depth_recordings")
  fulcrumr::fulcrum_table(ysi_tables[[table]])
}

#' Prepare Fulcrum YSI main table
#'
#'
#' @md
#' @param ysi_main_raw     Unprocessed YSI main table
#'
#' @export
#'

prep_ysi_main <- function(ysi_main_raw){
  ysi_main_raw |>
    dplyr::rename(ysi_main_id = `_record_id`) |>
    dplyr::select(!dplyr::starts_with("_") & !dplyr::contains("photos")) |>
    dplyr::mutate(clock_time_pst = lubridate::seconds_to_period(clock_time_pst)) |>
    dplyr::relocate(ysi_main_id, .after = dplyr::last_col())  |>
    dplyr::arrange(desc(date))
}

#' Prepare Fulcrum YSI sample table
#'
#'
#' @md
#' @param ysi_sample_raw            Unprocessed YSI sample table
#' @param ysi_main                  Processed YSI main table
#'
#' @export
#'

prep_ysi_sample <- function(ysi_sample_raw, ysi_main){
  ysi_sample_raw |>
    dplyr::rename(ysi_sample_id = `_child_record_id`, ysi_main_id = `_parent_id`) |>
    dplyr::select(!dplyr::starts_with("_")) |>
    dplyr::left_join(dplyr::select(ysi_main, ysi_main_id, date)) |>
    dplyr::relocate(date, .before = ysi_sample_id) |>
    dplyr::relocate(ysi_sample_id, ysi_main_id, .after = dplyr::last_col()) |>
    dplyr::arrange(desc(date))
}

#' Prepare Fulcrum YSI depth table
#'
#'
#' @md
#' @param ysi_depth_raw            Unprocessed YSI depth table
#' @param ysi_sample               Processed YSI sample table
#'
#' @export
#'

prep_ysi_depth <- function(ysi_depth_raw, ysi_sample){
  ysi_depth_raw |>
    dplyr::rename(ysi_depth_id = `_child_record_id`, ysi_sample_id = `_parent_id`) |>
    dplyr::select(!dplyr::starts_with("_")) |>
    dplyr::left_join(dplyr::select(ysi_sample, ysi_sample_id, date, sample_location)) |>
    dplyr::mutate(start_time_pst = lubridate::seconds_to_period(start_time_pst)) |>
    dplyr::relocate(date, sample_location, .before = ysi_depth_id) |>
    dplyr::relocate(ysi_depth_id, ysi_sample_id, .after = dplyr::last_col()) |>
    dplyr::arrange(desc(date))
}
