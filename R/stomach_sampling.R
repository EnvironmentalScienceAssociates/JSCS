#' Get Fulcrum stomach sampling tables
#'
#'
#' @md
#' @param table       Fulcrum table: main, predator, contents
#'
#' @export
#'

get_ss <- function(table = c("main", "predator", "contents")){
  ss_tables = c("main" = "JSCS Stomach Sampling 2024",
                "predator" = "JSCS Stomach Sampling 2024/predator_data",
                "contents" = "JSCS Stomach Sampling 2024/stomach_contents")
  fulcrumr::fulcrum_table(ss_tables[[table]])
}

#' Prepare Fulcrum stomach sampling main table
#'
#'
#' @md
#' @param ss_main_raw     Unprocessed stomach sampling main table
#'
#' @export
#'

prep_ss_main <- function(ss_main_raw){
  ss_main_raw |>
    dplyr::rename(ss_main_id = `_record_id`) |>
    dplyr::select(!dplyr::starts_with("_") & !dplyr::contains("photos")) |>
    dplyr::mutate(datetime = prep_dt_fulcrum(date, time)) |>
    dplyr::select(-time) |>
    dplyr::relocate(datetime, .after = date) |>
    dplyr::relocate(ss_main_id, .after = dplyr::last_col())
}

#' Prepare Fulcrum predator table
#'
#'
#' @md
#' @param ss_predator_raw     Unprocessed predator table
#' @param ss_main             Processed stomach sampling main table
#'
#' @export
#'

prep_ss_predator <- function(ss_predator_raw, ss_main){
  ss_predator_raw |>
    dplyr::rename(ss_predator_id = `_child_record_id`, ss_main_id = `_parent_id`) |>
    dplyr::select(!dplyr::starts_with("_")) |>
    dplyr::left_join(dplyr::select(ss_main, ss_main_id, date)) |>
    dplyr::relocate(date, .before = ss_predator_id) |>
    dplyr::relocate(ss_predator_id, ss_main_id, .after = dplyr::last_col())
}

#' Prepare Fulcrum stomach contents table
#'
#'
#' @md
#' @param ss_contents_raw     Unprocessed stomach contents table
#' @param ss_predator         Processed stomach sampling predator table
#'
#' @export
#'

prep_ss_contents <- function(ss_contents_raw, ss_predator){
  ss_contents_raw |>
    dplyr::rename(ss_contents_id = `_child_record_id`, ss_predator_id = `_parent_id`) |>
    dplyr::select(!dplyr::starts_with("_")) |>
    dplyr::left_join(dplyr::select(ss_predator, ss_predator_id, date)) |>
    dplyr::relocate(date, .before = ss_contents_id) |>
    dplyr::relocate(ss_contents_id, ss_predator_id, .after = dplyr::last_col())
}

