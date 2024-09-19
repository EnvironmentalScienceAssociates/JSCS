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
    dplyr::select(ss_main_id = `_record_id`, date, sampling_affiliation)
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
    dplyr::select(ss_predator_id = `_child_record_id`, ss_main_id = `_parent_id`,
                  species = predator_species, lifestage = predator_lifestage,
                  fl_mm, mark_status, fish_condition, disposition, catch_method,
                  stomach_method, stomach_content_disposition) |>
    dplyr::left_join(ss_main)
}

#' Prepare Fulcrum stomach contents table
#'
#'
#' @md
#' @param ss_contents_raw     Unprocessed stomach contents table
#' @param ss_main             Processed stomach sampling main table
#'
#' @export
#'

prep_ss_contents <- function(ss_contents_raw, ss_main){
  ss_contents_raw |>
    dplyr::select(ss_contents_id = `_child_record_id`, ss_main_id = `_parent_id`,
                  species = stomach_species, fl_mm = stomach_fl_mm,
                  mark_status = stomach_mark_status) |>
    dplyr::left_join(ss_main)
}

