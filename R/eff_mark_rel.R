#' Get Fulcrum trap efficiency marking and release tables
#'
#'
#' @md
#' @param table       Fulcrum table: main, marking, release, fl
#'
#' @export
#'

get_emr <- function(table = c("main", "marking", "release", "fl")){
  tp_tables = c("main" = "JSCS Efficiency Marking and Release 2024",
                "marking" = "JSCS Efficiency Marking and Release 2024/marking",
                "release" = "JSCS Efficiency Marking and Release 2024/release",
                "fl" = "JSCS Efficiency Marking and Release 2024/fork_lengths")
  fulcrumr::fulcrum_table(tp_tables[[table]])
}

#' Prepare Fulcrum efficiency marking and release main table
#'
#'
#' @md
#' @param emr_main_raw     Unprocessed marking and release main table
#'
#' @export
#'

prep_emr_main <- function(emr_main_raw){
  emr_main_raw |>
    dplyr::select(emr_main_id = `_record_id`, date_mark, date_release)
}

#' Prepare Fulcrum efficiency marking table
#'
#'
#' @md
#' @param emr_mark_raw     Unprocessed efficiency marking table
#' @param emr_main         Processed marking and release main table
#'
#' @export
#'

prep_emr_mark <- function(emr_mark_raw, emr_main){
  emr_mark_raw |>
    dplyr::rename(emr_mark_id = `_child_record_id`, emr_main_id = `_parent_id`) |>
    dplyr::select(!starts_with("_")) |>
    dplyr::left_join(dplyr::select(emr_main, emr_main_id, date = date_mark))
}

#' Prepare Fulcrum efficiency marking fork length table
#'
#'
#' @md
#' @param emr_fl_raw       Unprocessed efficiency marking fork length table
#' @param emr_mark         Processed marking table
#'
#' @export
#'

prep_emr_fl <- function(emr_fl_raw, emr_mark){
  emr_fl_raw |>
    dplyr::rename(emr_fl_id = `_child_record_id`, emr_mark_id = `_parent_id`) |>
    dplyr::select(!starts_with("_")) |>
    dplyr::left_join(dplyr::select(emr_mark, emr_mark_id, date = date_mark, release_batch))
}

#' Prepare Fulcrum efficiency release table
#'
#'
#' @md
#' @param emr_rel_raw     Unprocessed efficiency release table
#' @param emr_main        Processed marking and release main table
#'
#' @export
#'

prep_emr_rel <- function(emr_rel_raw, emr_main){
  emr_rel_raw |>
    dplyr::rename(emr_rel_id = `_child_record_id`, emr_main_id = `_parent_id`) |>
    dplyr::select(!starts_with("_")) |>
    dplyr::left_join(dplyr::select(emr_main, emr_main_id, date = date_release))|>
    dplyr::left_join(data.frame(release_site = c("Standard", "Near"),
                                LC = c(TRUE, FALSE))) |>
    dplyr::mutate(BBY = TRUE)
}
