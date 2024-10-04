#' Get Fulcrum fish processing tables
#'
#'
#' @md
#' @param table       Fulcrum table: main, fish, plus
#'
#' @export
#'

get_fp <- function(table = c("main", "fish", "plus")){
  fp_tables = c("main" = "JSCS Fish Processing 2024",
                "fish" = "JSCS Fish Processing 2024/fish_measurements",
                "plus" = "JSCS Fish Processing 2024/plus_counts")
  fulcrumr::fulcrum_table(fp_tables[[table]])
}


#' Prepare Fulcrum fish processing main table
#'
#'
#' @md
#' @param fp_main_raw     Unprocessed fish processing main table
#'
#' @export
#'

prep_fp_main <- function(fp_main_raw){
  fp_main_raw |>
    dplyr::rename(fp_main_id = `_record_id`) |>
    dplyr::select(!dplyr::starts_with("_") & !dplyr::contains("photos")) |>
    dplyr::relocate(dplyr::contains("id"), .after = dplyr::last_col()) |>
    dplyr::arrange(dplyr::desc(date))
}

#' Prepare Fulcrum fish measurements table
#'
#'
#' @md
#' @param fp_fish_raw     Unprocessed fish measurements table
#' @param fp_main         Processed fish processing main table
#'
#' @export
#'

prep_fp_fish <- function(fp_fish_raw, fp_main){
  fp_fish_raw |>
    dplyr::rename(fp_fish_id = `_child_record_id`, fp_main_id = `_parent_id`) |>
    dplyr::left_join(dplyr::select(fp_main, fp_main_id, date, trap_check)) |>
    dplyr::select(date, trap_check, species = species_measure, lifestage = lifestage_measure,
                  measure_type, fl_mm, fish_viewer_id, mark_status = mark_status_measure,
                  pit_tag_id, floy_tag_id, fish_condition = fish_condition_measure, stomach_sampled,
                  disposition, fp_fish_id, fp_main_id) |>
    dplyr::arrange(dplyr::desc(date))
}

#' Prepare Fulcrum plus counts table
#'
#'
#' @md
#' @param fp_plus_raw     Unprocessed plus counts table
#' @param fp_main         Processed fish processing main table
#'
#' @export
#'

prep_fp_plus <- function(fp_plus_raw, fp_main){
  fp_plus_raw |>
    dplyr::rename(fp_plus_id = `_child_record_id`, fp_main_id = `_parent_id`) |>
    dplyr::left_join(dplyr::select(fp_main, fp_main_id, date, trap_check)) |>
    dplyr::select(date, trap_check, species = species_plus, lifestage = lifestage_plus,
                  mark_status = mark_status_plus, fish_condition = fish_condition_plus,
                  count = plus_count, fp_plus_id, fp_main_id) |>
    dplyr::arrange(dplyr::desc(date))
}
