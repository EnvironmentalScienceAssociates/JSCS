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
    dplyr::select(fp_main_id = `_record_id`, date, trap_check_number)
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
    dplyr::select(fp_fish_id = `_child_record_id`, fp_main_id = `_parent_id`,
                  species = species_measure, lifestage = lifestage_measure,
                  measure_type, fl_mm = fork_length_mm, fish_viewer_id,
                  mark_status = mark_status_measure, pit_tag_id, floy_tag_id,
                  fish_condition = fish_condition_measure, stomach_sampled, disposition) |>
    dplyr::mutate(count = 1) |>
    dplyr::left_join(fp_main)
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
    dplyr::select(fp_plus_id = `_child_record_id`, fp_main_id = `_parent_id`,
                  species = species_plus, lifestage = lifestage_plus,
                  mark_status = mark_status_plus, fish_condition = fish_condition_plus,
                  count = plus_count) |>
    dplyr::left_join(fp_main)
}
