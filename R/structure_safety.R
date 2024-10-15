#' Get Fulcrum structure and safety inspection tables
#'
#'
#' @md
#' @param table       Fulcrum table: main, debris_boom, net_anchor, net_dock, structure
#'
#' @export
#'

get_ssi <- function(table = c("main", "debris_boom", "net_anchor", "net_dock", "structure")){
  ssi_tables = c("main" = "JSCS Structure and Safety Inspection 2024",
                 "debris_boom" = "JSCS Structure and Safety Inspection 2024/debris_boom_inspection",
                 "net_anchor" = "JSCS Structure and Safety Inspection 2024/guidance_net_anchor_inspection",
                 "net_dock" = "JSCS Structure and Safety Inspection 2024/guidance_net_dock_inspection",
                 "structure" = "JSCS Structure and Safety Inspection 2024/structure_inspection")
  fulcrumr::fulcrum_table(ssi_tables[[table]])
}

#' Prepare Fulcrum structure and safety main table
#'
#'
#' @md
#' @param ssi_main_raw     Unprocessed structure and safety main table
#'
#' @export
#'

prep_ssi_main <- function(ssi_main_raw){
  ssi_main_raw |>
    dplyr::rename(ssi_main_id = `_record_id`) |>
    dplyr::select(!dplyr::starts_with("_") & !dplyr::contains("photos")) |>
    dplyr::mutate(start_time = prep_dt_fulcrum(date, start_time),
                  end_time = prep_dt_fulcrum(date, end_time)) |>
    dplyr::relocate(ssi_main_id, .after = dplyr::last_col()) |>
    dplyr::arrange(dplyr::desc(date))
}

#' Prepare Fulcrum debris boom inspection table
#'
#'
#' @md
#' @param ssi_debris_boom_raw     Unprocessed debris boom inspection table
#' @param ssi_main                Processed structure and safety main table
#'
#' @export
#'

prep_ssi_debris_boom <- function(ssi_debris_boom_raw, ssi_main){
  ssi_debris_boom_raw |>
    dplyr::rename(ssi_debris_boom_id = `_child_record_id`, ssi_main_id = `_parent_id`) |>
    dplyr::left_join(dplyr::select(ssi_main, ssi_main_id, date)) |>
    dplyr::select(date, anchor = debris_boom_anchor, anchor_status = debris_boom_anchor_status,
                  debris_present = debris_boom_debris_present, debris_details,
                  debris_removed, debris_removal_details, debris_boom_notes,
                  ssi_debris_boom_id, ssi_main_id) |>
    dplyr::arrange(dplyr::desc(date))
}

#' Prepare Fulcrum guidance net anchor inspection table
#'
#'
#' @md
#' @param ssi_net_anchor_raw      Unprocessed guidance net anchor inspection table
#' @param ssi_main                Processed structure and safety main table
#'
#' @export
#'

prep_ssi_net_anchor <- function(ssi_net_anchor_raw, ssi_main){
  ssi_net_anchor_raw |>
    dplyr::rename(ssi_net_anchor_id = `_child_record_id`, ssi_main_id = `_parent_id`) |>
    dplyr::left_join(dplyr::select(ssi_main, ssi_main_id, date)) |>
    dplyr::mutate(datetime = prep_dt_fulcrum(date, anchor_load_time)) |>
    dplyr::select(date, datetime, anchor = guidance_net_anchor, anchor_load_lbs,
                  anchor_status = guidance_net_anchor_status,
                  ssi_net_anchor_id, ssi_main_id) |>
    dplyr::arrange(dplyr::desc(date))
}

#' Prepare Fulcrum guidance net dock inspection table
#'
#'
#' @md
#' @param ssi_net_dock_raw        Unprocessed guidance net dock inspection table
#' @param ssi_main                Processed structure and safety main table
#'
#' @export
#'

prep_ssi_net_dock <- function(ssi_net_dock_raw, ssi_main){
  ssi_net_dock_raw |>
    dplyr::rename(ssi_net_dock_id = `_child_record_id`, ssi_main_id = `_parent_id`) |>
    dplyr::left_join(dplyr::select(ssi_main, ssi_main_id, date)) |>
    dplyr::select(date, dock = guidance_net_dock, connection_status,
                  debris_present = guidance_net_debris_present,
                  ssi_net_dock_id, ssi_main_id) |>
    dplyr::arrange(dplyr::desc(date))
}

#' Prepare Fulcrum structure inspection table
#'
#'
#' @md
#' @param ssi_structure_raw       Unprocessed guidance structure inspection table
#' @param ssi_main                Processed structure and safety main table
#'
#' @export
#'

prep_ssi_structure <- function(ssi_structure_raw, ssi_main){
  ssi_structure_raw |>
    dplyr::rename(ssi_structure_id = `_child_record_id`, ssi_main_id = `_parent_id`, dock = structure_dock) |>
    dplyr::select(!dplyr::starts_with("_")) |>
    dplyr::left_join(dplyr::select(ssi_main, ssi_main_id, date)) |>
    dplyr::relocate(date, .before = ssi_structure_id) |>
    dplyr::relocate(ssi_structure_id, ssi_main_id, .after = dplyr::last_col()) |>
    dplyr::arrange(dplyr::desc(date))
}

