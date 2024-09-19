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
    dplyr::select(ssi_main_id = `_record_id`, date = date_temp, jscs_site, water_depth_tool,
                  camera_id, trap_anchor_status, trap_anchor_status_notes,
                  flow_patterns, modification_notes)
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
    dplyr::select(ssi_debris_boom_id = `_child_record_id`, ssi_main_id = `_parent_id`,
                  anchor = debris_boom_anchor, anchor_status = debris_boom_anchor_status,
                  debris_present = debris_boom_debris_present, debris_details,
                  debris_removed, debris_removal_details, debris_boom_notes) |>
    dplyr::left_join(dplyr::select(ssi_main, ssi_main_id, date))
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
    dplyr::select(ssi_net_anchor_id = `_child_record_id`, ssi_main_id = `_parent_id`,
                  anchor = guidance_net_anchor, anchor_load_lbs,
                  anchor_status = guidance_net_anchor_status) |>
    dplyr::left_join(dplyr::select(ssi_main, ssi_main_id, date))
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
    dplyr::select(ssi_net_dock_id = `_child_record_id`, ssi_main_id = `_parent_id`,
                  dock = guidance_net_dock, connection_status,
                  debris_present = guidance_net_debris_present) |>
    dplyr::left_join(dplyr::select(ssi_main, ssi_main_id, date))
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
    dplyr::select(ssi_structure_id = `_child_record_id`, ssi_main_id = `_parent_id`,
                  dock = structure_dock, water_depth_ft, guidance_net_depth_ft,
                  impermeable_panel_depth_ft, dock_winch_status, dock_modified) |>
    dplyr::left_join(dplyr::select(ssi_main, ssi_main_id, date))
}

