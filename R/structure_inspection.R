#' Get DELVE structure inspection tables
#'
#'
#' @md
#' @param table       DELVE table: meta, change, boom, guidance, curtain, wildlife, listed, anchor, dock, trap, velocity, float
#'
#' @export
#'

get_si <- function(table = c("meta", "change", "boom", "guidance", "curtain", "wildlife",
                             "listed", "anchor", "dock", "trap", "velocity", "float")){
  si_tables = c("meta" = "structure-inspection.csv",
                "change" = "change-log.csv",
                "boom" = "boom-inspection.csv",
                "guidance" = "guidance-inspection.csv",
                "curtain" = "curtain-inspection.csv",
                "wildlife" = "structure-wildlife.csv",
                "listed" = "legacy-file.csv",
                "anchor" = "anchor-inspections.csv",
                "dock" = "docks.csv",
                "trap" = "trap-inspection.csv",
                "velocity" = "trap-velocities.csv",
                "float" = "net-curtain-inspection.csv")
  delver::get_dataset_file(project_id = 73, dataset_id = 216, version = 1,
                           filename = si_tables[[table]])
}

#' Prepare DELVE structure inspection metadata
#'
#'
#' @md
#' @param si_meta_raw     Unprocessed structure inspection metadata
#' @param min_date        First date to use for structure inspection data
#' @param tz              Time zone name
#'
#' @export
#'

prep_si_meta <- function(si_meta_raw, min_date = min_tc_date, tz = tz_loc){
  si_meta_raw |>
    fix_names() |>
    dplyr::mutate(inspection_start = prep_dt_delve(inspection_start),
                  inspection_end = prep_dt_delve(inspection_end),
                  date = as.Date(inspection_start, tz = tz)) |>
    dplyr::filter(date >= min_date) |>
    dplyr::select(date, inspection_start, inspection_end, crew,
                  comments, si_meta_id = recordkey) |>
    dplyr::arrange(dplyr::desc(inspection_start))
}

#' Prepare DELVE structure inspection change log data
#'
#'
#' @md
#' @param si_change_raw     Unprocessed structure inspection change log data
#' @param si_meta           Processed structure inspection metadata
#'
#' @export
#'

prep_si_change <- function(si_change_raw, si_meta){
  si_change_raw |>
    fix_names() |>
    dplyr::rename(si_change_id = recordkey, si_meta_id = parentrecordkey) |>
    dplyr::left_join(dplyr::select(si_meta, si_meta_id, date)) |>
    dplyr::filter(!is.na(date)) |>
    dplyr::select(date, dock_id, panel_modified, panel_modification,
                  temperature_curtain_modified, temperature_curtain_modification,
                  trap_modified, trap_modifications,
                  si_change_id, si_meta_id) |>
    dplyr::arrange(dplyr::desc(date))
}

#' Prepare DELVE boom inspection data
#'
#'
#' @md
#' @param si_boom_raw     Unprocessed boom inspection data
#' @param si_meta_raw     Unprocessed structure inspection metadata
#' @param min_date        First date to use for structure inspection data
#' @param tz              Time zone name
#'
#' @export
#'

prep_si_boom <- function(si_boom_raw, si_meta_raw, min_date = min_tc_date, tz = tz_loc){
  si_boom_raw |>
    fix_names() |>
    dplyr::rename(si_boom_id = recordkey) |>
    dplyr::left_join(si_meta_raw |>
                       create_links("RecordKey", "debris-boom-inspection") |>
                       dplyr::rename(si_meta_id = parent, si_boom_id = child)) |>
    dplyr::mutate(inspection_start = prep_dt_delve(inspection_start),
                  inspection_end = prep_dt_delve(inspection_end),
                  date = as.Date(inspection_start, tz = tz)) |>
    dplyr::filter(date >= min_date) |>
    dplyr::select(date, inspection_start, inspection_end, safety_buoy_lines,
                  function_status, functionality_comments, overall_inspection,
                  anchor_comments, comments, si_boom_id, si_meta_id) |>
    dplyr::arrange(dplyr::desc(inspection_start))
}

#' Prepare DELVE guidance net inspection data
#'
#'
#' @md
#' @param si_guidance_raw     Unprocessed guidance net inspection data
#' @param si_meta_raw         Unprocessed structure inspection metadata
#' @param min_date            First date to use for structure inspection data
#' @param tz                  Time zone name
#'
#' @export
#'

prep_si_guidance <- function(si_guidance_raw, si_meta_raw, min_date = min_tc_date, tz = tz_loc){
  si_guidance_raw |>
    fix_names() |>
    dplyr::rename(si_guidance_id = recordkey) |>
    dplyr::left_join(si_meta_raw |>
                       create_links("RecordKey", "guidance-net-inspection") |>
                       dplyr::rename(si_meta_id = parent, si_guidance_id = child)) |>
    dplyr::mutate(inspection_start = prep_dt_delve(inspection_start),
                  inspection_end = prep_dt_delve(inspection_end),
                  date = as.Date(inspection_start, tz = tz)) |>
    dplyr::filter(date >= min_date) |>
    dplyr::select(date, inspection_start, inspection_end, docks_inspected = number_of_docks_inpected,
                  in_water_anchors, alignment, comments, si_guidance_id, si_meta_id) |>
    dplyr::arrange(dplyr::desc(inspection_start))
}

#' Prepare DELVE temperature curtain inspection data
#'
#'
#' @md
#' @param si_curtain_raw      Unprocessed temperature curtain inspection data
#' @param si_meta_raw         Unprocessed structure inspection metadata
#' @param min_date            First date to use for structure inspection data
#' @param tz                  Time zone name
#'
#' @export
#'

prep_si_curtain <- function(si_curtain_raw, si_meta_raw, min_date = min_tc_date, tz = tz_loc){
  si_curtain_raw |>
    fix_names() |>
    dplyr::rename(si_curtain_id = recordkey) |>
    dplyr::left_join(si_meta_raw |>
                       create_links("RecordKey", "temperature-curtain-inspection") |>
                       dplyr::rename(si_meta_id = parent, si_curtain_id = child)) |>
    dplyr::mutate(inspection_start = prep_dt_delve(inspection_start),
                  inspection_end = prep_dt_delve(inspection_end),
                  date = as.Date(inspection_start, tz = tz)) |>
    dplyr::select(date, inspection_start, inspection_end, depth_ft, depth_m, curtain_deployed, closed_panels,
                  safety_device_inspection, safety_device_comments, alignment, alignment_comments,
                  overall_anchor_status, safety_buoy_line, comments, si_curtain_id, si_meta_id) |>
    dplyr::arrange(dplyr::desc(inspection_start))
}

#' Prepare DELVE wildlife observations
#'
#'
#' @md
#' @param si_wildlife_raw      Unprocessed wildlife observations
#' @param si_meta_raw          Unprocessed structure inspection metadata
#' @param si_meta              Processed structure inspection metadata
#'
#' @export
#'

prep_si_wildlife <- function(si_wildlife_raw, si_meta_raw, si_meta){
  si_wildlife_raw |>
    fix_names() |>
    dplyr::rename(si_wildlife_id = recordkey) |>
    dplyr::left_join(si_meta_raw |>
                       create_links("RecordKey", "wildlife-observations") |>
                       dplyr::rename(si_meta_id = parent, si_wildlife_id = child)) |>
    dplyr::left_join(dplyr::select(si_meta, si_meta_id, date)) |>
    dplyr::select(date, listed_species_observed, bird_species, amphibian_species,
                  mammal_species, reptile_species, fish_species, species_write_in,
                  comments = non_listed_species_comments, si_wildlife_id, si_meta_id) |>
    dplyr::arrange(dplyr::desc(date))
}

#' Prepare DELVE listed species impacts
#'
#'
#' @md
#' @param si_listed_raw            Unprocessed listed species impacts
#' @param si_wildlife_raw          Unprocessed wildlife observations
#' @param si_wildlife              Processed wildlife observations
#'
#' @export
#'

prep_si_listed <- function(si_listed_raw, si_wildlife_raw, si_wildlife){
  si_listed_raw |>
    fix_names() |>
    dplyr::rename(si_listed_id = recordkey) |>
    dplyr::left_join(si_wildlife_raw |>
                       create_links("RecordKey", "listed-species-impact-log") |>
                       dplyr::rename(si_wildlife_id = parent, si_listed_id = child)) |>
    dplyr::left_join(dplyr::select(si_wildlife, si_wildlife_id, date)) |>
    dplyr::select(date, species_name, bird_species, impact_type, comments,
                  si_listed_id, si_wildlife_id) |>
    dplyr::arrange(dplyr::desc(date))
}

#' Prepare DELVE trap platform data
#'
#'
#' @md
#' @param si_trap_raw         Unprocessed trap platform data
#' @param si_guidance_raw     Unprocessed guidance net inspection data
#' @param min_date            First date to use for structure inspection data
#' @param tz                  Time zone name
#'
#' @export
#'

prep_si_trap <- function(si_trap_raw, si_guidance_raw, min_date = min_tc_date, tz = tz_loc){
  si_trap_raw |>
    fix_names() |>
    dplyr::rename(si_trap_id = recordkey) |>
    dplyr::left_join(si_guidance_raw |>
                       create_links("RecordKey", "trap-platform-inspection") |>
                       dplyr::rename(si_guidance_id = parent, si_trap_id = child)) |>
    dplyr::mutate(datetime = prep_dt_delve(datetime),
                  date = as.Date(datetime, tz = tz)) |>
    dplyr::filter(date >= min_date) |>
    dplyr::select(date, datetime, dock_id, depth_m, depth_ft, velocity_cms, velocity_fps,
                  platform_connections, trap_net_connections, connections_notes,
                  comments, si_trap_id, si_guidance_id) |>
    dplyr::arrange(dplyr::desc(datetime))
}

#' Prepare DELVE trap velocity data
#'
#'
#' @md
#' @param si_velocity_raw      Unprocessed trap velocity data
#' @param si_trap              Processed trap platform data
#'
#' @export
#'

prep_si_velocity <- function(si_velocity_raw, si_trap){
  tmp = si_velocity_raw |>
    fix_names() |>
    dplyr::rename(si_velocity_id = recordkey, si_trap_id = parentrecordkey) |>
    dplyr::select(-sampleid, -sampledate, -lastupdatedate) |>
    dplyr::mutate(x_velocity = fix_velocity(x_velocity, x_veloc_neg),
                  y_velocity = fix_velocity(y_velocity, y_veloc_neg),
                  z_velocity = fix_velocity(z_velocity, z_veloc_neg),
                  depth = ifelse(grepl("depth 3|3 ft", sample_location, ignore.case = TRUE), "3 ft",
                                 ifelse(grepl("depth 1|1 ft", sample_location, ignore.case = TRUE), "1 ft", "NA")),
                  loc = ifelse(grepl("DS", sample_location, ignore.case = TRUE), "DS",
                               ifelse(grepl("US", sample_location, ignore.case = TRUE), "US", "NA")),
                  fyke = ifelse(grepl("F1", sample_location, ignore.case = TRUE), "F1",
                                ifelse(grepl("F2", sample_location, ignore.case = TRUE), "F2", "NA")))

  si_trap |>
    dplyr::select(si_trap_id, date, datetime, velocity_fps_overall1 = velocity_fps) |>
    dplyr::left_join(dplyr::select(tmp, si_velocity_id, si_trap_id, sample_location,
                                   fyke, loc, depth, modification_status, cleaning_status,
                                   x_velocity, y_velocity, z_velocity, velocity_fps_overall2 = velocity_fps, comments)) |>
    dplyr::ungroup() |>
    dplyr::mutate(velocity_fps_xyz = sqrt(x_velocity^2 + y_velocity^2 + z_velocity^2),
                  velocity_fps_sel = ifelse(!is.na(velocity_fps_xyz), velocity_fps_xyz,
                                            ifelse(!is.na(velocity_fps_overall2), velocity_fps_overall2, velocity_fps_overall1))) |>
    dplyr::select(date, datetime, sample_location, fyke, loc, depth, modification_status, cleaning_status,
                  velocity_fps_overall1, velocity_fps_overall2, x_velocity, y_velocity, z_velocity, velocity_fps_xyz,
                  velocity_fps_sel, comments, si_velocity_id, si_trap_id) |>
    dplyr::arrange(dplyr::desc(datetime))
}

#' Prepare DELVE dock inspection data
#'
#'
#' @md
#' @param si_dock_raw         Unprocessed dock inspection data
#' @param si_guidance_raw     Unprocessed guidance net inspection data
#' @param min_date            First date to use for structure inspection data
#' @param tz                  Time zone name
#'
#' @export
#'

prep_si_dock <- function(si_dock_raw, si_guidance_raw, min_date = min_tc_date, tz = tz_loc){
  si_dock_raw |>
    fix_names() |>
    dplyr::rename(si_dock_id = recordkey) |>
    dplyr::left_join(si_guidance_raw |>
                       create_links("RecordKey", "walkway-inspections") |>
                       dplyr::rename(si_guidance_id = parent, si_dock_id = child)) |>
    dplyr::mutate(datetime = prep_dt_delve(datetime),
                  date = as.Date(datetime, tz = tz)) |>
    dplyr::filter(date >= min_date) |>
    dplyr::select(date, datetime, dock_id, depth_m, depth_ft, panel_height_m, panel_height_ft,
                  connections, connections_notes, winch_inspection, winch_comments,
                  panel_deployed, panel_deployment_location, fish_passage, fish_passage_comments,
                  fish_passage_pit_tag_array, drone_control_point, comments, si_dock_id, si_guidance_id) |>
    dplyr::arrange(dplyr::desc(datetime))
}

#' Prepare DELVE float and curtain inspection data
#'
#'
#' @md
#' @param si_float_raw        Unprocessed float and curtain inspection data
#' @param si_curtain_raw      Unprocessed temperature curtain inspection data
#' @param min_date            First date to use for structure inspection data
#' @param tz                  Time zone name
#'
#' @export
#'

prep_si_float <- function(si_float_raw, si_curtain_raw, min_date = min_tc_date, tz = tz_loc){
  si_float_raw |>
    fix_names() |>
    dplyr::rename(si_float_id = recordkey) |>
    dplyr::left_join(si_curtain_raw |>
                       create_links("RecordKey", "float-and-curtain-inspection") |>
                       dplyr::rename(si_curtain_id = parent, si_float_id = child)) |>
    dplyr::mutate(datetime = prep_dt_delve(datetime),
                  date = as.Date(datetime, tz = tz)) |>
    dplyr::filter(date >= min_date) |>
    dplyr::select(date, datetime, float_segment_id, float_connections, connections_notes,
                  winch_inspection, winch_comments, anchor_inspection = in_water_anchor_inspections,
                  comments, si_float_id, si_curtain_id) |>
    dplyr::arrange(dplyr::desc(datetime))
}

#' Prepare DELVE anchor inspection data
#'
#'
#' @md
#' @param si_anchor_raw       Unprocessed anchor inspection data
#' @param si_boom_raw         Unprocessed boom inspection data
#' @param si_guidance_raw     Unprocessed guidance net inspection data
#' @param si_curtain_raw      Unprocessed temperature curtain inspection data
#' @param min_date            First date to use for structure inspection data
#' @param tz                  Time zone name
#'
#' @export
#'

prep_si_anchor <- function(si_anchor_raw, si_boom_raw, si_guidance_raw, si_curtain_raw,
                           min_date = min_tc_date, tz = tz_loc){
  tmp = si_anchor_raw |>
    fix_names() |>
    dplyr::rename(si_anchor_id = recordkey)

  # todo: clean up the redundancy in the next 3 blocks of code
  tmp_boom = tmp |>
    dplyr::left_join(si_boom_raw |>
                       create_links("RecordKey", "river-right-anchoring-system-inspection") |>
                       dplyr::rename(si_boom_id = parent, si_anchor_id = child)) |>
    dplyr::filter(!is.na(si_boom_id)) |>
    dplyr::bind_rows(tmp |>
                       dplyr::left_join(si_boom_raw |>
                                          create_links("RecordKey", "river-left-anchoring-system-inspection") |>
                                          dplyr::rename(si_boom_id = parent, si_anchor_id = child)) |>
                       dplyr::filter(!is.na(si_boom_id)))

  tmp_guidance = tmp |>
    dplyr::left_join(si_guidance_raw |>
                       create_links("RecordKey", "river-right-anchor-inspection") |>
                       dplyr::rename(si_guidance_id = parent, si_anchor_id = child)) |>
    dplyr::filter(!is.na(si_guidance_id)) |>
    dplyr::bind_rows(tmp |>
                       dplyr::left_join(si_guidance_raw |>
                                          create_links("RecordKey", "river-left-anchor-inspection") |>
                                          dplyr::rename(si_guidance_id = parent, si_anchor_id = child)) |>
                       dplyr::filter(!is.na(si_guidance_id)))

  tmp_curtain = tmp |>
    dplyr::left_join(si_curtain_raw |>
                       create_links("RecordKey", "river-right-anchor-inspection") |>
                       dplyr::rename(si_curtain_id = parent, si_anchor_id = child)) |>
    dplyr::filter(!is.na(si_curtain_id)) |>
    dplyr::bind_rows(tmp |>
                       dplyr::left_join(si_curtain_raw |>
                                          create_links("RecordKey", "river-left-anchor-inspection") |>
                                          dplyr::rename(si_curtain_id = parent, si_anchor_id = child)) |>
                       dplyr::filter(!is.na(si_curtain_id)))

  dplyr::bind_rows(list(tmp_boom, tmp_guidance, tmp_curtain)) |>
    dplyr::mutate(datetime = prep_dt_delve(inspection_datetime),
                  date = as.Date(datetime, tz = tz),
                  river_position = from_canonical(river_position),
                  anchored_structure = from_canonical(anchored_structure),
                  anchor_type = from_canonical(anchor_type),
                  anchor_status = from_canonical(anchor_status)) |>
    dplyr::filter(date >= min_date) |>
    dplyr::select(date, datetime, river_position, anchored_structure, anchor_type,
                  # wrong units used in original name for load
                  anchor_status, load_lbs = load_kn, comments, si_anchor_id,
                  si_boom_id, si_guidance_id, si_curtain_id) |>
    dplyr::arrange(dplyr::desc(datetime))
}
