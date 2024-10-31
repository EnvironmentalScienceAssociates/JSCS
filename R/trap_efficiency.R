#' Get DELVE trap efficiency tables
#'
#'
#' @md
#' @param table       DELVE table: meta, fish, transport, transport_wq, tempering, holding, release
#'
#' @export
#'

get_te <- function(table = c("meta", "fish", "transport", "transport_wq", "tempering", "holding", "release")){
  te_tables = c("meta" = "organism-collection-v2.csv",
                "fish" = "organism-sample.csv",
                "transport" = "organism_transport.csv",
                "transport_wq" = "transport-water-quality.csv",
                "tempering" = "organism-tempering.csv",
                "holding" = "holding-tank-meta.csv",
                "release" = "organism-release.csv")
  delver::get_dataset_file(project_id = 73, dataset_id = 217, version = 1,
                           filename = te_tables[[table]])
}

#' Prepare DELVE trap efficiency metadata
#'
#'
#' @md
#' @param te_meta_raw     Unprocessed trap collection metadata
#' @param min_date        First date to use for trap efficiency data
#' @param tz              Time zone name
#'
#' @export
#'

prep_te_meta <- function(te_meta_raw, min_date = min_tc_date, tz = tz_loc){
  te_meta_raw |>
    fix_names() |>
    dplyr::mutate(datetime = prep_dt_delve(sampling_datetime),
                  date = as.Date(datetime, tz = tz)) |>
    dplyr::filter(date >= min_date) |>
    dplyr::select(date, datetime, location = sampling_loc, lat = sampling_lat,
                  lon = sampling_long, crew, comments, te_meta_id = recordkey) |>
    dplyr::arrange(dplyr::desc(datetime))
}

#' Prepare DELVE trap efficiency fish data
#'
#'
#' @md
#' @param te_fish_raw     Unprocessed trap collection fish measurement data
#' @param te_meta_raw     Unprocessed trap collection metadata
#' @param min_date        First date to use for trap efficiency data
#' @param tz              Time zone name
#'
#' @export
#'

prep_te_fish <- function(te_fish_raw, te_meta_raw, min_date = min_tc_date, tz = tz_loc){
  te_fish_raw |>
    fix_names() |>
    dplyr::rename(te_fish_id = recordkey) |>
    dplyr::left_join(te_meta_raw |>
                       create_links("RecordKey", "fish-data") |>
                       dplyr::rename(te_meta_id = parent, te_fish_id = child)) |>
    dplyr::mutate(datetime = prep_dt_delve(sampling_datetime),
                  date = as.Date(datetime, tz = tz),
                  species = from_canonical(common_name),
                  clip_loc = fix_clip_loc(clip_loc)) |>
    dplyr::filter(date >= min_date) |>
    dplyr::select(date, datetime, species, fl_mm = fork_length_mm,
                  clip_loc, other_mark_desc = other_marks,
                  comment, te_fish_id, te_meta_id) |>
    dplyr::arrange(dplyr::desc(datetime))
}

#' Prepare DELVE trap efficiency transport data
#'
#'
#' @md
#' @param te_transport_raw     Unprocessed trap efficiency transport data
#' @param min_date             First date to use for trap efficiency data
#' @param tz                   Time zone name
#'
#' @export
#'

prep_te_transport <- function(te_transport_raw, min_date = min_tc_date, tz = tz_loc){
  te_transport_raw |>
    fix_names() |>
    dplyr::rename(te_transport_id = recordkey) |>
    dplyr::mutate(transport_start = prep_dt_delve(transport_start),
                  date = as.Date(transport_start, tz = tz)) |>
    dplyr::filter(date >= min_date) |>
    dplyr::select(date, transport_start, crew, num_containers, fish_count = total_number_of_fish_transported,
                  mortality = mortalities, origin_meter, transport_meter, tempering = `tempering?`,
                  holding = pre_release_holding, te_transport_id) |>
    dplyr::arrange(dplyr::desc(transport_start))
}

#' Prepare DELVE trap efficiency transport water quality data
#'
#'
#' @md
#' @param te_transport_wq_raw     Unprocessed trap efficiency transport data
#' @param te_transport_raw        Unprocessed trap efficiency transport data
#' @param min_date                First date to use for trap efficiency data
#' @param tz                      Time zone name
#'
#' @export
#'

prep_te_transport_wq <- function(te_transport_wq_raw, te_transport_raw, min_date = min_tc_date, tz = tz_loc){
  te_transport_wq_raw |>
    fix_names() |>
    dplyr::rename(te_transport_wq_id = recordkey) |>
    dplyr::left_join(te_transport_raw |>
                       create_links("RecordKey", "transport-water-quality-log") |>
                       dplyr::rename(te_transport_id = parent, te_transport_wq_id = child)) |>
    dplyr::mutate(sample_datetime = prep_dt_delve(sample_datetime),
                  date = as.Date(sample_datetime, tz = tz)) |>
    dplyr::filter(date >= min_date) |>
    dplyr::select(date, sample_datetime, temp_c = temp_w_c, do_sat = do_perc_sat, do_mgl,
                  te_transport_wq_id, te_transport_id) |>
    dplyr::arrange(dplyr::desc(sample_datetime))
}

#' Prepare DELVE trap efficiency transport tempering data
#'
#'
#' @md
#' @param te_tempering_raw        Unprocessed trap efficiency transport tempering data
#' @param te_transport_raw        Unprocessed trap efficiency transport data
#' @param te_transport            Processed trap efficiency transport data
#'
#' @export
#'

prep_te_tempering <- function(te_tempering_raw, te_transport_raw, te_transport){
  te_tempering_raw |>
    fix_names() |>
    dplyr::rename(te_tempering_id = recordkey) |>
    dplyr::left_join(te_transport_raw |>
                       create_links("RecordKey", "tempering-log") |>
                       dplyr::rename(te_transport_id = parent, te_tempering_id = child)) |>
    dplyr::left_join(dplyr::select(te_transport, te_transport_id, date)) |>
    dplyr::select(date, holding_tank = holding_tank_id, phase_1_temp, phase_2_temp,
                  phase_3_temp, final_temp, te_tempering_id, te_transport_id) |>
    dplyr::arrange(dplyr::desc(date))
}

#' Prepare DELVE trap efficiency transport holding data
#'
#'
#' @md
#' @param te_holding_raw          Unprocessed trap efficiency transport holding data
#' @param te_transport_raw        Unprocessed trap efficiency transport data
#' @param te_transport            Processed trap efficiency transport data
#'
#' @export
#'

prep_te_holding <- function(te_holding_raw, te_transport_raw, te_transport){
  te_holding_raw |>
    fix_names() |>
    dplyr::rename(te_holding_id = recordkey) |>
    dplyr::left_join(te_transport_raw |>
                       create_links("RecordKey", "pre-release-holding-logs") |>
                       dplyr::rename(te_transport_id = parent, te_holding_id = child)) |>
    dplyr::left_join(dplyr::select(te_transport, te_transport_id, date)) |>
    dplyr::select(date, enclosure_id, enclosure_type, te_holding_id, te_transport_id) |>
    dplyr::arrange(dplyr::desc(date))
}

#' Prepare DELVE trap efficiency release data
#'
#'
#' @md
#' @param te_release_raw          Unprocessed trap efficiency release data
#' @param te_transport_raw        Unprocessed trap efficiency transport data
#' @param min_date                First date to use for trap efficiency data
#' @param tz                      Time zone name
#'
#' @export
#'

prep_te_release <- function(te_release_raw, te_transport_raw, min_date = min_tc_date, tz = tz_loc){
  te_release_raw |>
    fix_names() |>
    dplyr::rename(te_release_id = recordkey) |>
    dplyr::left_join(te_transport_raw |>
                       create_links("RecordKey", "release-log") |>
                       dplyr::rename(te_transport_id = parent, te_release_id = child)) |>
    dplyr::mutate(release_datetime = prep_dt_delve(release_datetime),
                  date = as.Date(release_datetime, tz = tz)) |>
    dplyr::filter(date >= min_date) |>
    dplyr::select(date, release_datetime, release_location, release_lat, release_lon = release_long,
                  release_group, number_released = total_number_released, te_release_id, te_transport_id) |>
    dplyr::arrange(dplyr::desc(release_datetime))
}

