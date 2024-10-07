#' Get DELVE trap collection tables
#'
#'
#' @md
#' @param table       DELVE table: meta, fish, plus, stomach, transport, transport_wq
#'
#' @export
#'

get_tc <- function(table = c("meta", "fish", "plus", "stomach", "transport", "transport_wq")){
  tc_tables = c("meta" = "observation-metadata.csv",
                "fish" = "organism-sample.csv",
                "plus" = "organismal-experiment.csv",
                "stomach" = "legacy-file.csv",
                "transport" = "organism_transport.csv",
                "transport_wq" = "transport-water-quality.csv")
  delver::get_dataset_file(project_id = 73, dataset_id = 206, version = 1,
                           filename = tc_tables[[table]])
}

#' Prepare DELVE trap collection metadata
#'
#'
#' @md
#' @param tc_meta_raw     Unprocessed trap collection metadata
#' @param min_date        First date to use for trap collection data
#' @param tz              Time zone name
#'
#' @export
#'

prep_tc_meta <- function(tc_meta_raw, min_date = min_tc_date, tz = tz_loc){
  tc_meta_raw |>
    fix_names() |>
    dplyr::mutate(start_datetime = prep_dt_delve(start_datetime),
                  end_datetime = prep_dt_delve(end_datetime),
                  shift_hrs = round(as.numeric(difftime(end_datetime, start_datetime, units = "hours")), 2),
                  trap_open_time = prep_dt_delve(trap_open_time),
                  trap_closed_time = prep_dt_delve(trap_closed_time),
                  trap_hrs = round(as.numeric(difftime(trap_open_time, trap_closed_time, units = "hours")), 2),
                  start_date = as.Date(start_datetime, tz = tz),
                  temp_c = ifelse(temp_c == 0, NA, temp_c),
                  trap_status = from_canonical(trap_status)) |>
    dplyr::filter(start_date >= min_date) |>
    # default order of fields is illogical enough to warrant this massive select rather than using relocate
    dplyr::select(date = start_date, start_datetime, end_datetime, shift_hrs,
                  trap_closed_time, trap_open_time, trap_hrs, obs_latitude, obs_longitude, crew,
                  temp_c, temp_c_top, do_sat = do_sat_perc, do_mgl = trap_conc_mgl,
                  trap_status, trap_cleaned = trap_cleaning_conducted, livebox_condition, livewell_condition,
                  frybox_condition, predator_exclusion_device = pred_exclus_device,
                  predator_exclusion_condition, entrance_fyke_design, entrance_fyke_condition,
                  trap_box_fyke_design, trap_box_fyke_condition, gantry_condition,
                  comments, tc_meta_id = recordkey) |>
    dplyr::arrange(dplyr::desc(start_datetime))
}

#' Prepare DELVE trap collection fish measurement data
#'
#'
#' @md
#' @param tc_fish_raw     Unprocessed trap collection fish measurement data
#' @param tc_meta_raw     Unprocessed trap collection metadata
#' @param min_date        First date to use for trap collection data
#' @param tz              Time zone name
#'
#' @export
#'

prep_tc_fish <- function(tc_fish_raw, tc_meta_raw, min_date = min_tc_date, tz = tz_loc){
  tc_fish_raw |>
    fix_names() |>
    dplyr::rename(tc_fish_id = recordkey) |>
    dplyr::left_join(tc_meta_raw |>
                       create_links("RecordKey", "individual-data") |>
                       dplyr::rename(tc_meta_id = parent, tc_fish_id = child)) |>
    dplyr::mutate(datetime = prep_dt_delve(sampling_datetime),
                  date = as.Date(datetime, tz = tz),
                  species = from_canonical(common_name),
                  lifestage = stringr::str_to_title(lifestage),
                  clip_loc = fix_clip_loc(clip_loc),
                  pit_applied = ifelse(pit_applied, "Yes", "No"),
                  other_mark_desc = fix_other_mark_desc(other_mark_desc)) |>
    dplyr::filter(date >= min_date) |>
    dplyr::select(date, datetime, species, lifestage, fl_mm = fork_length_mm,
                  weight_g, condition_factor, condition, injury, mortality = morts,
                  pit_id, clip_loc, other_marks, other_mark_desc,
                  comment, tc_fish_id, tc_meta_id) |>
    dplyr::arrange(dplyr::desc(datetime))
}

#' Prepare DELVE trap collection stomach data from the fish measurement table
#'
#'
#' @md
#' @param tc_fish_raw     Unprocessed trap collection fish measurement data
#' @param tc_meta_raw     Unprocessed trap collection metadata
#' @param tz              Time zone name
#'
#' @export
#'

prep_tc_fish_stomach <- function(tc_fish_raw, tc_meta_raw, tz = tz_loc){
  tc_fish_raw |>
    fix_names() |>
    dplyr::rename(tc_fish_id = recordkey) |>
    dplyr::left_join(tc_meta_raw |>
                       create_links("RecordKey", "individual-data") |>
                       dplyr::rename(tc_meta_id = parent, tc_fish_id = child)) |>
    dplyr::mutate(datetime = prep_dt_delve(sampling_datetime),
                  date = as.Date(datetime, tz = tz)) |>
    dplyr::filter(predated_salmon > 0) |>
    dplyr::select(date, predated_salmon, tc_fish_id, tc_meta_id) |>
    dplyr::arrange(dplyr::desc(date))
}

#' Prepare DELVE trap collection plus count data
#'
#'
#' @md
#' @param tc_plus_raw     Unprocessed trap collection plus count data
#' @param tc_meta_raw     Unprocessed trap collection metadata
#' @param tc_meta         Processed trap collection metadata
#' @param min_date        First date to use for trap collection data
#'
#' @export
#'

prep_tc_plus <- function(tc_plus_raw, tc_meta_raw, tc_meta, min_date = min_tc_date){
  tc_plus_raw |>
    fix_names() |>
    dplyr::rename(tc_plus_id = recordkey) |>
    dplyr::left_join(tc_meta_raw |>
                       create_links("RecordKey", "tally-data") |>
                       dplyr::rename(tc_meta_id = parent, tc_plus_id = child)) |>
    dplyr::left_join(dplyr::select(tc_meta, tc_meta_id, date)) |>
    dplyr::mutate(mortality = as.numeric(morts),
                  mortality = ifelse(!is.na(comments) & comments == "Predated", 0, mortality),
                  count = as.numeric(obs_value),
                  count = ifelse(!is.na(comments) & comments == "Predated", 0, count),
                  species = from_canonical(common_name),
                  lifestage = stringr::str_to_title(lifestage),
                  clip_loc = fix_clip_loc(clip_loc),
                  other_mark_desc = fix_other_mark_desc(other_mark_desc)) |>
    dplyr::select(date, species, lifestage, clip_loc, other_marks, other_mark_desc, count,
                  mortality, comments, tc_plus_id, tc_meta_id) |>
    dplyr::filter(count > 0) |>
    dplyr::arrange(dplyr::desc(date))
}

#' Prepare DELVE trap collection stomach data from the plus count table
#'
#'
#' @md
#' @param tc_plus_raw     Unprocessed trap collection plus count data
#' @param tc_meta_raw     Unprocessed trap collection metadata
#' @param tc_meta         Processed trap collection metadata
#'
#' @export
#'

prep_tc_plus_stomach <- function(tc_plus_raw, tc_meta_raw, tc_meta){
  tc_plus_raw |>
    fix_names() |>
    dplyr::rename(tc_plus_id = recordkey) |>
    dplyr::left_join(tc_meta_raw |>
                       create_links("RecordKey", "tally-data") |>
                       dplyr::rename(tc_meta_id = parent, tc_plus_id = child)) |>
    dplyr::left_join(dplyr::select(tc_meta, tc_meta_id, date)) |>
    dplyr::mutate(predated_salmon = ifelse(!is.na(comments) & comments == "Predated", as.numeric(morts), 0)) |>
    dplyr::filter(predated_salmon > 0) |>
    dplyr::select(date, predated_salmon, tc_plus_id, tc_meta_id) |>
    dplyr::arrange(dplyr::desc(date))
}

#' Prepare DELVE trap collection stomach data
#'
#'
#' @md
#' @param tc_stomach_raw     Unprocessed trap collection stomach data
#' @param tc_fish_raw        Unprocessed trap collection fish measurement data
#' @param tc_fish            Processed trap collection fish measurement data
#' @param tc_fish_stomach    Processed trap collection stomach data from fish measurement table
#' @param tc_plus_stomach    Processed trap collection stomach data from plus count table
#'
#' @export
#'

prep_tc_stomach <- function(tc_stomach_raw, tc_fish_raw, tc_fish, tc_fish_stomach, tc_plus_stomach){
  tc_stomach_raw |>
    fix_names() |>
    dplyr::rename(tc_stomach_id = recordkey) |>
    dplyr::left_join(tc_fish_raw |>
                       create_links("RecordKey", "stomach-salmon-records") |>
                       dplyr::rename(tc_fish_id = parent, tc_stomach_id = child)) |>
    dplyr::left_join(dplyr::select(tc_fish, tc_fish_id, date)) |>
    dplyr::bind_rows(tc_fish_stomach) |>
    dplyr::bind_rows(tc_plus_stomach) |>
    dplyr::mutate(count = ifelse(!is.na(tc_stomach_id), 1, predated_salmon)) |>
    dplyr::select(date, count, fork_length, clip_loc, comments,
                  tc_stomach_id, tc_fish_id, tc_plus_id, tc_meta_id) |>
    dplyr::arrange(dplyr::desc(date)) |> View()
}

#' Prepare DELVE trap collection transport data
#'
#'
#' @md
#' @param tc_transport_raw     Unprocessed trap collection transport data
#' @param tc_meta_raw          Unprocessed trap collection metadata
#' @param min_date             First date to use for trap collection data
#' @param tz                   Time zone name
#'
#' @export
#'

prep_tc_transport <- function(tc_transport_raw, tc_meta_raw, min_date = min_tc_date, tz = tz_loc){
  tc_transport_raw |>
    fix_names() |>
    dplyr::rename(tc_transport_id = recordkey) |>
    dplyr::left_join(tc_meta_raw |>
                       create_links("RecordKey", "transport-log") |>
                       dplyr::rename(tc_meta_id = parent, tc_transport_id = child)) |>
    dplyr::mutate(transport_start = prep_dt_delve(transport_start),
                  date = as.Date(transport_start, tz = tz),
                  cdfw_transfer_datetime = prep_dt_delve(cdfw_transfer_datetime)) |>
    dplyr::filter(date >= min_date) |>
    dplyr::select(date, transport_start, cdfw_transfer_datetime, crew,
                  num_containers, fish_count = total_number_of_fish,
                  origin_meter, transport_meter, tc_transport_id, tc_meta_id) |>
    dplyr::arrange(dplyr::desc(transport_start))
}

#' Prepare DELVE trap collection transport water quality data
#'
#'
#' @md
#' @param tc_transport_wq_raw     Unprocessed trap collection transport data
#' @param tc_transport_raw        Unprocessed trap collection transport data
#' @param min_date                First date to use for trap collection data
#' @param tz                      Time zone name
#'
#' @export
#'

prep_tc_transport_wq <- function(tc_transport_wq_raw, tc_transport_raw, min_date = min_tc_date, tz = tz_loc){
  tc_transport_wq_raw |>
    fix_names() |>
    dplyr::rename(tc_transport_wq_id = recordkey) |>
    dplyr::left_join(tc_transport_raw |>
                       create_links("RecordKey", "transport-water-quality-logs") |>
                       dplyr::rename(tc_transport_id = parent, tc_transport_wq_id = child)) |>
    dplyr::mutate(sample_datetime = prep_dt_delve(sample_datetime),
                  date = as.Date(sample_datetime, tz = tz)) |>
    dplyr::filter(date >= min_date) |>
    dplyr::select(date, sample_datetime, sample_location, temp_c = temp_w_c,
                  do_sat = do_perc_sat, do_mgl, comments,
                  tc_transport_wq_id, tc_transport_id) |>
    dplyr::arrange(dplyr::desc(sample_datetime))
}
