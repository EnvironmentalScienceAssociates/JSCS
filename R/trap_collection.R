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
                  trap_open_time = prep_dt_delve(trap_open_time),
                  trap_closed_time = prep_dt_delve(trap_closed_time),
                  trap_hours = as.numeric(difftime(end_datetime, start_datetime, units = "hours")),
                  start_date = as.Date(start_datetime, tz = tz),
                  temp_c = ifelse(temp_c == 0, NA, temp_c),
                  trap_status = from_canonical(trap_status)) |>
    dplyr::filter(start_date >= min_date) |>
    dplyr::select(sample_id = sampleid, tc_meta_id = recordkey, obs_loc, date = start_date,
                  datetime = start_datetime, end_datetime, trap_hours, trap_status, temp_c,
                  trap_open_time, trap_closed_time)
}

#' Prepare DELVE trap collection fish measurement data
#'
#'
#' @md
#' @param tc_fish_raw     Unprocessed trap collection fish measurement data
#' @param tc_meta_raw     Unprocessed trap collection metadata
#' @param tc_meta         Processed trap collection metadata
#' @param min_date        First date to use for trap collection data
#' @param tz              Time zone name
#'
#' @export
#'

prep_tc_fish <- function(tc_fish_raw, tc_meta_raw, tc_meta,
                         min_date = min_tc_date, tz = tz_loc){
  tc_fish_raw |>
    fix_names() |>
    dplyr::mutate(datetime = prep_datetime(sampling_datetime),
                  date = as.Date(datetime, tz = tz),
                  count = 1) |>
    dplyr::filter(date >= min_date) |>
    dplyr::select(sample_id = sampleid, tc_fish_id = recordkey, date, datetime, common_name,
                  stock, lifestage, clip_loc, pit_applied, pit_id, other_mark_desc,
                  count, injury, morts, predated_salmon, fork_length_mm, condition) |>
    dplyr::mutate(species = from_canonical(common_name),
                  lifestage = stringr::str_to_title(lifestage),
                  clip_loc = fix_clip_loc(clip_loc),
                  pit_applied = ifelse(pit_applied, "Yes", "No"),
                  other_mark_desc = fix_other_mark_desc(other_mark_desc)) |>
    dplyr::left_join(tc_meta_raw |>
                       create_links("RecordKey", "individual-data") |>
                       dplyr::rename(tc_meta_id = parent, tc_fish_id = child)) |>
    dplyr::left_join(dplyr::select(tc_meta, tc_meta_id, obs_loc))
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
    dplyr::mutate(morts = as.numeric(morts),
                  predated_salmon = ifelse(!is.na(comments) & comments == "Predated", morts, 0),
                  morts = ifelse(!is.na(comments) & comments == "Predated", 0, morts),
                  injury = 0, # tracked in fish table
                  pit_applied = "No",  # tracked in fish table
                  obs_value = as.numeric(obs_value),
                  obs_value = ifelse(!is.na(comments) & comments == "Predated", 0, obs_value),
                  species = from_canonical(common_name),
                  lifestage = stringr::str_to_title(lifestage),
                  clip_loc = fix_clip_loc(clip_loc),
                  other_mark_desc = fix_other_mark_desc(other_mark_desc)) |>
    dplyr::select(sample_id = sampleid, tc_plus_id = recordkey, common_name, species,
                  stock, lifestage, clip_loc, other_mark_desc, pit_applied, count = obs_value,
                  predated_salmon, morts, injury) |>
    # tally was required at one point so 'dummy' species were included with count of zero
    dplyr::filter(count > 0) |>
    dplyr::left_join(tc_meta_raw |>
                       create_links("RecordKey", "tally-data") |>
                       dplyr::rename(tc_meta_id = parent, tc_plus_id = child)) |>
    dplyr::left_join(dplyr::select(tc_meta, tc_meta_id, obs_loc, date, datetime)) |>
    dplyr::filter(date >= min_date)
}


#' Prepare DELVE trap collection stomach data
#'
#'
#' @md
#' @param tc_stomach_raw     Unprocessed trap collection stomach data
#' @param tc_fish_raw        Unprocessed trap collection fish measurement data
#' @param tc_fish            Processed trap collection fish measurement data
#' @param tc_plus            Processed trap collection plus count data
#' @param min_date           First date to use for trap collection data
#'
#' @export
#'

prep_tc_stomach <- function(tc_stomach_raw, tc_fish_raw, tc_fish, tc_plus){
  tc_stomach_raw |>
    fix_names() |>
    dplyr::select(sample_id = sampleid, tc_stomach_id = recordkey, fork_length, clip_loc, comments) |>
    dplyr::left_join(tc_fish_raw |>
                       create_links("RecordKey", "stomach-salmon-records") |>
                       dplyr::rename(tc_fish_id = parent, tc_stomach_id = child)) |>
    dplyr::left_join(dplyr::select(tc_fish, tc_fish_id, date, predator_species = species,
                                   predator_fork_length = fork_length_mm, predated_salmon)) |>
    dplyr::bind_rows(tc_plus |>
                       dplyr::filter(predated_salmon > 0) |>
                       dplyr::select(sample_id, tc_plus_id, date, predated_salmon)) |>
    dplyr::bind_rows(tc_fish |>
                       dplyr::filter(predated_salmon > 0) |>
                       dplyr::select(sample_id, tc_fish_id, date, predator_species = species,
                                     predator_fork_length = fork_length_mm, predated_salmon)) |>
    dplyr::mutate(qc_note = ifelse(!is.na(tc_stomach_id) & !is.na(predated_salmon), "check for double count", NA_character_),
                  count = ifelse(!is.na(tc_stomach_id), 1, predated_salmon))
}
