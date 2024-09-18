#' Get DELVE trap collection tables
#'
#'
#' @md
#' @param table       DELVE table: meta, fish, tally, stomach, transport, transport_wq
#'
#' @export
#'

get_tc <- function(table = c("meta", "fish", "tally", "stomach", "transport", "transport_wq")){
  tc_tables = c("meta" = "observation-metadata.csv",
                "fish" = "organism-sample.csv",
                "tally" = "organismal-experiment.csv",
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
#' @param min_date     First date to use for trap collection data
#' @param tz              Time zone name
#'
#' @export
#'

prep_tc_meta <- function(tc_meta_raw, min_date = min_tc_date, tz = tz_loc){
  tc_meta_raw |>
    fix_names() |>
    dplyr::mutate(start_datetime = prep_datetime(start_datetime),
                  end_datetime = prep_datetime(end_datetime),
                  trap_open_time = prep_datetime(trap_open_time),
                  trap_closed_time = prep_datetime(trap_closed_time),
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
#' @param tc_meta_fish    Unprocessed trap collection fish measurement data
#' @param tc_meta_raw     Unprocessed trap collection metadata
#' @param tc_meta         Processed trap collection metadata
#' @param min_date     First date to use for trap collection data
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
                  origin, stock, lifestage, clip_loc, pit_applied, pit_id, other_mark_desc,
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
