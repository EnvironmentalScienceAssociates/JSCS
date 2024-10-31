#' Get DELVE hook-and-line tables
#'
#'
#' @md
#' @param table       DELVE table: meta, fish, water_quality, sensor
#'
#' @export
#'

get_hl <- function(table = c("meta", "fish", "water_quality", "sensor")){
  hl_tables = c("meta" = "organism-collection-v2.csv",
                "fish" = "fish-data.csv",
                "water_quality" = "water-quality-monitoring.csv",
                "sensor" = "sensor-data.csv")
  delver::get_dataset_file(project_id = 73, dataset_id = 212, version = 1,
                           filename = hl_tables[[table]])
}

#' Prepare DELVE hook-and-line metadata
#'
#'
#' @md
#' @param hl_meta_raw     Unprocessed hook-and-line metadata
#' @param min_date        First date to use for hook-and-line data
#' @param tz              Time zone name
#'
#' @export
#'

prep_hl_meta <- function(hl_meta_raw, min_date = min_hl_date, tz = tz_loc){
  hl_meta_raw |>
    fix_names() |>
    dplyr::mutate(datetime = prep_dt_delve(sampling_datetime),
                  end_datetime = prep_dt_delve(end_datetime),
                  date = as.Date(datetime, tz = tz)) |>
    dplyr::filter(date >= min_date) |>
    dplyr::select(date, datetime, end_datetime, crew, sampling_loc, sampling_lat,
                  sampling_lon = sampling_long, comments, hl_meta_id = recordkey) |>
    dplyr::arrange(dplyr::desc(datetime))
}

#' Prepare DELVE hook-and-line fish measurement data
#'
#'
#' @md
#' @param hl_fish_raw     Unprocessed hook-and-line fish measurement data
#' @param hl_meta_raw     Unprocessed hook-and-line metadata
#' @param min_date        First date to use for hook-and-line data
#' @param tz              Time zone name
#'
#' @export
#'

prep_hl_fish <- function(hl_fish_raw, hl_meta_raw, min_date = min_hl_date, tz = tz_loc){
  hl_fish_raw |>
    fix_names() |>
    dplyr::rename(hl_fish_id = recordkey) |>
    dplyr::left_join(hl_meta_raw |>
                       create_links("RecordKey", "fish-data") |>
                       dplyr::rename(hl_meta_id = parent, hl_fish_id = child)) |>
    dplyr::mutate(datetime = prep_dt_delve(sampling_datetime),
                  date = as.Date(datetime, tz = tz),
                  release_datetime = prep_dt_delve(release_datetime),
                  species = from_canonical(common_name),
                  lifestage = stringr::str_to_title(lifestage),
                  pit_applied = ifelse(pit_applied, "Yes", "No")) |>
    dplyr::filter(date >= min_date) |>
    dplyr::select(date, datetime, release_datetime, sampling_lat = sample_coordinates_latitude,
                  sampling_lon = sample_coordinates_longitude, species, lifestage, fl_mm = fork_length_mm,
                  pit_applied, pit_id, comment, hl_fish_id, hl_meta_id) |>
    dplyr::arrange(dplyr::desc(datetime))
}

#' Prepare DELVE hook-and-line water quality data
#'
#'
#' @md
#' @param hl_wq_raw       Unprocessed hook-and-line water quality data
#' @param hl_meta_raw     Unprocessed hook-and-line metadata
#' @param min_date        First date to use for hook-and-line data
#' @param tz              Time zone name
#'
#' @export
#'

prep_hl_wq <- function(hl_wq_raw, hl_meta_raw, min_date = min_hl_date, tz = tz_loc){
  hl_wq_raw |>
    fix_names() |>
    dplyr::rename(hl_wq_id = recordkey) |>
    dplyr::left_join(hl_meta_raw |>
                       create_links("RecordKey", "water-quality-monitoring") |>
                       dplyr::rename(hl_meta_id = parent, hl_wq_id = child)) |>
    dplyr::mutate(datetime = prep_dt_delve(datetime),
                  date = as.Date(datetime, tz = tz)) |>
    dplyr::filter(date >= min_date) |>
    dplyr::select(date, datetime, sampling_loc = station_id, sampling_lat = latitude,
                  sampling_lon = longitude, depth_m, comments, hl_wq_id, hl_meta_id) |>
    dplyr::arrange(dplyr::desc(datetime))
}

#' Prepare DELVE hook-and-line water quality sensor data
#'
#'
#' @md
#' @param hl_sensor_raw       Unprocessed hook-and-line water quality sensor data
#' @param hl_wq_raw           Unprocessed hook-and-line water quality data
#' @param hl_wq               Processed hook-and-line water quality data
#'
#' @export
#'

prep_hl_sensor <- function(hl_sensor_raw, hl_wq_raw, hl_wq){
  hl_sensor_raw |>
    fix_names() |>
    dplyr::rename(hl_sensor_id = recordkey) |>
    dplyr::left_join(hl_wq_raw |>
                       create_links("RecordKey", "sensor-data") |>
                       dplyr::rename(hl_wq_id = parent, hl_sensor_id = child)) |>
    dplyr::left_join(dplyr::select(hl_wq, hl_wq_id, date)) |>
    # drop a row with no data
    dplyr::filter(!is.na(date)) |>
    dplyr::mutate(dplyr::across(chl_a:turb_ntu, ~ ifelse(.x == 999, NA, .x))) |>
    dplyr::select(date, do_mgl = do_conc_mgl, do_sat = do_sat_perc, ph, sal_ppt,
                  spec_cond_us, temp_c, turb_ntu, hl_sensor_id, hl_wq_id) |>
    dplyr::arrange(dplyr::desc(date))
}

