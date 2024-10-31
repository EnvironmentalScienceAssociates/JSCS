#' Get DELVE water quality tables
#'
#'
#' @md
#' @param table       DELVE table: meta, station, retrieval, deployment, equipment, calibration,
#'
#' @export
#'

get_wq <- function(table = c("meta", "station", "retrieval", "deployment", "equipment", "calibration")){
  wq_tables = c("meta" = "general-survey-metadata.csv",
                "station" = "collecting-water-quality-and-time-series-data.csv",
                "retrieval" = "wq-logger-retrieval.csv",
                "deployment" = "wq-deployment.csv",
                "equipment" = "wq-equipment.csv",
                "calibration" = "wq-calibration.csv")
  delver::get_dataset_file(project_id = 73, dataset_id = 211, version = 1,
                           filename = wq_tables[[table]])
}

#' Prepare DELVE water quality metadata
#'
#'
#' @md
#' @param wq_meta_raw     Unprocessed water quality metadata
#' @param min_date        First date to use for water quality data
#' @param tz              Time zone name
#'
#' @export
#'

prep_wq_meta <- function(wq_meta_raw, min_date = min_tc_date, tz = tz_loc){
  wq_meta_raw |>
    fix_names() |>
    dplyr::mutate(start_datetime = prep_dt_delve(start_datetime),
                  end_datetime = prep_dt_delve(end_datetime),
                  date = as.Date(start_datetime, tz = tz)) |>
    dplyr::filter(date >= min_date) |>
    dplyr::select(date, start_datetime, end_datetime, location, crew,
                  surface_conditions, wq_meta_id = recordkey) |>
    dplyr::arrange(dplyr::desc(start_datetime))
}

#' Prepare DELVE water quality station data
#'
#'
#' @md
#' @param wq_station_raw     Unprocessed water quality station data
#' @param wq_meta_raw        Unprocessed water quality metadata
#' @param wq_meta            Processed water quality metadata
#'
#' @export
#'

prep_wq_station <- function(wq_station_raw, wq_meta_raw, wq_meta){
  wq_station_raw |>
    fix_names() |>
    dplyr::rename(wq_station_id = recordkey) |>
    dplyr::left_join(wq_meta_raw |>
                       create_links("RecordKey", "station-visits") |>
                       dplyr::rename(wq_meta_id = parent, wq_station_id = child)) |>
    dplyr::left_join(dplyr::select(wq_meta, wq_meta_id, date, start_datetime, end_datetime)) |>
    # samples with no date are from before the min_date
    dplyr::filter(!is.na(date)) |>
    dplyr::select(date, start_datetime, end_datetime, station_id, latitude, longitude,
                  depth_m, depth_ft, comments, wq_station_id, wq_meta_id) |>
    dplyr::arrange(dplyr::desc(start_datetime))
}

#' Prepare DELVE water quality retrieval data
#'
#'
#' @md
#' @param wq_retrieval_raw     Unprocessed water quality retrieval data
#' @param wq_station_raw       Unprocessed water quality station metadata
#' @param min_date             First date to use for water quality data
#' @param tz                   Time zone name
#'
#' @export
#'

prep_wq_retrieval <- function(wq_retrieval_raw, wq_station_raw, min_date = min_tc_date, tz = tz_loc){
  wq_retrieval_raw |>
    fix_names() |>
    dplyr::rename(wq_retrieval_id = recordkey) |>
    dplyr::left_join(wq_station_raw |>
                       create_links("RecordKey", "retrieval-record") |>
                       dplyr::rename(wq_station_id = parent, wq_retrieval_id = child)) |>
    dplyr::mutate(datetime = prep_dt_delve(datetime),
                  date = as.Date(datetime, tz = tz)) |>
    dplyr::filter(date >= min_date) |>
    dplyr::select(date, datetime, fouling, comments, wq_retrieval_id, wq_station_id) |>
    dplyr::arrange(dplyr::desc(datetime))
}

#' Prepare DELVE water quality deployment data
#'
#'
#' @md
#' @param wq_deployment_raw     Unprocessed water quality deployment data
#' @param wq_station_raw        Unprocessed water quality station metadata
#' @param min_date              First date to use for water quality data
#' @param tz                    Time zone name
#'
#' @export
#'

prep_wq_deployment <- function(wq_deployment_raw, wq_station_raw, min_date = min_tc_date, tz = tz_loc){
  wq_deployment_raw |>
    fix_names() |>
    dplyr::rename(wq_deployment_id = recordkey) |>
    dplyr::left_join(wq_station_raw |>
                       create_links("RecordKey", "deployment-record") |>
                       dplyr::rename(wq_station_id = parent, wq_deployment_id = child)) |>
    dplyr::mutate(datetime = prep_dt_delve(deployment_datetime),
                  date = as.Date(datetime, tz = tz)) |>
    dplyr::filter(date >= min_date) |>
    dplyr::select(date, datetime, deployment_type, frequency, comments,
                  wq_deployment_id, wq_station_id) |>
    dplyr::arrange(dplyr::desc(datetime))
}

#' Prepare DELVE water quality equipment data
#'
#'
#' @md
#' @param wq_equipment_raw      Unprocessed water quality equipment data
#' @param wq_retrieval_raw     Unprocessed water quality retrieval data
#' @param wq_deployment_raw     Unprocessed water quality deployment data
#'
#' @export
#'

prep_wq_equipment <- function(wq_equipment_raw, wq_retrieval_raw, wq_deployment_raw){
  tmp = wq_equipment_raw |>
    fix_names() |>
    dplyr::rename(wq_equipment_id = recordkey)

  ret = dplyr::left_join(tmp, wq_retrieval_raw |>
                           create_links("RecordKey", "equipment-retrieved") |>
                           dplyr::rename(wq_retrieval_id = parent, wq_equipment_id = child)) |>
    dplyr::filter(!is.na(wq_retrieval_id))

  dep = dplyr::left_join(tmp, wq_deployment_raw |>
                           create_links("RecordKey", "equipment-record") |>
                           dplyr::rename(wq_deployment_id = parent, wq_equipment_id = child)) |>
    dplyr::filter(!is.na(wq_deployment_id))

  dplyr::bind_rows(dep, ret) |>
    dplyr::mutate(calibration_datetime = prep_dt_delve(calibration_date),
                  equipment_function = from_canonical(equipment_function)) |>
    dplyr::select(equipment_type, equipment_serial_number, equipment_function, equipment_depth, equipment_depth_ft,
                  equipment_depth_reference, depth_in_air, calibration_date, clock_check,
                  comments, wq_equipment_id, wq_retrieval_id, wq_deployment_id)
}

#' Prepare DELVE water quality calibration data
#'
#'
#' @md
#' @param wq_calibration_raw      Unprocessed water quality calibration data
#' @param wq_equipment_raw        Unprocessed water quality equipment data
#'
#' @export
#'

prep_wq_calibration <- function(wq_calibration_raw, wq_equipment_raw){
  wq_calibration_raw |>
    fix_names() |>
    dplyr::rename(wq_calibration_id = recordkey) |>
    dplyr::left_join(wq_equipment_raw |>
                       create_links("RecordKey", "calibration-log") |>
                       dplyr::rename(wq_equipment_id = parent, wq_calibration_id = child)) |>
    dplyr::select(parameter, standard_value, pre_cal_reading, post_cal_reading,
                  passing_value, additional_info, wq_calibration_id, wq_equipment_id)
}
