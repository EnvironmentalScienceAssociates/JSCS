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
#'
#' @export
#'

prep_tc_meta <- function(tc_meta_raw){
  tc_meta_raw |>
    fix_names() |>
    dplyr::mutate(start_datetime = prep_datetime(start_datetime),
                  end_datetime = prep_datetime(end_datetime),
                  trap_open_time = prep_datetime(trap_open_time),
                  trap_closed_time = prep_datetime(trap_closed_time),
                  trap_hours = as.numeric(difftime(end_datetime, start_datetime, units = "hours")),
                  start_date = as.Date(start_datetime, tz = "America/Los_Angeles"),
                  temp_c = ifelse(temp_c == 0, NA, temp_c),
                  trap_status = from_canonical(trap_status)) |>
    dplyr::filter(start_date >= min_tc_date) |>
    dplyr::select(sample_id = sampleid, tc_meta_id = recordkey, obs_loc, date = start_date,
                  datetime = start_datetime, end_datetime, trap_hours, trap_status, temp_c,
                  trap_open_time, trap_closed_time)
}
