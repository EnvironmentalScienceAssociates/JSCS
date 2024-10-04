#' Standardize names from DELVE tables
#'
#'
#' @md
#' @param data       Dataframe of DELVE table
#'
#' @export
#'

fix_names <- function(data){
  new_names = gsub("-|\\s+", "_", tolower(names(data)))
  setNames(data, new_names)
}

#' Convert canonical DELVE names to title case
#'
#'
#' @md
#' @param x       DELVE canonical name(s)
#'
#' @export
#'

from_canonical <- function(x){
  stringr::str_to_title(gsub("-", " ", x))
}

#' Fix clip location from DELVE tables
#'
#'
#' @md
#' @param x       DELVE clip location
#'
#' @export
#'

fix_clip_loc <- function(x){
  dplyr::case_when(
    is.na(x) | x == "na" ~ NA_character_,
    x == "caud-fin" ~ "Lower Caudal",
    TRUE ~ from_canonical(x)
  )
}

#' Fix other mark description from DELVE tables
#'
#'
#' @md
#' @param x       DELVE other mark description
#'
#' @export
#'

fix_other_mark_desc <- function(x){
  dplyr::case_when(
    is.na(x) | x %in% c("n/a", "na", "none", "did not appear to have BB") ~ NA_character_,
    x %in% c("bismark", "bismark brown", "bismarck brown", "bismarck brroown", "bizmarck brown", "Bismark Brown", "bb", "BB") ~ "Bismarck Brown",
    TRUE ~ x
  )
}

#' Fix velocity values from DELVE tables
#'
#'
#' @md
#' @param x       DELVE velocity value
#'
#' @export
#'

fix_velocity <- function(vel, neg){
  vel2 = as.numeric(vel)
  ifelse(is.na(neg) | !neg, vel2, vel2 * -1)
}

#' Create links between DELVE tables
#'
#'
#' @md
#' @param data       Dataframe of DELVE table
#' @param parent     Column containing IDs for parent table
#' @param child      Column containing IDs (comma-separated strings) for child table
#'
#' @export
#'

create_links <- function(data, parent, child){
  child_list = strsplit(data[[child]], split = ", ")
  mapply(function(p, c) data.frame(parent = p, child = c),
         data[[parent]], child_list, SIMPLIFY = FALSE) |>
    dplyr::bind_rows()
}

#' Prepare DELVE datetime values
#'
#'
#' @md
#' @param x       DELVE datetime string
#' @param tz      Time zone
#'
#' @export
#'

prep_dt_delve <- function(x, tz = tz_loc){
  lubridate::with_tz(lubridate::mdy_hms(x), tz)
}

#' Make Fulcrum datetime from date and time fields
#'
#'
#' @md
#' @param date       Fulcrum date field
#' @param time       Fulcrum time field
#' @param tz         Time zone
#'
#' @export
#'

prep_dt_fulcrum <- function(date, time, tz = tz_loc){
  lubridate::force_tz(lubridate::ymd_hms(paste(date, lubridate::seconds_to_period(time))), tz)
}

#' Make time string from Fulcrum time field
#'
#'
#' @md
#' @param time       Fulcrum time field
#'
#' @export
#'

prep_time_fulcrum <- function(time){
  pd = lubridate::seconds_to_period(time)
  ifelse(is.na(pd), NA_character_, sprintf("%02i:%02i", lubridate::hour(pd), lubridate::minute(pd)))
}

#' Extract marks from mark status field
#'
#'
#' @md
#' @param data       Dataframe with mark_status column
#'
#' @export
#'

extract_mark <- function(data){
  data |>
    dplyr::mutate(NM = grepl("NM -", mark_status),
                  UC = grepl("UC -", mark_status),
                  LC = grepl("LC -", mark_status),
                  BBY = grepl("BB-Y -", mark_status),
                  VIE = grepl("VIE -", mark_status))
}

#' Extract condition from fish_condition field
#'
#'
#' @md
#' @param data       Dataframe with fish_condition column
#'
#' @export
#'

extract_condition <- function(data){
  data |>
    dplyr::mutate(injury = grepl("I -", fish_condition),
                  mortality = grepl("M -", fish_condition))
}

