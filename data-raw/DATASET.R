library(dplyr)
library(usethis)
library(lubridate)

tz_loc = "America/Los_Angeles"
use_data(tz_loc, overwrite = TRUE)

min_tc_date = ymd("2023-09-20", tz = tz_loc)
use_data(min_tc_date, overwrite = TRUE)
