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
