#' Return output of an NSSP-ESSENCE API URL
#'
#' @description
#' `get_essence_data` is a wrapper function that uses the `myProfile` credentials
#' and API pull functions from the `Rnssp` package. It detects what the API type
#' is and returns the object associated with that API type. Developed based on
#' examples from \url{https://cdn.ymaws.com/www.cste.org/resource/resmgr/RStudio_ESSENCE_API_Guide_J.html}
#'
#' @param url NSSP-ESSENCE URL
#' @param start_date use format "YYYY-MM-DD"
#' @param end_date use format "YYY-MM-DD"
#'
#' @return dependent on API type
#' @import Rnssp
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom magrittr extract2
#' @export
#'
#' @examples
#' ts_graph_url <- "https://essence2.syndromicsurveillance.org/nssp_essence/api
#' /timeSeries/graph?datasource=va_hosp&startDate=4Oct2024&medicalGroupingSystem
#' =essencesyndromes&endDate=2Jan2025&percentParam=noPercent&aqtTarget=TimeSeries
#' &ccddCategory=cdc%20respiratory%20syncytial%20virus%20dd%20v1&geographySystem
#' =hospitalregion&detector=nodetectordetector&timeResolution=daily&hasBeenE=1"
#' try(get_essence_data(ts_graph_url))
#'
#' data_details_json_url <- "https://essence2.syndromicsurveillance.org/
#' nssp_essence/api/dataDetails?datasource=va_hosp&startDate=04Oct24
#' &medicalGroupingSystem=essencesyndromes&endDate=02Jan25&percentParam=noPercent
#' &aqtTarget=TimeSeries&ccddCategory=cdc%20respiratory%20syncytial%20virus%20dd%20v1
#' &geographySystem=hospitalregion&detector=nodetectordetector&
#' timeResolution=daily&hasBeenE=1"
#' try(get_essence_data(data_details_json_url))
#'
#' table_builder_json_url <- "https://essence2.syndromicsurveillance.org/
#' nssp_essence/api/tableBuilder?datasource=va_hosp&startDate=4Oct2024
#' &medicalGroupingSystem=essencesyndromes&endDate=2Jan2025&percentParam=noPercent
#' &aqtTarget=TableBuilder&ccddCategory=cdc%20respiratory%20syncytial%20virus%20dd%20v1
#' &geographySystem=hospitalregion&detector=nodetectordetector&timeResolution=daily
#' &hasBeenE=1&rowFields=timeResolution&columnField=ccddCategory"
#' try(get_essence_data(table_builder_json_url))
get_essence_data <- function(url, start_date = NULL, end_date = NULL) {

  url <- url
  start_date <- start_date
  end_data <- end_date

  api_type <- str_extract(url,"(?<=api/).+(?=\\?)")

  url_new <- Rnssp::change_dates(url, start_date, end_date)

  if (api_type == "timeSeries") {
    api_response <- myProfile$get_api_response(url_new)
    api_response_json <- content(api_response, as = "text")
    api_data <- fromJSON(api_response_json) %>%
      extract2("timeSeriesData")
  } else if (api_type == "timeSeries/graph") {
    api_png <- myProfile$get_api_tsgraph(url_new)
    knitr::include_graphics(api_png$tsgraph)
  } else if (api_type == "tableBuilder") {
    api_data <- myProfile$get_api_data(url_new)
  } else if (api_type == "tableBuilder/csv") {
    api_data <- myProfile$get_api_data(url_new, fromCSV = TRUE)
  } else if (api_type == "dataDetails") {
    api_data <- myProfile$get_api_data(url_new) %>%
      extract2("dataDetails")
  } else if (api_type == "dataDetails/csv") {
    api_data <- myProfile$get_api_data(url_new, fromCSV = TRUE)
  } else if (api_type == "summaryData") {
    api_data <- myProfile$get_api_data(url_new) %>%
      extract2("summaryData")
  } else if (api_type == "alerts/regionSyndromeAlerts") {
    api_data <- myProfile$get_api_data(url_new) %>%
      extract2("regionSyndromeAlerts")
  } else if (api_type == "alerts/hospitalSyndromeAlerts") {
    api_data <- myProfile$get_api_data(url_new) %>%
      extract2("hospitalSyndromeAlerts")
  } else {
    writeLines("Error: API did not work as expected. Please check your URL, dates, and password before trying again.")
  }

}
