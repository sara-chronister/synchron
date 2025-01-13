#' Get a table of the NSSP-ESSENCE API Components
#'
#' @param url NSSP-ESSENCE API URL. Intended to be used in other `synchron` functions, but can be useful on its own as well
#'
#' @return a data frame with two columns, `component` (i.e., query or API component) and `value` (i.e, query criteria)
#' @import tidyr
#' @export
#'
#' @examples
#' url <- "https://essence2.syndromicsurveillance.org/nssp_essence/api/
#' dataDetails?datasource=va_hosp&startDate=1Jan2023&medicalGroupingSystem
#' =essencesyndromes&userId=5099&endDate=31Dec2024&cDeath=no&percentParam
#' =noPercent&hospFacilityType=emergency%20care&aqtTarget=DataDetails
#' &ccddCategory=cdc%20motor%20vehicle%20crash%20occupant%20injury%20v1
#' &geographySystem=hospitalregion&detector=probrepswitch&timeResolution=daily
#' &hasBeenE=1"
#' url_comps <- get_api_components(url)
get_api_components <- function(url) {

  url_parts <- data.frame(component = url) %>%
    separate_longer_delim(component, "&") %>%
    separate_wider_delim(component, "=", names = c("component", "value"))

}
