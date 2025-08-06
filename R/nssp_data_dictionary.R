#' NSSP Data Dictionary R Object
#'
#' A subset of the tabs from the NSSP Data Dictionary most commonly used in syndromic surveillance practice.
#'
#' @format ## `nssp_data_dictionary`
#' A list with length 3:
#' \describe{
#'  \item{essence_tab}{data.frame of the ESSENCE tab}
#'  \item{essence_api_and_data_details_tab}{List with length 3:}
#'    \item{essence_apis_legend}{API components for available result types}
#'    \item{essence_data_sources_legend}{API components for available data sources}
#'    \item{data_details_display_name_reference_availability}{Data Details display name and corresponding API name for pulling specific fields and their availability by result type}
#'  \item{essence_api_query_parameters_tab}{Query portal display name and corresponding API name for composing the query}
#' }
#' @source <https://www.cdc.gov/nssp/biosense/docs/NSSP-Data-Dictionary-508.xlsx>
"nssp_data_dictionary"
