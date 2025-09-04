#' Break up long-term queries into manageable pieces
#'
#' @param url an ESSENCE data details API
#' @param start_date date formate YYYY-MM-DD
#' @param end_date date format YYYY-MM-DD
#' @param by number of days for each loop, default is 14 days
#'
#' @returns data frame
#' @import Rnssp
#' @export
#'
#' @examples
get_longterm_data <- function(url, start_date = NULL, end_date = NULL, by = 14) {

  ## Setup for loop ##
  # function to create a data frame with the start and end dates you are trying to use
  # default is 14 day intervals, but you can specify any number of days in the by argument when you call the function
  loop_start <- as.Date(start_date)
  loop_end <- as.Date(end_date)

  loop_dates <- data.frame(start = seq.Date(from = loop_start, to = loop_end, by = by)) %>%
    mutate(end = start+(by-1)) %>%
    mutate(end = ifelse((loop_end>=start&end>loop_end),loop_end,end)) %>%
    mutate(end = as.Date.numeric(end, origin = "1970-01-01"))

  # initiate a blank list to store the output from the loop
  output_list <- list()

  ## Loop over multiple time frames ##
  # loop to change the dates, get csv from url, and store output in a list
  for (i in 1:nrow(loop_dates)) {

    # update the to the i set of start and end dates in the url
    url_update <- Rnssp::change_dates(url, start_date = loop_dates$start[i], end_date = loop_dates$end[i])
    # generate the output from the i set of start and end dates
    new_output <- Rnssp::get_essence_data(url = url_update) %>%
      # set all variables to character to allow bind_rows to work in case of difference between pulls
      mutate_all(as.character)
    # store the results from the i set of start and end dates as the i element in a list
    output_list[[i]] <- new_output

  }

  ## Result ##
  # collapse results of the list generated in the output above into a data frame
  all_data <- plyr::ldply(output_list, bind_rows)

  return(all_data)

}
