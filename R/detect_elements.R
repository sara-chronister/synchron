#' Detect elements of an assigned vector
#'
#' @param df Data frame (typically data details).
#' @param terms Vector of terms and/or codes to search for.
#' @param text_field Existing variable in `df` to search for `terms` in.
#' @param group_name Optional string to create and name a new variable that will indicate whether each record in the data set matched any `term` found in `text_field`. If left as `NULL` the variable will not be created.
#' @param id_field Individual row/record identifier. "C_BioSense_ID" unless otherwise specified. If no row identifier exists, create one using `mutate(id_field = row_number)` and set `id_field = id_field` in the function call.
#'
#' @return a data frame
#' @importFrom magrittr %>%
#' @import dplyr
#' @import purrr
#' @import stringr
#' @import tidyselect
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' try(def_icd_codes <- detect_elements(df = data_details,
#' terms = c("J09", "J10", "J11"), text_field = "DDParsed",
#' group_name = "Any_Flu_ICD_Codes"))
detect_elements <- function(df, text_field, terms, id_field = "C_BioSense_ID", group_name = NULL) {

  terms <- stringr::str_to_lower(terms)
  terms_colnames <- stringr::str_replace_all(terms," ",".")

  terms_detected_setup <- df %>%
    dplyr::select(id = !!id_field, field = !!text_field) %>%
    dplyr::mutate(field = stringr::str_to_lower(field))

  terms_detected_list <- list()

  for (i in 1:length(terms)) {
    terms_detected_list[[i]] <- terms_detected_setup %>%
      dplyr::mutate(term = stringr::str_detect(field,terms[i])) %>%
      dplyr::mutate(term = ifelse(term==TRUE,1,0))

    names(terms_detected_list[[i]]) <- c(id_field,text_field,paste(terms_colnames[i],"in",text_field,sep="_"))
  }

  terms_detected <- purrr::reduce(terms_detected_list,dplyr::full_join) %>%
    dplyr::select(tidyselect::all_of(id_field),tidyselect::everything(),-text_field) %>%
    dplyr::distinct()

  group_name <- group_name

  if (is.null(group_name)) {
    df_to_return <- dplyr::full_join(df, terms_detected, by = id_field)
  } else {
    group_name_added <- terms_detected %>%
      dplyr::mutate(sumTerm = dplyr::select(.data, tidyselect::contains("in")) %>% rowSums()) %>%
      dplyr::mutate(AnyTerm = ifelse(sumTerm>0,1,0))%>%
      dplyr::select(tidyselect::all_of(id_field), AnyTerm)
    colnames(group_name_added) <- c(id_field, group_name)
    df_to_return <- dplyr::full_join(df, group_name_added, by = id_field) %>%
      dplyr::full_join(terms_detected, by = id_field)
  }

  return(df_to_return)

}
