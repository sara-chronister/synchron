detect_elements <- function(data, terms, text_field, group_name = NULL, id_field = "C_BioSense_ID") {

  terms <- str_to_lower(terms)
  terms_colnames <- str_replace_all(terms," ",".")

  terms_detected_setup <- data %>%
    select(id = !!id_field, field = !!text_field) %>%
    mutate(field = str_to_lower(field))

  terms_detected_list <- list()

  for (i in 1:length(terms)) {
    terms_detected_list[[i]] <- terms_detected_setup %>%
      dplyr::mutate(term = str_detect(field,terms[i])) %>%
      dplyr::mutate(term = ifelse(term==TRUE,1,0))

    names(terms_detected_list[[i]]) <- c(id_field,text_field,paste(terms_colnames[i],"in",text_field,sep="_"))
  }

  terms_detected <- purrr::reduce(terms_detected_list,full_join) %>%
    select(all_of(id_field),everything(),-text_field) %>%
    distinct()

  if (is.null(group_name)) {
    df_to_return <- full_join(data, terms_detected, by = id_field)
  } else {
    group_name_added <- terms_detected %>%
      mutate(sumTerm = select(., contains("in")) %>% rowSums()) %>%
      mutate(AnyTerm = ifelse(sumTerm>0,1,0))%>%
      select(all_of(id_field), AnyTerm)
    colnames(group_name_added) <- c(id_field, group_name)
    df_to_return <- full_join(data, group_name_added, by = id_field) %>%
      full_join(terms_detected, by = id_field)
  }

  return(df_to_return)

}
