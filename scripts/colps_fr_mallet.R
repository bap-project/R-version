collapsed <-xx%>%bind_tf_idf(term, doc_id, freq)%>%group_by(term) %>% mutate(x= mean(tf_idf)) %>% 
  dplyr::filter(x>.03)%>%distinct(doc_id, .keep_all=TRUE) %>%
  ungroup() %>%dplyr::select(doc_id, term)%>%group_by(doc_id) %>%
  summarize(text = paste(term, collapse = " "))