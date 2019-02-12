top_sources <- top_documents %>%
  #join back to the tidy form to get the source field
  inner_join(dd) %>%
  select(document,xx , topic) %>%
  distinct() %>%
  group_by(topic) %>%
  #needed by word_chart (not relevant here)
  mutate(row = row_number()) %>%
  ungroup()


