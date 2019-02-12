unigram_probs <- df%>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

library(widyr)

tidy_skipgrams <- df%>%
  unnest_tokens(ngram, text, token = "ngrams", n = 8) %>%
  mutate(ngramID = row_number()) %>% 
  unite(skipgramID, docid, ngramID) %>%
  unnest_tokens(word, ngram)

skipgram_probs <- tidy_skipgrams %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

normalized_prob <- skipgram_probs %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

normalized_prob %>% 
  filter(word1 == "artificial") %>%
  arrange(-p_together)

pmi_matrix <- normalized_prob %>%
  mutate(pmi = log10(p_together)) %>%
  cast_sparse(word1, word2, pmi)

library(irlba)
pmi_svd <- irlba(pmi_matrix, 256, maxit = 1e3)

word_vectors <- pmi_svd$u
rownames(word_vectors) <- rownames(pmi_matrix)

library(broom)

search_synonyms <- function(word_vectors, selected_vector) {
  
  similarities <- word_vectors %*% selected_vector %>%
    tidy() %>%
    as_tibble() %>%
    rename(token = .rownames,
           similarity = unrowname.x.)
  
  similarities %>%
    arrange(-similarity)    
}

facebook <- search_synonyms(word_vectors, word_vectors["robot",])
facebook

library(wordcloud)
tidy_skipgrams %>%  anti_join(stop_words) %>%count(word, skipgramID,sort = TRUE) %>% 
  bind_tf_idf( word, skipgramID, n) %>% dplyr::filter(tf_idf>.0009)%>% 
  with(wordcloud(word, n, max.words = 100 ))
