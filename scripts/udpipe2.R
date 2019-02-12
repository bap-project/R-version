library(RJSONIO);library(readr); library(tidyverse)
library(tidytext)
library(quanteda)
library(wordcloud)
library(microbenchmark)
library(stringi)
hryt <-read_csv("HurriyetCleaned.csv")

library(RJSONIO);library(readr); library(lubridate); library(tidyverse)
uk <-RJSONIO::fromJSON("uk.json",encoding = "UTF-8")
removeSpecialChars <- function(x) gsub("[^a-zA-Z]", " ", x)
uk$text <- lapply(uk$text, removeSpecialChars)
uk<-uk%>%mutate(date =as.Date.POSIXct(date/1000))
uk = uk %>%   mutate_at(vars(date), funs(year, month, day))

tdy <-function(x) {
  x %>%unnest_tokens(word, text) %>% anti_join(stop_words)%>%
    count(word, sort = TRUE) %>% with(wordcloud(word, n, max.words = 30 ))
}
 
qntd<-function(x) {
 x %>%corpus()%>%
    dfm(remove = stopwords("english"))%>%textplot_wordcloud( max_words = 30)
}


library(udpipe)
udmodel <- udpipe_load_model(file = "english-ewt-ud-2.3-181115.udpipe")

uk <-udpipe_annotate(udmodel, uk$text)
#saveRDS(uk, "ukPOS.rds")
dfpos <-as.data.frame(readRDS( "ukPOS.rds"))

#saveRDS(mydf, "hrytPOS.rds")
dfpos <-as.data.frame(readRDS( "hrytPOS.rds"))
dfpos%>%dplyr::filter(upos==c("NOUN", "ADJ"))
•# counts pos
stats <- txt_freq(dfpos$upos)
stats%>% mutate(key = reorder(key, freq_pct)) %>%
  ggplot(aes(key, freq_pct)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
  

nouns <-dplyr::filter(dfpos, upos==c("NOUN"))
txt_freq(nouns$lemma)%>%with(wordcloud(key, freq_pct, max.words = 30 ))
adjs <-dplyr::filter(dfpos, upos==c("ADJ"))
txt_freq(adjs$lemma)%>%with(wordcloud(key, freq_pct , min.freq=.1))

rake <- keywords_rake(x = dfpos, term = "lemma", group = "doc_id", 
                       relevant = dfpos$upos %in% c("NOUN", "ADJ"))
rake%>%with(wordcloud(keyword, rake , min.freq = 10))

collo <- keywords_collocation(x = dfpos, term = "lemma", group = "doc_id")

dfpos$phrase_tag <- as_phrasemachine(dfpos$upos, type = "upos")
stats <- keywords_phrases(x = dfpos$phrase_tag , term = dfpos$lemma, 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats <- dplyr::filter(stats, ngram > 1 & freq > 3)
stats%>% mutate(key = reorder(keyword, freq)) %>%head(n=50) %>%
  ggplot(aes(keyword, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()



cooc <- cooccurrence(x = dplyr::filter(dfpos, upos %in% c("NOUN", "ADJ")), 
                     term = "lemma", 
                     group = c("doc_id", "paragraph_id", "sentence_id"))
head(cooc)


library(igraph)
library(ggraph)
wordnetwork <- head(cooc, 30)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Cooccurrences within sentence", subtitle = "Nouns & Adjective")


cooc <- cooccurrence(dfpos$lemma, relevant = dfpos$upos %in% c("NOUN", "ADJ"), skipgram = 1)
head(cooc)


library(igraph)
library(ggraph)
library(ggplot2)
wordnetwork <- head(cooc, 30)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc)) +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  labs(title = "Words following one another", subtitle = "Nouns & Adjective")


dfpos$id <- unique_identifier(dfpos, fields = c("sentence_id", "doc_id"))
dtm <- dfpos %>%dplyr::filter( upos %in% c("NOUN", "ADJ"))
dtm <- document_term_frequencies(dtm, document = "id", term = "lemma")
dtm <- document_term_matrix(dtm)
dtm_cl <- dtm_remove_lowfreq(dtm, minfreq = 5)

termcorrelations <- dtm_cor(dtm_cl)
y <- as_cooccurrence(termcorrelations)
y <- subset(y, term1 < term2 & abs(cooc) > 0.2)
y <- y[order(abs(y$cooc), decreasing = TRUE), ]
head(y)




head(dtm_colsums(dtm_clean))
dtm_clean <- dtm_remove_terms(dtm_cl, terms = c(".component-content{margin-right:16px}.quote__source",	
                                                   ".html .html-embed.component .quote.component",
                                                   ".quote.component{margin-left:-60.83px}.html-embed.component",
                                                   ".quote.component{margin-left:-82.33px",
                                                ".quote.component{margin-left:0}.html-embed.component",	
                                                ".quote__author",	
                                                ".quote__content:before{margin-left:-12px;padding-right:1px}}@media",
                                                "-embed.component", "#", ".html .html-embed.component",
                                                ".quote.component", ".html  .html-embed.component", "-human", "?400",
                                                "@elonmusk", "tmgAds.embedPlayer"
))

dtm_clean <- dtm_remove_tfidf(dtm_cl, top = 100)

library(topicmodels)
m <- LDA(dtm_clean, k = 25, method = "Gibbs", 
         control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 1:5))
predict(m, type = "terms", min_posterior = 0.05, min_terms = 3)


keyw_rake <- keywords_rake(dfpos, 
                           term = "token", group = c("doc_id", "paragraph_id", "sentence_id"), 
                           relevant = dfpos$upos %in% c("NOUN", "ADJ"), 
                           ngram_max = 3, n_min = 5)

dfpos$term <- dfpos$token

dfpos$mwe <- txt_recode_ngram(dfpos$token, compound = keyw_rake$keyword, ngram = keyw_rake$ngram)
dfpos$mwe <- ifelse(dfpos$mwe %in% keyw_rake$keyword, dfpos$mwe, NA)
dfpos$term_noun <- ifelse(dfpos$upos %in% "NOUN", dfpos$token, NA)

dtm <- document_term_frequencies(dfpos, document = "topic_level_id", term = c("term_noun", "mwe"))
dtm <- document_term_matrix(x = dtm)
dtm <- dtm_remove_lowfreq(dtm, minfreq = 3)

dtm_clean <- dtm_remove_terms(dtm, terms = c("artificial intelligence",	
                                                "Artificial intelligence"))


dfpos$topic_level_id <- unique_identifier(dfpos, fields = c("doc_id", "paragraph_id", "sentence_id"))



m <- LDA(dtm_clean, k = 25, method = "Gibbs", 
         control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 1:5))
predict(m, type = "terms", min_posterior = 0.05, min_terms = 10)
