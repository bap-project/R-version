source("beg_lib.R")
uk <-RJSONIO::fromJSON("UK_afterJaccard.json",encoding = "UTF-8")
us <- RJSONIO::fromJSON("US_afterJaccard.json",encoding = "UTF-8")
cn <- RJSONIO::fromJSON("CN_afterJaccard.json",encoding = "UTF-8")


uk <- data_frame(docid=uk$docid, text =uk$content, date=uk$date, source=uk$source,title=uk$title, country="uk")
us <- data_frame(docid=us$docid, text =us$content, date=us$date, source=us$source,title=us$title, country="us")
cn <- data_frame(docid=cn$docid, text =cn$content, date=cn$date, source=cn$source,title=cn$title , country="cn")
df<-  rbind(uk, us, cn) %>% 
  filter(Reduce(`+`, lapply(., is.na)) != ncol(.))#delete empty rows

df<-df%>%mutate(date =as.Date.POSIXct(date/1000))
df = df %>%   mutate_at(vars(date), funs(year, month, day))
doc_id<-str_c("doc", rownames(df))
df$docid<-doc_id

#str_extract_all(df$text[1:416],"[qualcomm].*" )
#str_view(df$text, ".html")
#sum(str_count(df$text,"a.i"))
dd<-df$text
source("cln_txt.R")
df$text <-dd


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
  filter(word1 == "china") %>%
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

facebook <- search_synonyms(word_vectors, word_vectors["quantum",])
facebook

library(wordcloud)
tidy_skipgrams %>%  anti_join(stop_words) %>%count(word, skipgramID,sort = TRUE) %>% 
  bind_tf_idf( word, skipgramID, n) %>% dplyr::filter(tf_idf>.0009)%>% 
  with(wordcloud(word, n, max.words = 100 ))






library(ggpmisc)
min <- as.Date("2013-1-1")
max <- as.Date("2015-1-1")
df %>% group_by(date) %>% count() %>%ggplot( aes(date, n)) +geom_line()  +
  scale_x_date(  breaks='months' , limits = c(min, max),date_labels = "%b/%Y" )+ ggpubr::rotate_x_text(-45)
#EXAMINE THE DATA
df %>% group_by(year) %>% mutate(n= n()) %>% distinct(year, .keep_all=TRUE) %>% ggplot() + geom_bar(aes(year,n, fill = source), stat ='identity')
df %>% group_by(country) %>% count()  %>% ggplot() + geom_bar(aes(country,n), stat ='identity')



#udmodel <- udpipe_load_model(file = "english-ewt-ud-2.3-181115.udpipe")


#dfp <-udpipe_annotate(udmodel, df$text)
#saveRDS(dfp, "dfPOS.rds")
dfpos <-as.data.frame(readRDS( "dfPOS.rds"))



dfpos$id <- unique_identifier(dfpos, fields = c("sentence_id", "doc_id"))

dff <- dfpos %>%dplyr::filter( upos %in% c("NOUN", "ADJ"))
dff <-trimws(gsub("\\w*[0-9]+\\w*\\s*", "", dff))#removes numbers with letters
dfpos<-gsub(" *\\b(?<!-)\\w{1,2}(?!-)\\b *", " ",dfpos, perl=T)#removes 1 and 2 chr tokens
stopwrd <- c("*.jpg","http","info","imgs","syndigate","*.png", "abff", "fbeb", "per", "cent", "artificial intelligence")
dfpos<-gsub(paste0("\\b(",paste(stopwrd, collapse="|"),")\\b"), "", dtf$term)


dfdt <- document_term_frequencies(dff, document = "id", term = "lemma")
dtmuk <- document_term_matrix(dfdt)
dtmuk <- dtm_remove_lowfreq(dtmuk, minfreq = 5)
dtm_clean <- dtm_remove_terms(dtmuk, terms = c( "hl", "rm"))

termcorrelations <- dtm_cor(dtm_clean)
y <- as_cooccurrence(termcorrelations)
y <- subset(y, term1 < term2 & abs(cooc) > 0.2)
y <- y[order(abs(y$cooc), decreasing = TRUE), ]
head(y, 30)


cooccurrence(x = dff, 
                     term = "lemma", 
                     group = c("doc_id", "paragraph_id", "sentence_id"))
dfpos$phrase_tag <- as_phrasemachine(dfpos$upos, type = "upos")
phrs <- keywords_phrases(x = dfpos$phrase_tag , term = dfpos$lemma, 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
dplyr::filter(phrs, ngram > 1 & freq > 3)
keywords_collocation(x = dfpos, term = "lemma", group = "doc_id")

rake <- keywords_rake(x = dfpos, term = "lemma", group = "doc_id", 
                      relevant = dfpos$upos %in% c("NOUN", "ADJ"))

dfpos$mwe <- txt_recode_ngram(dfpos$token, compound = rake$keyword, ngram = rake$ngram)
dfpos$mwe <- ifelse(dfpos$mwe %in% rake$keyword, dfpos$mwe, NA)
dfpos$term_noun <- ifelse(dfpos$upos %in% "NOUN", dfpos$lemma, NA)

dtf <- document_term_frequencies(dfpos, document = "doc_id", c("lemma", "mwe"))




dtm <- document_term_matrix(x = dtf)
dtm <- dtm_remove_lowfreq(dtm, minfreq = 3)




library(text2vec)
lda_model = LDA$new(n_topics = 50, doc_topic_prior = 0.1, topic_word_prior = 0.01)
doc_topic_distr = 
  lda_model$fit_transform(x = dtm, n_iter = 1000, 
                          convergence_tol = 0.001, n_check_convergence = 25, 
                          progressbar = FALSE)
lda_model$plot()


x <- dtf$term
#cl_fr_mallet
dtf$term <-x

xx <-data.frame(dtf)
source("colps_fr_mallet.R")

collapsed$text <-trimws(gsub("\\w*[0-9]+\\w*\\s*", "", collapsed$text))#removes numbers with letters
collapsed$text<-gsub(" *\\b(?<!-)\\w{1,2}(?!-)\\b *", " ", collapsed$text, perl=T)
stopwrd <- c("*.jpg","http","info","img","syndigate","*.png", "abff", "fbeb", "per", "cent", "artificial intelligence")
collapsed$text<-gsub(paste0("\\b(",paste(stopwrd, collapse="|"),")\\b"), "", collapsed$text)

library("dfrtopics")
library(mallet)
# create an empty file of "stopwords"
file.create(empty_file <- tempfile())
docs <- mallet.import(collapsed$doc_id, collapsed$text, empty_file)
#check for Topic model diagnostics for number of topics
k<- 100 #number of topics
n<- 5 #sequences, intervals
#source("no_tops_coh.R"); pp


#a low alpha value: more weight on having each document composed of only a few dominant topics
#a low beta value: more weight on having each topic composed of only a few dominant words.
mm <-train_model(docs, n_topics=50,
                 n_iters=1000,
                 seed=1066)


write_mallet_model(mm, "modeling_results")
m <- load_mallet_model_directory("modeling_results")

d <- read_diagnostics(file.path("modeling_results", "diagnostics.xml"))
which.min(d$topics$coherence)



# n is the number of words to return for each topic
top_wrd <-top_words(m, n=10)
lbls <-topic_labels(m, n=3)  
xxx<-as.factor(top_wrd$topic)
levels(xxx) <- paste0(lbls)
top_wrd$topic<-xxx

top_wrd%>%dplyr::filter(topic==19 )

top_wrd%>% mutate(word = reorder(word, weight))%>% 
  ggplot(aes(word, weight, fill = topic)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free")+theme(text = element_text(size=10)) +
  coord_flip()


library(ggwordcloud)
set.seed(42)
top_words(m, n=30) %>%
  mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1)))%>%
  ggplot(aes(label = word, size = weight,color = weight,
             angle = angle)) +
  geom_text_wordcloud_area(rm_outside = TRUE) +
  scale_size_area(max_size = 24) +
  scale_color_gradient(low = "darkred", high = "red")+
  theme_minimal()+facet_wrap(~ topic, scales = "free")


#print some diagnosis measures 
t = list()
for(i in 1:25)
{
  str = paste0(names(d$'topics'[3:13]), "=", eval(d$'topics'[i,c(3:13)]))
  t[[length(t)+1]] = str
}
for(i in 1:25){
  xxx <-top_words(m, n=50) %>%filter(topic==i ) %>%
    mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1)))
  p <-ggplot(xxx,aes(label = word, size = weight,color = weight,
                     angle = angle)) +
    geom_text_wordcloud_area(rm_outside = TRUE) +
    scale_size_area(max_size = 24) +
    scale_color_gradient(low = "darkred", high = "red")+
    theme_minimal()
  library(gridExtra)
  grid.arrange(p, right = tableGrob(matrix(t[[i]],ncol=1),
                                    theme = ttheme_minimal(padding = unit(c(.1,.1),"line")))
               ,vp=viewport(width=.75, height=1.5))
}



