<<<<<<< HEAD
source("scripts/beg_lib.R")
=======
source("BAP/scripts/beg_lib.R")
>>>>>>> Added pos filter and correlation
hryt <-read_csv("resources/HurriyetCleaned.csv",
                locale = locale(encoding = "UTF-8"))


doc_id<-str_c("doc", rownames(hryt))
hryt$docid<-doc_id
hryt <- data_frame(doc_id=hryt$docid, text =hryt$Text, date=as.Date(hryt$StartDate) , title=hryt$Title)
hryt = hryt %>%   mutate_at(vars(date), funs(year, month, day))
hryt <-hryt %>% filter(Reduce(`+`, lapply(., is.na)) != ncol(.))#delete empty rows

source("scripts/str_cl_tr.R")


library(stopwords)
data.frame(stopwords::stopwords("tr", source = "stopwords-iso"), 20)

library(dplyr)
custom_stop_words <- bind_rows(data_frame(word = stopwords::stopwords("tr", source = "stopwords-iso"),
                                          lexicon = "custom"))

library(tidytext)
library(wordcloud)
library(stringr)


hryt%>%unnest_tokens(word, text) %>%  anti_join(custom_stop_words) %>%count(word, doc_id,sort = TRUE) %>% 
  bind_tf_idf( word, doc_id, n) %>% dplyr::filter(tf_idf>.0009)%>% 
  with(wordcloud(word, n, max.words = 100 ))



library(ggpmisc)
min <- as.Date("2005-1-1")
max <- as.Date("2017-1-1")
hryt %>% group_by(date) %>% count() %>%ggplot( aes(date, n)) +geom_line()  +
<<<<<<< HEAD
  scale_x_date(  breaks='months' , limits = c(min, max),date_labels = "%b/%Y" )+ ggpubr::rotate_x_text(-45)
=======
  scale_x_date(  breaks='months' , limits = c(min, max),date_labels = "%b/%Y" )+ ggpubr::rotate_x_text(-90)
>>>>>>> Added pos filter and correlation
#EXAMINE THE DATA

library(formattable)
hryt %>%
  group_by(year) %>%
  mutate(word_count = n()) %>%
  dplyr::select( year, word_count) %>% #only need these fields
  distinct() %>%
  ungroup()



<<<<<<< HEAD
ud_model <- udpipe_download_model(language = "turkish-imst")
=======
#ud_model <- udpipe_download_model(language = "turkish")
>>>>>>> Added pos filter and correlation
#ud_model <- udpipe_load_model(file = "turkish-imst-ud-2.3-181115.udpipe")

#hr <-udpipe_annotate(ud_model, hryt$text)
#saveRDS(hr, "hrPOS.rds")
dfpos <-as.data.frame(readRDS( "drive/hrPOS.rds"))
dfpos$id <- unique_identifier(dfpos, fields = c("sentence_id", "doc_id"))

dff <- dfpos %>%dplyr::filter( upos %in% c("NOUN", "ADJ"))

#########
dfdt <- document_term_frequencies(dff, document = "id", term = "lemma")
dtmuk <- document_term_matrix(dfdt)
dtmuk <- dtm_remove_lowfreq(dtmuk, minfreq = 3)
dtmtf <- dtm_remove_tfidf(dtmuk, prob=0.01)

dtm_clean <- dtm_remove_terms(dtmtf, terms = c("NA", "yapay zeka", "Yapay zeka"))


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
##########
rake <- keywords_rake(x = dfpos, term = "token", group = "doc_id", 
                      relevant = dfpos$upos %in% c("NOUN", "ADJ"))
tail(arrange(rake, freq),n=100)

dfpos$mwe <- txt_recode_ngram(dfpos$token, compound = rake$keyword, ngram = rake$ngram)
dfpos$mwe <- ifelse(dfpos$mwe %in% rake$keyword, dfpos$mwe, NA)
dfpos$term_noun <- ifelse(dfpos$upos %in% "NOUN", dfpos$lemma, NA)

dtf <- document_term_frequencies(dfpos, document = "doc_id", c("term_noun", "mwe"))
dtm <- document_term_matrix(x = dtf)
dtm <- dtm_remove_lowfreq(dtm, minfreq = 3)
dtmtf <- dtm_remove_tfidf(dtm, top = ncol(dtm)*.97)


x <- dtf$term
#cl_fr_mallet
dtf$term <-x

#Decide on the number of topics, too long find another algorthm
dd<-dtmtf
<<<<<<< HEAD
source("nof_topics.R")
=======
source("scripts/nof_topics.R")
>>>>>>> Added pos filter and correlation

#collapse document term frequencies for mallet
xx <-data.frame(dtf)
source("colps_fr_mallet.R")
x<-collapsed$text
source("cln_fr_mllt.R")
collapsed$text<-x
#clean unicode <U+307

library("dfrtopics")
library(mallet)
# create an empty file of "stopwords"
file.create(empty_file <- tempfile())
docs <- mallet.import(collapsed$doc_id, collapsed$text, empty_file)
#check for Topic model diagnostics for number of topics
k<- 50 #number of topics
n<- 5 #sequences, intervals
#source("no_tops_coh.R"); pp

library(text2vec)
lda_model = LDA$new(n_topics = 10, doc_topic_prior = 0.1, topic_word_prior = 0.01)
doc_topic_distr = 
  lda_model$fit_transform(x = dtmtf, n_iter = 1000, 
                          convergence_tol = 0.001, n_check_convergence = 25, 
                          progressbar = FALSE)

lda_model$plot()

#a low alpha value: more weight on having each document composed of only a few dominant topics
#a low beta value: more weight on having each topic composed of only a few dominant words.
mm <-train_model(docs, n_topics=25,alpha_sum = 2, beta = 0.001,
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

top_wrd%>%dplyr::filter(topic=="12 festival takim teknoloji" )

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




#######

##############
top_d <- top_docs(m, n=10)#number of documents

lbls <-topic_labels(m, n=3)  
xxx<-as.factor(top_d$topic)
levels(xxx) <- paste0(lbls)
top_d$topicL<-xxx

#plot top docs for each topic
top_d%>% mutate(doc = reorder(doc, weight))%>% 
  ggplot(aes(doc, weight, fill = topic)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topicL, scales = "free")+theme(text = element_text(size=10)) +
  coord_flip()

#retrieve docs
ids <- doc_ids(m)[top_d$doc[top_d$topic == 1]]#which topic
hryt%>%filter(doc_id %in% ids)%>%select(title)#tit

#visualize topic distribution per doc
ss <-doc_topics(m)
document <- doc_ids(m)
doc.topics.df <-data.frame(document, ss)
lbls <-topic_labels(m, n=3)  
colnames(doc.topics.df)<-c("document", lbls)
exampleIds <- c("doc147", "doc426") #ids
topicProportionExamples <- doc.topics.df%>%dplyr::filter(document %in% exampleIds)


topicProportionExamples%>% 
  gather(topic, value, -document )%>% 
  ggplot( aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(text = element_text(size=10)) +  
  coord_flip() +
  facet_wrap(~ document)+ theme(legend.position="none")

#retrieve
hryt%>%filter(doc_id %in% exampleIds)%>%select(text)#tit


#####ccheck if any of the topics are concentrated in a few documents
tpc <-4#which toic?
barplot(doc.topics.df[, tpc+1],names = doc.topics.df$document ,cex.names = .5,  las=0)

#topic distrbution by year, source etc.. metadata
topicProportions <- colSums(doc.topics.df[-1])/nrow(hryt)
sort(topicProportions, decreasing = TRUE) # show sum
dd<-hryt
dd<-dd%>%rename(document=doc_id)
#topic distribution by year
m2 <-RTopicModel(mm)
topics_tidy <- tidy(m2, matrix = "gamma")
source("tpc_dist_yr.R");p



#MDS
library(smacof)

zz <-cor(doc.topics.df[-c(1,6,10,11,15,18) ]) %>%sim2diss(method="corr")
res <- mds(zz,ndim = 2, type = "interval",  itmax = 10000, init = "torgerson")
DF1 <-res$conf
library(ggpubr)

ggscatter(data.frame(DF1), x = "D1", y = "D2", 
               label = rownames(DF1), size =max(res$spp)-res$spp,alpha=.3, legend="none",
               repel = TRUE)


#cluster
dd <-doc.topics.df[-c(1,6,10,11,15,18) ] %>%map_dfr(~as.numeric(.))
dis.scaled <- na.omit(scale(dd))
dis <- dist(t(dis.scaled))
res.hc <- hclust(dis,  method = "ward.D2")
print(fviz_dend(res.hc, cex = 0.65, k = 4, palette = "jco") )
require("igraph")
print(fviz_dend(res.hc, k = 4, k_colors = "jco",
                type = "phylogenic", repel = TRUE))
res.coph <- cophenetic(res.hc)
cor(dis, res.coph)



set.seed(123466)
res.pam <- pam(t(dis.scaled), 6)k
fviz_silhouette(res.pam, palette = "jco",
                ggtheme = theme_classic())
print(fviz_cluster(res.pam, data=data.frame(t(dis.scaled)),
                   ellipse.type = "euclid", # Concentration ellipse
                   star.plot = TRUE, # Add segments from centroids to items
                   repel = TRUE, # Avoid label overplotting (slow)
                   ggtheme = theme_minimal()))


