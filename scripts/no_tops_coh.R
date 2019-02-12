
fn <- function(x,k) {x %>%train_model( n_topics=k,
            n_iters=300,
            seed=1066)%>%write_mallet_model("modeling_results")
d <-read_diagnostics(file.path("modeling_results", "diagnostics.xml"))
d$'topics'%>% summarise_all(funs(mean))

}


ccc <- list()
sequ <- seq(2, k, n) # in this case a sequence of numbers from 1 to 50, by ones.
for(i in sequ) {
  # i-th element of `u1` squared into `i`-th position of `usq`
  
  ccc[[i]]<- fn(docs,i)
  ccc[[i]]
}
library(purrr)
cc <-do.call(rbind.data.frame, ccc)%>%map_dfr(~scale(.))
cc <-data.frame(sequ, cc)


pp <-cc%>%gather(key,value, coherence, uniform_dist,token.doc.diff,rank_1_docs)%>%
  ggplot(aes(sequ, value,colour=key)) +
  geom_point() +
  geom_line() +
  labs(title = "Evaluating LDA topic models",
       subtitle = "Optimal number of topics",
       x = "Number of topics",
       y = "measure")
                    


