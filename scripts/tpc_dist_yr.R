vizDataFrame<-topics_tidy %>%inner_join(dd, by = "document") %>%
  select(year, topic, gamma)%>%
  group_by(year, topic) %>%
  #get the avg doc gamma value per source/topic
  mutate(mean = mean(gamma)) %>%
  #remove the gamma value as you only need the mean
  select(-gamma) %>%
  #removing gamma created duplicates so remove them
  distinct()

require(pals)
nl<- nlevels(as.factor(vizDataFrame$topic))
p <-ggplot(vizDataFrame, aes(x=year, y=mean, fill=as.factor(topic))) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(nl), "FF"), name = "topic") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


  