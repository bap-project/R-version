p <-topicProportionExamples%>% 
  gather(topic, value, -document )%>% 
  ggplot( aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document)+ theme(legend.position="none")