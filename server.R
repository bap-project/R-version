#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
<<<<<<< HEAD
library(tidytext) #text mining, unnesting
library(topicmodels) #the LDA algorithm
library(tidyverse) #
library(kableExtra) #create attractive tables
library(knitr) #simple table generator
library(ggrepel) #text and label geoms for ggplot2
library(gridExtra)
library(formattable) #color tile and color bar in `kables`
library(tm) #text mining
library(circlize) #already loaded, but just being comprehensive
library(plotly) #interactive ggplot graphs
library(RJSONIO)
library(lubridate)
library(stringi)
library(rebus)
library(udpipe)
library(mallet)
library(dfrtopics)
library(dplyr)



=======
>>>>>>> Added pos filter and correlation
options(shiny.maxRequestSize=100*1024^2)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  cleanData <- eventReactive(input$upload,{
    
    hryt <-read_csv(input$file1$datapath,
                    locale = locale(encoding = "UTF-8"))
    
    
    doc_id<-str_c("doc", rownames(hryt))
    hryt$docid<-doc_id
    hryt <- data_frame(doc_id=hryt$docid, text =hryt$Text, date=as.Date(hryt$StartDate) , title=hryt$Title)
    hryt = hryt %>%   mutate_at(vars(date), funs(year, month, day))
    hryt <-hryt %>% filter(Reduce(`+`, lapply(., is.na)) != ncol(.))#delete empty rows
    
    
    hryt$text<-str_replace_all(hryt$text,"yapay zeka", "" )
    hryt$text<-str_replace_all(hryt$text,"Yapay zeka", "" )
    hryt$text<-str_replace_all(hryt$text,"e ticaret", "eticaret" )
    
    hryt$text<-str_replace_all(hryt$text,"$", "ş" )
    
    hryt$text<-str_replace_all(hryt$text,"&Ouml;", "Ö" )
    hryt$text<-str_replace_all(hryt$text,"&ouml;", "ö" )
    hryt$text<-str_replace_all(hryt$text,"&Ccedil;", "Ç" )
    hryt$text<-str_replace_all(hryt$text,"&ccedil;", "ç" )
    hryt$text<-str_replace_all(hryt$text,"&#286;", "Ğ" )
    hryt$text<-str_replace_all(hryt$text,"&#287;", "ğ" )
    hryt$text<-str_replace_all(hryt$text,"&#304;", "İ" )
    hryt$text<-str_replace_all(hryt$text,"&#305;", "ı" )
    hryt$text<-str_replace_all(hryt$text,"&#350;", "Ş" )
    hryt$text<-str_replace_all(hryt$text,"&#351;", "ş" )
    hryt$text<-str_replace_all(hryt$text,"&Uuml;", "Ü" )
    hryt$text<-str_replace_all(hryt$text,"&uuml;", "ü" )
    hryt$text<-str_replace_all(hryt$text,"&acirc;", "a" )
    hryt$text<-str_replace_all(hryt$text,"&rsquo;", "'" )
    hryt$text<-str_replace_all(hryt$text,"&rdquo;", "'" )
    hryt$text<-str_replace_all(hryt$text,"&lsquo;", "" )
    hryt$text<-str_replace_all(hryt$text,"&ldquo;", "" )
    hryt$text<-str_replace_all(hryt$text,"&nbsp;", "" )
    hryt$text<-str_replace_all(hryt$text,"&gt;", "" )
    hryt$text<-str_replace_all(hryt$text,"&lt;", "" )
    hryt$text<-str_replace_all(hryt$text,"iframe;", " " )
    hryt$text<-str_replace_all(hryt$text,"allowfullscreen", " " )
    hryt$text<-str_replace_all(hryt$text,"height", " " )
    hryt$text<-str_replace_all(hryt$text,"class=\"hr", " " )
    hryt$text<-str_replace_all(hryt$text,"10px", " " )
    hryt$text<-str_replace_all(hryt$text,"background", " " )
    hryt$text<-str_replace_all(hryt$text,"allow", " " )
    hryt$text<-str_replace_all(hryt$text,"autoplay", " " )
    hryt$text<-str_replace_all(hryt$text,"background", " " )
    hryt$text<-str_replace_all(hryt$text,"rjet", " " )
    hryt$text<-str_replace_all(hryt$text,"seperator", " " )
    hryt$text<-str_replace_all(hryt$text,"style=", " " )
    hryt$text<-str_replace_all(hryt$text,"media", " " )
    hryt$text<-str_replace_all(hryt$text,"encrypted", " " )
    hryt$text<-str_replace_all(hryt$text,"style", " " )
    hryt$text<-str_replace_all(hryt$text,"NA", " " )
    hryt$text<-str_replace_all(hryt$text,"#ff017e", " " )
    hryt$text<-str_replace_all(hryt$text,"&loz", " " )
    hryt$text<-str_replace_all(hryt$text,"quot", " " )
    hryt$text<-str_replace_all(hryt$text,"yüzde", " " )
    
    hryt$text<-hryt$text %>%str_replace_all("bugü", "bugün")
    hryt$text<-hryt$text %>%str_replace_all("ümle", "cümle")
    hryt$text<-hryt$text %>%str_replace_all("çerik", "içerik")
    hryt$text<-hryt$text %>%str_replace_all("nde", "")
    hryt$text<-hryt$text %>%str_replace_all("nda", "")
    hryt$text<-hryt$text %>%str_replace_all("yla", "")
    hryt$text<-hryt$text %>%str_replace_all("com", "")
    hryt$text<-hryt$text %>%str_replace_all("www", "")
    hryt$text<-hryt$text %>%str_replace_all("dan", "")
    hryt$text<-hryt$text %>%str_replace_all("Hürriyet", "")
    hryt$text<-hryt$text %>%str_replace_all("nin", "")
    hryt$text<-hryt$text %>%str_replace_all("doğru", "")
    hryt$text<-hryt$text %>%str_replace_all("dogru", "")
    hryt$text<-hryt$text %>%str_replace_all("arasi", "")
    hryt$text<-hryt$text %>%str_replace_all("arası", "")
    hryt$text<-hryt$text %>%str_replace_all("dedi", "")
    hryt$text<-hryt$text %>%str_replace_all("var", "")
    hryt$text<-hryt$text %>%str_replace_all("embed", "")
    hryt$text<-hryt$text %>%str_replace_all("sh", "")
    hryt$text<-hryt$text %>%str_replace_all("ürriyet", "")
    hryt$text<-hryt$text %>%str_replace_all("i<U+0307>lgi<U+0307>ni<U+0307>zi<U+0307>", "ilgi")
    hryt$text<-hryt$text %>%str_replace_all("bi<U+0307>lgi<U+0307>sayariniz", "bilgisayar")
    
    hryt$text<-hryt$text %>%str_replace_all("Ä±", "ı")  
    hryt$text<-hryt$text %>%str_replace_all("Ã¼", "ü")
    hryt$text<-hryt$text %>%str_replace_all("Ã¶", "ö")
    hryt$text<-hryt$text %>%str_replace_all("Ã§", "ç")  
    hryt$text<-hryt$text %>%str_replace_all("ÅŸ", "ş")  
    hryt$text<-hryt$text %>%str_replace_all("ÄŸ", "ğ")  
    
    hryt$text<-hryt$text %>%str_replace_all("ÅŸ", "Ş")
    hryt$text<-hryt$text %>%str_replace_all("Ãœ", "Ü")
    hryt$text<-hryt$text %>%str_replace_all("Ã‡", "Ç")
    hryt$text<-hryt$text %>%str_replace_all("Ã–", "Ö")
    hryt$text<-hryt$text %>%str_replace_all("Ä°", "İ")
    hryt$text<-hryt$text %>%str_replace_all("â", "a")
    hryt$text<-hryt$text %>%str_replace_all("\u0307", "")
    
    hryt$text<-hryt$text %>%str_replace_all("^\\s*<U\\+\\w+>\\s*", " ")
    hryt$text<-hryt$text %>%str_replace_all("\\b\\w{1,2}\\s", " ")
    hryt$text<-hryt$text %>%str_replace_all("§", " ")
    hryt$text<-hryt$text %>%str_replace_all("Ã", "")
    hryt$text<-hryt$text %>%str_replace_all("ã", "")
    hryt$text<-hryt$text %>%str_replace_all(" *\\b[[:alpha:]]{1,2}\\b *", "")
    hryt$text<-hryt$text %>%str_replace_all("^ +| +$|( ) +", "\\1")
    
    
   
    
    
    
    
    
    
    
    
    
    
    hryt
    
    
    
    
    
  })

output$plot1 <- renderPlot({
  
  hryt <- cleanData()
  library(dplyr)
  custom_stop_words <- bind_rows(data_frame(word = stopwords::stopwords("tr", source = "stopwords-iso"),
                                            lexicon = "custom"))
  
  library(tidytext)
  library(wordcloud)
  library(stringr)
  
  
  hryt%>%unnest_tokens(word, text) %>%  anti_join(custom_stop_words) %>%count(word, doc_id,sort = TRUE) %>% 
    bind_tf_idf( word, doc_id, n) %>% dplyr::filter(tf_idf>.0009)%>% 
    with(wordcloud(word, n, max.words = 100 ))
  
  
  
})
datePlot <- eventReactive(input$goDate,{
  hryt <- cleanData()
  min <- as.Date(input$dateSelect1)
  max <- as.Date(input$dateSelect2)
  hryt %>% group_by(date) %>% count() %>%ggplot( aes(date, n)) +geom_line()  +
    scale_x_date(  breaks='months' , limits = c(min, max),date_labels = "%b/%Y" )+ ggpubr::rotate_x_text(-90)
  
  
})
output$plot2 <- renderPlot({
  
  datePlot()
  
})

output$table1 <- renderDT({
  
  hryt <- cleanData()
  library(formattable)
  hryt %>%
    group_by(year) %>%
    mutate(word_count = n()) %>%
    dplyr::select( year, word_count) %>% #only need these fields
    distinct() %>%
    ungroup()
})
<<<<<<< HEAD

useModel <- eventReactive(input$udpipeGo,{
  
  hryt <- cleanData()
  
  if(input$downloadModel == T){
    lng <- input$modelLang
    ud_model <- udpipe_download_model(language = lng)
    
  }
  
  if(input$useModel == T)
  {
    
    ud_model <- udpipe_load_model(file = input$modelInput$datapath)
  }
  
   hr <-udpipe_annotate(ud_model, hryt$text)
   dfpos <- as.data.frame(hr)
   dfpos$id <- unique_identifier(dfpos, fields = c("sentence_id", "doc_id"))
   
})

output$text1 <- renderDT({
  
  useModel()
})
=======
>>>>>>> Added pos filter and correlation
  
})
