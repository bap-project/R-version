library(shiny)
library(shinycssloaders)
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


library(DT)
=======
>>>>>>> Added pos filter and correlation
ui <- navbarPage("Enter App Name Here",
                 navbarMenu("Discover",
           tabPanel("DisIntro"),  
                            
           tabPanel("Plot",
                    sidebarLayout(
                      sidebarPanel(
                       fileInput("file1","Upload Your file"),
                       
                       actionButton("upload","Upload")
                        ),
                      
                      mainPanel(
                        plotOutput("plot1", width = "auto") %>% withSpinner()
                      )
                    )
                    ),
           tabPanel("Dates", 
                    sidebarLayout(
             sidebarPanel(
               DTOutput("table1"),
               textInput("dateSelect1","Filter Date"),
               textInput("dateSelect2","Filter Date"),
               actionButton("goDate","Go")
             ),
             
             mainPanel(
               plotOutput("plot2", width = "auto") %>% withSpinner()
               
             )
<<<<<<< HEAD
           )),
           tabPanel("Udpipe",
                   checkboxInput("downloadModel","Download udpipe model"),
                   checkboxInput("useModel","I have already downloaded the model"),
                   conditionalPanel(
                     condition = "input.downloadModel == true",
                     selectInput("modelLang","Select Model Language",c("afrikaans-afribooms",
                                                                       "ancient_greek-perseus", "ancient_greek-proiel", "arabic-padt",
                                                                       "armenian-armtdp", "basque-bdt", "belarusian-hse", "bulgarian-btb",
                                                                       "buryat-bdt", "catalan-ancora", "chinese-gsd", "coptic-scriptorium",
                                                                       "croatian-set", "czech-cac", "czech-cltt", "czech-fictree", "czech-pdt",
                                                                       "danish-ddt", "dutch-alpino", "dutch-lassysmall", "english-ewt",
                                                                       "english-gum", "english-lines", "english-partut", "estonian-edt",
                                                                       "finnish-ftb", "finnish-tdt", "french-gsd", "french-partut", "french-sequoia",
                                                                       "french-spoken", "galician-ctg", "galician-treegal", "german-gsd",
                                                                       "gothic-proiel", "greek-gdt", "hebrew-htb", "hindi-hdtb", "hungarian-szeged",
                                                                       "indonesian-gsd", "irish-idt", "italian-isdt", "italian-partut",
                                                                       "italian-postwita", "japanese-gsd", "kazakh-ktb", "korean-gsd",
                                                                       "korean-kaist", "kurmanji-mg", "latin-ittb", "latin-perseus", "latin-proiel",
                                                                       "latvian-lvtb", "lithuanian-hse", "maltese-mudt", "marathi-ufal",
                                                                       "north_sami-giella", "norwegian-bokmaal", "norwegian-nynorsk",
                                                                       "norwegian-nynorsklia",      "old_church_slavonic-proiel", "old_french-srcmf",
                                                                       "persian-seraji", "polish-lfg", "polish-sz", "portuguese-bosque",
                                                                       "portuguese-br", "portuguese-gsd", "romanian-nonstandard", "romanian-rrt",
                                                                       "russian-gsd", "russian-syntagrus", "russian-taiga", "sanskrit-ufal",
                                                                       "serbian-set", "slovak-snk", "slovenian-ssj", "slovenian-sst",
                                                                       "spanish-ancora", "spanish-gsd", "swedish-lines", "swedish-talbanken",
                                                                       "tamil-ttb", "telugu-mtg", "turkish-imst", "ukrainian-iu",
                                                                       "upper_sorbian-ufal", "urdu-udtb", "uyghur-udt",      "vietnamese-vtb"))
                        
                   ),
                   conditionalPanel(
                     condition ="input.useModel == true",
                     fileInput("modelInput","Upload Your Model")
                   ),
                   actionButton("udpipeGo","Go"),
                   DTOutput("text1") %>% withSpinner()
                    )
=======
           ))
>>>>>>> Added pos filter and correlation
           
           ),#end of Discover,
                    
           tabPanel("Summary",
                    verbatimTextOutput("summary")
           ),
           navbarMenu("More",
                      tabPanel("Table",
                               DT::dataTableOutput("table")
                      ),
                      tabPanel("About"
                              
                      )
           )

           

)#end of navbar

ui