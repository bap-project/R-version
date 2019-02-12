

dd <- str_replace_all(dd,  "[^A-Za-z0-9(),!?\\'\\`\\.]", " ")#REMOVES anything not alphanum, !? ' and .
dd<-str_replace_all(dd,"[^ -~]", " " )

dd <-str_replace_all(dd,"\\.html", " ")
dd <-str_replace_all(dd,"\\margin-right", " ")
dd <-str_replace_all(dd,"\\margin-left", " ")
dd <-str_replace_all(dd,"urls.", " ")
dd <-str_replace_all(dd,"font", " ")
dd <-str_replace_all(dd,"serif", " ")
dd <-str_replace_all(dd,"px", " ")
dd<-str_replace_all(dd, pattern = ("(?=.inline).*(?= !)") , " ")#rmv between html and 
dd<-str_replace_all(dd, pattern = ("(?=.inline).*(?= 003366)") , " ")#rmv between html and 
dd <-str_replace_all(dd, pattern = ("(?=!).*(?= Times)") , " ")#rmv between html and 
dd<-str_replace_all(dd, pattern = ("(?= embed.component).*(?= 82.33)") , " ")#rmv between html and 
dd<-str_replace_all(dd, pattern = ("(?= \\.tg).*(?= 122842)") , " ")#rmv between html and 

