install.packages("networkD3")
install.packages("hpackedbubble")
install.packages("lookbehind")
install.packages("viridis")
library("viridis")
library("hpackedbubble")
install.packages("tidyverse")
install.packages("gapminder")
library("gapminder")
install.packages("plotly")
library("tidyverse")
library("plotly")
install.packages("ggplots")
install.packages("heatmap.plus")
install.packages("RColorBrewer")
install.packages("circlize")
install.packages("alluvial")
install.packages("ggalluvial")
library("networkD3")
library("alluvial")
library("ggalluvial")
library("circlize")
library("heatmap.plus")
library("RColorBrewer")
install.packages("colorRamps")
install.packages("plyr")
library("plyr")
install.packages("dplyr")
library("dplyr")
install.packages("lubridate")
library("lubridate")
install.packages("sjlabelled")
library("sjlabelled")
install.packages("tidyr")
library("tidyr")
install.packages("readr")
library("readr")
install.packages("ggplot2")
library("ggplot")
install.packages("extrafont")
library("extrafont")
install.packages("RecordLinkage")
library("RecordLinkage")
install.packages("wordcloud")
library("wordcloud")
install.packages("wordcloud2")
library("wordcloud2")
install.packages("tm")
library("tm")
install.packages("http://cran.r-project.org/bin/windows/contrib/3.0/tm_0.5-10.zip")

## Import The data ##
newvandana <- read.csv(file.choose(), header = TRUE)
## View iported dataframe ##
View(newvandana)
## Cleaning Runtime Column for removing alphabetical part   ##
newvandana<-mutate(newvandana,Runtime =as.character(Runtime))
newvandana_df1 <-mutate(newvandana,Runtime=sapply(strsplit(newvandana$Runtime, split=' ', fixed=TRUE),function(x) (x[1])))
View(newvandana_df1)
## CHecking Missing Data ##
sum(is.na(newvandana_df1$Runtime))
sum(is.na(newvandana_df1$IMDB_Rating))
sum(is.na(newvandana_df1$Meta_score))
sum(is.na(newvandana_df1$No_of_Votes))
sum(is.na(newvandana_df1$Gross))
## Replace Missing Value with Zeroes ##
newvandana_df1$Meta_score[is.na(newvandana_df1$Meta_score)] <- 0
newvandana_df1$Gross[is.na(newvandana_df1$Gross)] <- 0
## Check the structure of the dataframe##
str(newvandana_df1)
## Convert character variable into numeric vvariable ##
## 1.) Runtime into class interval
newvandana_df1$Runtime <- as.numeric(newvandana_df1$Runtime)
summary(newvandana_df1$Runtime)
newvandana_df1$Runtime<- cut(newvandana_df1$Runtime, c(40,100,160,220,360))
digit_to_words <- function(digit){
  return(switch(digit, "(40,100]" = {"40 to 100"}, "(100,160]" = {"100 to 160"}, "(160,220]" = {"160 to 220"},  "(220,360]" = {"More Than 220"}))
}
newvandana_df1$Runtime <- as.character(sapply(newvandana_df1$Runtime, digit_to_words))
## 2.) Released Year into Class Interval 
newvandana_df1$Released_Year <- as.numeric(newvandana_df1$Released_Year )
summary(newvandana_df1$Released_Year)
##### a.) Converting to Class Interval ####
newvandana_df1$Released_Year <- cut(newvandana_df1$Released_Year , c(1920,1930,1940, 1950, 1960, 1970,1980,1990,2000,2010,2020))
digit_to_words1 <- function(digit){
  return(switch(digit, "(1920,1930]" = {"1920 to 1930"}, "(1930,1940]" = {"1930 to 1940"},
                "(1940,1950]" = {"1940 to 1950"},  "(1950,1960]" = {"1950 to 1960"},
                "(1960, 1970]" = {"1960 to 1970"}, "(1970, 1980]" = {"1970 to 1980"},
                "(1980, 1990]" = {"1980 to 1990"}, "(1990, 2000]"={"1990 to 2000"},
                "(2000, 2010]" ={"2000 to 2010"},"(2010, 2020]"= {"2010 to 2020"}))
}
newvandana_df1$Released_Year <- as.character(sapply(newvandana_df1$Released_Year, digit_to_words1))
### b.) Remove NULL values #####
newvandana_df1 <- newvandana_df1[!newvandana_df1$Released_Year == "NULL", ] 
View(newvandana_df1)
## 3.) IMDB rating into Class Interval 
summary(newvandana_df1$IMDB_Rating)
#### a.) Converting into class Interval ###
newvandana_df1$IMDB_Rating<- cut(newvandana_df1$IMDB_Rating, c(7.5,8,8.5,9,9.5))
digit_to_words2 <- function(digit){
  return(switch(digit, "(7.5,8]" = {"7.5 to 8"}, "(8,8.5]" = {"8 to 8.5"}, "(8.5,9]" = {"8.5 to 9"},  "(9,9.5]" = {"9 to 9.5"}))
}
newvandana_df1$IMDB_Rating <- as.character(sapply(newvandana_df1$IMDB_Rating, digit_to_words2))
## 4.) Meta Score into Class Interval ##
summary(newvandana_df1$Meta_score)
newvandana_df1$Meta_score<- cut(newvandana_df1$Meta_score, c(0,20,40,60,80,100))
digit_to_words3 <- function(digit){
  return(switch(digit, "(0,20]" = {"0 to 20"}, "(20,40]" = {"20 to 40"}, "(40,60]" = {"40 to 60"},  "(60,80]" = {"60 to 80"}, "(80,100]" = {"80 to 100"}))
}
newvandana_df1$Meta_score <- as.character(sapply(newvandana_df1$Meta_score, digit_to_words3))
newvandana_df1 <- newvandana_df1[!newvandana_df1$Meta_score == "NULL", ] 
## 5.Remove Blank from Certificates ##
newvandana_df1<- newvandana_df1[!(newvandana_df1$Certificate == "" | is.na(newvandana_df1$Certificate)), ]
View(newvandana_df1)
## 6. Convert Gross into Gross_in_1000
newvandana_df1<- newvandana_df1[!(newvandana_df1$Gross == "0" | is.na(newvandana_df1$Gross)), ]
newvandana_df1<- newvandana_df1%>% 
  mutate(Gross_in_1000 = Gross / 1000)
summary(newvandana_df1$Gross_in_1000)
ggplot(newvandana_df1, aes(x = No_of_Votes , y = Gross_in_1000, size = IMDB_Rating, color = Certificate))+
  geom_point(alpha = 0.9)
colnames(newvandana_df1)
# DIVIDE DATA into Textual and Class ##
newvandana_df111 <- newvandana_df1 %>%
  group_by(Released_Year, Runtime, Certificate, IMDB_Rating, Meta_score)%>%
  dplyr::summarize(Freq = n())
View(newvandana_df111)
count(newvandana_df111$Certificate)
## 1.) Textual Data frame ##
newvandana_df1_text <- newvandana_df1[,c(1,)]
#### ALUVIAL PLOT####
colnames(newvandana_df111)
ggplot(data = newvandana_df111,
       aes(axis1 = Released_Year, axis2 = Runtime, axis3 =IMDB_Rating,
           axis4 = Meta_score,
           y = Freq)) +
  scale_x_discrete(limits = c("Released_Year", "Runtime", "IMDB_Rating","Meta_score"), expand = c(.2, .05)) +
  xlab("Movie_Detail") +
  geom_alluvium(aes(fill = Certificate)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  ggtitle("Movie Analysis")
count(newvandana_df111$Certificate)
### Separate Data ###
vec <- c("A","R")
vec
nedata_newvandana_df111 <- newvandana_df111[newvandana_df111$Certificate %in% vec,]
### Aluvial Diagram of First Set of Data ###
ggplot(data = nedata_newvandana_df111,
       aes(axis1 = Released_Year, axis2 = Runtime, axis3 =IMDB_Rating,
           axis4 = Meta_score,
           y = Freq)) +
  scale_x_discrete(limits = c("Released_Year", "Runtime", "IMDB_Rating","Meta_score"), expand = c(.2, .05)) +
  xlab("Movie_Detail1") +
  geom_alluvium(aes(fill = Certificate)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  ggtitle("Movie Analysis")
alluvial(nedata_newvandana_df111[,1:5], freq=nedata_newvandana_df111$Freq, 
         col = ifelse(nedata_newvandana_df111$Certificate == "A", "green", "blue"),
         alpha = 0.5,cex = 0.8,
         blocks=FALSE
)
### Separate Data2 ###
vec1 <- c("Approved","G")
vec1
nedata_newvandana_df1112 <- newvandana_df111[newvandana_df111$Certificate %in% vec1,]
### Aluvial Diagram of First Set of Data ###
ggplot(data = nedata_newvandana_df1112,
       aes(axis1 = Released_Year, axis2 = Runtime, axis3 =IMDB_Rating,
           axis4 = Meta_score,
           y = Freq)) +
  scale_x_discrete(limits = c("Released_Year", "Runtime", "IMDB_Rating","Meta_score"), expand = c(.2, .05)) +
  xlab("Movie_Detail1") +
  geom_alluvium(aes(fill = Certificate)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  ggtitle("Movie Analysis")
alluvial(nedata_newvandana_df1112[,1:5], freq=nedata_newvandana_df1112$Freq, 
         col = ifelse(nedata_newvandana_df1112$Certificate == "Approved", "green", "blue"),
         alpha = 0.5,cex = 0.8,
         blocks=FALSE
)
### Separate Data2 ###
vec2 <- c("GP","Passed")
vec2
nedata_newvandana_df1113 <- newvandana_df111[newvandana_df111$Certificate %in% vec2,]
### Aluvial Diagram of First Set of Data ###
ggplot(data = nedata_newvandana_df1113,
       aes(axis1 = Released_Year, axis2 = Runtime, axis3 =IMDB_Rating,
           axis4 = Meta_score,
           y = Freq)) +
  scale_x_discrete(limits = c("Released_Year", "Runtime", "IMDB_Rating","Meta_score"), expand = c(.2, .05)) +
  xlab("Movie_Detail1") +
  geom_alluvium(aes(fill = Certificate)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  ggtitle("Movie Analysis")
alluvial(nedata_newvandana_df1113[,1:5], freq=nedata_newvandana_df1113$Freq, 
         col = ifelse(nedata_newvandana_df1113$Certificate == "GP", "green", "blue"),
         alpha = 0.5,cex = 0.8,
         blocks=FALSE
)
### Separate Data3 ###
vec3 <- c("PG","PG-13")
vec3
nedata_newvandana_df1114 <- newvandana_df111[newvandana_df111$Certificate %in% vec3,]
### Aluvial Diagram of First Set of Data ###
ggplot(data = nedata_newvandana_df1114,
       aes(axis1 = Released_Year, axis2 = Runtime, axis3 =IMDB_Rating,
           axis4 = Meta_score,
           y = Freq)) +
  scale_x_discrete(limits = c("Released_Year", "Runtime", "IMDB_Rating","Meta_score"), expand = c(.2, .05)) +
  xlab("Movie_Detail1") +
  geom_alluvium(aes(fill = Certificate)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  ggtitle("Movie Analysis")
alluvial(nedata_newvandana_df1114[,1:5], freq = nedata_newvandana_df1114$Freq, 
         col = ifelse(nedata_newvandana_df1114$Certificate == "PG", "green", "blue"),
         alpha = 0.5,cex = 0.8,
         blocks=FALSE
)
### Separate Data4 ###
vec4 <- c("TV-PG","U/A")
vec4
nedata_newvandana_df1115 <- newvandana_df111[newvandana_df111$Certificate %in% vec4,]
### Aluvial Diagram of First Set of Data ###
ggplot(data = nedata_newvandana_df1115,
       aes(axis1 = Released_Year, axis2 = Runtime, axis3 =IMDB_Rating,
           axis4 = Meta_score,
           y = Freq)) +
  scale_x_discrete(limits = c("Released_Year", "Runtime", "IMDB_Rating","Meta_score"), expand = c(.2, .05)) +
  xlab("Movie_Detail1") +
  geom_alluvium(aes(fill = Certificate)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  ggtitle("Movie Analysis")
alluvial(nedata_newvandana_df1115[,1:5], freq = nedata_newvandana_df1115$Freq, 
         col = ifelse(nedata_newvandana_df1115$Certificate == "TV-PG", "green", "blue"),
         alpha = 0.5,cex = 0.8,
         blocks=FALSE
)
### Separate Data5 ###
vec5 <- c("U","UA")
vec5
nedata_newvandana_df1116 <- newvandana_df111[newvandana_df111$Certificate %in% vec5,]
### Aluvial Diagram of First Set of Data ###
ggplot(data = nedata_newvandana_df1116,
       aes(axis1 = Released_Year, axis2 = Runtime, axis3 =IMDB_Rating,
           axis4 = Meta_score,
           y = Freq)) +
  scale_x_discrete(limits = c("Released_Year", "Runtime", "IMDB_Rating","Meta_score"), expand = c(.2, .05)) +
  xlab("Movie_Detail1") +
  geom_alluvium(aes(fill = Certificate)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  ggtitle("Movie Analysis")
alluvial(nedata_newvandana_df1116[,1:5], freq = nedata_newvandana_df1116$Freq, 
         col = ifelse(nedata_newvandana_df1116$Certificate == "U", "green", "blue"),
         alpha = 0.5,cex = 0.8,
         blocks=FALSE
)
## Extract only Text data for Word Cloud ##
colnames(newvandana_df1)
newvandana_df1_text <- newvandana_df1[,c(1,5,7,9,10,11,12,13)]
View(newvandana_df1_text)
#Create a vector containing only the text
text1 <- newvandana_df1_text$Genre
text2 <- newvandana_df1_text$Director
text2 <- gsub( " ", "", text2)
text3 <- newvandana_df1_text$Star1
text3 <- gsub( " ", "", text3)
text4 <- newvandana_df1_text$Star2
text4 <- gsub( " ", "", text4)
text5 <- newvandana_df1_text$Star3
text5 <- gsub( " ", "", text5)
text6 <- newvandana_df1_text$Star4
text6 <- gsub( " ", "", text6)
text7 <- newvandana_df1_text$Overview
# Create a corpus  
docs1 <- Corpus(VectorSource(text1))
docs2 <- Corpus(VectorSource(text2))
docs3 <- Corpus(VectorSource(text3))
docs4 <- Corpus(VectorSource(text4))
docs5 <- Corpus(VectorSource(text5))
docs6 <- Corpus(VectorSource(text6))
docs7 <- Corpus(VectorSource(text7))
# Clean the text data for Genre##
dtm <- TermDocumentMatrix(docs1)
Vandana_m <- as.matrix(dtm)
Vandana_v <- sort(rowSums(Vandana_m),decreasing=TRUE)
Vandana_d <- data.frame(word = names(Vandana_v),freq=Vandana_v)
wordcloud(words = Vandana_d$word, freq = Vandana_d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
# Clean the text data for Director#
dtm1 <- TermDocumentMatrix(docs2)
Vandana_m1 <- as.matrix(dtm1)
Vandana_v1 <- sort(rowSums(Vandana_m1),decreasing=TRUE)
Vandana_d1 <- data.frame(word = names(Vandana_v1),freq=Vandana_v1)
wordcloud(words = Vandana_d1$word, freq = Vandana_d1$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
barplot(Vandana_d1[1:10,]$freq, las = 2, names.arg = Vandana_d1[1:10,]$word,
        col ="lightblue", main ="Most frequent Director",
        ylab = "Number of Movie Directed", xlab = "Director",cex.lab = 0.8, cex.names = 0.6)
# Clean the text data for Star1#
dtm2 <- TermDocumentMatrix(docs3)
Vandana_m2 <- as.matrix(dtm2)
Vandana_v2 <- sort(rowSums(Vandana_m2),decreasing=TRUE)
Vandana_d2 <- data.frame(word = names(Vandana_v2),freq=Vandana_v2)
wordcloud(words = Vandana_d2$word, freq = Vandana_d2$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
barplot(Vandana_d2[1:10,]$freq, las = 2, names.arg = Vandana_d2[1:10,]$word,
        col ="lightblue", main ="Most frequent Star1",
        ylab = "Number of Appearance in The Movie", xlab = "Actor/Actress",cex.lab = 0.8, cex.names = 0.7)
# Clean the text data for Star2#
dtm3 <- TermDocumentMatrix(docs4)
Vandana_m3 <- as.matrix(dtm3)
Vandana_v3 <- sort(rowSums(Vandana_m3),decreasing=TRUE)
Vandana_d3 <- data.frame(word = names(Vandana_v3),freq=Vandana_v3)
wordcloud(words = Vandana_d3$word, freq = Vandana_d3$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
barplot(Vandana_d3[1:10,]$freq, las = 2, names.arg = Vandana_d3[1:10,]$word,
        col ="lightblue", main ="Most frequent Star2",
        ylab = "Number of Appearance in The Movie", xlab = "Actor/Actress",cex.lab = 0.8, cex.names = 0.7)

# Clean the text data for Star3#
dtm4 <- TermDocumentMatrix(docs5)
Vandana_m4 <- as.matrix(dtm4)
Vandana_v4 <- sort(rowSums(Vandana_m4),decreasing=TRUE)
Vandana_d4 <- data.frame(word = names(Vandana_v4),freq=Vandana_v4)
wordcloud(words = Vandana_d4$word, freq = Vandana_d4$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
barplot(Vandana_d4[1:10,]$freq, las = 2, names.arg = Vandana_d4[1:10,]$word,
        col ="lightblue", main ="Most frequent Star3",
        ylab = "Number of Appearance in The Movie", xlab = "Actor/Actress",cex.lab = 0.8, cex.names = 0.7)

# Clean the text data for Star4#
dtm5 <- TermDocumentMatrix(docs6)
Vandana_m5 <- as.matrix(dtm5)
Vandana_v5 <- sort(rowSums(Vandana_m5),decreasing=TRUE)
Vandana_d5 <- data.frame(word = names(Vandana_v5),freq=Vandana_v5)
wordcloud(words = Vandana_d5$word, freq = Vandana_d5$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
barplot(Vandana_d5[1:10,]$freq, las = 2, names.arg = Vandana_d5[1:10,]$word,
        col ="lightblue", main ="Most frequent Star4",
        ylab = "Number of Appearance in The Movie", xlab = "Actor/Actress",cex.lab = 0.8, cex.names = 0.7)
##  