#Rulare####
library(writexl);library(tidytext);library(tidyverse);library(readxl);library(tm);library(textdata);library("SnowballC");library("wordcloud");library("RColorBrewer");library("syuzhet");library("ggplot2");library(lubridate);library(tm);library(scales);library(SentimentAnalysis);library(dplyr)
library(lubridate)
#Importare date####

facebook <- read_xlsx("facebook.xlsx"); facebook$Page <- "Facebook"; facebook$ER = facebook$Reactions/facebook$Fans
twitter <- read_xlsx("twitter.xlsx"); twitter$Page <- "Twitter"; twitter$ER = twitter$Reactions/twitter$Fans
instagram <- read_xlsx("instagram.xlsx"); instagram$Page <- "Instagram"; instagram$ER = instagram$Reactions/instagram$Fans
youtube <- read_xlsx("youtube.xlsx"); youtube$Page <- "Youtube"; youtube$ER = youtube$Reactions/youtube$Fans
date <- rbind(facebook,twitter, youtube,instagram); date$ER = date$Reactions/date$Fans

#Salvare set de date####
write_xlsx(x = date, path = "fbbucha.xlsx", col_names = TRUE)

#Nr. postari / canal####
date %>%
  group_by(Page) %>%
  dplyr::summarise(n = n()) %>%
  ggplot(aes(n, fct_reorder(Page, n), label = n)) +
  geom_col(fill="red", alpha=0.4) +
  geom_text(position = position_stack(vjust = .5)) + theme_minimal() +
  ylab("World political leaders") + xlab("Number of posts") 

#Tonul mesajelor - pozitiv vs. negativ####
date$Message <- gsub("@[[:alpha:]]*","", date$Message)
text_corpus <- Corpus(VectorSource(date$Message))
text_corpus <- tm_map(text_corpus, tolower)
text_corpus <- tm_map(text_corpus, removeWords, c("rt", "re", "amp"))
text_corpus <- tm_map(text_corpus, removeWords, stopwords("english"))
text_corpus <- tm_map(text_corpus, removePunctuation)
text_df <- data.frame(text_clean = get("content", text_corpus), stringsAsFactors = FALSE)
date <- cbind.data.frame(date, text_df)
date_sentiment <- analyzeSentiment(date$text_clean)
date_sentiment <- dplyr::select(date_sentiment, 
                                SentimentGI, SentimentHE,
                                SentimentLM, SentimentQDAP, 
                                WordCount)
#date_sentiment <- dplyr::mutate(date_sentiment, mean_sentiment = rowMeans(date_sentiment[,-5]))
# date_sentiment <- dplyr::select(date_sentiment, 
#                                 WordCount, 
#                                 mean_sentiment)
date <- cbind.data.frame(date, date_sentiment)
# date_negative <- filter(date, mean_sentiment < 0); date_negative$tone <- "negative"
# date_pozitive <- filter(date, mean_sentiment > 0); date_pozitive$tone <- "pozitive"
# date <- rbind(date_negative, date_pozitive)
ggplot(date, aes(Date, SentimentGI)) + geom_bin2d(colour="blue") + 
  scale_fill_gradient(low="light blue", high="dark blue") +
  geom_hline(yintercept=0, linetype="dashed", color = "red", size = 1.5) +
  ylab("Sentiment Indicator Average") + xlab("Months") +
  facet_wrap(~Page, ncol = 4, scales="free_x") + theme_minimal() + theme(legend.position = "none")

#plot - sentiment ER####
ggplot(data = date, aes(x = Page, y = ER, fill=Page)) + 
  geom_boxplot() + 
  facet_wrap(~Page, ncol = 4, scales = "free_x")+
  theme_minimal()



#new
corpus <- Corpus(DirSource(directory="C:/RProjects/ConvergentaSM/Mesaje"))
summary(corpus)

#cleaning
corpus <- tm_map(corpus, toSpace, "/")
corpus <- tm_map(corpus, toSpace, "@")
corpus <- tm_map(corpus, toSpace, "\\|")
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, c("I", "my", "https", "tco", "amp", "today", "bogdanaurescu", "romania"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

#comparatie
speech_term_matrix <- TermDocumentMatrix(corpus)
speech_term_matrix <- as.matrix(speech_term_matrix)
colnames(speech_term_matrix) <- c("Facebook", "Instagram", "Twitter", "Youtube")
comparison.cloud(speech_term_matrix,scale=c(4,.8), max.words=300,
                 random.order=FALSE, rot.per=.1,
                 colors=brewer.pal(max(3,ncol(speech_term_matrix)),"Dark2"),
                 use.r.layout=FALSE, title.size=2,
                 title.colors=NULL, match.colors=FALSE,
                 title.bg.colors="grey90")

commonality.cloud(speech_term_matrix, max.words = 500, colors=brewer.pal(max(10,ncol(speech_term_matrix)),"Dark2"))

facebook_mean <- cumsum(facebook$ER) / (1:length(facebook$ER))
instagram_mean <- cumsum(instagram$ER) / (1:length(instagram$ER))
twitter_mean <- cumsum(twitter$ER) / (1:length(twitter$ER))
youtube_mean <- cumsum(youtube$ER) / (1:length(youtube$ER))


plot(facebook_mean, type="l", col="blue", ylim=c(0, max(instagram_mean)), 
     xlab="Numărul de postări", ylab="Media rata de angajament (ER)")
lines(instagram_mean, type="l", col="red")
lines(twitter_mean, type="l", col="green")
lines(youtube_mean, type="l", col="orange")
legend("topright", c("Facebook", "Instagram", "Twitter", "Youtube"), 
       col=c("blue", "red", "green", "orange"), lty=1)

# compararea ratei medii de angajament între platforme
mean_ER <- data.frame(Platforma = c("Facebook", "Instagram", "Twitter", "YouTube"),
                      Media_ER = c(mean(facebook$ER), mean(instagram$ER), mean(twitter$ER), mean(youtube$ER)))
barplot(mean_ER$Media_ER, names.arg = mean_ER$Platforma, col = "blue",
        xlab = "Platforme sociale", ylab = "Media rata de angajament (ER)")

<<<<<<< HEAD
library(ggplot2)
ggplot(date, aes(x = Page, y = ER)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparatie rata de angajament in functie de Page",
       x = "Page",
       y = "Engagement Rate")

ggplot(data = date, aes(x = Page, y = ER, fill = tone)) + 
  geom_boxplot() + 
  facet_wrap(~Page, ncol = 4, scales = "free_x")+
 theme_minimal()
=======

>>>>>>> 64c9f5e7c3b08194aec30a6a44ad1c856621970d
