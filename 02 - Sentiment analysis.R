# Installing and loading libraries
if (!require(tm)) install.packages("tm")
if (!require(SentimentAnalysis)) install.packages("SentimentAnalysis")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")

library(tm); library(SentimentAnalysis); library(ggplot2); library(dplyr)

# Loading datasets
load(file = "Raw data.RData"); load(file = "Pure data.RData"); head(ds)

# Sentiment analysis - Unpurified dataset ####
ds$Message <- gsub("@[[:alpha:]]*","", ds$Message)
tc <- Corpus(VectorSource(ds$Message))
tc <- tm_map(tc, tolower); tc <- tm_map(tc, removeWords, c("rt", "re", "amp"))
tc <- tm_map(tc, removeWords, stopwords("english")); tc <- tm_map(tc, removePunctuation)
## Updating Message field in main dataset
ds$Message  <- data.frame(Message = get("content", tc), stringsAsFactors = FALSE)$Message
## *******  Doing sentiment analysis and updating main dataset ********
#ds <- cbind(ds, analyzeSentiment(ds$Message)); save(ds, file = "Raw Sentiment.RData")
## ********************************************************************
## Plotting sentiment data
load(file = "Raw Sentiment.RData"); names(ds); rm(tc)
tmp <- ds %>%  dplyr::filter(Page == "Facebook")
fb.sent.min.ext <- round(min(tmp$SentimentGI, na.rm = T), 5)
fb.sent.max.ext <- round(max(tmp$SentimentGI, na.rm = T), 5)
fb.sent.mean.ext <- round(mean(tmp$SentimentGI, na.rm = T), 5)
fb.sent.sd.ext <- round(sd(tmp$SentimentGI, na.rm = T), 5)
fb.neg.min.ext <- round(min(tmp$NegativityGI, na.rm = T), 5)
fb.neg.max.ext <- round(max(tmp$NegativityGI, na.rm = T), 5)
fb.neg.mean.ext <- round(mean(tmp$NegativityGI, na.rm = T), 5)
fb.neg.sd.ext <- round(sd(tmp$NegativityGI, na.rm = T), 5)
fb.poz.min.ext <- round(min(tmp$PositivityGI, na.rm = T), 5)
fb.poz.max.ext <- round(max(tmp$PositivityGI, na.rm = T), 5)
fb.poz.mean.ext <- round(mean(tmp$PositivityGI, na.rm = T), 5)
fb.poz.sd.ext <- round(sd(tmp$PositivityGI, na.rm = T), 5)

tmp <- ds %>%  dplyr::filter(Page == "Instagram")
ig.sent.min.ext <- round(min(tmp$SentimentGI, na.rm = T), 5)
ig.sent.max.ext <- round(max(tmp$SentimentGI, na.rm = T), 5)
ig.sent.mean.ext <- round(mean(tmp$SentimentGI, na.rm = T), 5)
ig.sent.sd.ext <- round(sd(tmp$SentimentGI, na.rm = T), 5)
ig.neg.min.ext <- round(min(tmp$NegativityGI, na.rm = T), 5)
ig.neg.max.ext <- round(max(tmp$NegativityGI, na.rm = T), 5)
ig.neg.mean.ext <- round(mean(tmp$NegativityGI, na.rm = T), 5)
ig.neg.sd.ext <- round(sd(tmp$NegativityGI, na.rm = T), 5)
ig.poz.min.ext <- round(min(tmp$PositivityGI, na.rm = T), 5)
ig.poz.max.ext <- round(max(tmp$PositivityGI, na.rm = T), 5)
ig.poz.mean.ext <- round(mean(tmp$PositivityGI, na.rm = T), 5)
ig.poz.sd.ext <- round(sd(tmp$PositivityGI, na.rm = T), 5)

tmp <- ds %>%  dplyr::filter(Page == "Twitter")
tw.sent.min.ext <- round(min(tmp$SentimentGI, na.rm = T), 5)
tw.sent.max.ext <- round(max(tmp$SentimentGI, na.rm = T), 5)
tw.sent.mean.ext <- round(mean(tmp$SentimentGI, na.rm = T), 5)
tw.sent.sd.ext <- round(sd(tmp$SentimentGI, na.rm = T), 5)
tw.neg.min.ext <- round(min(tmp$NegativityGI, na.rm = T), 5)
tw.neg.max.ext <- round(max(tmp$NegativityGI, na.rm = T), 5)
tw.neg.mean.ext <- round(mean(tmp$NegativityGI, na.rm = T), 5)
tw.neg.sd.ext <- round(sd(tmp$NegativityGI, na.rm = T), 5)
tw.poz.min.ext <- round(min(tmp$PositivityGI, na.rm = T), 5)
tw.poz.max.ext <- round(max(tmp$PositivityGI, na.rm = T), 5)
tw.poz.mean.ext <- round(mean(tmp$PositivityGI, na.rm = T), 5)
tw.poz.sd.ext <- round(sd(tmp$PositivityGI, na.rm = T), 5)

tmp <- ds %>%  dplyr::filter(Page == "Youtube")
yt.sent.min.ext <- round(min(tmp$SentimentGI, na.rm = T), 5)
yt.sent.max.ext <- round(max(tmp$SentimentGI, na.rm = T), 5)
yt.sent.mean.ext <- round(mean(tmp$SentimentGI, na.rm = T), 5)
yt.sent.sd.ext <- round(sd(tmp$SentimentGI, na.rm = T), 5)
yt.neg.min.ext <- round(min(tmp$NegativityGI, na.rm = T), 5)
yt.neg.max.ext <- round(max(tmp$NegativityGI, na.rm = T), 5)
yt.neg.mean.ext <- round(mean(tmp$NegativityGI, na.rm = T), 5)
yt.neg.sd.ext <- round(sd(tmp$NegativityGI, na.rm = T), 5)
yt.poz.min.ext <- round(min(tmp$PositivityGI, na.rm = T), 5)
yt.poz.max.ext <- round(max(tmp$PositivityGI, na.rm = T), 5)
yt.poz.mean.ext <- round(mean(tmp$PositivityGI, na.rm = T), 5)
yt.poz.sd.ext <- round(sd(tmp$PositivityGI, na.rm = T), 5)

bp.ext <- ggplot(data = ds, aes(x = Page, y = ER, fill=Page)) + 
  geom_boxplot() + 
  ylab("Engagement Rates. Extremes included") +
  xlab("Social media page") +
  facet_wrap(~Page, ncol = 4, scales = "free_x")+
  theme_minimal(); bp.ext
st.ext <- ggplot(ds, aes(Date, SentimentGI)) + geom_bin2d(colour="blue") + 
  scale_fill_gradient(low="light blue", high="dark blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1.5) +
  ylab("Sentiment Values (Harvard-IV dictionary). Extremes included") + xlab("Months") +
  facet_wrap(~Page, ncol = 4, scales="free_x") + theme_minimal() + theme(legend.position = "none"); st.ext

# Sentiment analysis - Purified dataset ####
ds.pur$Message <- gsub("@[[:alpha:]]*","", ds.pur$Message)
tc <- Corpus(VectorSource(ds.pur$Message))
tc <- tm_map(tc, tolower); tc <- tm_map(tc, removeWords, c("rt", "re", "amp"))
tc <- tm_map(tc, removeWords, stopwords("english")); tc <- tm_map(tc, removePunctuation)
## Updating Message field in main dataset
ds.pur$Message  <- data.frame(Message = get("content", tc), stringsAsFactors = FALSE)$Message
## ******* Doing sentiment analysis and updating main dataset ********
#ds.pur <- cbind(ds.pur, analyzeSentiment(ds.pur$Message)); save(ds.pur, file = "Pure Sentiment.RData")
## *******************************************************************
## Plotting sentiment data
load(file = "Pure Sentiment.RData"); names(ds.pur); rm(tc)
tmp <- ds.pur %>%  dplyr::filter(Page == "Facebook")
fb.sent.min.next <- round(min(tmp$SentimentGI, na.rm = T), 5)
fb.sent.max.next <- round(max(tmp$SentimentGI, na.rm = T), 5)
fb.sent.mean.next <- round(mean(tmp$SentimentGI, na.rm = T), 5)
fb.sent.sd.next <- round(sd(tmp$SentimentGI, na.rm = T), 5)
fb.neg.min.next <- round(min(tmp$NegativityGI, na.rm = T), 5)
fb.neg.max.next <- round(max(tmp$NegativityGI, na.rm = T), 5)
fb.neg.mean.next <- round(mean(tmp$NegativityGI, na.rm = T), 5)
fb.neg.sd.next <- round(sd(tmp$NegativityGI, na.rm = T), 5)
fb.poz.min.next <- round(min(tmp$PositivityGI, na.rm = T), 5)
fb.poz.max.next <- round(max(tmp$PositivityGI, na.rm = T), 5)
fb.poz.mean.next <- round(mean(tmp$PositivityGI, na.rm = T), 5)
fb.poz.sd.next <- round(sd(tmp$PositivityGI, na.rm = T), 5)

tmp <- ds.pur %>%  dplyr::filter(Page == "Instagram")
ig.sent.min.next <- round(min(tmp$SentimentGI, na.rm = T), 5)
ig.sent.max.next <- round(max(tmp$SentimentGI, na.rm = T), 5)
ig.sent.mean.next <- round(mean(tmp$SentimentGI, na.rm = T), 5)
ig.sent.sd.next <- round(sd(tmp$SentimentGI, na.rm = T), 5)
ig.neg.min.next <- round(min(tmp$NegativityGI, na.rm = T), 5)
ig.neg.max.next <- round(max(tmp$NegativityGI, na.rm = T), 5)
ig.neg.mean.next <- round(mean(tmp$NegativityGI, na.rm = T), 5)
ig.neg.sd.next <- round(sd(tmp$NegativityGI, na.rm = T), 5)
ig.poz.min.next <- round(min(tmp$PositivityGI, na.rm = T), 5)
ig.poz.max.next <- round(max(tmp$PositivityGI, na.rm = T), 5)
ig.poz.mean.next <- round(mean(tmp$PositivityGI, na.rm = T), 5)
ig.poz.sd.next <- round(sd(tmp$PositivityGI, na.rm = T), 5)

tmp <- ds.pur %>%  dplyr::filter(Page == "Twitter")
tw.sent.min.next <- round(min(tmp$SentimentGI, na.rm = T), 5)
tw.sent.max.next <- round(max(tmp$SentimentGI, na.rm = T), 5)
tw.sent.mean.next <- round(mean(tmp$SentimentGI, na.rm = T), 5)
tw.sent.sd.next <- round(sd(tmp$SentimentGI, na.rm = T), 5)
tw.neg.min.next <- round(min(tmp$NegativityGI, na.rm = T), 5)
tw.neg.max.next <- round(max(tmp$NegativityGI, na.rm = T), 5)
tw.neg.mean.next <- round(mean(tmp$NegativityGI, na.rm = T), 5)
tw.neg.sd.next <- round(sd(tmp$NegativityGI, na.rm = T), 5)
tw.poz.min.next <- round(min(tmp$PositivityGI, na.rm = T), 5)
tw.poz.max.next <- round(max(tmp$PositivityGI, na.rm = T), 5)
tw.poz.mean.next <- round(mean(tmp$PositivityGI, na.rm = T), 5)
tw.poz.sd.next <- round(sd(tmp$PositivityGI, na.rm = T), 5)

tmp <- ds.pur %>%  dplyr::filter(Page == "Youtube")
yt.sent.min.next <- round(min(tmp$SentimentGI, na.rm = T), 5)
yt.sent.max.next <- round(max(tmp$SentimentGI, na.rm = T), 5)
yt.sent.mean.next <- round(mean(tmp$SentimentGI, na.rm = T), 5)
yt.sent.sd.next <- round(sd(tmp$SentimentGI, na.rm = T), 5)
yt.neg.min.next <- round(min(tmp$NegativityGI, na.rm = T), 5)
yt.neg.max.next <- round(max(tmp$NegativityGI, na.rm = T), 5)
yt.neg.mean.next <- round(mean(tmp$NegativityGI, na.rm = T), 5)
yt.neg.sd.next <- round(sd(tmp$NegativityGI, na.rm = T), 5)
yt.poz.min.next <- round(min(tmp$PositivityGI, na.rm = T), 5)
yt.poz.max.next <- round(max(tmp$PositivityGI, na.rm = T), 5)
yt.poz.mean.next <- round(mean(tmp$PositivityGI, na.rm = T), 5)
yt.poz.sd.next <- round(sd(tmp$PositivityGI, na.rm = T), 5)

bp.next <- ggplot(data = ds.pur, aes(x = Page, y = ER, fill=Page)) + 
  geom_boxplot() + 
  ylab("Engagement Rates. Extremes excluded") +
  xlab("Social media page") +
  facet_wrap(~Page, ncol = 4, scales = "free_x")+
  theme_minimal(); bp.next
st.next <- ggplot(ds.pur, aes(Date, SentimentGI)) + geom_bin2d(colour="blue") + 
  scale_fill_gradient(low="light blue", high="dark blue") +
  geom_hline(yintercept=0, linetype="dashed", color = "red", size = 1.5) +
  ylab("Sentiment Indicator Values (Harvard-IV dictionary). Extremes excluded") + xlab("Months") +
  facet_wrap(~Page, ncol = 4, scales="free_x") + theme_minimal() + theme(legend.position = "none"); st.next


