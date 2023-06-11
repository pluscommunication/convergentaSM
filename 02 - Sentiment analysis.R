# Installing and loading libraries
if (!require(tm)) install.packages("tm")
if (!require(SentimentAnalysis)) install.packages("SentimentAnalysis")
if (!require(ggplot2)) install.packages("ggplot2")

library(tm); library(SentimentAnalysis); library(ggplot2)

# Loading datasets
load(file = "Raw data.RData"); load(file = "Pure data.RData"); head(ds)

# Sentiment analysis - Unpurified dataset ####
ds$Message <- gsub("@[[:alpha:]]*","", ds$Message)
tc <- Corpus(VectorSource(ds$Message))
tc <- tm_map(tc, tolower); tc <- tm_map(tc, removeWords, c("rt", "re", "amp"))
tc <- tm_map(tc, removeWords, stopwords("english")); tc <- tm_map(tc, removePunctuation)
## Updating Message field in main dataset
ds$Message  <- data.frame(Message = get("content", tc), stringsAsFactors = FALSE)$Message
## Doing sentiment analysis and updating main dataset
#ds <- cbind(ds, analyzeSentiment(ds$Message)); save(ds, file = "Raw Sentiment.RData")
## Plotting sentiment data
load(file = "Raw Sentiment.RData"); names(ds); rm(tc)
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
## Doing sentiment analysis and updating main dataset
#ds.pur <- cbind(ds.pur, analyzeSentiment(ds.pur$Message)); save(ds.pur, file = "Pure Sentiment.RData")
## Plotting sentiment data
load(file = "Pure Sentiment.RData"); names(ds.pur); rm(tc)
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
