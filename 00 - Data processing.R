# Installing and loading libraries
if (!require(readxl)) install.packages("readxl")
if (!require(dplyr)) install.packages("dplyr")
# if (!require(writexl)) install.packages("writexl")
# if (!require(tidytext)) install.packages("tidytext")
# if (!require(tidyverse)) install.packages("tidyverse")
# if (!require(tm)) install.packages("tm")
# if (!require(textdata)) install.packages("textdata")
# if (!require(textdata)) install.packages("textdata")
# if (!require(SnowballC)) install.packages("SnowballC")
# if (!require(wordcloud)) install.packages("wordcloud")
# if (!require(RColorBrewer)) install.packages("RColorBrewer")
# if (!require(syuzhet)) install.packages("syuzhet")
# if (!require(ggplot2)) install.packages("ggplot2")
# if (!require(lubridate)) install.packages("lubridate")
# if (!require(scales)) install.packages("scales")
# if (!require(SentimentAnalysis)) install.packages("SentimentAnalysis")

library(readxl); library(dplyr)
# library(writexl); library(tidytext); library(tidyverse); library(tm); library(textdata)
# library(SnowballC); library(wordcloud); library(RColorBrewer); library(syuzhet); library(ggplot2 )
# library(lubridate); library(scales); library(SentimentAnalysis)

# Data import
facebook <- read_xlsx("facebook.xlsx"); facebook$Page <- "Facebook"; facebook <- select(facebook, Page, Message, Type, Date, Reactions, Likes, Comments, Shares, Fans)
twitter <- read_xlsx("twitter.xlsx"); twitter$Page <- "Twitter"; twitter <- select(twitter, Page, Message, Type, Date, Reactions, Likes, Comments, Shares, Fans)
instagram <- read_xlsx("instagram.xlsx"); instagram$Page <- "Instagram"; instagram <- select(instagram, Page, Message, Type, Date, Reactions, Likes, Comments, Shares, Fans)
youtube <- read_xlsx("youtube.xlsx"); youtube$Page <- "Youtube"; youtube <- select(youtube, Page, Message, Type, Date, Reactions, Likes, Comments, Shares, Fans)
ds <- rbind(facebook, youtube, instagram, twitter); rm(facebook, instagram, twitter, youtube)

# Dataset saving
save(ds, file = "Raw data.RData")

