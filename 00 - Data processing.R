# Installing and loading libraries
if (!require(readxl)) install.packages("readxl")
if (!require(dplyr)) install.packages("dplyr")

library(readxl); library(dplyr)

# Data import
facebook <- read_xlsx("facebook.xlsx"); facebook$Page <- "Facebook"; facebook <- select(facebook, Page, Message, Type, Date, Reactions, Likes, Comments, Shares, Fans)
twitter <- read_xlsx("twitter.xlsx"); twitter$Page <- "Twitter"; twitter <- select(twitter, Page, Message, Type, Date, Reactions, Likes, Comments, Shares, Fans)
instagram <- read_xlsx("instagram.xlsx"); instagram$Page <- "Instagram"; instagram <- select(instagram, Page, Message, Type, Date, Reactions, Likes, Comments, Shares, Fans)
youtube <- read_xlsx("youtube.xlsx"); youtube$Page <- "Youtube"; youtube <- select(youtube, Page, Message, Type, Date, Reactions, Likes, Comments, Shares, Fans)
ds <- rbind(facebook, youtube, instagram, twitter); rm(facebook, instagram, twitter, youtube)

# Dataset saving
save(ds, file = "Raw data.RData")

