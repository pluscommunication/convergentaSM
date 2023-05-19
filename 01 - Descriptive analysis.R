# Installing and loadinng required libraries
if(!require(psych)) install.packages("psych")
if(!require(sasLM)) install.packages("sasLM")
if(!require(dplyr)) install.packages("dplyr")
if(!require(rstatix)) install.packages("rstatix")
if(!require(naniar)) install.packages("naniar")
if(!require(lubridate)) install.packages("lubridate")
if(!require(flextable)) install.packages("flextable")

library(dplyr); library(psych); library(sasLM); library(rstatix); library(naniar)
library(lubridate); library(flextable)

# Loading dataset and computing ER
load(file = "Raw data.RData"); head(ds)
ds$ER = ds$Reactions / ds$Fans; ds <- ds %>% filter(ER != Inf); save(ds, file = "Raw data.RData")

# Outliers detection and treatment ####
tmp <- ds %>% filter(Page == "Facebook")
out <- tmp %>% rstatix::identify_outliers(ER); out <- out[, c('ER', 'is.extreme')]
colnames(out) <- c("Values", "Extreme"); out[out$Extreme == "TRUE",]
boxplot(tmp$ER, horizontal = T, col = "dark green", outline = T)
mtext(paste("Outliers: ", paste(out$Values, collapse=", ")), cex=0.6)
tmp$ER[which(tmp$ER >= .00484)] <- NA
ds.pur <- tmp

tmp <- ds %>% filter(Page == "Instagram")
out <- tmp %>% rstatix::identify_outliers(ER); out <- out[, c('ER', 'is.extreme')]
colnames(out) <- c("Values", "Extreme"); out[out$Extreme == "TRUE",]
boxplot(tmp$ER, horizontal = T, col = "dark green", outline = T)
mtext(paste("Outliers: ", paste(out$Values, collapse=", ")), cex=0.6)
tmp$ER[which(tmp$ER >= .0215)] <- NA
ds.pur <- rbind(ds.pur, tmp)

tmp <- ds %>% filter(Page == "Twitter")
out <- tmp %>% rstatix::identify_outliers(ER); out <- out[, c('ER', 'is.extreme')]
colnames(out) <- c("Values", "Extreme"); out[out$Extreme == "TRUE",]
boxplot(tmp$ER, horizontal = T, col = "dark green", outline = T)
mtext(paste("Outliers: ", paste(out$Values, collapse=", ")), cex=0.6)
tmp$ER[which(tmp$ER >= .000696)] <- NA
ds.pur <- rbind(ds.pur, tmp)

tmp <- ds %>% filter(Page == "Youtube")
out <- tmp %>% rstatix::identify_outliers(ER); out <- out[, c('ER', 'is.extreme')]
colnames(out) <- c("Values", "Extreme"); out[out$Extreme == "TRUE",]
boxplot(tmp$ER, horizontal = T, col = "dark green", outline = T)
mtext(paste("Outliers: ", paste(out$Values, collapse=", ")), cex=0.6)
tmp$ER[which(tmp$ER >= .000962)] <- NA
ds.pur <- rbind(ds.pur, tmp)

# Missing cases analysis ####
rm(out, tmp); miss <- miss_var_summary(ds.pur); miss
colnames(miss) <- c("Variable", "N", "%")
## Select only complete cases
#complete <- ds.pur[complete.cases(ds.pur),]
complete <- ds.pur %>% dplyr::filter(!is.na(ER))
loss <- nrow(ds.pur) - nrow(complete); ds.pur <- complete
prc.miss <- as.numeric(round(miss[4,3], 2)); rm(complete, miss)

# Saving dataset
save(ds.pur, file = "Pure data.RData")

# Filtering dataset #### 
# ds <- filter(ds, Date >= as.Date("2023-03-24"), Date <= as.Date("2023-04-20"))
# ds.pur <- filter(ds.pur, Date >= as.Date("2023-03-24"), Date <= as.Date("2023-04-20"))

# Univariate statistics ####
## Unpurified data ####
univar <- psych::describeBy(ds$ER, group = ds$Page)
univar <- rbind(univar$Facebook, univar$Instagram, univar$Twitter, univar$Youtube)
univar <- as.data.frame(univar); univar$vars <- c("Facebook", "Instagram", "Twitter", "Youtube")
univar <- univar %>% dplyr::select(-trimmed, -mad, -range, -se)
tmp <- ds %>%  dplyr::filter(Page == "Facebook")
univar$skew.se <- round(SkewnessSE(tmp$ER), 3); univar$kurtosis.se <- round(KurtosisSE(tmp$ER), 3)
tmp <- ds %>%  dplyr::filter(Page == "Instagram")
univar$skew.se[2] <- round(SkewnessSE(tmp$ER), 3); univar$kurtosis.se[2] <- round(KurtosisSE(tmp$ER), 3)
tmp <- ds %>%  dplyr::filter(Page == "Twitter")
univar$skew.se[3] <- round(SkewnessSE(tmp$ER), 3); univar$kurtosis.se[3] <- round(KurtosisSE(tmp$ER), 3)
tmp <- ds %>%  dplyr::filter(Page == "Youtube")
univar$skew.se[4] <- round(SkewnessSE(tmp$ER), 3); univar$kurtosis.se[4] <- round(KurtosisSE(tmp$ER), 3)
colnames(univar) <- c("Page", "Posts", "Mean", "SD", "Median", "Min", "Max", "Skew", "Kurt", "SE Skew", "SE Kurt")
rownames(univar) <- NULL; univar[, c(-1)] <- round(univar[, c(-1)], 4); univar.row <- univar
## Prepare table for print
print.univar <- univar; rec <- 1
while (rec <= nrow(print.univar)) {
  print.univar$Skew[rec] <- paste(print.univar$Skew[rec], " (", print.univar$`SE Skew`[rec], ")", sep = "")
  print.univar$Kurt[rec] <- paste(print.univar$Kurt[rec], " (", print.univar$`SE Kurt`[rec], ")", sep = "")
  rec <- rec + 1}
print.univar$Shapiro <- NULL; print.univar$p <- NULL;print.univar$R <- NULL
print.univar$`SE Skew` <- NULL; print.univar$`SE Kurt` <- NULL
colnames(print.univar) <- c("Variables", "N", "Mean", "SD", "Median", "Min", "Max","Skew (SE)", "Kurt (SE)")
flextable(print.univar) %>% theme_apa() %>% save_as_docx(path = "Documents/Tab_Univariates_Row.docx")

## Purified data ####
univar <- psych::describeBy(ds.pur$ER, group = ds.pur$Page)
univar <- rbind(univar$Facebook, univar$Instagram, univar$Twitter, univar$Youtube)
univar <- as.data.frame(univar); univar$vars <- c("Facebook", "Instagram", "Twitter", "Youtube")
univar <- univar %>% dplyr::select(-trimmed, -mad, -range, -se)
tmp <- ds.pur %>%  dplyr::filter(Page == "Facebook")
univar$skew.se <- round(SkewnessSE(tmp$ER), 3); univar$kurtosis.se <- round(KurtosisSE(tmp$ER), 3)
tmp <- ds.pur %>%  dplyr::filter(Page == "Instagram")
univar$skew.se[2] <- round(SkewnessSE(tmp$ER), 3); univar$kurtosis.se[2] <- round(KurtosisSE(tmp$ER), 3)
tmp <- ds.pur %>%  dplyr::filter(Page == "Twitter")
univar$skew.se[3] <- round(SkewnessSE(tmp$ER), 3); univar$kurtosis.se[3] <- round(KurtosisSE(tmp$ER), 3)
tmp <- ds.pur %>%  dplyr::filter(Page == "Youtube")
univar$skew.se[4] <- round(SkewnessSE(tmp$ER), 3); univar$kurtosis.se[4] <- round(KurtosisSE(tmp$ER), 3)
colnames(univar) <- c("Page", "Posts", "Mean", "SD", "Median", "Min", "Max", "Skew", "Kurt", "SE Skew", "SE Kurt")
rownames(univar) <- NULL; univar[, c(-1)] <- round(univar[, c(-1)], 4); univar.pur <- univar
## Prepare table for print
print.univar <- univar; rec <- 1
while (rec <= nrow(print.univar)) {
  print.univar$Skew[rec] <- paste(print.univar$Skew[rec], " (", print.univar$`SE Skew`[rec], ")", sep = "")
  print.univar$Kurt[rec] <- paste(print.univar$Kurt[rec], " (", print.univar$`SE Kurt`[rec], ")", sep = "")
  rec <- rec + 1}
print.univar$Shapiro <- NULL; print.univar$p <- NULL;print.univar$R <- NULL
print.univar$`SE Skew` <- NULL; print.univar$`SE Kurt` <- NULL
colnames(print.univar) <- c("Variables", "N", "Mean", "SD", "Median", "Min", "Max","Skew (SE)", "Kurt (SE)")
flextable(print.univar) %>% theme_apa() %>% save_as_docx(path = "Documents/Tab_Univariates_Pure.docx")
