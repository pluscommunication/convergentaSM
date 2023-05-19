# Installing and loading libraries
if(!require(dplyr)) install.packages("dplyr")
if(!require(caret)) install.packages("caret")
if(!require(rstatix)) install.packages("rstatix")
if(!require(lm.beta)) install.packages("lm.beta")
if(!require(relaimpo)) install.packages("relaimpo")
if(!require(ggpubr)) install.packages("ggpubr")
if(!require(nortest)) install.packages("nortest")

library(dplyr); library(caret); library(rstatix); library(ggpubr); library(lm.beta)
library(relaimpo); library(nortest)

# Function for p-values management ####
p <- function(x, digits = 3){
  if (x < 10^-digits) return(paste('<', 10^-digits))
  paste('=', round(x, 3))}

# Loading datasets ####
load(file = "Raw Sentiment.RData"); load(file = "Pure Sentiment.RData"); head(ds)

# Preparing datasets
set.seed(453447)
selection <- ds.pur$ER %>% createDataPartition(p = 0.8, list = FALSE)
ds.estim  <- ds.pur[selection, ]; ds.asses <- ds.pur[-selection, ]; rm(selection)
ds.estim$Set <- "Estimation"; ds.asses$Set <- "Assessment"
ds.pur <- rbind(ds.estim, ds.asses); ds.pur$Set <- factor(ds.pur$Set); names(ds.pur)

# Descriptive analysis on estimation and assessment sets ####
ds.estim %>% get_summary_stats(type = "full", show = c("n", "min", "max", "median", "mean", "sd"))
ds.asses %>% get_summary_stats(type = "full", show = c("n", "min", "max", "median", "mean", "sd"))
t_test(ER ~ Set, data = ds.pur); t_test( ~ Set, data = ds.pur); t_test(PositivityGI ~ Set, data = ds.pur)

# ds.estim <- ds.pur
# Assumptions assessment ####
## I. Normality of dependent variable
ad.test(ds.estim$ER)
out <- ds.estim %>% identify_outliers(ER); out <- out[, c('ER', 'is.extreme')]
range(out[out$is.extreme == "TRUE",]$ER)

ggqqplot(ds.estim, x="ER", bxp.errorbar = T,
         xlab = "Theoretical normal", ylab = "Empirical ER distribution")
plot(density(ds.estim$ER), ylab="Frequency"); polygon(density(ds.estim$ER), col = "dark green")

## Liniarity check
# 2. Analiza liniaritatii relatiei
ggplot(ds.estim, aes(y = ER, x = NegativityGI)) +
  geom_point() + stat_smooth()
ggplot(ds.estim, aes(y = ER, x = PositivityGI)) +
  geom_point() + stat_smooth()

# Regression analysis ####
mod <- lm(ER ~ NegativityGI + PositivityGI, 
          data = ds.estim); mod.std <- lm.beta(mod); summary(mod.std)

ds.estim$Page <- factor(ds.estim$Page, levels = c("Twitter", "Facebook", "Instagram", "Youtube"))
mod <- lm(ER ~ NegativityGI + PositivityGI + factor(Page), 
          data = ds.estim); mod.std <- lm.beta(mod); summary(mod.std)

# Relative predictors importance ####
calc.relimp(mod, type = c("lmg"), rela = T)

# Testing predictions ####
predictions <- mod %>% predict(ds.asses)
RMSE(predictions, ds.asses$ER)
R2(predictions, ds.asses$ER)
RMSE(predictions, ds.asses$ER) / mean(ds.asses$ER)

# Model diagnosis ####
diag.mod <- augment(mod); diag.mod
ggplot(diag.mod, aes(x = NegativityGI, y = ER)) +
  geom_point() + stat_smooth(method = "lm", se = T) +
  geom_segment(aes(xend = NegativityGI, yend = .fitted),
               color = "red", sixe = 0.3)
ggplot(diag.mod, aes(x = PositivityGI, y = ER)) +
  geom_point() + stat_smooth(method = "lm", se = T) +
  geom_segment(aes(xend = PositivityGI, yend = .fitted),
               color = "red", sixe = 0.3)
## Extreme residuals analysis
par(mfrow = c(2, 2)); plot(mod)
diag.mod %>% identify_outliers(.std.resid)
# HLV Values analysis
diag.mod %>% filter(abs(.hat) > 0.037)
## Cook's distances analysis
plot(mod, 4); diag.mod %>% top_n(4, wt = .cooksd)
# Multicolinearity analysis
car::vif(mod)
