# Installing and loading libraries
if(!require(dplyr)) install.packages("dplyr")
if(!require(caret)) install.packages("caret")
if(!require(rstatix)) install.packages("rstatix")
if(!require(lm.beta)) install.packages("lm.beta")
if(!require(relaimpo)) install.packages("relaimpo")
if(!require(ggpubr)) install.packages("ggpubr")
if(!require(nortest)) install.packages("nortest")
if(!require(interactions)) install.packages("interactions")
if(!require(misty)) install.packages("misty")

library(dplyr); library(caret); library(rstatix); library(ggpubr); library(lm.beta)
library(relaimpo); library(nortest); library(interactions)
p <- function(x, digits = 3){
  if (x < 10^-digits) return(paste('<', 10^-digits))
  paste('=', round(x, 3))}

# Loading datasets ####
load(file = "Pure Sentiment.RData"); head(ds.pur)

# Preparing datasets ####
set.seed(453447)
selection <- ds.pur$ER %>% createDataPartition(p = 0.8, list = FALSE)
ds.estim  <- ds.pur[selection, ]; ds.asses <- ds.pur[-selection, ]; rm(selection)
ds.estim$Set <- "Estimation"; ds.asses$Set <- "Assessment"
ds.pur <- rbind(ds.estim, ds.asses); ds.pur$Set <- factor(ds.pur$Set); names(ds.pur)

# ds.estim <- ds.pur
# Assumptions assessment ####
## I. Normality of dependent variable
ds.estim <- ds.estim %>% dplyr::filter(ER != 0)
out <- ds.estim %>% identify_outliers(ER); out <- out[, c('ER', 'is.extreme')]; out
range(out[out$is.extreme == "TRUE",]$ER)
ds.estim <- ds.estim %>% dplyr::filter(ER <= 0.000929525)
nor.test <- ad.test(ds.estim$ER); nor.test
ad.stat <- round(nor.test$statistic); ad.p <- p(nor.test$p.value, digits = 3)
ggqqplot(ds.estim, x="ER", bxp.errorbar = T,
         xlab = "Theoretical normal", ylab = "Empirical ER distribution")
plot(density(ds.estim$ER), ylab="Frequency"); polygon(density(ds.estim$ER), col = "dark green")

# Descriptive analysis on estimation and assessment sets ####
ds.estim %>% get_summary_stats(type = "full", show = c("n", "min", "max", "median", "mean", "sd"))
ds.asses %>% get_summary_stats(type = "full", show = c("n", "min", "max", "median", "mean", "sd"))
ech.ER <- t_test(ER ~ Set, data = ds.pur); ech.ER
ech.PozGI <- t_test(PositivityGI ~ Set, data = ds.pur); ech.PozGI
ech.NegGI <- t_test(NegativityGI ~ Set, data = ds.pur); ech.NegGI
est.n <- nrow(ds.estim); ass.n <- nrow(ds.asses)

## Liniarity check
# 2. Analiza liniaritatii relatiei
ggplot(ds.estim, aes(y = ER, x = NegativityGI)) +
  geom_point() + stat_smooth()
ggplot(ds.estim, aes(y = ER, x = PositivityGI)) +
  geom_point() + stat_smooth()

# Regression analysis ####
## First hypothesis #####
mod.1 <- lm(ER ~ NegativityGI + PositivityGI, 
          data = ds.estim); mod.std <- lm.beta(mod.1)
mod.std <- summary(mod.std)
m1.F <- round(mod.std[["fstatistic"]][["value"]], 2)
m1.F.df1 <- round(mod.std[["fstatistic"]][["numdf"]], 2)
m1.F.df2 <- round(mod.std[["fstatistic"]][["dendf"]], 2)
m1.F.p <- p(pf(m1.F, m1.F.df1, m1.F.df2, lower.tail = F), digits = 3)
m1.RSE <- formatC(mod.std[["sigma"]], format = "f", digits = 4)
m1.r2 <- formatC(mod.std[["adj.r.squared"]], format = "f", digits = 4)
m1.r2.p <- round(mod.std[["adj.r.squared"]] * 100, 3)

m1.neg.B <- formatC(mod.std$coefficients[2, 1], format = "f", digits = 5)
m1.neg.t <- round(mod.std$coefficients[2, 4], 2)
m1.neg.p <- p(mod.std$coefficients[2, 5], digits = 3)
m1.neg.Bt <- round(mod.std$coefficients[2, 2], 3)

m1.poz.B <- formatC(mod.std$coefficients[3, 1], format = "f", digits = 5)
m1.poz.t <- round(mod.std$coefficients[3, 4], 2)
m1.poz.p <- p(mod.std$coefficients[3, 5], digits = 3)
m1.poz.Bt <- round(mod.std$coefficients[3, 2], 3)
## Relative predictors importance
m1.relimp <- calc.relimp(mod.1, type = c("lmg"), rela = T); m1.relimp
m1.poz.lmg <- round(as.numeric(m1.relimp@lmg[2]) * 100, 2)
m1.neg.lmg <- round(as.numeric(m1.relimp@lmg[1]) * 100, 2)

## Second hypothesis #####
ds.estim$Page <- factor(ds.estim$Page, levels = c("Twitter", "Facebook", "Instagram", "Youtube"))
mod.2 <- lm(ER ~ NegativityGI + PositivityGI + factor(Page), 
          data = ds.estim); mod.std <- lm.beta(mod.2)
mod.std <- summary(mod.std); mod.std
m2.F <- round(mod.std[["fstatistic"]][["value"]], 2)
m2.F.df1 <- round(mod.std[["fstatistic"]][["numdf"]], 2)
m2.F.df2 <- round(mod.std[["fstatistic"]][["dendf"]], 2)
m2.F.p <- p(pf(m2.F, m2.F.df1, m2.F.df2, lower.tail = F), digits = 3)
m2.RSE <- formatC(mod.std[["sigma"]], format = "f", digits = 4)
m2.r2 <- formatC(mod.std[["adj.r.squared"]], format = "f", digits = 3)
m2.r2.p <- round(mod.std[["adj.r.squared"]] * 100, 3)

m2.neg.B <- formatC(mod.std$coefficients[2, 1], format = "f", digits = 5)
m2.neg.t <- round(mod.std$coefficients[2, 4], 2)
m2.neg.p <- p(mod.std$coefficients[2, 5], digits = 3)
m2.neg.Bt <- round(mod.std$coefficients[2, 2], 3)

m2.poz.B <- formatC(mod.std$coefficients[3, 1], format = "f", digits = 5)
m2.poz.t <- round(mod.std$coefficients[3, 4], 2)
m2.poz.p <- p(mod.std$coefficients[3, 5], digits = 3)
m2.poz.Bt <- round(mod.std$coefficients[3, 2], 3)

m2.Fcb.B <- formatC(mod.std$coefficients[4, 1], format = "f", digits = 5)
m2.Fcb.t <- round(mod.std$coefficients[4, 4], 2)
m2.Fcb.p <- p(mod.std$coefficients[4, 5], digits = 3)
m2.Fcb.Bt <- round(mod.std$coefficients[4, 2], 3)

m2.Isg.B <- formatC(mod.std$coefficients[5, 1], format = "f", digits = 5)
m2.Isg.t <- round(mod.std$coefficients[5, 4], 2)
m2.Isg.p <- p(mod.std$coefficients[5, 5], digits = 3)
m2.Isg.Bt <- round(mod.std$coefficients[5, 2], 3)

m2.Ytb.B <- formatC(mod.std$coefficients[6, 1], format = "f", digits = 5)
m2.Ytb.t <- round(mod.std$coefficients[6, 4], 2)
m2.Ytb.p <- p(mod.std$coefficients[6, 5], digits = 3)
m2.Ytb.Bt <- round(mod.std$coefficients[6, 2], 3)

# Relative predictors importance
m2.relimp <- calc.relimp(mod.2, type = c("lmg"), rela = T); m2.relimp
m2.fac.lmg <- round(as.numeric(m2.relimp@lmg[1]) * 100, 2)
m2.neg.lmg <- round(as.numeric(m2.relimp@lmg[2]) * 100, 2)
m2.poz.lmg <- round(as.numeric(m2.relimp@lmg[3]) * 100, 2)

## Third hypothesis #####
ds.estim$Page <- factor(ds.estim$Page, levels = c("Twitter", "Facebook", "Instagram", "Youtube"))
# Centering predictors and computing interaction terms ####
#ds.estim$NegativityGI.c <- misty::center(ds.estim$NegativityGI, type = "CWC", cluster = ds.estim$Page)
#ds.estim$PositivityGI.c <- misty::center(ds.estim$PositivityGI, type = "CWC", cluster = ds.estim$Page)
mod.3 <- lm(ER ~ NegativityGI + PositivityGI + Page + 
              NegativityGI*Page + PositivityGI*Page, 
            data = ds.estim); mod.std <- lm.beta(mod.3)
mod.std <- summary(mod.std); mod.std
m3.F <- round(mod.std[["fstatistic"]][["value"]], 2)
m3.F.df1 <- round(mod.std[["fstatistic"]][["numdf"]], 2)
m3.F.df2 <- round(mod.std[["fstatistic"]][["dendf"]], 2)
m3.F.p <- p(pf(m2.F, m2.F.df1, m2.F.df2, lower.tail = F), digits = 3)
m3.RSE <- formatC(mod.std[["sigma"]], format = "f", digits = 4)
m3.r2 <- formatC(mod.std[["adj.r.squared"]], format = "f", digits = 3)
m3.r2.p <- round(mod.std[["adj.r.squared"]] * 100, 3)

m3.neg.B <- formatC(mod.std$coefficients[2, 1], format = "f", digits = 5)
m3.neg.t <- round(mod.std$coefficients[2, 4], 2)
m3.neg.p <- p(mod.std$coefficients[2, 5], digits = 3)
m3.neg.Bt <- round(mod.std$coefficients[2, 2], 3)

m3.poz.B <- formatC(mod.std$coefficients[3, 1], format = "f", digits = 5)
m3.poz.t <- round(mod.std$coefficients[3, 4], 2)
m3.poz.p <- p(mod.std$coefficients[3, 5], digits = 3)
m3.poz.Bt <- round(mod.std$coefficients[3, 2], 3)

m3.Fbk.B <- formatC(mod.std$coefficients[4, 1], format = "f", digits = 5)
m3.Fbk.t <- round(mod.std$coefficients[4, 4], 2)
m3.Fbk.p <- p(mod.std$coefficients[4, 5], digits = 3)
m3.Fbk.Bt <- round(mod.std$coefficients[4, 2], 3)

m3.Ins.B <- formatC(mod.std$coefficients[5, 1], format = "f", digits = 5)
m3.Ins.t <- round(mod.std$coefficients[5, 4], 2)
m3.Ins.p <- p(mod.std$coefficients[5, 5], digits = 3)
m3.Ins.Bt <- round(mod.std$coefficients[5, 2], 3)

m3.Yut.B <- formatC(mod.std$coefficients[6, 1], format = "f", digits = 5)
m3.Yut.t <- round(mod.std$coefficients[6, 4], 2)
m3.Yut.p <- p(mod.std$coefficients[6, 5], digits = 3)
m3.Yut.Bt <- round(mod.std$coefficients[6, 2], 3)

m3.NegFbk.B <- formatC(mod.std$coefficients[7, 1], format = "f", digits = 5)
m3.NegFbk.t <- round(mod.std$coefficients[7, 4], 2)
m3.NegFbk.p <- p(mod.std$coefficients[7, 5], digits = 3)
m3.NegFbk.Bt <- round(mod.std$coefficients[7, 2], 3)

m3.NegIns.B <- formatC(mod.std$coefficients[8, 1], format = "f", digits = 5)
m3.NegIns.t <- round(mod.std$coefficients[8, 4], 2)
m3.NegIns.p <- p(mod.std$coefficients[8, 5], digits = 3)
m3.NegIns.Bt <- round(mod.std$coefficients[8, 2], 3)

m3.NegYut.B <- formatC(mod.std$coefficients[9, 1], format = "f", digits = 5)
m3.NegYut.t <- round(mod.std$coefficients[9, 4], 2)
m3.NegYut.p <- p(mod.std$coefficients[9, 5], digits = 3)
m3.NegYut.Bt <- round(mod.std$coefficients[9, 2], 3)

m3.PozFbk.B <- formatC(mod.std$coefficients[10, 1], format = "f", digits = 5)
m3.PozFbk.t <- round(mod.std$coefficients[10, 4], 2)
m3.PozFbk.p <- p(mod.std$coefficients[10, 5], digits = 3)
m3.PozFbk.Bt <- round(mod.std$coefficients[10, 2], 3)

m3.PozIns.B <- formatC(mod.std$coefficients[11, 1], format = "f", digits = 5)
m3.PozIns.t <- round(mod.std$coefficients[11, 4], 2)
m3.PozIns.p <- p(mod.std$coefficients[11, 5], digits = 3)
m3.PozIns.Bt <- round(mod.std$coefficients[11, 2], 3)

m3.PozYut.B <- formatC(mod.std$coefficients[12, 1], format = "f", digits = 5)
m3.PozYut.t <- round(mod.std$coefficients[12, 4], 2)
m3.PozYut.p <- p(mod.std$coefficients[12, 5], digits = 3)
m3.PozYut.Bt <- round(mod.std$coefficients[12, 2], 3)

Int.Plot.1 <- interact_plot(mod.3, pred = "NegativityGI", modx = "Page",
                          x.label = "Negative emotional resonance",
                          y.label = "Engagement rate",
                          legend.main = "Social network"); Int.Plot.1
Int.Plot.2 <- interact_plot(mod.3, pred = "PositivityGI", modx = "Page",
                            x.label = "Positive emotional resonance",
                            y.label = "Engagement rate",
                            legend.main = "Social network"); Int.Plot.2
# Comparing models
mod.comp <- anova(mod.2, mod.3); mod.comp
mod.com.F <- round(mod.comp[2, 5], 2)
mod.com.df1 <- round(mod.comp[2, 3], 2)
mod.com.df2 <- round(mod.comp[2, 1], 2)
mod.com.p <- p(mod.comp[2, 6], digits = 3)

# Testing predictions ####
# predictions <- mod %>% predict(ds.asses)
# RMSE(predictions, ds.asses$ER)
# R2(predictions, ds.asses$ER)
# RMSE(predictions, ds.asses$ER) / mean(ds.asses$ER)
# 
# # Model diagnosis ####
# diag.mod <- augment(mod); diag.mod
# ggplot(diag.mod, aes(x = NegativityGI, y = ER)) +
#   geom_point() + stat_smooth(method = "lm", se = T) +
#   geom_segment(aes(xend = NegativityGI, yend = .fitted),
#                color = "red", sixe = 0.3)
# ggplot(diag.mod, aes(x = PositivityGI, y = ER)) +
#   geom_point() + stat_smooth(method = "lm", se = T) +
#   geom_segment(aes(xend = PositivityGI, yend = .fitted),
#                color = "red", sixe = 0.3)
# ## Extreme residuals analysis
# par(mfrow = c(2, 2)); plot(mod)
# diag.mod %>% identify_outliers(.std.resid)
# # HLV Values analysis
# diag.mod %>% filter(abs(.hat) > 0.037)
# 
# ## Cook's distances analysis
# plot(mod, 4); diag.mod %>% top_n(4, wt = .cooksd)
# # Multicolinearity analysis
# car::vif(mod)
# 
