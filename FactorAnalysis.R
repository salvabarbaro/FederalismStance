## FedAtt_Party.R
library(dplyr)
library(ggplot2)
library(ggdist)
library(ggrepel)
library(ggpubr)
library(rstatix)
library(ggpp)
library(reshape2)
library(ggthemes)
library(colorspace)
library(texreg)
library(sf)
###########################################################################################
#setwd("")
load("microdf.RData")
###########################################################################################
df.melt <- melt(df, id.vars = c("abbreviation", "fedatt.micro"))
df.short <- df %>% group_by(abbreviation) %>% filter(row_number(fedatt.micro) == 1) %>% 
  dplyr::select(., -c("fedatt.micro")) %>%
  rename(fedatt.mean = 'mean(fedatt.micro)')
### get geo data from gisco (giscoR)
nuts.df <- data.frame(NUTS_ID = c("DEA", "DEB", "DEC", "DED", "DEE", "DE3", 
                                  "DE1", "DE2", "DE4", "DE5", "DE6", "DE7", 
                                  "DE8", "DE9", "DEF", "DEG"),
                      abbreviation = c("NW", "RP", "SL", "SN", "ST", "BE", "BW", "BY", 
                                       "BR", "HB", "HH", "HE", "MV", "NI", "SH", "TH"))
bund_shp <- giscoR::gisco_get_nuts(country = "DEU", nuts_level = 1, resolution = 03) %>%
  left_join(x = ., y = nuts.df, by = "NUTS_ID")
plot.data <- left_join(x = bund_shp, y = df.short, by = "abbreviation")  # for geo-data plotting
###################################################################################################################################
#1. Effect of government-heading parties
kw1 <- df %>% kruskal_test(fedatt.micro~party)
mw1 <- df %>% wilcox_test(fedatt.micro~party, alternative = "greater")
#2. Effect of Party shares
party.lm <- lm(data = df, fedatt.micro ~ rCDU_CSU + rFDP + rSPD + rGrüne + rLinke)
summary(party.lm)
#texreg::texreg(party.lm)
###################################################################################################################
## 3.  Prussian tradition
kw2 <- df %>%  kruskal_test(fedatt.micro~Prussian.char)
dt2<- df %>%  dunn_test(fedatt.micro~Prussian.char)
#
#####################################################################################################################
## 4. Catholic Share
## see plotscript
##########################################################################
## 5. FKM
## see plotscript
##6. Average attitude values (plot on map)
## see plotscript
#############################################################################
#### OLS
df <- df %>% mutate(Prussian.na  = ifelse(Prussian.char == "no", 0, ifelse(Prussian.char == "mixed", NA, 1)),
                    .after = Prussian.char)
mod1 <- lm(formula = fedatt.micro ~ cath.share21 + FKM21 + Prussian.char +
             rCDU_CSU + rFDP + rSPD + rGrüne + rLinke,
           data = df)
mod2 <- lm(formula = fedatt.micro ~ cath.share21  + Prussian.char +
             rCDU_CSU + rFDP + rSPD + rGrüne + rLinke,
           data = df)
mod3 <- lm(formula = fedatt.micro ~ cath.share21 + FKM21 + Prussian.na +
             rCDU_CSU + rFDP + rSPD + rGrüne + rLinke,
           data = df)
mod4 <- lm(formula = fedatt.micro ~ cath.share21 + FKM21 + 
             rCDU_CSU + rFDP + rSPD + rGrüne + rLinke,
           data = df)
#texreg::texreg(l = list(mod1, mod2, mod3, mod4),
#               custom.model.name =c("(S.1)", "(2)", "(3)", "(4)"),
#               caption="OLS regressions: base model and model variations",
#               caption.above=TRUE,
#               single.row=TRUE,
#               digits=3,
#               file = "basemodels.tex"
#               )

library(plm)
#
mod_S1plm <- plm(fedatt.micro ~ cath.share21 + FKM21 + rCDU_CSU + rFDP + rSPD + rGrüne + rLinke,
                 index = c("Prussian.char"), model = "within", data = df)
mod_S2plm <- plm(fedatt.micro ~ cath.share21 + FKM21 + rCDU_CSU + rFDP + rSPD + rGrüne + rLinke,
                 index = c("Prussian.na"), model = "within", data = df %>% filter(., is.na(Prussian.na) == F))
mod_S3plm <- plm(fedatt.micro ~ cath.share21 + FKM21 + Prussian.char + rCDU_CSU + rFDP + rSPD + rGrüne + rLinke,
                 index = c("party"), model = "within", data = df %>% filter(., is.na(party) == F))
# 
#texreg(l = list(mod_S1plm, mod_S2plm, mod_S3plm),
#       custom.model.name = c("S.1", "S.2", "S.3"),
#       caption = "OLS regression with fixed effects",
#       caption.above = TRUE,
#       single.row = TRUE,
#       digits = 3,
#       file = "~/Documents/Research/Yardstick/BR01/modelspecs.tex"
#       )
#
#summary(mod_S3plm)
######################################################################################
# Ordinal logistic regression (we use for higher stability the MASS package, with fixed effect we turn to the ordinal package)
## Main models
library(ordinal)  # For ordinal log reg with fixed / random effects
library(MASS)     # For ordinal log reg (polr)
library(brant)    # Brant test (proportional odds assumption)
#library(gtsummary)  # for display summary tables
mod01 <- polr(formula = as.factor(fedatt.micro) ~ cath.share21 + 
                FKM21 + Prussian.char +
                rCDU_CSU + rFDP + rSPD + rGrüne,
              data = df, Hess = T)
brant(mod01)  # Highly significant --> p>.05 --> Parallel regression assumption holds
#performance::check_model(mod01, residual_type = "normal")

mod02 <- polr(formula = as.factor(fedatt.micro) ~ cath.share21  + 
                Prussian.char + rCDU_CSU + rFDP + rSPD + rGrüne,
           data = df, Hess = T)
brant(mod02)
#
mod03 <- polr(formula = as.factor(fedatt.micro) ~ cath.share21 + FKM21 + 
                Prussian.na + rCDU_CSU + rFDP + rSPD + rGrüne,
           data = df, Hess = T)
brant(mod03)
#
mod04 <- polr(formula = as.factor(fedatt.micro) ~ cath.share21 + FKM21 + 
             rCDU_CSU + rFDP + rSPD + rGrüne,
           data = df, Hess = T)
#tbl_regression(mod04, exponentiate = T)
brant(mod04)
#broom::tidy(mod04, exponentiate = T, conf.int = T)

## Export results to a LaTeX table:
#polr.list <- list(mod01, mod02, mod03, mod04)
#coef_namesShort <- list(
#  "cath.share21" = "Catholic Share 2021",
#  "FKM21" = "Financial Strength",
#  "Prussian.charno" = "Prussian:no",
#  "Prussian.charyes" = "Prussian:yes",
#  "rCDU_CSU" = "Election Results (UNION)",
#  "rSPD" = "Election Results (SPD)",
#  "rFDP" = "Election Results (FDP)",
#  "rGrüne" = "Election Results (GREEN)"
#)
#texreg::texreg(l = polr.list,
#               override.coef = lapply(polr.list, function(model) exp(extract(model)@coef)),
#               override.ci.low = lapply(polr.list, function(model) exp(extract(model)@coef - 1.96 * extract(model)@se)),
#               override.ci.up = lapply(polr.list, function(model) exp(extract(model)@coef + 1.96 * extract(model)@se)),
#               ci.test = 1,
#               custom.coef.map = coef_namesShort,
#               omit.coef = "1|2",
#               custom.model.name =c("(1)", "(2)", "(3)", "(4)"),
#               caption="Ordinal logistic regressions: base model and model variations",
#               caption.above=TRUE,
#               single.row=F,
#               digits=3,
#               booktabs = TRUE, use.packages = F,
#               label = "tb.polr",
#               file = "polr01.tex"
#)
#############   Robustness Checks -- Fixed Effects (Table 6)
mod_S1clm <- clm(as.factor(fedatt.micro) ~ cath.share21 + FKM21 + 
                   rCDU_CSU + rFDP + rSPD + rGrüne  +
                   factor(Prussian.char),
                 data = df)
summary(mod_S1clm)
#
mod_S2clm <- clm(as.factor(fedatt.micro) ~ cath.share21 + FKM21 + 
                   rCDU_CSU + rFDP + rSPD + rGrüne +
                   factor(Prussian.na),
                 data = df %>% filter(!is.na(Prussian.na)))
summary(mod_S2clm)
#
mod_S3clm <- clm(as.factor(fedatt.micro) ~ cath.share21 + FKM21 + 
                   Prussian.char + rCDU_CSU + rFDP + rSPD + 
                   rGrüne +
                   factor(party),
                 data = df %>% filter(!is.na(party)))
summary(mod_S3clm)
#
clmFE.list <- list(mod_S1clm, mod_S2clm, mod_S3clm)
#
#texreg::texreg(l = clmFE.list,
#               override.coef = lapply(clmFE.list, function(model) exp(extract(model)@coef)),
#               override.ci.low = lapply(clmFE.list, function(model) exp(extract(model)@coef - 1.96 * extract(model)@se)),
#               override.ci.up = lapply(clmFE.list, function(model) exp(extract(model)@coef + 1.96 * extract(model)@se)),
#               ci.test = 1,
#               custom.coef.map = coef_namesShort,
#               custom.model.name =c("(S.1)", "(S.2)", "(S.3)"),
#               caption="Ordinal logistic regressions with fixed effects",
#               caption.above=TRUE,
#               single.row=F,
#               digits=3,
#               booktabs = TRUE, use.packages = F,
#               label = "tb.clmFE",
#               file = "~/Documents/Research/Yardstick/BR01/Publius2024RR/vNOV24/clmFE01.tex"
#)

## East-West as FE
westld <- c("BW", "BY", "HE", "RP", "SL", "NI", "NW", "SH", "HH", "HB")
eastld <- c("TH", "SN", "ST", "BR", "MV")
df.eastwest <- df %>% mutate(eastwest = ifelse(abbreviation %in% westld, 0, ifelse(abbreviation == "BE", NA, 1)))
## fixed effect
mod_EW1clm <- clm(as.factor(fedatt.micro) ~ cath.share21 + FKM21 + 
                   rCDU_CSU + rFDP + rSPD + rGrüne  + Prussian.char +
                   factor(eastwest),
                 data = df.eastwest)
summary(mod_EW1clm)
tbl_regression(mod_EW1clm, exponentiate = T)
## with MASS::polr()
mod_EW1polr <- polr(as.factor(fedatt.micro) ~ cath.share21 + FKM21 + 
                      rCDU_CSU + rFDP + rSPD + rGrüne + Prussian.char +
                      factor(eastwest),
                    data = df.eastwest, Hess = TRUE)
#
#EastWest.list <- list(mod_EW1polr)
#texreg::texreg(l = EastWest.list,
#               override.coef = lapply(EastWest.list, function(model) exp(extract(model)@coef)),
#               override.ci.low = lapply(EastWest.list, function(model) exp(extract(model)@coef - 1.96 * extract(model)@se)),
#               override.ci.up = lapply(EastWest.list, function(model) exp(extract(model)@coef + 1.96 * extract(model)@se)),
#               ci.test = 1,
#               custom.coef.map = coef_namesShort,
#               custom.model.name =c("Main Model"),
#               caption="Ordinal logistic regressions with East vs. West as fixed effects",
#               caption.above=TRUE,
#               single.row=T,
#               digits=3,
#               booktabs = TRUE, use.packages = F,
#               label = "tb.eastwest",
#               file = "~/Documents/Research/Yardstick/BR01/Publius2024RR/vNOV24/clmEastWest.tex"
#)

########################################
## BOOTSTRAP (Monte Carlo) Method for estimator accuracy.
## (Supplementary Material)
library(boot)
set.seed(123)
num_bootstrap_samples <- 1000
#
mod1_boot <- fedatt.micro ~ cath.share21 + FKM21 + Prussian.char +
  rCDU_CSU + rFDP + rSPD + rGrüne + rLinke
run_lm_on_bootstrap <- function(data, indices) {
bootstrap_sample <- data[indices, ]
lm_result <- lm(mod1_boot, data = bootstrap_sample)
return(coef(lm_result))
}
bootstrap_results <- boot(data = df, statistic = run_lm_on_bootstrap, R = num_bootstrap_samples)

coeff.bootstrap <- as.data.frame(bootstrap_results$t) %>% 
  setNames(do.call(cbind, list(names(coefficients(mod1)))))

ggplot(data = coeff.bootstrap, 
       aes(y = FKM21)) +
  geom_histogram(fill = "forestgreen", alpha = 0.4) +
  geom_hline(yintercept = coefficients(mod1)[3], col = "purple") +
  labs(y = "Financial Strength Estimator")
ggsave("bootstrapFKM.pdf")

### Regional identity  (Supplementary Material)
identity.df <-  data.frame(abbreviation = unique(df$abbreviation), 
                           identity.land = c(10, 18, 11, 16, 
                                          14, 25, 8, 11, 
                                          7, 15, 14, 4, 
                                          13, 9, 14, 11),
                           identity.comm = c(42, 38, 35, 41,
                                             30, 41, 44, 48,
                                             37, 46, 44, 38,
                                             35, 36, 39, 40),
                           east.west = c("W", "W", NA,  "E",
                                         "W", "E", "W", "E",
                                         "W", "E", "E", "W",
                                         "W", "W", "W", "W")) %>%
  mutate(identity.regional = identity.land + identity.comm)

#ggplot(data = identity.df, 
#       aes(x = reorder(abbreviation, identity.regional), y = identity.regional) ) + 
#  geom_bar(stat = "identity")

df.identity <- df %>% left_join(x = ., y = identity.df, by = "abbreviation")

id_mod01 <- fedatt.micro ~ cath.share21 + FKM21 + Prussian.char + 
  rCDU_CSU + rFDP + rSPD + rGrüne + rLinke + identity.regional

id_mod02 <- fedatt.micro ~ cath.share21 + FKM21 + Prussian.char + 
  rCDU_CSU + rFDP + rSPD + rGrüne + rLinke + identity.land

mod.id_01 <- lm(formula = id_mod01, data =  df.identity)
mod.id_02 <- lm(formula = id_mod02, data =  df.identity)
#mod.id_03 <- plm(formula = id_mod01, data =  df.identity)


#################################################################################################################
################ END ############################################################################################

