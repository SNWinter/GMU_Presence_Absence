rm(list = ls())
source("./CPUE_Analysis.R")
library(lme4)
library(emmeans)
 library(tidyverse)
library(ggplot2)
library(sf)
library(tidyverse)
library(viridis)
library(MuMIn)
library(marginaleffects)

#Standardize variables
mod$Proportion_Forest1 <- standardize(mod$Proportion_Forest)
mod$SuccessRate1 <- standardize(mod$SuccessRate)
mod$HunterDays1 <- standardize(mod$HunterDays)

# Hypothesis and model formulation ----------------------------------------

#Full model assessing harvest total to be influenced by GMU-specific proportion forested NLCD, Success rate of harvest, standardized TRI, and a human index of influence/presence/accessibility. Random effects of GMU and year.

nbmod1 <- glmer.nb(formula = HarvestTotal ~ Proportion_Forest + SuccessRate +
                     TRI_St + Human_Index +  (1|GMU) + (1|Year1),
                   offset = log(HunterDays),
                   data = mod)

#standardized vars
nbmod1st <- glmer.nb(formula = HarvestTotal ~ Proportion_Forest1 + SuccessRate1 +
                     TRI_St + Human_Index +  (1|GMU) + (1|Year1),
                   offset = log(HunterDays),
                   data = mod)

poismod1 <- glmer(formula = HarvestTotal ~ Proportion_Forest + SuccessRate +
                    TRI_St + Human_Index + (1|Year1)+ (1|GMU),
                  family= "poisson", offset = log(HunterDays),
                  data = mod)
#standardized vars
poismod1st <- glmer(formula = HarvestTotal ~ Proportion_Forest1 + SuccessRate1 +
                    TRI_St + Human_Index + (1|Year1)+ (1|GMU),
                  family= "poisson", offset = log(HunterDays),
                  data = mod)

intOnly <- glmer.nb(formula= HarvestTotal ~ 1 + (1|Year1)+ (1|GMU),
                    offset = log(HunterDays), 
                    data=mod)

# lmtest::lrtest(nbmod1, intOnly)
summary(nbmod1)
plot(nbmod1)
plot(poismod1)
#Compare to see if they're different
lmtest::lrtest(nbmod1, poismod1)
anova(nbmod1, poismod1)

car::vif(nbmod1); car::vif(poismod1)

AICc(nbmod1, poismod1, nbmod1st, poismod1st)
# Data agrees best with nbmod1

#Diagnostic plots for negative binomial model
# performance::check_model(poismod1)
performance::check_model(nbmod1)


ggsave("./Figures/GlobalNegativeBinomDiagnostics.tiff", plot = last_plot(),
       width = 15, height = 14, units = "cm", dpi = 300, scale = 1.8)

#SuccessRate removed, evaluate interaction between ruggedness and human index (people hunt in more rugged areas because too many surrounding people)
nbmod2 <- glmer.nb(formula = HarvestTotal ~ TRI_St * Human_Index +
                (1|Year1)+ (1|GMU),
             offset = log(HunterDays),
              data = mod)
summary(nbmod2)
#SuccessRate removed, evaluate interaction between SuccessRate and human index (i.e, )
nbHumanmod <- glmer.nb(formula = HarvestTotal ~ SuccessRate1 * Human_Index + Proportion_Forest1 + TRI_St+
                     (1|Year1)+ (1|GMU),
                   offset = log(HunterDays),
                   data = mod)
summary(nbHumanmod)

nbForestmod <- glmer.nb(formula = HarvestTotal ~ SuccessRate1 * Proportion_Forest1 + Human_Index + TRI_St+
                     (1|Year1)+ (1|GMU),
                   offset = log(HunterDays),
                   data = mod)
summary(nbForestmod)

performance::check_model(nbForestmod)
ggsave("./Figures/ForestNegativeBinomDiagnostics.tiff", plot = last_plot(),
       width = 16, height = 14, units = "cm", dpi = 300, scale = 1.8)



AICc(nbmod1, nbmod2, nbHumanmod, intOnly, nbForestmod)


options(na.action = na.fail)
dred <- dredge(nbForestmod, trace = TRUE, rank = "AICc", REML = FALSE)
dred

topmods <- get.models(dred, 1:nrow(dred[dred$delta<=2,]))

summary(model.avg(topmods))

modavg <- model.avg(topmods)

pred <- predict(modavg, type="response")
pred <- as.data.frame(pred)
mod$pred <- pred$pred

plot(mod$pred, mod$HarvestTotal)


#Standardizing CPUE
mod$AdjCPUE <- mod$pred / (mod$HunterDays)
mod$AdjCPUESm <- mod$pred / (mod$SuccessRate/mod$HunterDays) #A correction for small, highly effective GMUs

mod_Aggregate <- aggregate(AdjCPUE ~ GMU, data = mod, FUN = sum)
mod_Aggregate <- left_join(mod_Aggregate, aggregate(AdjCPUESm ~ GMU, data = mod, FUN = sum))
mod_Aggregate <- left_join(mod_Aggregate, aggregate(HarvestTotal ~ GMU, data = mod, FUN = sum))
mod_Aggregate <- left_join(mod_Aggregate, aggregate(pred ~ GMU, data = mod, FUN = sum))
mod_Aggregate <- left_join(mod_Aggregate, aggregate(SuccessRate ~ GMU, data = mod, FUN = mean))
mod_agg_sf <- full_join(WA_gmus, mod_Aggregate)
head(mod_agg_sf)

#Plotting

harv <- ggplot(data= mod_agg_sf)+
  geom_sf(aes(fill = HarvestTotal), #Adjust fill parameter to CPUE of choice
          # geom_sf(aes(fill = Ad), #Adjust fill parameter to CPUE of choice
          color="dimgray", lwd = 0.25)+
  theme_void()+
  ggtitle("Actual harvest sum")+
  scale_fill_viridis(option = "viridis")+
  theme(panel.grid = element_blank(), legend.position = c(0.05,0.2),
        legend.background = element_blank())

harv

diff <- ggplot(data= mod_agg_sf)+
  geom_sf(aes(fill = pred - HarvestTotal), #Adjust fill parameter to CPUE of choice
          # geom_sf(aes(fill = Ad), #Adjust fill parameter to CPUE of choice
          color="dimgray", lwd = 0.25)+
  theme_void()+
  labs(fill = "Difference")+
  ggtitle("Model predicted harvest - Actual harvest")+
  scale_fill_gradient2( low = "darkred", mid = "white", high = "darkgreen")+
  theme(panel.grid = element_blank(), legend.position = c(0.05,0.2),
        legend.background = element_blank())

diff

log <- ggplot(data= mod_agg_sf)+
  geom_sf(aes(fill = log(AdjCPUE)), #Adjust fill parameter to CPUE of choice
          # geom_sf(aes(fill = Ad), #Adjust fill parameter to CPUE of choice
          color="lightgray", lwd = 0.25)+
  theme_void()+
  ggtitle("Adjusted CPUE")+
  scale_fill_viridis(option = "plasma")+
  theme(panel.grid = element_blank(), legend.position = c(0.05,0.2),
        legend.background = element_blank())
log

smallnum <- ggplot(data= mod_agg_sf)+
  geom_sf(aes(fill = log(AdjCPUESm)), #Adjust fill parameter to CPUE of choice
          # geom_sf(aes(fill = Ad), #Adjust fill parameter to CPUE of choice
          color="lightgray", lwd = 0.25)+
  theme_void()+
  ggtitle("Adjusted CPUE for sparse hunting")+
  scale_fill_viridis(option = "plasma")+
  theme(panel.grid = element_blank(), legend.position = c(0.05,0.2),
        legend.background = element_blank())
smallnum

quadplot <- gridExtra::grid.arrange(harv, diff, log, smallnum, nrow = 2, ncol = 2)
ggsave("./Figures/NegativeBinomialAllYearsQuadplot.tiff",
       plot = quadplot, width = 18, height = 14, units = "cm", dpi = 300,
       scale = 2)


# Effect of Year Removed --------------------------------------------------
mod_sum <- aggregate(HarvestTotal~ GMU, data = mod, sum)
mod_sum <- left_join(mod_sum, aggregate(HunterDays~ GMU, data = mod, sum))
mod_sum <- left_join(mod_sum, aggregate(SuccessRate~ GMU, data = mod, mean))
mod_sum <- left_join(mod_sum, aggregate(Proportion_Forest~ GMU, data = mod, mean))
mod_sum <- left_join(mod_sum, aggregate(TRI_St~ GMU, data = mod, mean))
mod_sum <- left_join(mod_sum, aggregate(Human_Index~ GMU, data = mod, mean))

mod_sum$Proportion_Forest1 <- standardize(mod_sum$Proportion_Forest)
mod_sum$SuccessRate1 <- standardize(mod_sum$SuccessRate)
mod_sum$HunterDays1 <- standardize(mod_sum$HunterDays)

#Model fitting
# Compare nb vs poisson
nbYrRm1 <- glmer.nb(formula = HarvestTotal ~ Proportion_Forest + SuccessRate +
                     TRI_St + Human_Index +  (1|GMU),
                   offset = log(HunterDays),
                   data = mod_sum)

#standardized vars
nbYrRm1st <- glmer.nb(formula = HarvestTotal ~ Proportion_Forest1 + SuccessRate1 +
                       TRI_St + Human_Index +  (1|GMU),
                     offset = log(HunterDays),
                     data = mod_sum)

poisYrRm1 <- glmer(formula = HarvestTotal ~ Proportion_Forest + SuccessRate +
                    TRI_St + Human_Index + (1|GMU),
                  family= "poisson", offset = log(HunterDays),
                  data = mod_sum)

#standardized vars
poisYrRm1st <- glmer(formula = HarvestTotal ~ Proportion_Forest1 + SuccessRate1 +
                      TRI_St + Human_Index + (1|GMU),
                    family= "poisson", offset = log(HunterDays),
                    data = mod_sum)

summary(poisYrRm1);summary(poisYrRm1st)
lmtest::lrtest(nbYrRm1, poisYrRm1)
anova(nbYrRm1, poisYrRm1)
AICc(nbYrRm1, nbYrRm1st, poisYrRm1, poisYrRm1st)
#No significant difference between poisson and neg binomial... AIC is better in poisson.


HumanInteract <- glmer(formula = HarvestTotal ~ Proportion_Forest1 + SuccessRate1*Human_Index +
                     TRI_St + (1|GMU),
                   family= "poisson", offset = log(HunterDays),
                   data = mod_sum)

ForestInteract <- glmer(formula = HarvestTotal ~ Proportion_Forest1*SuccessRate1 + Human_Index +
                     TRI_St + (1|GMU),
                   family= "poisson", offset = log(HunterDays),
                   data = mod_sum)

AICc(poisYrRm1, poisYrRm1st, HumanInteract, ForestInteract)


options(na.action = na.fail)
YrRmDred <- dredge(ForestInteract, trace = TRUE, rank = "AICc", REML = FALSE)
YrRmDred

YrRMtopmods <- get.models(YrRmDred, 1:nrow(YrRmDred[YrRmDred$delta<=2,]))

summary(model.avg(YrRMtopmods))

YrRmmodavg <- model.avg(YrRMtopmods)

YrRMpred <- predict(YrRmmodavg, type="response")
YrRMpred <- as.data.frame(YrRMpred)

#MOD_Sum  -  Year-effect removed/summarized
mod_sum$pred <- YrRMpred$YrRMpred
head(mod_sum)

mod_sum$AdjCPUE <- mod_sum$pred/(mod_sum$HunterDays)
mod_sum$AdjCPUESm <- mod_sum$pred/( mod_sum$SuccessRate/ mod_sum$HunterDays)
mod_sum_agg_sf <- full_join(WA_gmus, mod_sum)

#Plotting

harv2 <- ggplot(data= mod_sum_agg_sf)+
  geom_sf(aes(fill = HarvestTotal), #Adjust fill parameter to CPUE of choice
          color="lightgray", lwd = 0.25)+
  theme_void()+
  ggtitle("Actual harvest sum")+
  scale_fill_viridis(option = "viridis")+
  theme(panel.grid = element_blank(), legend.position = c(0.05,0.2),
        legend.background = element_blank())

diff2 <-  ggplot(data= mod_sum_agg_sf)+
  geom_sf(aes(fill = pred - HarvestTotal), #Adjust fill parameter to CPUE of choice
          # geom_sf(aes(fill = Ad), #Adjust fill parameter to CPUE of choice
          color="dimgray", lwd = 0.25)+
  theme_void()+
  labs(fill = "Difference")+
  ggtitle("Model predicted harvest - Actual harvest (Years removed)")+
  scale_fill_gradient2( low = "darkred", mid = "white", high = "darkgreen")+
  theme(panel.grid = element_blank(), legend.position = c(0.05,0.2),
        legend.background = element_blank())


log2 <- ggplot(data= mod_sum_agg_sf)+
  geom_sf(aes(fill = log(AdjCPUE)), #Adjust fill parameter to CPUE of choice
          color="lightgray", lwd = 0.25)+
  theme_void()+
  ggtitle("Adjusted CPUE (Years removed")+
  scale_fill_viridis(option = "plasma")+
  theme(panel.grid = element_blank(), legend.position = c(0.05,0.2),
        legend.background = element_blank())

smallnum2 <- ggplot(data= mod_sum_agg_sf)+
  geom_sf(aes(fill = log(AdjCPUESm)), #Adjust fill parameter to CPUE of choice
          color="lightgray", lwd = 0.25)+
  theme_void()+
  ggtitle("Adjusted CPUE for sparse hunting (Years removed)")+
  scale_fill_viridis(option = "plasma")+
  theme(panel.grid = element_blank(), legend.position = c(0.05,0.2),
        legend.background = element_blank())

quadplot2 <- gridExtra::grid.arrange(harv2, diff2, log2, smallnum2, nrow = 2, ncol = 2)
ggsave("./Figures/NegativeBinomialYearRemovedQuadplot.tiff",
       plot = quadplot2, width = 18, height = 14, units = "cm", dpi = 300,
       scale = 2)


compareplots <- gridExtra::grid.arrange(log, smallnum, log2, smallnum2, nrow = 2, ncol = 2)
ggsave("./Figures/NegativeBinomialPoissonPlots.tiff",
       plot = compareplots, width = 18, height = 14, units = "cm", dpi = 300,
       scale = 2)




# install.packages("AER")
# AER::dispersiontest(mod1glm)
# overdisp_fun <- function(model) {
#   rdf <- df.residual(model)
#   rp <- residuals(model,type="pearson")
#   Pearson.chisq <- sum(rp^2)
#   prat <- Pearson.chisq/rdf
#   pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
#   c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
# }

overdisp_fun(mod1)
plot(modZI)
#Evaluate poisson distribution via histogram
table(mod$HarvestTotal)
hist(mod$HarvestTotal, breaks = 100)
#Generate one to really compare
lambda <- mean(mod$HarvestTotal)
n <- length(mod$HarvestTotal)
#Simulate poisson given model-derived lambda and n
tbl <- NULL
for (k in 0:50) {
   tbl <- rbind(tbl,c(obs=sum(mod$HarvestTotal==k),
                      exp=n*exp(-lambda) * lambda^k / factorial(k)))
}
hist(rpois(10000, 4), breaks = 50)
#Visually compare
barplot(tbl[,2])
#Different in predicted values, too...
dimnames(tbl)[[1]] <- paste(0:50)
tbl[1:5,]

#Plot predicted values
plot(mod1)
plot(log(fitted(mod6))~predict(mod6))
pred.val <- predict(mod6, type="response")
plot((mod$HarvestTotal - pred.val)/sqrt(pred.val))
plot(log(pred.val+1),log(mod$HarvestTotal+1))


res <- residuals(mod6, type="pearson")
plot(log(predict(mod1)), res)
par(mfrow=c(3,2), mar = c(2,2,2,2))
plot(mod1)
plot(mod$HarvestTotal,res)
plot(mod$Human_Index,res)
plot(mod$Proportion_Forest ,res)
plot(mod$SuccessRate ,res)
plot(mod$TRI_St ,res)


abline(h=0, lty=2)
qqnorm(res)
qqline(res)



car::vif(mod1);car::vif(mod2);car::vif(mod3);car::vif(mod4);car::vif(mod5); car::vif(mod6)

summary(mod2)

mod_Dred <- MuMIn::dredge(mod1,rank="AICc")
mod_Dred
# archglmm <- glmmTMB::glmmTMB(formula= HarvestTotal~ Proportion_Forest + TRI + Road_Density + HPopN + (1|Year),
#                        family= "poisson", offset = HunterDays,
#                        data=data[data$Method=="Archery",] )

# Dummy <- lme4::glmer(formula= HarvestTotal~ 1 + (1|Year1),
#                         family= "poisson", offset = log(HunterDays),
#                         data=data[data$Method=="Archery",]) 
# summary(Dummy)
 

#Success rate calculated by number animals harvested/# hunters.. but corrected with non-response survey. OMIT
# data$SuccessRate1 <- standardize(data$SuccessRate)





library(MuMIn)# Multimodel inference: model average of all possible models...all possible models ranked by AIC
library(marginaleffects)
library(emmeans)

# GLMM formation, dredging, model averaging and prediction ----------------
combinedglmm <- glmer(formula= HarvestTotal ~ Proportion_Forest + TRI_St + Human_Index + Method +(1|Year1) + (1|GMU1),
                  family= "poisson", offset = log(HunterDays),
                  data=data)
summary(combinedglmm)

# Predictions for Harvest total
pred_combined <- predict(combinedglmm, type= "response")

#Dredged model predictions
combined_MMS <- dredge(combinedglmm,rank="AICc")
# combined_MMS
combined_MAS <- model.avg(combined_MMS, beta = F, revised.var = TRUE)
# summary(combined_MAS)
CombPredMAS<-predict(combined_MAS, type="response")

#Adjusted predictions with fixed covariates
fixed_Pred_Comb <- predictions(combinedglmm, 
                               newdata = datagrid("Human_Index" =  mean(data$Human_Index),
                                                  grid.type = "counterfactual"))

#Combine into new df for plotting/comparasons
CPUE <- data%>%
  dplyr::select(GMU, HunterDays, HarvestTotal)%>%
  cbind(., c(CombPredMAS))%>%
  cbind(.,c(pred_combined))%>%
  cbind(., fixed_Pred_Comb$predicted)

#Calculate CPUE
CPUE$CPUE <- CPUE$HarvestTotal / c(CPUE$HunterDays/100)
CPUE$CPUE_dred <- CPUE$`c(CombPredMAS)` / c(CPUE$HunterDays/100)
CPUE$CPUE_base <- CPUE$`c(pred_combined)` / c(CPUE$HunterDays/100)
CPUE$CPUE_fixedHI <- CPUE$`fixed_Pred_Comb$predicted`/ c(CPUE$HunterDays/100)

#Aggregate for readable plots
CPUE_df <- aggregate(CPUE~GMU, data = CPUE, FUN = sum)
CPUE_df_dred <- aggregate(CPUE_dred~GMU, data = CPUE, FUN = sum)
CPUE_df_base <- aggregate(CPUE_base ~GMU, data = CPUE, FUN = sum)
CPUE_fixed_df <- aggregate(CPUE_fixedHI ~GMU, data = CPUE, FUN = sum)

CPUE_sf <- full_join(WA_gmus, CPUE_df_dred)%>%
  full_join(., CPUE_df)%>%
  full_join(., CPUE_df_base)%>%
  full_join(.,CPUE_fixed_df)
  


ggplot(data= CPUE_sf)+
  geom_sf(aes(fill = CPUE_dred), #Adjust fill parameter to CPUE of choice
          color="lightgray", lwd = 0.25)+
  theme_void()+
  scale_fill_viridis(option = "inferno")+
  theme(panel.grid = element_blank(), legend.position = c(0.05,0.2),
        legend.background = element_blank())


# testglmm <-  glmer(formula= HarvestTotal~ Proportion_Forest + TRI_St*standardize(Road_Density) + Method +(1|Year1),
#                    family= "poisson", offset = log(HunterDays),
#                    data=data)
# summary(testglmm)
# test_pred <- predict(testglmm, type = "response")


# Method Specific Models -------------------------------------------------

# Archery -----------------------------------------------------------------

archglmm <- glmer(formula= HarvestTotal~ Proportion_Forest + TRI_St + Human_Index + (1|Year1)+ (1|GMU1),
                  family= "poisson", offset = log(HunterDays),
                  data= arch)
summary(archglmm)
coef(summary(archglmm))
pred_arch <- predict(archglmm, type= "response")
a <- as.data.frame(pred_arch)
colnames(a) <- "Predicted_Harvest"

arch_MMS <- dredge(archglmm,rank="AICc")
arch_MMS
#Model average
arch_MAS <- model.avg(arch_MMS, beta = F, revised.var = TRUE)
summary(arch_MAS)

ArchPredMAS<-predict(arch_MAS, type="response")

fixed_Pred_Arch <- predictions(archglmm, 
                               newdata = datagrid("Human_Index" =  mean(data$Human_Index),
                                                  grid.type = "counterfactual"))

CPUE_arch <- arch%>%
  dplyr::select(GMU, HunterDays, HarvestTotal)%>%
  cbind(., c(ArchPredMAS))%>%
  cbind(.,c(pred_arch))%>%
  cbind(., fixed_Pred_Arch$predicted)

CPUE_arch$CPUE <- CPUE_arch$HarvestTotal / c(CPUE_arch$HunterDays/100)
CPUE_arch$CPUE_dred <- CPUE_arch$`c(ArchPredMAS)` / c(CPUE_arch$HunterDays/100)
CPUE_arch$CPUE_base <- CPUE_arch$`c(pred_arch)` / c(CPUE_arch$HunterDays/100)
CPUE_arch$CPUE_fixedHI <- CPUE_arch$`fixed_Pred_Arch$predicted`/ c(CPUE_arch$HunterDays/100)

#Aggregate for readable plots
CPUE_arch_df <- aggregate(CPUE~GMU, data = CPUE_arch, FUN = sum)
CPUE_arch_df_dred <- aggregate(CPUE_dred~GMU, data = CPUE_arch, FUN = sum)
CPUE_arch_df_base <- aggregate(CPUE_base ~GMU, data = CPUE_arch, FUN = sum)
CPUE_arch_fixed_df <- aggregate(CPUE_fixedHI ~GMU, data = CPUE_arch, FUN = sum)

CPUE_arch_sf <- full_join(WA_gmus, CPUE_arch_df_dred)%>%
  full_join(., CPUE_arch_df)%>%
  full_join(., CPUE_arch_df_base)%>%
  full_join(.,CPUE_arch_fixed_df)
# ArchMean_adjPred <- predictions(archglmm, newdata = datagrid())

#Plotting
ggplot(data= CPUE_arch_sf)+
  geom_sf(aes(fill = CPUE_base), #Adjust fill parameter to CPUE of choice
          color="lightgray", lwd = 0.25)+
  theme_void()+
  scale_fill_viridis(option = "inferno")+
  theme(panel.grid = element_blank(), legend.position = c(0.05,0.2),
        legend.background = element_blank())



# Muzzleloader ------------------------------------------------------------
muzzglmm <- glmer(formula= HarvestTotal~ Proportion_Forest + TRI_St + Human_Index  + (1|Year1)+ (1|GMU1),
                  family= "poisson", offset = log(HunterDays), 
                  data= muzz)
summary(muzzglmm)
pred_muzz <- predict(muzzglmm, type= "response")
z <- as.data.frame(pred_muzz)
colnames(z) <- "Predicted_Harvest"

muzz_MMS <- dredge(muzzglmm,rank="AICc")
muzz_MMS
#Model average
muzz_MAS <- model.avg(muzz_MMS, beta = F, revised.var = TRUE)
summary(muzz_MAS)

MuzzPredMAS<-predict(muzz_MAS, type="response")

fixed_Pred_muzz <- predictions(muzzglmm, 
                               newdata = datagrid("Human_Index" =  mean(data$Human_Index),
                                                  grid.type = "counterfactual"))

CPUE_muzz <- muzz%>%
  dplyr::select(GMU, HunterDays, HarvestTotal)%>%
  cbind(., c(MuzzPredMAS))%>%
  cbind(.,c(pred_muzz))%>%
  cbind(., fixed_Pred_muzz$predicted)

CPUE_muzz$CPUE <- CPUE_muzz$HarvestTotal / c(CPUE_muzz$HunterDays/100)
CPUE_muzz$CPUE_dred <- CPUE_muzz$`c(MuzzPredMAS)` / c(CPUE_muzz$HunterDays/100)
CPUE_muzz$CPUE_base <- CPUE_muzz$`c(pred_muzz)` / c(CPUE_muzz$HunterDays/100)
CPUE_muzz$CPUE_fixedHI <- CPUE_muzz$`fixed_Pred_muzz$predicted`/ c(CPUE_muzz$HunterDays/100)

#Aggregate for readable plots
CPUE_muzz_df <- aggregate(CPUE~GMU, data = CPUE_muzz, FUN = sum)
CPUE_muzz_df_dred <- aggregate(CPUE_dred~GMU, data = CPUE_muzz, FUN = sum)
CPUE_muzz_df_base <- aggregate(CPUE_base ~GMU, data = CPUE_muzz, FUN = sum)
CPUE_muzz_fixed_df <- aggregate(CPUE_fixedHI ~GMU, data = CPUE_muzz, FUN = sum)

CPUE_muzz_sf <- full_join(WA_gmus, CPUE_muzz_df_dred)%>%
  full_join(., CPUE_muzz_df)%>%
  full_join(., CPUE_muzz_df_base)%>%
  full_join(.,CPUE_muzz_fixed_df)

#Plotting
ggplot(data= CPUE_muzz_sf)+
  geom_sf(aes(fill = CPUE_base), #Adjust fill parameter to CPUE of choice
          color="lightgray", lwd = 0.25)+
  theme_void()+
  scale_fill_viridis(option = "inferno")+
  theme(panel.grid = element_blank(), legend.position = c(0.05,0.2),
        legend.background = element_blank())



# Modern Firearm ----------------------------------------------------------

modglmm <- glmer(formula= HarvestTotal~ Proportion_Forest + TRI_St + Human_Index + (1|Year1)+ (1|GMU1),
                      family= "poisson", offset = log(HunterDays), data= mod)
summary(modglmm)
pred_mod <- predict(modglmm, type= "response")
m <- as.data.frame(pred_mod)
colnames(m) <- "Predicted_Harvest"

mod_MMS <- dredge(modglmm,rank="AICc")
mod_MMS
#Model average
mod_MAS <- model.avg(mod_MMS, beta = F, revised.var = TRUE)
summary(mod_MAS)

ModPredMAS<-predict(mod_MAS, type="response")

fixed_Pred_mod <- predictions(modglmm, 
                               newdata = marginaleffects::datagrid("Human_Index" =  mean(mod$Human_Index),
                                                  grid.type = "counterfactual"))

CPUE_mod <- mod%>%
  dplyr::select(GMU, HunterDays, HarvestTotal)%>%
  cbind(., c(ModPredMAS))%>%
  cbind(.,c(pred_mod))#%>%
  # cbind(., fixed_Pred_mod$predicted)

CPUE_mod$CPUE <- CPUE_mod$HarvestTotal / c(CPUE_mod$HunterDays/100)
CPUE_mod$CPUE_dred <- CPUE_mod$`c(ModPredMAS)` / c(CPUE_mod$HunterDays/100)
CPUE_mod$CPUE_base <- CPUE_mod$`c(pred_mod)` / c(CPUE_mod$HunterDays/100)
CPUE_mod$CPUE_fixedHI <- CPUE_mod$`fixed_Pred_mod$predicted`/ c(CPUE_mod$HunterDays/100)

#Aggregate for readable plots
CPUE_mod_df <- aggregate(CPUE~GMU, data = CPUE_mod, FUN = sum)
CPUE_mod_df_dred <- aggregate(CPUE_dred~GMU, data = CPUE_mod, FUN = sum)
CPUE_mod_df_base <- aggregate(CPUE_base ~GMU, data = CPUE_mod, FUN = sum)
CPUE_mod_fixed_df <- aggregate(CPUE_fixedHI ~GMU, data = CPUE_mod, FUN = sum)
CPUE_mod_sim <- aggregate(`c(ModPredMAS)` ~GMU, data = CPUE_mod, FUN = sum) #Simulated just for proof of concept

CPUE_mod_sf <- full_join(WA_gmus, CPUE_mod_df_dred)%>%
  full_join(., CPUE_mod_df)%>%
  full_join(., CPUE_mod_df_base)%>%
  # full_join(.,CPUE_mod_fixed_df)#%>%
  full_join(.,CPUE_mod_sim)#Simulated just for proof of concept


head(CPUE_mod_sf)
#Plotting
ggplot(data= CPUE_mod_sf)+
  geom_sf(aes(fill = `c(ModPredMAS)`), #Adjust fill parameter to CPUE of choice #Simulated just for proof of concept
          color="lightgray", lwd = 0.25)+
  theme_void()+
  scale_fill_viridis(option = "inferno")+
  theme(panel.grid = element_blank(), legend.position = c(0.05,0.2),
        legend.background = element_blank())

# #Simulated just for proof of concept
simulated_CPUEMap <-  subset(CPUE_mod_sf, select = c(`c(ModPredMAS)`, GMU_Num, geometry))
simulated_CPUEMap <- simulated_CPUEMap[!is.na(simulated_CPUEMap$`c(ModPredMAS)`),]
write_sf(simulated_CPUEMap, "./Simulated_Elk_Abundance.shp")


# Combine dfs into one for easy data manipulations ------------------------
data$ID <- 1:nrow(data)
head(ArchPredMAS); head(ModPredMAS); head(MuzzPredMAS)
# install.packages("https://cran.r-project.org/src/contrib/Archive/rowr/rowr_1.1.3.tar.gz", repos=NULL)

ArchPredMAS_df <- data.frame(ArchPredMAS)
ArchPredMAS_df$ID <- as.integer(rownames(ArchPredMAS_df))
# ArchPredMAS_df$Method <- "Archery"

ModPredMAS_df <- data.frame(ModPredMAS)
ModPredMAS_df$ID <- as.integer(rownames(ModPredMAS_df) )
# ModPredMAS_df$Method <- "Modern"

MuzzPredMAS_df <- data.frame(MuzzPredMAS)
MuzzPredMAS_df$ID <- as.integer(rownames(MuzzPredMAS_df) )
# MuzzPredMAS_df$Method <- "Muzzle"

test <- data.frame(1:nrow(data))
colnames(test) <- "ID"
test$GMU <- data$GMU
test$Effort <- data$DaysPerKill
test <- cbind(test, data.frame(CombPredMAS))

full <- left_join(test, ArchPredMAS_df)%>%
  left_join(., ModPredMAS_df, by = "ID")%>%
  left_join(., MuzzPredMAS_df, by = "ID")

# export <- full
# export$HarvestTotal <- data$HarvestTotal
# write.csv(export, "C:/Users/steven.winter/Fernandez Lab Dropbox/Steven Winter/Actual_VS_Predicted_Harvest.csv")

# test_Pred_sum <- aggregate(test_pred~GMU, data = test, FUN = mean, na.action = na.omit )
# test_sf <- full_join(WA_gmus, test_Pred_sum)


# Aggregate into data frame for plotting purposes -------------------------

#Develop method specific dfs for comparing effects of method indivudially vs combined
Mod_Pred_sum <- aggregate(ModPredMAS~GMU, data = full, FUN = sum, na.action = na.omit )
Mod_Pred_sum <- left_join(Mod_Pred_sum, aggregate(CombPredMAS~GMU, data = full, FUN = sum, na.action = na.omit ))
Mod_Pred_sum <- left_join(Mod_Pred_sum, aggregate(Effort~GMU, data = full, FUN = sum, na.action = na.omit))
head(Mod_Pred_sum)

Muzz_Pred_sum <- aggregate(MuzzPredMAS~GMU, data = full, FUN = sum, na.action = na.omit )
Muzz_Pred_sum <- left_join(Muzz_Pred_sum, aggregate(CombPredMAS~GMU, data = full, FUN = sum, na.action = na.omit ))
Muzz_Pred_sum <- left_join(Muzz_Pred_sum, aggregate(Effort~GMU, data = full, FUN = sum, na.action = na.omit))
head(Muzz_Pred_sum)

Arch_Pred_sum <- aggregate(ArchPredMAS~GMU, data = full, FUN = sum, na.action = na.omit )
Arch_Pred_sum <- left_join(Arch_Pred_sum, aggregate(CombPredMAS~GMU, data = full, FUN = sum, na.action = na.omit ))
Arch_Pred_sum <- left_join(Arch_Pred_sum, aggregate(Effort~GMU, data = full, FUN = sum, na.action = na.omit))
head(Arch_Pred_sum)

MAS_Sum_Full <- full_join(Arch_Pred_sum, Muzz_Pred_sum)%>%
  full_join(., Mod_Pred_sum)
head(MAS_Sum_Full)
MAS_sf <- full_join(WA_gmus, MAS_Sum_Full)
head(MAS_Sum_Full)


# test1 <- rowr::cbind.fill(test, ArchPredMAS, ModPredMAS, MuzzPredMAS)
# cbin
# a1 <- 
# 
# 
# 
# predicted_harv <- sort(predicted_harv, )
# 
# all <- as.data.frame(pred_combined)
# colnames(all) <- "Predicted_Harvest"
# data1 <- data
# data1 <-cbind(data1, all)
# head(data1)
# 
# 
# GMU_harv <- aggregate(Predicted_Harvest ~ GMU,data = data1, FUN = mean )
# WA_gmus1 <- WA_gmus%>% left_join(., GMU_harv)


# Plotting ----------------------------------------------------------------
library(viridis)

archery <- ggplot(data= MAS_sf)+
  geom_sf(aes(fill = ArchPredMAS), 
          color="lightgray", lwd = 0.25)+
  theme_void()+
  scale_fill_viridis(option = "inferno")+
  labs(fill="Archery")+
  theme(panel.grid = element_blank(), legend.position = c(0.05,0.2),
        legend.background = element_blank())

modern <- ggplot(data= MAS_sf)+
  geom_sf(aes(fill = ModPredMAS), 
          color="lightgray", lwd = 0.25)+
  scale_fill_viridis(option = "inferno")+
  theme_void()+
  labs(fill="Modern")+
  theme(panel.grid = element_blank(), legend.position = c(0.05,0.2),
        legend.background = element_blank())

muzzleloader <- ggplot(data= MAS_sf)+
  geom_sf(aes(fill = MuzzPredMAS), 
          color="lightgray", lwd = 0.25)+
  scale_fill_viridis(option = "inferno")+
  theme_void()+
  labs(fill="Muzzle")+
  theme(panel.grid = element_blank(), legend.position = c(0.05,0.2),
        legend.background = element_blank())

combined <- ggplot(data= MAS_sf)+
  geom_sf(aes(fill = CombPredMAS), 
          color="lightgray", lwd = 0.25)+
  scale_fill_viridis(option = "inferno")+
  theme_void()+
  labs(fill="Combined")+
  theme(panel.grid = element_blank(), legend.position = c(0.05,0.2),
        legend.background = element_blank())
combined
# ggsave("./Figures/Mean_GLMM_Output.tiff", plot = last_plot(),
#        dpi=300, width = 10, height = 7)

library(patchwork)

areas <- c(area(1,1),
           area(1,2),
           area(2,1),
           area(2,2))
plot(areas)# Show the layout to make sure it looks as it should


# Figure Making -----------------------------------------------------------
combined + modern + muzzleloader + archery + plot_layout(design = areas)

ggsave("./Figures/Multi_model_Avg_GLMM_Output.tiff", plot = last_plot(),
              dpi=300, width = 12, height = 8)

