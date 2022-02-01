# Defining CPUE
rm(list = ls())
library(tidyverse)
library(sf)


files <- list.files("C:/Users/steven.winter/OneDrive - Washington State University (email.wsu.edu)/Datasets/GMU-level_Presence_Absence", pattern = ".csv", full.names = TRUE)

WA_gmus <- st_read("C:/Users/steven.winter/OneDrive - Washington State University (email.wsu.edu)/Datasets/Game_management_units/Washington_GMUs/WA_GMUs_Metadata.shp")
head(WA_gmus)

decade_raw <- read.csv(files[grepl("2011-2020", files)])
decade <- decade_raw
head (decade)

names(decade)[names(decade)==names(decade)[1]] <- "Year"
names(decade)[names(decade)=="UnitNumberTypeText"] <- "GMU"
decade$GMU <- gsub(pattern = "GMU ", replacement = "", x = decade$GMU)
decade$GMU <- as.integer(decade$GMU)

decade <- subset(decade, decade$Method!="All Methods")
decade <- subset(decade, decade$Method!="Multiple Weapons")

#Do we have any hunters participating with 0 hunter days reported?
decade[decade$Hunters>0 & decade$HunterDays==0,]
#asuume if a hunter is recorded that they attempted at least one day (or else why would they be recorded?)
decade[decade$Hunters>0 & decade$HunterDays==0,]$HunterDays <- decade[decade$Hunters>0 & decade$HunterDays==0,]$Hunters

decade$Hunters2 <- ifelse(decade$HunterDays==decade$HarvestTotal,decade$HunterDays, decade$Hunters)

#aggregate(HarvestTotal~GMU, data =decade, FUN =sum)

decade$CPUE <- decade$HarvestTotal / (decade$HunterDays/100)
decade[is.na(decade$Method),]

# #SKIP
# cpue_sum <- aggregate(CPUE~GMU, data = decade, FUN = sum)
# cpue_method <- aggregate(CPUE~GMU + Method, data = decade, FUN = sum)


# wa <- st_read("C:/Users/steven.winter/OneDrive - Washington State University (email.wsu.edu)/Projects/Elk_Hoof_Disease/USA_adm", layer = "USA_adm1")
# wa <- wa[wa$NAME_1=="Washington",]
# wa <- st_transform(wa, crs = my_crs)
# HPop_10 <- raster("C:/Users/steven.winter/OneDrive - Washington State University (email.wsu.edu)/GIS/gpw-v4-population-count-rev11_2010_30_sec_tif/gpw_v4_population_count_rev11_2010_30_sec.tif")
# 
# HPop_15 <- raster("C:/Users/steven.winter/OneDrive - Washington State University (email.wsu.edu)/GIS/gpw-v4-population-count-rev11_2015_30_sec_tif/gpw_v4_population_count_rev11_2015_30_sec.tif")
# 
# HPop_20 <- raster("C:/Users/steven.winter/OneDrive - Washington State University (email.wsu.edu)/GIS/gpw-v4-population-count-rev11_2020_30_sec_tif/gpw_v4_population_count_rev11_2020_30_sec.tif")
# 
# HPop_Dens <- raster::stack(HPop_10,HPop_15,HPop_20) #Stack and calculate the mean population size
# HPop_Dens <- crop(HPop_Dens, wa)
# HPop_Dens <- calc(HPop_Dens, fun = mean)
# HPop_Dens <- mask(HPop_Dens, wa)
# plot(HPop_Dens)
# hpop_df <- data.frame()
# 
# for (i in unique(WA_gmus$GMU_Num)) {
#   gmu <-  WA_gmus[WA_gmus$GMU_Num==i,]
#   cropped <- crop(HPop_Dens, gmu)
#   cropped <- mask(cropped, gmu)
#   n <- cellStats(cropped, stat = sum)
#   hpop_df <- rbind(hpop_df, c(gmu$GMU_Num,n) )
# }
# colnames(hpop_df) <- c("GMU", "HPopN")
# write.csv(hpop_df, "C:/Users/steven.winter/OneDrive - Washington State University (email.wsu.edu)/Datasets/GMU_Level_Human_Population/HumanPopN.csv", row.names = F)
hpop_df <- read.csv("C:/Users/steven.winter/OneDrive - Washington State University (email.wsu.edu)/Datasets/GMU_Level_Human_Population/HumanPopN.csv")

WA_gmus <- left_join(WA_gmus, hpop_df)


colnames(WA_gmus)

data <- WA_gmus %>%
  dplyr::select(GMU, Prp_For, Pct_For, TriArea, 
         Pct_TRI, MElevM, SDElevM,
         Rd_Lgth, AreSqKm, Rd_DMSK, PDnPSKM, Pub_Prp, InvPbPr,
         WDFW_Pr, WUIntmx, WUIntfc, HPopN)%>%
  dplyr::rename(Proportion_Forest = Prp_For, TRI = TriArea, 
         Mean_Elevation = MElevM, SD_Elevation = SDElevM,
         Road_Length = Rd_Lgth, Area_SqKm = AreSqKm, 
         Road_Density = Rd_DMSK, HPopN = HPopN, HPop_Dens = PDnPSKM,
         Public_Prop = Pub_Prp, PrivateNOTPub = InvPbPr,
         WDFW_PrivateLands = WDFW_Pr, WUI_Intermix = WUIntmx, 
         WUI_Interface = WUIntfc)%>%
  full_join(decade)%>%
  # full_join(cpue_method)%>%
  st_drop_geometry() # Remove the geometry aspect of the sf data frame

data$HPopN <- round(data$HPopN,0)
# data$cpueadj <- data$CPUE / log(data$HPopN)

# write_sf(data,"C:/Users/steven.winter/OneDrive - Washington State University (email.wsu.edu)/GIS/CPUE.shp")

data <- data[!is.na(data$CPUE),]


# Standardize -------------------------------------------------------------
# 
# 
# colnames(data)
# data_stnd <- data
# 
# head(data)
# non_standard <- c("GMU", "Method", "Pct_TRI", "Pct_For", "Year", "Animal",
#                   "Opportunity", "UnitName", "CPUE")
# 
# #Standardize GMU-related covariate data.
# data_stnd <- data%>%
#   select( -one_of(non_standard))%>%
#   purrr::map(.,.f = rethinking::standardize)%>% #use purrr's map() to apply rethinking's standardize() function to each vector within data.
#   as.tibble()
# #Append columns that are irrelevant to standardization back to df.
# 
# 
# data_stnd <- data%>%
#   select( one_of(non_standard ))%>%
#   bind_cols(., data_stnd)
# 
# # install.packages("glmm")
# library(glmm)
# 
# data <- data_stnd
# 
# 
# method <- lm(data = data, formula = CPUE ~ Method)
# summary(method)
# exp(method$coefficients)
# 
# proportion_forest <- lm(data = data, formula = CPUE ~ Proportion_Forest)
# summary(proportion_forest)
# exp(proportion_forest$coefficients)
# 
# tri <- lm(data = data, formula = CPUE ~ TRI)
# summary(tri)
# exp(tri$coefficients)
# 
# roads <- lm(data = data, CPUE ~ Road_Density)
# summary(roads)
# exp(roads$coefficients)
# 
# human_pop <- lm(data= data, CPUE ~ HPop_Dens)
# summary(human_pop)
# 
# wuimix <- lm(data = data, CPUE~WUI_Intermix)
# summary(wuimix)
# 
# wui <- lm(data = data, CPUE~WUI_Interface)
# summary(wui)
# 
# pub <- lm(data = data, CPUE~Public_Prop)
# summary(pub)
# exp(pub$coefficients)
# 
# priv <- lm(data = data, CPUE~PrivateNOTPub)
# summary(priv)
# exp(priv$coefficients)
# 
# 
# library(GGally)
# data%>%
#   select( -one_of("GMU"))%>%
#   ggcorr(., label = T)
# 
# 
# 
# all <- lm(data = data, CPUE ~ Road_Density + TRI + Mean_Elevation + Proportion_Forest )
# summary(all)
# 
# str(data)
# data$Method <- as.factor(data$Method) #CATEGORIAL DATA NEED TO BE AS FACTOR FOR GAMS
# 
# all_gam <- gam(formula = CPUE ~ s(Proportion_Forest, by = Method) + s(TRI, by  = Method) + s(Road_Density, by  = Method) + Method,data =  data, method = "REML")
# 
# summary(all_gam)
# plot(all_gam)
# 
# car::vif(all)
# 
# 
# pca_data <- data[2:6]
# 
# str(pca_data)
# pca <- prcomp(pca_data, center = TRUE,scale. = TRUE)
# summary(pca)
# 
# data$CPUE_Normal <- rethinking::normalize(data$CPUE)
# pca_df <- as.data.frame(pca$x)
# lm1 <- lm(data$CPUE_Normal~ pca_df$PC1)
# summary(lm)
# plot(lm1)
# 
# 
# ggbiplot::ggbiplot(pca, alpha=0, groups =data$Method,
#                    ellipse = TRUE)+
#   scale_y_continuous(limits =c(-2.5,2.5))
# 
# pca
# 
# plot(effects::allEffects(all))
# 
# head(data)
# 
# 
# library(mgcv)
# 
# 
# model <- gam(data = data, formula = CPUE ~ s(Proportion_Forest))
# 
# termplot(model, partial.resid = TRUE, se = TRUE)
# summary(model)
# 
# plot(model, residuals=TRUE, pch = 1)
# 
# coef(model)
# 
# 
# 
# # colnames(cpue_sum) <- c("GMU", "CPUE_sum")
# # cpue_sd <- aggregate(CPUE~GMU, data = decade, FUN = sum)
# # colnames(cpue_sd) <- c("GMU", "CPUE_sd")
# # 
# # library(ggplot2)
# # library(tidyverse)
# # data <- WA_gmus@data
# # data <- full_join(x= data, y = cpue_sum)
# # data <- full_join(x= data, y = cpue_sd)
# # WA_gmus@data <- data
# # 
# # writeOGR(obj = WA_gmus, 
# #          dsn = "C:/Users/steven.winter/OneDrive - Washington State University (email.wsu.edu)",
# #          layer = "WA_GMUs_CPUE",driver = "ESRI Shapefile")
# # gmu_fort <- fortify(WA_gmus)
# # gmu_fort <- gmu_fort%>%
# #   left_join(., data, by = c("id"="depcom"))
# # 
# # plot(WA_gmus, col = WA_gmus$CPUE_sum)
# 
# 
# cpue_decade <- aggregate(CPUE~GMU + Year + Method, data = decade, FUN = sum)
# head(cpue_decade)
# 
# library(tidyverse)
# cpue_decade1 <- left_join(cpue_decade, WA_gmus)
# 
# library(ggplot2)
# ggplot(data = cpue_decade1, aes(x=as.factor(GMU), y= log(CPUE)))+ 
#                                 # color = as.factor(EastWest_I)))+
#   geom_hline(aes(yintercept = 0), color="red")+
#   scale_color_discrete(labels = c("East", "West"))+
#   geom_boxplot()+
#   labs(color = "Longitudinal\nGrouping")+
#   facet_wrap(~Method, nrow=3)+
#   theme(axis.text.x = element_text(angle = -90, vjust = 0.25))
# table(cpue_decade$GMU[cpue_decade$CPUE==0])
# #Need to calculate per 100 days 
# #
# #Do we have any hunter days reported with 0 hunters reported?
# decade[decade$HunterDays>0 & decade$Hunters==0,]
# 
# 
# 
# 
# names(decade)
# #harvested/Hunter-Day per year and per method using all GMUs 
# decade_method <- aggregate(DaysPerKill ~ Year + Method + GMU,data=decade, FUN=sum)
# decade_gmu <- aggregate(DaysPerKill ~ GMU + Method,data=decade, FUN=sum)
