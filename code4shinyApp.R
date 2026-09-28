# Figure 4. main draft, shiny app
# This analysis was done on 28 Sep 2026
# species distribution models for species using 
# long-term and soil/static predictors
# short-term climate and static predictors (wet year 1965)
# short-term climate and static predictors (dry year 1982)
# long-term climate only (no static predictors)
# load libraries
library(data.table)
library(dplyr)
library(stringr)
library(dismo)
library(ggplot2)
library(arm) # for 'bayesglm' function
library(viridis)
library(raster)
library(precrec)
library(cowplot)


# load data
df0 <- read.csv("D:/PhD related/2nd chapter/final analysis 21Sep2026/data/long_short_term_bioclim_soil_21Sep2026.csv")
names(df0)
nrow(df0)

length(unique(df0$grasshopper)) # 480 species in total (including speciemns where only genus identified)
df0$lonlat <- paste0(df0$Long, df0$Lat)
# df0 <- subset(df0, soilClay >= 0)


# found that Desertaria longirugosa and faciata both present. 
# Replace Desertaria longirugosa as Desertaria fasciata in df0 file above 

df0$grasshopper[df0$grasshopper == "Desertaria longirugosa"] <- "Desertaria fasciata"
names(df0)

# do the same for Austroicetes vulgaris corallipes
df0$grasshopper[df0$grasshopper == "Austroicetes vulgaris corallipes"] <- "Austroicetes vulgaris"



### for sdm of each species need to extract soil predictors from 5km rasters
# soil rasters aggregated to 5km as mean for soilClay
# and standard deviation for demH (hydrologically enforced digital elevation) and topographic wetness

# bioclim raster loaded below were created as the long-term average for 49 years
# bio1 <- raster("D:/PhD related/2nd chapter/rasters/bioclimRasters/Bio1.tif")
# bio1 <- scale(bio1)
# names(bio1) <- "bio1"
# 
# 
# bio2 <- raster("D:/PhD related/2nd chapter/rasters/bioclimRasters/Bio2.tif")
# bio2 <- scale(bio2)
# names(bio2) <- "bio2"


# load rasters
bio8 <- raster("D:/PhD related/2nd chapter/rasters/bioclimRasters/Bio8.tif")
bio8 <- scale(bio8)
names(bio8) <- "bio8"

bio12 <- raster("D:/PhD related/2nd chapter/rasters/bioclimRasters/Bio12.tif")
bio12 <- resample(bio12, bio8, method = "bilinear") # to make sure extents are same
bio12 <- scale(bio12)
names(bio12) <- "bio12"

bio15 <- raster("D:/PhD related/2nd chapter/rasters/bioclimRasters/Bio15.tif")
bio15 <- resample(bio15, bio8, method = "bilinear")
bio15 <- scale(bio15)
names(bio15) <- "bio15"

soilClay <- raster("E:/Predictors_grasshoppers/Soil layers AU/soil_clay_5kmRes.tif")
soilClay <- resample(soilClay, bio8, method = "bilinear")
soilClay <- scale(soilClay)
names(soilClay) <- "soilClay"

demH <- raster("D:/PhD related/3rd chapter/rasters/demH/DemH_5km_standardDeviation.tif")
demH <- resample(demH, bio8, method = "bilinear")
demH <- scale(demH)
names(demH) <- "demH"

twi <- raster("D:/PhD related/3rd chapter/rasters/demH/twi_5km_standardDeviation.tif")
twi <- resample(twi, bio8, method = "bilinear")
twi <- scale(twi)
names(twi) <- "topoWetness"


## now create raster for short-term year
# in this case year 1968 was the survey year, 
# so load data for year 1967

wet_yr <- 1965 # wet year in Western Australia 
# see drought year on another script

library(raster)
library(curl)
# prec <- brick(paste("D:/PhD related/Historic climate data Australia/histPrecAu/bom-rain_month-", drought_yr, "0101-", drought_yr, "1231.nc", sep = ""))
prec1 <- brick(paste("D:/PhD related/Historic climate data Australia/histPrecAu/bom-rain_month-", wet_yr, "0101-", wet_yr, "1231.nc", sep = ""))
# prec <- brick("D:/PhD related/Historic climate data Australia/histPrecAu/bom-rain_month-19430101-19431231.nc")
precStack1 <- stack(prec1)

# tmax <- brick(paste("D:/PhD related/Historic climate data Australia/histTempAu/tmax/bom-tmax_month-", drought_yr, "0101-", drought_yr, "1231.nc", sep = ""))
tmax1 <- brick(paste("D:/PhD related/Historic climate data Australia/histTempAu/tmax/bom-tmax_month-", wet_yr, "0101-", wet_yr, "1231.nc", sep = ""))
tmaxStack1 <- stack(tmax1)

# tmin <- brick(paste("D:/PhD related/Historic climate data Australia/histTempAu/tmin/bom-tmin_month-", drought_yr, "0101-", drought_yr, "1231.nc", sep = ""))
tmin1 <- brick(paste("D:/PhD related/Historic climate data Australia/histTempAu/tmin/bom-tmin_month-", wet_yr, "0101-", wet_yr, "1231.nc", sep = ""))
tminStack1 <- stack(tmin1)

library(dismo)
bio_st1 <- biovars(prec = precStack1, tmax = tmaxStack1, tmin = tminStack1)


bio8_st <- bio8 - bio_st1$bio8 # getting anomaly of short-term raster
bio8_st <- resample(bio8_st, bio8, method = "bilinear")
bio8_st <- scale(bio8_st)
names(bio8_st) <- "bio8_st"

bio12_st <- bio12 - bio_st1$bio12 # get anomaly
bio12_st <- resample(bio12_st, bio8, method = "bilinear")
bio12_st <- scale(bio12_st)
names(bio12_st) <- "bio12_st"

bio15_st <- bio15 - bio_st1$bio15 # get anomaly
bio15_st <- resample(bio15_st, bio8, method = "bilinear")
bio15_st <- scale(bio15_st)
names(bio15_st) <- "bio15_st"


drought_yr <- 1982 # drought year in Western Australia

prec1982 <- brick(paste("D:/PhD related/Historic climate data Australia/histPrecAu/bom-rain_month-", drought_yr, "0101-", drought_yr, "1231.nc", sep = ""))
# prec <- brick(paste("D:/PhD related/Historic climate data Australia/histPrecAu/bom-rain_month-", wet_yr, "0101-", wet_yr, "1231.nc", sep = ""))
# prec <- brick("D:/PhD related/Historic climate data Australia/histPrecAu/bom-rain_month-19430101-19431231.nc")
precStack1982 <- stack(prec1982)

tmax1982 <- brick(paste("D:/PhD related/Historic climate data Australia/histTempAu/tmax/bom-tmax_month-", drought_yr, "0101-", drought_yr, "1231.nc", sep = ""))
# tmax <- brick(paste("D:/PhD related/Historic climate data Australia/histTempAu/tmax/bom-tmax_month-", wet_yr, "0101-", wet_yr, "1231.nc", sep = ""))
tmaxStack1982 <- stack(tmax1982)

tmin1982 <- brick(paste("D:/PhD related/Historic climate data Australia/histTempAu/tmin/bom-tmin_month-", drought_yr, "0101-", drought_yr, "1231.nc", sep = ""))
# tmin <- brick(paste("D:/PhD related/Historic climate data Australia/histTempAu/tmin/bom-tmin_month-", wet_yr, "0101-", wet_yr, "1231.nc", sep = ""))
tminStack1982 <- stack(tmin1982)

library(dismo)
bio_st2 <- biovars(prec = precStack1982, tmax = tmaxStack1982, tmin = tminStack1982)


bio8_st2 <- bio8 - bio_st2$bio8 # getting anomaly of short-term raster
bio8_st2 <- resample(bio8_st2, bio8, method = "bilinear")
bio8_st2 <- scale(bio8_st2)
names(bio8_st2) <- "bio8_st2"

bio12_st2 <- bio12 - bio_st2$bio12 # get anomaly
bio12_st2 <- resample(bio12_st2, bio8, method = "bilinear")
bio12_st2 <- scale(bio12_st2)
names(bio12_st2) <- "bio12_st2"

bio15_st2 <- bio15 - bio_st2$bio15 # get anomaly
bio15_st2 <- resample(bio15_st2, bio8, method = "bilinear")
bio15_st2 <- scale(bio15_st2)
names(bio15_st2) <- "bio15_st2"

# predictor raster stack for long-term climate and static predictors
predRasLTCS <- raster::stack(bio8, bio12, bio15, soilClay, demH, twi)

# raster stack for short-term climate 1965 and static predictors
predRas1965 <- raster::stack(bio8, bio12, bio15, bio8_st, bio12_st, bio15_st, soilClay, demH, twi)

# raster stack for short-term climate 1982 and static predictors
predRas1982 <- raster::stack(bio8, bio12, bio15, bio8_st2, bio12_st2, bio15_st2, soilClay, demH, twi)


names(df0)
df1 <- df0[, c(22, 4, 5, 2, 95)]


df1$bio8 <- raster::extract(bio8, df1[, c(2, 3)])
df1$bio12 <- raster::extract(bio12, df1[, c(2, 3)])
df1$bio15 <- raster::extract(bio15, df1[, c(2, 3)])
df1$bio8_st <- raster::extract(bio8_st, df1[, c(2, 3)])
df1$bio12_st <- raster::extract(bio12_st, df1[, c(2, 3)])
df1$bio15_st <- raster::extract(bio15_st, df1[, c(2, 3)])

df1$bio8_st2 <- raster::extract(bio8_st2, df1[, c(2, 3)])
df1$bio12_st2 <- raster::extract(bio12_st2, df1[, c(2, 3)])
df1$bio15_st2 <- raster::extract(bio15_st2, df1[, c(2, 3)])

df1$soilClay <- raster::extract(soilClay, df1[, c(2, 3)])
df1$demH <- raster::extract(demH, df1[, c(2, 3)])
df1$topoWetness <- raster::extract(twi, df1[, c(2, 3)])


# summary of occurrences for species recorded in WA
dt <- as.data.frame(table(df1$grasshopper))
# View(dt) #  view dataframe in Rstudio tab
dt <- dt[-1,] # remove 1st row for empty sites
# rename columns
colnames(dt)[1] <- "species"
colnames(dt)[2] <- "occurrences"

dt2 <- dt[dt$occurrences >19, ] # select rows having species with at least 30 occurrences
# dt2 <- dt[dt$occurrences >24 & dt$occurrences <30, ] # select rows having species having 25 to 29 occurrences

# now remove specimens where only genus mentioned (a species name was not provided)
dt2$species <- as.character(dt2$species) # convert species names into character form
dt3 <- dt2[!grepl("\\.\\b", dt2$species),] # here \\b detects word boundaries (from stringr package)

# get species names
species <- unique(dt3$species) 
length(species) # 74 species in total
species



# load WA polygon for cropping sdm prediction
library(sf)
wa <- st_read("D:/PhD related/1st chapter/Australia boundary shp/WesternAustralia.shp") # this australia spdf was clipped from 'world land boundary' (available from: https://osmdata.openstreetmap.de/data/land-polygons.html), and then unnecessary columns from attribute table deteled using ArcMap
wa <- st_transform(wa, crs = 4326)

wa_sp <- as(wa, Class = "Spatial")

# dir.create(
#   "D:/PhD related/2nd chapter/final analysis 21Sep2026/Shiny/models",
#   recursive = TRUE,
#   showWarnings = FALSE
# )

dir.create(
  "D:/PhD related/2nd chapter/final analysis 21Sep2026/Shiny/maps",
  recursive = TRUE,
  showWarnings = FALSE
)

for (i in 1:length(species)){
  print(i)
  
  Focal_species <- as.character(species[i])
  # Focal_species <- "Austroicetes arida"
  # Focal_species <- "Austroicetes nullarborensis"
  # Focal_species <- "Austroicetes vulgaris"
  # Focal_species <- "GenusNov41 sp1"
  # Focal_species <- "Chortoicetes terminifera"
  # Focal_species <- "Pycnostictus seriatus" # used for testing only without loop
  df <- df1
  df_pres <- subset(df1, grasshopper == Focal_species)
  pres_sites <- unique(df_pres$lonlat)
  # df_abs_old <- subset(df0, grasshopper != Focal_species)
  df_abs <- subset(df1, !(lonlat %in% pres_sites))
  #df_abs <- df_abs[sample(nrow(df_abs), 500), ]
  df_abs2 <- df_abs[!duplicated(df_abs$lonlat),]
  
  df <- rbind(df_pres, df_abs2)
  
  
  df$PA <- ifelse(df$grasshopper == Focal_species, 1, 0)
  
  
  # model species distribution using long-term and soil/static predictors
  S_LT <- bayesglm(PA ~ 
                     # bio1 + I(bio1^2) +
                     # bio2 + I(bio2^2) +
                     bio8 + I(bio8^2) +
                     bio12 + I(bio12^2) + bio15 + I(bio15^2)
                   +
                     # bio8_anmly + I(bio8_anmly^2) +
                     # bio12_anmly + I(bio12_anmly^2) +
                     # bio15_anmly + I(bio15_anmly^2) +
                     soilClay + I(soilClay^2) +
                     topoWetness + I(topoWetness^2) +
                     demH + I(demH^2)
                   , 
                   family = binomial(link = "logit"), 
                   maxit = 200,data = df)  
  
  newData_S_LT <- df[, c("bio8", "bio12", "bio15",
                         "soilClay", "topoWetness", "demH")]
  newData_S_LT$observed <- df$PA
  
  prediction_S_LT <- predict(S_LT, newData_S_LT, type = "response")
  # library(precrec)
  precrec_obj_S_LT <- evalmod(scores = prediction_S_LT, labels = newData_S_LT[,"observed"])
  # print(precrec_obj_full)
  
  # get auc values from the above object (i.e., precrec_obj)
  aucs_S_LT <- auc(precrec_obj_S_LT)
  aucs_S_LT
  auc_roc_S_LT <- subset(aucs_S_LT, curvetypes == "ROC")
  # auc_roc_full
  auc_roc_S_LT$aucs
  
  auc_pr_S_LT <- subset(aucs_S_LT, curvetypes == "PRC")
  # auc_pr_full
  auc_pr_S_LT$aucs
  
  
  pred <- raster::predict(predRasLTCS, S_LT, type = "response")
  predWA <- crop (pred, wa_sp)
  
  # plot(predWA, main = paste("SDM for ", Focal_species))
  # points(df[df[,13]==1, 2:3], col = "red", pch=20, cex=0.8)
  # points(df[df[,13]==0, 2:3], col = "black", pch=3, cex=0.8)
  
  
  
  predPlotWA <- as(predWA, "SpatialPixelsDataFrame")
  predPlotWA_df <- as.data.frame(predPlotWA)
  colnames(predPlotWA_df) <- c("Value", "x", "y")
  
  predWA_plot <- ggplot() +
    geom_tile(data = predPlotWA_df, aes(x=x, y=y, fill = Value)) + 
    geom_polygon(data = wa_sp, aes(x=long, y=lat, group = group), alpha = 0, color = "grey") +
    # geom_polygon(data = ibraWA, aes(x=long, y=lat, group = group), alpha = 0, color = "grey") +
    # add presence points for the species
    geom_point(data = df[df[,18]==1, 2:3], aes(x=Long, y=Lat), color="magenta", pch = 18, size = 3, alpha = 0.8)+
    coord_equal() +
    theme_bw() +
    # adding texts for bioregions at selected locations
    # annotate("text", x = c(127, 120.077, 114.566, 114.566, 117.191670, 117, 123.2, 125.9, 128.715510, 122.533164, 126.536081),
    #          y = c(-32.5, -34.502, -31.476, -25.222, -22.413562, -24.4, -18.4, -17.2, -15.763438, -32.810679, -14.901289),
    #          label = c("Ha", "EsPl", "SwCoPl", "Ca", "Pi", "Ga", "Da", "CeKi", "ViBo", "Ma", "NoKi"),
    #          colour = "black", alpha = 0.5) +
    # scale_fill_viridis_c(limits = c(0.05, 0.7), option = "D", 
    #                      direction = -1, 
    #                      breaks = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)) + # from library (viridis)
    scale_fill_viridis_c(limits = c(0.005, 1), option = "D",
                         direction = -1) + # from library (viridis)
    # scale_fill_gradientn(colors=rev(c("firebrick2", "magenta", "deeppink2", "mediumorchid1", "pink", "salmon2", "darkorange3", "orange", "yellow", "green", "lightgreen", "blue", "azure3", "lightgrey")))+
    # labs(subtitle = paste("(d) A. arida (only long-term predictors)", sep = ""), size = 0.8) +
    labs(title = "Long-term climate and static predictors",
      subtitle = paste(Focal_species, " (sites = ", nrow(df_pres), ", AUC-ROC = ", round(auc_roc_S_LT$aucs, digits = 2), ")", sep = "")) +
    # annotate("text", x = 120, y = -13, label = paste(Focal_species, " (sites = ", nrow(df_pres), ", AUC-ROC = ", round(auc_roc_S_LT$aucs, digits = 2), ")", sep = "")) +
    theme(axis.title = element_blank(),
      plot.margin = unit(c(0, 0, 5.5, 0), "pt")) # remove axis title (i.e., long/lat)
  
  # save for shiny app purposes
  ggsave(
    filename = paste0(
      "D:/PhD related/2nd chapter/final analysis 21Sep2026/Shiny/maps/",
      make.names(Focal_species),
      "_LTC.png"
    ),
    plot = predWA_plot,
    width = 6,
    height = 6,
    dpi = 300
  )
  
  
  # predWA_plot
  
  # ggsave(paste("D:/PhD related/2nd chapter/final analysis 21Sep2026/plots/species distribution models/", Focal_species, ".png", sep = ""),
  #        plot = predWA_plot,
  #        height = 5, width = 5,
  #        dpi = 300)
  # knitr::plot_crop(paste("D:/PhD related/2nd chapter/final analysis 21Sep2026/plots/species distribution models/", Focal_species, ".png", sep = ""))

  # full model year 1965 and static predictors
  ###
  ###
  ###
  full_1965 <- bayesglm(PA ~ 
                          # bio1 + I(bio1^2) +
                          # bio2 + I(bio2^2) +
                          bio8 + I(bio8^2) +
                          bio12 + I(bio12^2) + bio15 + I(bio15^2)
                        +
                          bio8_st + I(bio8_st^2) +
                          bio12_st + I(bio12_st^2) +
                          bio15_st + I(bio15_st^2) +
                          soilClay + I(soilClay^2) +
                          topoWetness + I(topoWetness^2) +
                          demH + I(demH^2)
                        , 
                        family = binomial(link = "logit"), 
                        maxit = 200,data = df)  
  
  newData_full_1965 <- df[, c("bio8", "bio12", "bio15",
                              "bio8_st", "bio12_st", "bio15_st",
                              "soilClay", "topoWetness", "demH")]
  newData_full_1965$observed <- df$PA
  
  prediction_full_1965 <- predict(full_1965, newData_full_1965, type = "response")
  # library(precrec)
  precrec_obj_full_1965 <- evalmod(scores = prediction_full_1965, labels = newData_full_1965[,"observed"])
  # print(precrec_obj_full)
  
  # get auc values from the above object (i.e., precrec_obj)
  aucs_full_1965 <- auc(precrec_obj_full_1965)
  aucs_full_1965
  auc_roc_full_1965 <- subset(aucs_full_1965, curvetypes == "ROC")
  # auc_roc_full
  auc_roc_full_1965$aucs
  
  auc_pr_full_1965 <- subset(aucs_full_1965, curvetypes == "PRC")
  # auc_pr_full
  auc_pr_full_1965$aucs
  
  
  pred_full_1965 <- raster::predict(predRas1965, full_1965, type = "response")
  predWA_full_1965 <- crop (pred_full_1965, wa_sp)
  
  # plot(predWA, main = paste("SDM for ", Focal_species))
  # points(df[df[,15]==1, 2:3], col = "red", pch=20, cex=0.8)
  # points(df[df[,13]==0, 2:3], col = "black", pch=3, cex=0.8)
  
  # convert 0 to NA for plotting purpose
  # values(predWA)[values(predWA) <= 0] <- NA
  
  
  # for plotting in ggplot
  predPlotWA_full_1965 <- as(predWA_full_1965, "SpatialPixelsDataFrame")
  predPlotWA_df_full_1965 <- as.data.frame(predPlotWA_full_1965)
  colnames(predPlotWA_df_full_1965) <- c("Value", "x", "y")
  
  
  
  predWA_full_1965 <- ggplot() +
    geom_tile(data = predPlotWA_df_full_1965, aes(x=x, y=y, fill = Value)) + 
    geom_polygon(data = wa_sp, aes(x=long, y=lat, group = group), alpha = 0, color = "grey") +
    # geom_polygon(data = ibraWA, aes(x=long, y=lat, group = group), alpha = 0, color = "grey") +
    # add presence points for the species
    geom_point(data = df[df[,18]==1, 2:3], aes(x=Long, y=Lat), color="magenta", pch = 18, size = 3, alpha = 0.8)+
    coord_equal() +
    theme_bw() +
    # adding texts for bioregions at selected locations
    # annotate("text", x = c(127, 120.077, 114.566, 114.566, 117.191670, 117, 123.2, 125.9, 128.715510, 122.533164, 126.536081),
    #          y = c(-32.5, -34.502, -31.476, -25.222, -22.413562, -24.4, -18.4, -17.2, -15.763438, -32.810679, -14.901289),
    #          label = c("Ha", "EsPl", "SwCoPl", "Ca", "Pi", "Ga", "Da", "CeKi", "ViBo", "Ma", "NoKi"),
    #          colour = "black", alpha = 0.5) +
    scale_fill_viridis_c(limits = c(0.01, 1), option = "D", 
                         direction = -1, 
                         breaks = c(0.01, 0.4, 0.7, 1)) + # from library (viridis)
    # scale_fill_gradientn(colors=rev(c("firebrick2", "magenta", "deeppink2", "mediumorchid1", "pink", "salmon2", "darkorange3", "orange", "yellow", "green", "lightgreen", "blue", "azure3", "lightgrey")))+
    labs(title = "Short-term climate (wet year 1965) and static predictors",
      subtitle = paste(Focal_species, " (sites = ", nrow(df_pres), ", AUC-ROC = ", round(auc_roc_full_1965$auc, digits = 2), ")", sep = "")) +
    # annotate("text", x = 120, y = -13, label = cat("Short-term Climate \n(wet year 1965) \nand Static Predictors")) +
    theme(axis.title = element_blank(),
          plot.margin = unit(c(0, 0, 5.5, 0), "pt")) # remove axis title (i.e., long/lat)
  
  
  # save for shiny app purposes
  ggsave(
    filename = paste0(
      "D:/PhD related/2nd chapter/final analysis 21Sep2026/Shiny/maps/",
      make.names(Focal_species),
      "_LTC_ONLY.png"
    ),
    plot = predWA_LT,
    width = 6,
    height = 6,
    dpi = 300
  )
  
  
  
  ## full model year 1982 and static predictors
  ###
  ###
  full_1982 <- bayesglm(PA ~ 
                          # bio1 + I(bio1^2) +
                          # bio2 + I(bio2^2) +
                          bio8 + I(bio8^2) +
                          bio12 + I(bio12^2) + bio15 + I(bio15^2)
                        +
                          bio8_st2 + I(bio8_st2^2) +
                          bio12_st2 + I(bio12_st2^2) +
                          bio15_st2 + I(bio15_st2^2) +
                          soilClay + I(soilClay^2) +
                          topoWetness + I(topoWetness^2) +
                          demH + I(demH^2)
                        , 
                        family = binomial(link = "logit"), 
                        maxit = 200,data = df)  
  
  newData_full_1982 <- df[, c("bio8", "bio12", "bio15",
                              "bio8_st2", "bio12_st2", "bio15_st2",
                              "soilClay", "topoWetness", "demH")]
  newData_full_1982$observed <- df$PA
  
  prediction_full_1982 <- predict(full_1982, newData_full_1982, type = "response")
  # library(precrec)
  precrec_obj_full_1982 <- evalmod(scores = prediction_full_1982, labels = newData_full_1982[,"observed"])
  # print(precrec_obj_full)
  
  # get auc values from the above object (i.e., precrec_obj)
  aucs_full_1982 <- auc(precrec_obj_full_1982)
  aucs_full_1982
  auc_roc_full_1982 <- subset(aucs_full_1982, curvetypes == "ROC")
  # auc_roc_full
  auc_roc_full_1982$aucs
  
  auc_pr_full_1982 <- subset(aucs_full_1982, curvetypes == "PRC")
  # auc_pr_full
  auc_pr_full_1982$aucs
  
  
  pred_full_1982 <- raster::predict(predRas1982, full_1982, type = "response")
  predWA_full_1982 <- crop (pred_full_1982, wa_sp)
  
  # plot(predWA, main = paste("SDM for ", Focal_species))
  # points(df[df[,15]==1, 2:3], col = "red", pch=20, cex=0.8)
  # points(df[df[,13]==0, 2:3], col = "black", pch=3, cex=0.8)
  
  # convert 0 to NA for plotting purpose
  # values(predWA)[values(predWA) <= 0] <- NA
  
  # for plotting in ggplot
  predPlotWA_full_1982 <- as(predWA_full_1982, "SpatialPixelsDataFrame")
  predPlotWA_df_full_1982 <- as.data.frame(predPlotWA_full_1982)
  colnames(predPlotWA_df_full_1982) <- c("Value", "x", "y")
  
  
  
  predWA_full_1982 <- ggplot() +
    geom_tile(data = predPlotWA_df_full_1982, aes(x=x, y=y, fill = Value)) + 
    geom_polygon(data = wa_sp, aes(x=long, y=lat, group = group), alpha = 0, color = "grey") +
    # geom_polygon(data = ibraWA, aes(x=long, y=lat, group = group), alpha = 0, color = "grey") +
    # add presence points for the species
    geom_point(data = df[df[,18]==1, 2:3], aes(x=Long, y=Lat), color="magenta", pch = 18, size = 3, alpha = 0.8)+
    coord_equal() +
    theme_bw() +
    # adding texts for bioregions at selected locations
    # annotate("text", x = c(127, 120.077, 114.566, 114.566, 117.191670, 117, 123.2, 125.9, 128.715510, 122.533164, 126.536081),
    #          y = c(-32.5, -34.502, -31.476, -25.222, -22.413562, -24.4, -18.4, -17.2, -15.763438, -32.810679, -14.901289),
    #          label = c("Ha", "EsPl", "SwCoPl", "Ca", "Pi", "Ga", "Da", "CeKi", "ViBo", "Ma", "NoKi"),
    #          colour = "black", alpha = 0.5) +
    scale_fill_viridis_c(limits = c(0.01, 1), option = "D", 
                         direction = -1, 
                         breaks = c(0.01, 0.4, 0.7, 1)) + # from library (viridis)
    # scale_fill_gradientn(colors=rev(c("firebrick2", "magenta", "deeppink2", "mediumorchid1", "pink", "salmon2", "darkorange3", "orange", "yellow", "green", "lightgreen", "blue", "azure3", "lightgrey")))+
    labs(title = "Short-term climate (dry year 1982) and static predictors",
      subtitle = paste(Focal_species, " (sites = ", nrow(df_pres), ", AUC-ROC = ", round(auc_roc_full_1982$aucs, digits = 2), ")", sep = "")) +
    # annotate("text", x = 120, y = -13, label = cat("Short-term Climate \n(dry year 1965) \nand Static Predictors")) +
    theme(axis.title = element_blank(),
          plot.margin = unit(c(0, 0, 5.5, 0), "pt")) # remove axis title (i.e., long/lat)
  
  
  # save for shiny app purposes
  ggsave(
    filename = paste0(
      "D:/PhD related/2nd chapter/final analysis 21Sep2026/Shiny/maps/",
      make.names(Focal_species),
      "_1965.png"
    ),
    plot = predWA_full_1965,
    width = 6,
    height = 6,
    dpi = 300
  )
  
  
  ### Long-term climate only no static predictors
  ###
  ###
  LT <- bayesglm(PA ~ 
                   # bio1 + I(bio1^2) +
                   # bio2 + I(bio2^2) +
                   bio8 + I(bio8^2) +
                   bio12 + I(bio12^2) + bio15 + I(bio15^2)
                 # +
                 # bio8_st + I(bio8_st^2) +
                 # bio12_st + I(bio12_st^2) +
                 # bio15_st + I(bio15_st^2) +
                 # soilClay + I(soilClay^2) + 
                 # topoWetness + I(topoWetness^2) + 
                 # demH + I(demH^2)
                 , 
                 family = binomial(link = "logit"), 
                 maxit = 200,data = df)  
  
  newData_LT <- df[, c("bio8", "bio12", "bio15")]
  newData_LT$observed <- df$PA
  
  prediction_LT <- predict(LT, newData_LT, type = "response")
  # library(precrec)
  precrec_obj_LT <- evalmod(scores = prediction_LT, labels = newData_LT[,"observed"])
  # print(precrec_obj_full)
  
  # get auc values from the above object (i.e., precrec_obj)
  aucs_LT <- auc(precrec_obj_LT)
  aucs_LT
  auc_roc_LT <- subset(aucs_LT, curvetypes == "ROC")
  # auc_roc_full
  auc_roc_LT$aucs
  
  auc_pr_LT <- subset(aucs_LT, curvetypes == "PRC")
  # auc_pr_full
  auc_pr_LT$aucs
  
  
  pred_LT <- raster::predict(predRasLTCS, LT, type = "response")
  predWA_LT <- crop (pred_LT, wa_sp)
  
  # plot(predWA, main = paste("SDM for ", Focal_species))
  # points(df[df[,15]==1, 2:3], col = "red", pch=20, cex=0.8)
  # points(df[df[,13]==0, 2:3], col = "black", pch=3, cex=0.8)
  
  # convert 0 to NA for plotting purpose
  # values(predWA)[values(predWA) <= 0] <- NA
  
  
  # for plotting in ggplot
  predPlotWA_LT <- as(predWA_LT, "SpatialPixelsDataFrame")
  predPlotWA_df_LT <- as.data.frame(predPlotWA_LT)
  colnames(predPlotWA_df_LT) <- c("Value", "x", "y")
  
  
  
  predWA_LT <- ggplot() +
    geom_tile(data = predPlotWA_df_LT, aes(x=x, y=y, fill = Value)) + 
    geom_polygon(data = wa_sp, aes(x=long, y=lat, group = group), alpha = 0, color = "grey") +
    # geom_polygon(data = ibraWA, aes(x=long, y=lat, group = group), alpha = 0, color = "grey") +
    # add presence points for the species
    geom_point(data = df[df[,18]==1, 2:3], aes(x=Long, y=Lat), color="magenta", pch = 18, size = 3, alpha = 0.6)+
    coord_equal() +
    theme_bw() +
    # adding texts for bioregions at selected locations
    # annotate("text", x = c(127, 120.077, 114.566, 114.566, 117.191670, 117, 123.2, 125.9, 128.715510, 122.533164, 126.536081),
    #          y = c(-32.5, -34.502, -31.476, -25.222, -22.413562, -24.4, -18.4, -17.2, -15.763438, -32.810679, -14.901289),
    #          label = c("Ha", "EsPl", "SwCoPl", "Ca", "Pi", "Ga", "Da", "CeKi", "ViBo", "Ma", "NoKi"),
    #          colour = "black", alpha = 0.5) +
    scale_fill_viridis_c(limits = c(0.01, 1), option = "D", 
                         direction = -1, 
                         breaks = c(0.01, 0.4, 0.7, 1)) + # from library (viridis)
    # scale_fill_gradientn(colors=rev(c("firebrick2", "magenta", "deeppink2", "mediumorchid1", "pink", "salmon2", "darkorange3", "orange", "yellow", "green", "lightgreen", "blue", "azure3", "lightgrey")))+
    labs(title = "Long-term climate only (no Static predictors)",
      subtitle = paste(Focal_species, " (sites = ", nrow(df_pres), ", AUC-ROC = ", round(auc_roc_LT$aucs, digits = 2), ")", sep = "")) +
    # annotate("text", x = 120, y = -13, label = cat("Short-term Climate only \nno Static Predictors")) +
    theme(axis.title = element_blank(),
          plot.margin = unit(c(0, 0, 5.5, 0), "pt")) # remove axis title (i.e., long/lat)
  
  # save for shiny app purposes
  ggsave(
    filename = paste0(
      "D:/PhD related/2nd chapter/final analysis 21Sep2026/Shiny/maps/",
      make.names(Focal_species),
      "_1982.png"
    ),
    plot = predWA_full_1982,
    width = 6,
    height = 6,
    dpi = 300
  )
  
  
  # combine all plots together for shiny app
  # model_results <- list(
  #   
  #   species = Focal_species,
  #   
  #   occurrences = nrow(df_pres),
  #   
  #   auc_roc = auc_roc_S_LT$aucs,
  #   
  #   auc_pr = auc_pr_S_LT$aucs,
  #   
  #   predWA_plot = predWA_plot, # long-term climate and static predictors
  #   
  #   predWA_LT_plot = predWA_LT, # long-term climate only
  #   
  #   predWA_full_1982_plot = predWA_full_1982, # short-term climate and static, dry year 1982
  #   
  #   predWA_full_1965_plot = predWA_full_1965 # short-term climate and static, wet year 1965
  #   
  # )
  # 
  
  # save only images for github uploads
  model_results <- list(
    
    species = Focal_species,
    
    occurrences = nrow(df_pres),
    
    auc_roc = auc_roc_S_LT$aucs,
    
    auc_pr = auc_pr_S_LT$aucs,
    
    LTC_map = paste0(
      make.names(Focal_species),
      "_LTC.png"
    ),
    
    LTC_only_map = paste0(
      make.names(Focal_species),
      "_LTC_ONLY.png"
    ),
    
    Wet1965_map = paste0(
      make.names(Focal_species),
      "_1965.png"
    ),
    
    Dry1982_map = paste0(
      make.names(Focal_species),
      "_1982.png"
    )
    
  )
  
  saveRDS(
    model_results,
    file = paste0(
      #"D:/PhD related/2nd chapter/final analysis 21Sep2026/Shiny/models/",
      "D:/PhD related/2nd chapter/final analysis 21Sep2026/Shiny/maps/",
      make.names(Focal_species),
      ".rds"
    )
  )
  
}
