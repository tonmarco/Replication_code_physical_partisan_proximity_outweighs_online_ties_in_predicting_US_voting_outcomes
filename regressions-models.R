library(sf)
library(tidyverse)
library(spatialreg)
library(stargazer)
library(spdep)
library(jtools)
library(ggplot2)



df <- read.csv('data/county_data.csv')



 df <- df %>%
   mutate(across(c(perc_nv_rep, perc_nv_dem, proximity_rep_coloc, proximity_rep_sci, proximity_rep_res, proximity_dem_coloc, proximity_dem_sci, proximity_dem_res, proximity_rep_comm, proximity_dem_comm), ~. * 100))
 
 
 
 ##########################
 ### SPATIAL REGRESSION ###
 ##########################
 
 
 centroids_us <- st_centroid(df)
 
 w_5_us <- nb2listw(knn2nb(knearneigh(centroids_us, k = 5)))
 w_7_us <- nb2listw(knn2nb(knearneigh(centroids_us, k = 7)))
 w_10_us <- nb2listw(knn2nb(knearneigh(centroids_us, k = 10)))
 
 
 #KNN=5
 m_rep_coloc_5 <- lagsarlm(perc_nv_rep ~ proximity_rep_coloc, data = df, listw = w_5_us)
 m_rep_sci_5 <- lagsarlm(perc_nv_rep ~ proximity_rep_sci, data = df, listw = w_5_us)
 m_rep_res_5 <- lagsarlm(perc_nv_rep ~ proximity_rep_res, data = df, listw = w_5_us)
 
 export_summs(m_rep_coloc_5, m_rep_sci_5, m_rep_res_5, scale = TRUE, digits=3)
 
 m_dem_coloc_5 <- lagsarlm(perc_nv_dem ~ proximity_dem_coloc , data = df, listw = w_5_us)
 m_dem_sci_5 <- lagsarlm(perc_nv_dem ~ proximity_dem_sci, data = df, listw = w_5_us)
 m_dem_res_5 <- lagsarlm(perc_nv_dem ~ proximity_dem_res, data = df, listw = w_5_us)
 
 export_summs(m_dem_coloc_5, m_dem_sci_5, m_dem_res_5, scale = TRUE, digits=3)
 
 #KNN=7
 m_rep_coloc_7 <- lagsarlm(perc_nv_rep ~ proximity_rep_coloc, data = df, listw = w_7_us)
 m_rep_sci_7 <- lagsarlm(perc_nv_rep ~ proximity_rep_sci, data = df, listw = w_7_us)
 m_rep_res_7 <- lagsarlm(perc_nv_rep ~ proximity_rep_res, data = df, listw = w_7_us)
 
 export_summs(m_rep_coloc_7, m_rep_sci_7, m_rep_res_7, scale = TRUE, digits=3)
 
 m_dem_coloc_7 <- lagsarlm(perc_nv_dem ~ proximity_dem_coloc , data = df, listw = w_7_us)
 m_dem_sci_7 <- lagsarlm(perc_nv_dem ~ proximity_dem_sci, data = df, listw = w_7_us)
 m_dem_res_7 <- lagsarlm(perc_nv_dem ~ proximity_dem_res, data = df, listw = w_7_us)
 
 export_summs(m_dem_coloc_7, m_dem_sci_7, m_dem_res_7, scale = TRUE, digits=3)
 
 #KNN=10
 m_rep_coloc_10 <- lagsarlm(perc_nv_rep ~ proximity_rep_coloc, data = df, listw = w_10_us)
 m_rep_sci_10 <- lagsarlm(perc_nv_rep ~ proximity_rep_sci, data = df, listw = w_10_us)
 m_rep_res_10 <- lagsarlm(perc_nv_rep ~ proximity_rep_res, data = df, listw = w_10_us)
 
 export_summs(m_rep_coloc_10, m_rep_sci_10, m_rep_res_10, scale = TRUE, digits=3)
 
 m_dem_coloc_10 <- lagsarlm(perc_nv_dem ~ proximity_dem_coloc , data = df, listw = w_10_us)
 m_dem_sci_10 <- lagsarlm(perc_nv_dem ~ proximity_dem_sci, data = df, listw = w_10_us)
 m_dem_res_10 <- lagsarlm(perc_nv_dem ~ proximity_dem_res, data = df, listw = w_10_us)
 
 export_summs(m_dem_coloc_10, m_dem_sci_10, m_dem_res_10, scale = TRUE, digits=3)
 
 

 
############################
### OLS METRO - NONMETRO ###
############################

metro <- df[df$rucc_grouped == 1, ]
nonmetro <- df[df$rucc_grouped %in% c(2, 3, 4), ]


# METRO

metro_rep_coloc <- lm(perc_nv_rep ~ proximity_rep_coloc, data = metro)
metro_rep_sci <- lm(perc_nv_rep ~ proximity_rep_sci, data = metro)
metro_rep_res <- lm(perc_nv_rep ~ proximity_rep_res, data = metro)

summary(metro_rep_coloc)
AIC(metro_rep_coloc)
logLik(metro_rep_coloc)

summary(metro_rep_sci)
AIC(metro_rep_sci)
logLik(metro_rep_sci)

summary(metro_rep_res)
AIC(metro_rep_res)
logLik(metro_rep_res)


metro_dem_coloc <- lm(perc_nv_dem ~ proximity_dem_coloc, data = metro)
metro_dem_sci <- lm(perc_nv_dem ~ proximity_dem_sci, data = metro)
metro_dem_res <- lm(perc_nv_dem ~ proximity_dem_res, data = metro)

summary(metro_dem_coloc)
AIC(metro_dem_coloc)
logLik(metro_dem_coloc)

summary(metro_dem_sci)
AIC(metro_dem_sci)
logLik(metro_dem_sci)

summary(metro_dem_res)
AIC(metro_dem_res)
logLik(metro_dem_res)


# NON-METRO

nonmetro_rep_coloc <- lm(perc_nv_rep ~ proximity_rep_coloc, data = nonmetro)
nonmetro_rep_sci <- lm(perc_nv_rep ~ proximity_rep_sci, data = nonmetro)
nonmetro_rep_res <- lm(perc_nv_rep ~ proximity_rep_res, data = nonmetro)

summary(nonmetro_rep_coloc)
AIC(nonmetro_rep_coloc)
logLik(nonmetro_rep_coloc)

summary(nonmetro_rep_sci)
AIC(nonmetro_rep_sci)
logLik(nonmetro_rep_sci)

summary(nonmetro_rep_res)
AIC(nonmetro_rep_res)
logLik(nonmetro_rep_res)



nonmetro_dem_coloc <- lm(perc_nv_dem ~ proximity_dem_coloc, data = nonmetro)
nonmetro_dem_sci <- lm(perc_nv_dem ~ proximity_dem_sci, data = nonmetro)
nonmetro_dem_res <- lm(perc_nv_dem ~ proximity_dem_res, data = nonmetro)

summary(nonmetro_dem_coloc)
AIC(nonmetro_dem_coloc)
logLik(nonmetro_dem_coloc)

summary(nonmetro_dem_sci)
AIC(nonmetro_dem_sci)
logLik(nonmetro_dem_sci)

summary(nonmetro_dem_res)
AIC(nonmetro_dem_res)
logLik(nonmetro_dem_res)




######################
### SWING COUNTIES ###
######################

noswing <- df[df$n_swing == 0, ]
swing <- df[df$n_swing > 0, ]



noswing_rep_coloc <- lm(perc_nv_rep ~ proximity_rep_coloc, data = noswing)
noswing_rep_sci <- lm(perc_nv_rep ~ proximity_rep_sci, data = noswing)
noswing_rep_res <- lm(perc_nv_rep ~ proximity_rep_res, data = noswing)

summary(noswing_rep_coloc)
AIC(noswing_rep_coloc)
logLik(noswing_rep_coloc)

summary(noswing_rep_sci)
AIC(noswing_rep_sci)
logLik(noswing_rep_sci)

summary(noswing_rep_res)
AIC(noswing_rep_res)
logLik(noswing_rep_res)


noswing_dem_coloc <- lm(perc_nv_dem ~ proximity_dem_coloc, data = noswing)
noswing_dem_sci <- lm(perc_nv_dem ~ proximity_dem_sci, data = noswing)
noswing_dem_res <- lm(perc_nv_dem ~ proximity_dem_res, data = noswing)

summary(noswing_dem_coloc)
AIC(noswing_dem_coloc)
logLik(noswing_dem_coloc)

summary(noswing_dem_sci)
AIC(noswing_dem_sci)
logLik(noswing_dem_sci)

summary(noswing_dem_res)
AIC(noswing_dem_res)
logLik(noswing_dem_res)



swing_rep_coloc <- lm(perc_nv_rep ~ proximity_rep_coloc, data = swing)
swing_rep_sci <- lm(perc_nv_rep ~ proximity_rep_sci, data = swing)
swing_rep_res <- lm(perc_nv_rep ~ proximity_rep_res, data = swing)

summary(swing_rep_coloc)
AIC(swing_rep_coloc)
logLik(swing_rep_coloc)

summary(swing_rep_sci)
AIC(swing_rep_sci)
logLik(swing_rep_sci)

summary(swing_rep_res)
AIC(swing_rep_res)
logLik(swing_rep_res)


swing_dem_coloc <- lm(perc_nv_dem ~ proximity_dem_coloc, data = swing)
swing_dem_sci <- lm(perc_nv_dem ~ proximity_dem_sci, data = swing)
swing_dem_res <- lm(perc_nv_dem ~ proximity_dem_res, data = swing)

summary(swing_dem_coloc)
AIC(swing_dem_coloc)
logLik(swing_dem_coloc)

summary(swing_dem_sci)
AIC(swing_dem_sci)
logLik(swing_dem_sci)

summary(swing_dem_res)
AIC(swing_dem_res)
logLik(swing_dem_res)





#################
### COMMUTING ###
#################


m_rep_comm_7 <- lagsarlm(perc_nv_rep ~ proximity_rep_comm, data = df, listw = w_7_us)
m_dem_comm_7 <- lagsarlm(perc_nv_dem ~ proximity_dem_comm , data = df, listw = w_7_us)

export_summs(m_rep_coloc_7, m_rep_comm_7, scale = TRUE, digits=3)
export_summs(m_dem_coloc_7, m_dem_comm_7, scale = TRUE, digits=3)


##############################
### ROBUSTNESS CHECK SWING ###
##############################


noswing_08 <- df[df$n_swing_0820 == 0, ]
swing_08 <- df[df$n_swing_0820 > 0, ]



noswing08_rep_coloc <- lm(perc_nv_rep ~ proximity_rep_coloc, data = noswing_08)
noswing08_rep_sci <- lm(perc_nv_rep ~ proximity_rep_sci, data = noswing_08)
noswing08_rep_res <- lm(perc_nv_rep ~ proximity_rep_res, data = noswing_08)

summary(noswing08_rep_coloc)
AIC(noswing08_rep_coloc)
AIC(noswing08_rep_sci)
AIC(noswing08_rep_res)

export_summs(noswing08_rep_coloc, noswing08_rep_sci, noswing08_rep_res, scale = TRUE, digits=4)

noswing08_dem_coloc <- lm(perc_nv_dem ~ proximity_dem_coloc, data = noswing_08)
noswing08_dem_sci <- lm(perc_nv_dem ~ proximity_dem_sci, data = noswing_08)
noswing08_dem_res <- lm(perc_nv_dem ~ proximity_dem_res, data = noswing_08)

AIC(noswing08_dem_coloc)
AIC(noswing08_dem_sci)
AIC(noswing08_dem_res)

export_summs(noswing08_dem_coloc, noswing08_dem_sci, noswing08_dem_res, scale = TRUE, digits=4)


swing08_rep_coloc <- lm(perc_nv_rep ~ proximity_rep_coloc, data = swing_08)
swing08_rep_sci <- lm(perc_nv_rep ~ proximity_rep_sci, data = swing_08)
swing08_rep_res <- lm(perc_nv_rep ~ proximity_rep_res, data = swing_08)

AIC(swing08_rep_coloc)
AIC(swing08_rep_sci)
AIC(swing08_rep_res)

export_summs(swing08_rep_coloc, swing08_rep_sci, swing08_rep_res, scale = TRUE, digits=4)

swing08_dem_coloc <- lm(perc_nv_dem ~ proximity_dem_coloc, data = swing_08)
swing08_dem_sci <- lm(perc_nv_dem ~ proximity_dem_sci, data = swing_08)
swing08_dem_res <- lm(perc_nv_dem ~ proximity_dem_res, data = swing_08)

AIC(swing08_dem_coloc)
AIC(swing08_dem_sci)
AIC(swing08_dem_res)

export_summs(swing08_dem_coloc, swing08_dem_sci, swing08_dem_res, scale = TRUE, digits=4)

