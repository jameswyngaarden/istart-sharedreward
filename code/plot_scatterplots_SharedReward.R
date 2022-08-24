# set working directory
setwd("~/Desktop")
maindir <- getwd()
datadir <- file.path("~/Desktop/")

# load packages
library("readxl")
library("ggplot2")
library("ggpubr")
library("reshape2")
library("emmeans")
library("hrbrthemes")
library("umx")
library("interactions")
library("car")

# import data
sharedreward_model2 <- read_excel("~/Documents/Documents_Air/Github/istart-socdoors/derivatives/ppi_wholebrain_scatterplots.xls")


# Shared Reward Model 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Friend v Computer, zstat 8 cluster (SU-neg) Ventral Striatum ppi seed
model1 <- lm(ppi_c9_F-C_sub-neg_type-ppi_seed-VS_thr5_cope-09 ~
               tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward_model2)
model1
crPlots(model1, smooth=FALSE)

# Friend + Stranger v Computer, Cluster zstat 8 cluster 1 (SU-neg) Ventral Striatum ppi seed
model2 <- lm(ppi_C10_FS-C_z8_sub-neg_cluster1_type-ppi_seed-VS_thr5_cope-10 ~
               tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward_model2)
model2
crPlots(model2, smooth=FALSE)

# Friend + Stranger v Computer, zstat 8 cluster 2 (SU-neg) Ventral Striatum ppi seed
model3 <- lm(ppi_C10_FS-C_z8_sub-neg_cluster2_type-ppi_seed-VS_thr5_cope-10 ~
               tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward_model2)
model3
crPlots(model3, smooth=FALSE)

# Reward v Punishment with Friend v Computer, zstat 1 cluster (main effect) Ventral Striatum ppi seed
model4 <- lm(ppi_C13_rew-pun_F-C_z1_main-effect_type-ppi_seed-VS_thr5_cope-13 ~
               tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward_model2)
model4
crPlots(model4, smooth=FALSE)

# Reward v Punishment with Friend v Computer, zstat 12 cluster 1 (SUxRS_square-neg) Ventral Striatum ppi seed
model5 <- lm(ppi_C13_rew-pun_F-C_z12_su-rs2-neg_cluster1_type-ppi_seed-VS_thr5_cope-13 ~
               tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward_model2)
model5
crPlots(model5, smooth=FALSE)

# Reward with Friend v Computer, zstat 1 cluster 1 (main effect) Ventral Striatum ppi seed
model6 <- lm(ppi_C16_rew_F-C_z1_main-effect_cluster1_type-ppi_seed-VS_thr5_cope-16 ~
               tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward_model2)
model6
crPlots(model6, smooth=FALSE)

# Reward with Friend v Computer, zstat 1 cluster 2 (main effect) Ventral Striatum ppi seed
model7 <- lm(ppi_C16_rew_F-C_z1_main-effect_cluster2_type-ppi_seed-VS_thr5_cope-16 ~
               tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward_model2)
model7
crPlots(model7, smooth=FALSE)

# Reward with Friend v Stranger + Computer, zstat 1 cluster (main effect) Ventral Striatum ppi seed
model8 <- lm(ppi_C21_rew_F-SC_z1_main-effect_type-ppi_seed-VS_thr5_cope-21 ~
               tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward_model2)
model8
crPlots(model8, smooth=FALSE)

# Reward with Friend v Stranger + Computer, zstat 12 cluster (SUxRS_square-neg) Ventral Striatum ppi seed
model9 <- lm(ppi_C23_rew-pun_F-SC_z12_su-rs2-neg_cluster1_type-ppi_seed-VS_thr5_cope-23 ~
               tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward_model2)
model9
crPlots(model9, smooth=FALSE)