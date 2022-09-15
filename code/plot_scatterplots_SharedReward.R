# set working directory
#library(here)

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
library("dplyr")

setwd("../derivatives/")
maindir <- getwd()
datadir <- file.path("../derivatives/")

# import data
#here()
sharedreward <- read_excel("ppi_wholebrain_scatterplot.xls")


# Shared Reward Model 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Friend v Computer, zstat 8 cluster (SU-neg) Ventral Striatum ppi seed
model1 <- lm(`ppi_c9_F-C_sub-neg_type-ppi_seed-VS_thr5_cope-09` ~ tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
summary(model1)
crPlots(model1, smooth=FALSE)

# Friend + Stranger v Computer, Cluster zstat 8 cluster 1 (SU-neg) Ventral Striatum ppi seed
model2 <- lm(`ppi_C10_FS-C_z8_sub-neg_cluster1_type-ppi_seed-VS_thr5_cope-10` ~ tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
model2
crPlots(model2, smooth=FALSE)

# Friend + Stranger v Computer, zstat 8 cluster 2 (SU-neg) Ventral Striatum ppi seed
model3 <- lm(`ppi_C10_FS-C_z8_sub-neg_cluster2_type-ppi_seed-VS_thr5_cope-10` ~
               tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
model3
crPlots(model3, smooth=FALSE)

# Reward v Punishment with Friend v Computer, zstat 1 cluster (main effect) Ventral Striatum ppi seed
model4 <- lm(`ppi_C13_rew-pun_F-C_z1_main-effect_type-ppi_seed-VS_thr5_cope-13` ~
               tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
model4
crPlots(model4, smooth=FALSE)

# Reward v Punishment with Friend v Computer, zstat 12 cluster 1 (SUxRS_square-neg) Ventral Striatum ppi seed
model5 <- lm(`ppi_C13_rew-pun_F-C_z12_su-rs2-neg_cluster1_type-ppi_seed-VS_thr5_cope-13` ~
               tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
model5
crPlots(model5, smooth=FALSE)

# Reward with Friend v Computer, zstat 1 cluster 1 (main effect) Ventral Striatum ppi seed
model6 <- lm(`ppi_C16_rew_F-C_z1_main-effect_cluster1_type-ppi_seed-VS_thr5_cope-16` ~
               tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
model6
crPlots(model6, smooth=FALSE)

# Reward with Friend v Computer, zstat 1 cluster 2 (main effect) Ventral Striatum ppi seed
model7 <- lm(`ppi_C16_rew_F-C_z1_main-effect_cluster2_type-ppi_seed-VS_thr5_cope-16` ~
               tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
model7
crPlots(model7, smooth=FALSE)

# Reward with Friend v Stranger + Computer, zstat 1 cluster (main effect) Ventral Striatum ppi seed
model8 <- lm(`ppi_C21_rew_F-SC_z1_main-effect_type-ppi_seed-VS_thr5_cope-21` ~
               tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
model8
crPlots(model8, smooth=FALSE)

# Reward with Friend v Stranger + Computer, zstat 12 cluster (SUxRS_square-neg) Ventral Striatum ppi seed
model9 <- lm(`ppi_C23_rew-pun_F-SC_z12_su-rs2-neg_cluster1_type-ppi_seed-VS_thr5_cope-23` ~
               tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
model9
crPlots(model9, smooth=FALSE)

# Reward with Friend v Computer, zstat 2 cluster 1 (substance use) activation model exploratory result
model10 <- lm(`act_C16_rew_F-C_z2_sub_cluster1_type-act_cope-16` ~
               tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
model10
crPlots(model10, smooth=FALSE)

# Reward with Friend v Computer, zstat 2 cluster 1 (substance use) activation model exploratory result
model11 <- lm(`act_C16_rew_F-C_z2_sub_cluster2_type-act_cope-16` ~
                tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
model11
crPlots(model11, smooth=FALSE)

# Reward with Friend v Computer, zstat 2 cluster 1 (substance use) activation model exploratory result
model12 <- lm(`act_C16_rew_F-C_z2_sub_cluster3_type-act_cope-16` ~
                tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
model12
crPlots(model12, smooth=FALSE)


# Reward with Friend v Stranger, zstat 9 cluster 1 (reward sensitivity) activation model exploratory result
model13 <- lm(`act_C13_rew-pun_F-C_z9_rs-neg_cluster1_type-act_cope-13` ~
                tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
model13
crPlots(model13, smooth=FALSE)

# Reward with Friend v Stranger, zstat 9 cluster 2 (reward sensitivity) activation model exploratory result
model14 <- lm(`act_C13_rew-pun_F-C_z9_rs-neg_cluster2_type-act_cope-13` ~
                tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
model14
crPlots(model14, smooth=FALSE)

# Reward with Friend v Stranger, zstat 10 (reward sensitivity squared) activation model exploratory result
model15 <- lm(`act_C13_rew-pun_F-C_z10_rs2-neg_type-act_cope-13` ~
                tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
model15
crPlots(model15, smooth=FALSE)

# Reward with Friend v Stranger, zstat 2 (substance use) activation model exploratory result
model16 <- lm(`act_C14_rew_F-S_z2_sub_type-act_cope-14` ~
                tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
model16
crPlots(model16, smooth=FALSE)



sharedreward$quant <- cut(sharedreward$RS_square,
                          breaks = 2,
                          labels = c("1","2"))
sharedreward$quant

ggplot(data = sharedreward, aes(x=SU,y=`ppi_C13_rew-pun_F-C_z12_su-rs2-neg_cluster1_type-ppi_seed-VS_thr5_cope-13`))+
  geom_smooth(aes(group = quant, color = quant), method = "lm", se = F)+geom_point()

ggplot(data = sharedreward, aes(x=SU,y=`ppi_C23_rew-pun_F-SC_z12_su-rs2-neg_cluster1_type-ppi_seed-VS_thr5_cope-23`))+
  geom_smooth(aes(group = quant, color = quant), method = "lm", se = F)+geom_point()
