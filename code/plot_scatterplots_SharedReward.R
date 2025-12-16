# set working directory
library("here")

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
library("lme4")
library("lmerTest")
library("car")
library("emmeans")
library("MuMIn")

library ("tidyverse")
library("rstatix")
library("reshape")
library("datarium")

setwd("~/Documents/GitHub/istart-sharedreward/derivatives/")
maindir <- getwd()
datadir <- file.path("../derivatives/")

# import data
#here()
sharedreward <- read_excel("ppi_wholebrain_scatterplot.xls")
behavioral <- read_excel("ISTART-ALL-Combined-042122.xlsx")
postscan_ratings <- read.csv("df_psr.csv")
df_TPJ <- read_excel("df_TPJ.xlsx")
df_VS_ROI <- read_excel("VS_ROI_activation.xlsx")
df_VS_TPJ_ROI <- read_excel("VS-TPJ_ROI_connectivity.xlsx")
total <- inner_join(sharedreward, df_TPJ, by = "sub")
#srpr <- read.csv("../../istart/Shared_Reward/Behavioral_Analysis/SharedRewardPeerRatingsLongform.csv")


#Behavioral ratings and personality factors plot
Win_F_C <- lm(`Win_F_C` ~ RS + RS_square + SU + SUxRS + SUxRS_sq, data=postscan_ratings)
Behavior_RS_r <- cor(postscan_ratings$RS, postscan_ratings$`Win_F_C`, method = 'pearson')
print(Behavior_RS_r)
summary(Win_F_C)
scatter <- ggplot(data = postscan_ratings, aes(x=RS,
                                    y=`Win_F_C`))+
  geom_smooth(method=lm, formula = y ~ poly(x,2), level = 0.99, 
              se=TRUE, fullrange=TRUE, color="black")+
  geom_point(shape=1,color="black")
scatter + scale_color_grey() + theme(panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank(), 
                                     panel.background = element_blank(), 
                                     axis.line =  element_line(colour="black"))
ggsave(
  "../derivatives/Figures/RS_behavioral.svg",
  plot = scatter, bg = "white")



# Shared Reward Model 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

VS_substance_F_C <- lm(`VS_seed_F_C` ~ tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
summary(VS_substance_F_C)
crModel <- crPlots(VS_substance_F_C,
                   smooth=FALSE,
                   pch=21, #shape of dot
                   col='black', #dot outline color
                   bg='blue', #unclear
                   col.lines='yellow', #trend line color
                   lwd=1,
                   grid=FALSE)

# IMPORTANT -- VS-FFA Connectivity: Reward with Friend v Computer, zstat 1 cluster 1 (main effect) Ventral Striatum ppi seed
model6 <- lm(`ppi_C16_rew_F-C_z1_main-effect_cluster1_type-ppi_seed-VS_thr5_cope-16` ~
               tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
summary(model6)

scatter <- ggplot(data = total, aes(x=RS,
                                    y=`ppi_C16_rew_F-C_z1_main-effect_cluster1_type-ppi_seed-VS_thr5_cope-16`))+
  geom_smooth(method=lm, level = 0.99, 
              se=TRUE, fullrange=TRUE, color="black")+
  geom_point(shape=1,color="black")
scatter + scale_color_grey() + theme(panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank(), 
                                     panel.background = element_blank(), 
                                     axis.line =  element_line(colour="black"))
#above plots RS result, below plots SU result
#scatter <- ggplot(data = total, aes(x=SU,
#                                    y=`ppi_C16_rew_F-C_z1_main-effect_cluster1_type-ppi_seed-VS_thr5_cope-16`))+
#  geom_smooth(method=lm, level = 0.99, 
#              se=FALSE, fullrange=TRUE, linetype="dashed",)+
#  geom_point(shape=1,color="black")
#scatter + scale_color_grey() + theme(panel.grid.major = element_blank(), 
#                                     panel.grid.minor = element_blank(), 
#                                     panel.background = element_blank(), 
#                                     axis.line =  element_line(colour="black"))

summary(model6)
      
      #checking if result exists in stranger v computer as well (it doesn't)
model6b <- lm(`ppi_C16_rew_F-C_z1_main-effect_cluster1_type-ppi_seed-VS_thr5_cope-15_rew_S-C` ~
               tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
model6b
summary(model6b)

# VS-left occipital (cluset 2)/FFA (cluster 1): Reward with Friend v Computer, zstat 1 cluster 2 (main effect) Ventral Striatum ppi seed
model7 <- lm(`ppi_C16_rew_F-C_z1_main-effect_cluster1_type-ppi_seed-VS_thr5_cope-16` ~
               tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
summary(model7)
crPlots(model7, smooth=FALSE)

# Calculate f squared for occipital
# Full model (your existing occipital model)
occipital_full <- lm(`ppi_C16_rew_F-C_z1_main-effect_cluster2_type-ppi_seed-VS_thr5_cope-16` ~ 
                       tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, 
                     data = sharedreward)

# Reduced model without reward sensitivity (RS)
occipital_reduced <- lm(`ppi_C16_rew_F-C_z1_main-effect_cluster2_type-ppi_seed-VS_thr5_cope-16` ~ 
                          tsnr + fd_mean + RS_square + SU + SUxRS + SUxRS_sq, 
                        data = sharedreward)

# Get R-squared values
R2_full <- summary(occipital_full)$r.squared
R2_reduced <- summary(occipital_reduced)$r.squared

# Calculate Cohen's f² for the RS effect
f_squared_RS_occipital <- (R2_full - R2_reduced) / (1 - R2_full)

cat("R-squared full model:", round(R2_full, 4), "\n")
cat("R-squared reduced model:", round(R2_reduced, 4), "\n")
cat("Cohen's f² for Reward Sensitivity (occipital):", round(f_squared_RS_occipital, 4), "\n")

# Interpretation
if(f_squared_RS_occipital < 0.02) {
  effect_size_interp <- "small"
} else if(f_squared_RS_occipital < 0.15) {
  effect_size_interp <- "medium"
} else {
  effect_size_interp <- "large"
}
cat("Effect size interpretation:", effect_size_interp, "\n")

# Calculate f squared for rFFA findings:
# Full model (your existing rFFA model)
rffa_full <- lm(`ppi_C16_rew_F-C_z1_main-effect_cluster1_type-ppi_seed-VS_thr5_cope-16` ~
                  tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, 
                data = sharedreward)

# === Cohen's f² for Reward Sensitivity (RS) effect ===
# Reduced model without RS
rffa_reduced_RS <- lm(`ppi_C16_rew_F-C_z1_main-effect_cluster1_type-ppi_seed-VS_thr5_cope-16` ~
                        tsnr + fd_mean + RS_square + SU + SUxRS + SUxRS_sq, 
                      data = sharedreward)

R2_full <- summary(rffa_full)$r.squared
R2_reduced_RS <- summary(rffa_reduced_RS)$r.squared

# Calculate Cohen's f² for RS effect
f_squared_RS_rffa <- (R2_full - R2_reduced_RS) / (1 - R2_full)

cat("=== REWARD SENSITIVITY EFFECT ON rFFA ===\n")
cat("R-squared full model:", round(R2_full, 4), "\n")
cat("R-squared reduced model (no RS):", round(R2_reduced_RS, 4), "\n")
cat("Cohen's f² for Reward Sensitivity:", round(f_squared_RS_rffa, 4), "\n")

# Interpretation for RS
if(f_squared_RS_rffa < 0.02) {
  effect_size_interp_RS <- "small"
} else if(f_squared_RS_rffa < 0.15) {
  effect_size_interp_RS <- "medium"
} else {
  effect_size_interp_RS <- "large"
}
cat("Effect size interpretation (RS):", effect_size_interp_RS, "\n\n")

# === Cohen's f² for Substance Use (SU) main effect ===
# Reduced model without SU main effect (keeping interactions)
rffa_reduced_SU <- lm(`ppi_C16_rew_F-C_z1_main-effect_cluster1_type-ppi_seed-VS_thr5_cope-16` ~
                        tsnr + fd_mean + RS + RS_square + SUxRS + SUxRS_sq, 
                      data = sharedreward)

R2_reduced_SU <- summary(rffa_reduced_SU)$r.squared

# Calculate Cohen's f² for SU main effect
f_squared_SU_rffa <- (R2_full - R2_reduced_SU) / (1 - R2_full)

cat("=== SUBSTANCE USE EFFECT ON rFFA ===\n")
cat("R-squared full model:", round(R2_full, 4), "\n")
cat("R-squared reduced model (no SU main effect):", round(R2_reduced_SU, 4), "\n")
cat("Cohen's f² for Substance Use:", round(f_squared_SU_rffa, 4), "\n")

# Interpretation for SU
if(f_squared_SU_rffa < 0.02) {
  effect_size_interp_SU <- "small"
} else if(f_squared_SU_rffa < 0.15) {
  effect_size_interp_SU <- "medium"
} else {
  effect_size_interp_SU <- "large"
}
cat("Effect size interpretation (SU):", effect_size_interp_SU, "\n")

# IMPORANT -- Reward with Friend v Stranger + Computer, zstat 12 cluster (SUxRS_square-neg) Ventral Striatum ppi seed
model9 <- lm(`ppi_C23_rew-pun_F-SC_z12_su-rs2-neg_cluster3_type-ppi_seed-VS_thr5_cope-23` ~
               tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
model9
total$SU_qualifier <- cut(sharedreward$SU,
                          breaks = c(-2, 0, 6),
                          labels = c("low","high"))
total$SU_qualifier

# IMPORANT -- Reward with Friend v Stranger + Computer, zstat 12 cluster (SUxRS_square-neg) Ventral Striatum ppi seed
model9 <- lm(`ppi_C23_rew-pun_F-SC_z12_su-rs2-neg_cluster3_type-ppi_seed-VS_thr5_cope-23` ~
               tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
summary(model9)
total$SU_qualifier <- cut(sharedreward$SU,
                          breaks = c(-2, 0, 6),
                          labels = c("low","high"))
total$SU_qualifier
            #VS-STS Rew-Pun for Friend v Str + Comp, moderated by SUxRS^2 interaction
scatter <- ggplot(data = total, aes(x=RS,
                                    y=`ppi_C23_rew-pun_F-SC_z12_su-rs2-neg_cluster3_type-ppi_seed-VS_thr5_cope-23`, 
                                    col = SU_qualifier))+
  geom_smooth(method=lm, formula = y ~ poly(x,2), level = 0.99, 
              se=FALSE, fullrange=TRUE, linetype="dashed")+
  geom_point(shape=1,color="black")+
  scale_x_continuous(breaks = seq(-6, 6, by = 2))
scatter + scale_color_grey() + theme(panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank(), 
                                     panel.background = element_blank(), 
                                     axis.line =  element_line(colour="black"))

# STG activation: Reward with Friend v Computer, zstat 2 cluster 1 (substance use) activation model exploratory result
model10 <- lm(`act_C16_rew_F-C_z2_sub_cluster1_type-act_cope-16` ~
               tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
summary(model10)
scatter <- ggplot(data = sharedreward, aes(x=SU,
                                    y=`act_C16_rew_F-C_z2_sub_cluster1_type-act_cope-16`))+
  geom_smooth(method=lm, level = 0.99, 
              se=TRUE, fullrange=TRUE, color="black")+
  geom_point(shape=1,color="black")
scatter + scale_color_grey() + theme(panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank(), 
                                     panel.background = element_blank(), 
                                     axis.line =  element_line(colour="black"))


# IMPORANT -- Reward with Friend v Stranger + Computer, zstat 12 cluster (SUxRS_square-neg) Ventral Striatum ppi seed
model9 <- lm(`ppi_C23_rew-pun_F-SC_z12_su-rs2-neg_cluster3_type-ppi_seed-VS_thr5_cope-23` ~
               tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
summary(model9)
total$SU_qualifier <- cut(sharedreward$SU,
                          breaks = c(-2, 0, 6),
                          labels = c("low","high"))
total$SU_qualifier
#VS-STG Rew-Pun for Friend v Str + Comp, moderated by SUxRS^2 interaction
scatter <- ggplot(data = total, aes(x=RS,
                                    y=`ppi_C23_rew-pun_F-SC_z12_su-rs2-neg_cluster3_type-ppi_seed-VS_thr5_cope-23`, 
                                    col = SU_qualifier))+
  geom_smooth(method=lm, formula = y ~ poly(x,2), level = 0.99, 
              se=FALSE, fullrange=TRUE, linetype="dashed")+
  geom_point(shape=1,color="black")+
  scale_x_continuous(breaks = seq(-6, 6, by = 2))
scatter + scale_color_grey() + theme(panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank(), 
                                     panel.background = element_blank(), 
                                     axis.line =  element_line(colour="black"))

# Pre-Frontal Motor activation: Reward with Friend v Stranger, zstat 9 cluster 2 (reward sensitivity) activation model exploratory result
model14 <- lm(`act_C13_rew-pun_F-C_z9_rs-neg_cluster2_type-act_cope-13` ~
                tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
summary(model14)
crPlots(model14, smooth=FALSE)

# Frontal Pole activation: Reward with Friend v Stranger, zstat 10 (reward sensitivity squared) activation model exploratory result - interaction with substance use
model15 <- lm(`act_C13_rew-pun_F-C_z10_rs2-neg_type-act_cope-13` ~
                tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
summary(model15)
crPlots(model15, smooth=FALSE)

# IMPORTANT - Reward with Friend v Stranger, zstat 2 (substance use) activation model exploratory result
model16 <- lm(`act_C14_rew_F-S_z2_sub_type-act_cope-14` ~
                tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
model16
summary(model16)

scatter <- ggplot(data = total, aes(x=SU,
                                    y=`act_C14_rew_F-S_z2_sub_type-act_cope-14`))+
  geom_smooth(method=lm, level = 0.99, 
              se=TRUE, fullrange=TRUE, color="black")+
  geom_point(shape=1,color="black")
scatter + scale_color_grey() + theme(panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank(), 
                                     panel.background = element_blank(), 
                                     axis.line =  element_line(colour="black"))

#pTPJ ROI multiple regressions

model20 <- lm(`pTPJ_R_P_F_C` ~
                tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=total)
model20
crPlots(model20, smooth=FALSE)
summary(model20)

model21 <- lm(`pTPJ_R_P_F_S` ~
                tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=total)
summary(model21)

# Calculate f squared for SU
# Full model
model21_full <- lm(`pTPJ_R_P_F_S` ~
                     tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, 
                   data = total)

# Reduced model without substance use (SU) - removing both main effect and interactions
model21_reduced <- lm(`pTPJ_R_P_F_S` ~
                        tsnr + fd_mean + RS + RS_square, 
                      data = total)

# Get R-squared values
R2_full <- summary(model21_full)$r.squared
R2_reduced <- summary(model21_reduced)$r.squared

# Calculate Cohen's f² for the SU effect (including its interactions)
f_squared_SU <- (R2_full - R2_reduced) / (1 - R2_full)

cat("R-squared full model:", round(R2_full, 4), "\n")
cat("R-squared reduced model:", round(R2_reduced, 4), "\n")
cat("Cohen's f² for Substance Use (including interactions):", round(f_squared_SU, 4), "\n")

# If you want JUST the main effect of SU (without interactions), use this instead:
model21_reduced_mainonly <- lm(`pTPJ_R_P_F_S` ~
                                 tsnr + fd_mean + RS + RS_square + SUxRS + SUxRS_sq, 
                               data = total)

R2_reduced_mainonly <- summary(model21_reduced_mainonly)$r.squared
f_squared_SU_mainonly <- (R2_full - R2_reduced_mainonly) / (1 - R2_full)

cat("Cohen's f² for SU main effect only:", round(f_squared_SU_mainonly, 4), "\n")

# Interpretation for main effect only
if(f_squared_SU_mainonly < 0.02) {
  effect_size_interp <- "small"
} else if(f_squared_SU_mainonly < 0.15) {
  effect_size_interp <- "medium"
} else {
  effect_size_interp <- "large"
}
cat("Effect size interpretation (main effect):", effect_size_interp, "\n")

scatter <- ggplot(data = total, aes(x=SU,
                                    y=`pTPJ_R_P_F_S`))+
  geom_smooth(method=lm, level = 0.99, 
              se=FALSE, fullrange=TRUE, linetype="dashed",)+
  geom_point(shape=1,color="black")
scatter + scale_color_grey() + theme(panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank(), 
                                     panel.background = element_blank(), 
                                     axis.line =  element_line(colour="black"))
#reward vs punishment in social closeness condition is moderated by substance use - t = 2.482, p = 0.0177


#VS results

#exploratory - regression with VS wholebrain finding for Reward with Friend v Stranger
model22 <- lm(`act_c14_VS-wholebrain_rew-F-S_zstat1_cluster1` ~
                tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
model22
summary(model22)

#VS ROI findings
  #Reward Sensitivity finding
model23 <- lm(`act_VS-seed_11-rew-pun_F-S` ~
                tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
model23
VS_RS_r <- cor(sharedreward$RS, sharedreward$`act_VS-seed_11-rew-pun_F-S`, method = 'pearson')
print(VS_RS_r)
summary(model23)
crPlots(model23, smooth=FALSE, grid=FALSE)
                #difference in VS activity for rew vs pun in friends vs strangers went down as reward sensitivity went up - p = 0.04717, t = -2.053
scatter <- ggplot(data = total, aes(x=RS,
                                    y=`act_VS-seed_11-rew-pun_F-S`))+
  geom_smooth(method=lm, level = 0.99, 
              se=TRUE, fullrange=TRUE, color="black")+
  geom_point(shape=1,color="black")
scatter + scale_color_grey() + theme(panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank(), 
                                     panel.background = element_blank(), 
                                     axis.line =  element_line(colour="black"))
  #Substance Use finding
model24 <- lm(`act_VS-seed_13-rew-pun_F-C` ~
                tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
summary(model24)
crPlots(model24, smooth=FALSE, grid=FALSE)
                #difference in VS activity for rew vs pun in friends vs computers went down as substance use went up - p = 0.0271, t = -2.301
  #Substance Use finding
model28 <- lm(`act_VS-seed_23-rew-pun_F-SC` ~
                tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
model28
summary(model28)
crPlots(model28, smooth=FALSE, grid=FALSE)
                #difference in VS activity for rew vs pun in friends vs strangers & computers went down as substance use went up - p = 0.04945, t = -2.031


# ANOVA for VS_ROI - repeated measures
res.aov <- anova_test(
  data = df_VS_ROI, dv = Betas, wid = sub,
  within = c(Partner, Outcome),
  effect.size = "pes"  # This specifies partial eta squared instead of generalized
)
get_anova_table(res.aov)

# Pairwise comparisons across all conditions
pair <- df_VS_ROI %>% 
  pairwise_t_test(Betas ~ Partner2, paired = TRUE, p.adjust.method = "bonferroni") 
data.frame(pair)

# Pairwise comparisons within each outcome type
pwc <- df_VS_ROI %>%
  group_by(Outcome) %>%
  pairwise_t_test(
    Betas ~ Partner, paired = TRUE,
    p.adjust.method = "bonferroni"  # Changed from "fdr" to "bonferroni" for consistency
  )
data.frame(pwc)

###
# JW: replacing ANOVA above with lm:
# Load required libraries
library(lme4)      # for lmer()
library(lmerTest)  # for p-values in lmer
library(emmeans)   # for post-hoc comparisons
#install.packages("effectsize")
library(effectsize) # for effect size calculations

# Convert repeated measures ANOVA to Linear Mixed Model
# Assuming your data is in long format with columns: sub, Partner, Outcome, Betas

# Fit the linear mixed model
# Random intercepts for subjects to account for repeated measures
model <- lmer(Betas ~ Partner * Outcome + (1|sub), data = df_VS_ROI)

# Get model summary
summary(model)

# Get ANOVA-style F-tests (Type III)
library(car)
Anova(model, type = "III")

# Calculate effect sizes
# For mixed models, we can use marginal R² (fixed effects) and conditional R² (fixed + random)
library(MuMIn)
r.squaredGLMM(model)

# Alternative: Calculate eta-squared from the Anova output
anova_results <- Anova(model, type = "III")
# This will give you similar results to your repeated measures ANOVA

# Post-hoc comparisons (equivalent to your pairwise t-tests)
# Compare partners within reward outcomes
posthoc <- emmeans(model, pairwise ~ Partner | Outcome, adjust = "bonferroni")
posthoc$contrasts

# Or if you want to compare specific conditions (e.g., Friend vs Stranger for rewards only)
# First create the interaction term
emmeans_results <- emmeans(model, ~ Partner * Outcome)
contrast(emmeans_results, 
         list("Friend_Reward vs Stranger_Reward" = c(0, 0, 0, 1, 0, -1)),
         adjust = "bonferroni")

# Alternative simpler approach if you just want Friend vs Stranger for rewards:
# Filter data for reward outcomes only, then fit simpler model
#df_rewards_only <- df_VS_ROI[df_VS_ROI$Outcome == "Reward", ]
#model_rewards <- lmer(Betas ~ Partner + (1|sub), data = df_rewards_only)
#summary(model_rewards)

# Post-hoc for rewards only
#emmeans(model_rewards, pairwise ~ Partner, adjust = "bonferroni")
###

#ANOVA for VS_TPJ_ROI - repeated measures
#model30 <- aov(Betas ~ Partner + Outcome, data = df_VS_TPJ_ROI)
res.aov <- anova_test(
  data = df_VS_TPJ_ROI, dv = Betas, wid = sub,
  within = c(Partner, Outcome),
  effect.size =
)
get_anova_table(res.aov)

pwc <- df_VS_TPJ_ROI %>%
  group_by(Outcome) %>%
  pairwise_t_test(
    Betas ~ Partner, paired = TRUE,
    p.adjust.method = "bonferroni"
  )

###

# Load required libraries
library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(MuMIn)

# Convert repeated measures ANOVA to Linear Mixed Model for VS-TPJ connectivity
model_vstpj <- lmer(Betas ~ Partner * Outcome + (1|sub), data = df_VS_TPJ_ROI)

# Get model summary
summary(model_vstpj)

# Get ANOVA-style F-tests (Type III)
Anova(model_vstpj, type = "III")

# Calculate effect sizes
r.squaredGLMM(model_vstpj)

# Post-hoc comparisons grouped by outcome
posthoc_vstpj <- emmeans(model_vstpj, pairwise ~ Partner | Outcome, adjust = "bonferroni")
posthoc_vstpj$contrasts

###

data.frame(pwc)

###

# Testing Friend vs. Computer

# Check: can we replicate the act VS result for Friend vs. Stranger:

# Original: Reward Sensitivity finding
model23 <- lm(`act_VS-seed_11-rew-pun_F-S` ~
                tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
summary(model23)

# Replication:
sharedreward$`rep_act_VS-seed_11-rew-pun_F-S` <- read.table("~/Documents/GitHub/istart-sharedreward/derivatives/imaging_plots/test_act_seed-VS_cnum-11_cname-rew-pun_F-S.txt")[,1]

modelrep <- lm(`rep_act_VS-seed_11-rew-pun_F-S` ~
                tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
summary(modelrep)
# Model correctly replicates

# Calculate F-squared?

# Full model (your existing model)
model23_full <- lm(`act_VS-seed_11-rew-pun_F-S` ~
                     tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, 
                   data = sharedreward)

# Reduced model without reward sensitivity (RS)
model23_reduced <- lm(`act_VS-seed_11-rew-pun_F-S` ~
                        tsnr + fd_mean + RS_square + SU + SUxRS + SUxRS_sq, 
                      data = sharedreward)

# Get R-squared values
R2_full <- summary(model23_full)$r.squared
R2_reduced <- summary(model23_reduced)$r.squared

# Calculate Cohen's f² for the RS effect
f_squared_RS <- (R2_full - R2_reduced) / (1 - R2_full)

cat("R-squared full model:", round(R2_full, 4), "\n")
cat("R-squared reduced model:", round(R2_reduced, 4), "\n")
cat("Cohen's f² for Reward Sensitivity:", round(f_squared_RS, 4), "\n")

# Interpretation
if(f_squared_RS < 0.02) {
  effect_size_interp <- "small"
} else if(f_squared_RS < 0.15) {
  effect_size_interp <- "medium"
} else {
  effect_size_interp <- "large"
}
cat("Effect size interpretation:", effect_size_interp, "\n")

# Test Friend vs. Computer:
sharedreward$`rep_act_VS-seed_13-rew-pun_F-C` <- read.table("~/Documents/GitHub/istart-sharedreward/derivatives/imaging_plots/test_act_seed-VS_cnum-13_cname-rew-pun_F-C.txt")[,1]

modelrep <- lm(`rep_act_VS-seed_13-rew-pun_F-C` ~
                 tsnr + fd_mean + RS + RS_square + SU + SUxRS + SUxRS_sq, data=sharedreward)
summary(modelrep)

### Manuscript 
