####################################### Main Analyses ####################################
##### Packages #####
library(dplyr)
library(emmeans)
library(effectsize)
library(multcomp)

########### Trait Reward - Parent SPSR ########### 
## Select relevant data
main2parSPSRrew <- dplyr::select(main2, ID, Group, Age, GenderBinary, R_SPSR_P01_Mean_Drive,
                                 R_SPSR_P01_Mean_Impulse, R_SPSR_P01_Mean_Response)

## Remove participants with missing data
main2parSPSRrew <- na.omit(main2parSPSRrew) # remove participants with missing data

## Analysis
dependent_vars <- cbind(main2parSPSRrew$R_SPSR_P01_Mean_Drive, main2parSPSRrew$R_SPSR_P01_Mean_Impulse, main2parSPSRrew$R_SPSR_P01_Mean_Response)
independent_var <- main2parSPSRrew$Group
manova_model <- manova(dependent_vars ~ independent_var + main2parSPSRrew$Age + main2parSPSRrew$GenderBinary, data = main2parSPSRrew)
summary(manova_model) # overall results
summary.aov(manova_model) # results by dependent variable

ref_grid (manova_model, mult.name = "dependent_var") 
emmeans <- emmeans(manova_model, ~ rep.meas:independent_var) # estimated marginal means
main2parSPSRrew2 <- summary(emmeans) # table of estimated marginal means
main2parSPSRrew2
main2parSPSRrew2$rep.meas <- as.factor(main2parSPSRrew2$rep.meas) 

## Effect size
eta_squared(manova_model)

## Post-hoc analysis
pwc <- main2parSPSRrew %>%
  gather(key = "variables", value = "value", R_SPSR_P01_Mean_Drive, 
         R_SPSR_P01_Mean_Impulse, R_SPSR_P01_Mean_Response) %>%
  group_by(variables) %>%
  tukey_hsd(value ~ Group) %>%
  dplyr::select(-estimate, -conf.low, -conf.high) # Remove details
pwc

######## Trait Punishment - Parent SPSR ##########
## Select relevant data
main2parSPSRpun <- dplyr::select(main2, ID, Group, Age, GenderBinary, R_SPSR_P01_Mean_Anxiety, 
                                 R_SPSR_P01_Mean_Fearshy)

## Remove participants with missing data
main2parSPSRpun <- na.omit(main2parSPSRpun) 

## Analysis
dependent_vars <- cbind(main2parSPSRpun$R_SPSR_P01_Mean_Anxiety, main2parSPSRpun$R_SPSR_P01_Mean_Fearshy)
independent_var <- main2parSPSRpun$Group
manova_model <- manova(dependent_vars ~ independent_var + main2parSPSRpun$Age + main2parSPSRpun$GenderBinary, data = main2parSPSRpun)
summary(manova_model) # overall results
summary.aov(manova_model) # results by dependent variable

ref_grid (manova_model, mult.name = "dependent_var") # estimated marginal means
emmeans <- emmeans(manova_model, ~ rep.meas:independent_var) # estimated marginal means
main2parSPSRpun2 <- summary(emmeans) # table of estimated marginal means
main2parSPSRpun2
main2parSPSRpun2$rep.meas <- as.factor(main2parSPSRpun2$rep.meas)

## Effect size
eta_squared(manova_model)

## Post-hoc analysis
pwc <- main2parSPSRpun %>%
  gather(key = "variables", value = "value", R_SPSR_P01_Mean_Anxiety, 
         R_SPSR_P01_Mean_Fearshy) %>%
  group_by(variables) %>%
  tukey_hsd(value ~ Group) %>%
  dplyr::select(-estimate, -conf.low, -conf.high) # Remove details
pwc

########## Tipping Point ##########
## Select relevant data
main2tp <- dplyr::select(main2, ID, Group, Age, GenderBinary, tippingpoint)

## Remove participants with missing data
main2tp <- na.omit(main2tp) # remove participants with missing data

## Analysis
ancova_model <- aov(tippingpoint ~ Group + Age + GenderBinary, data = main2tp)
summary(ancova_model)
postHocs <- glht(ancova_model, linfct = mcp(Group = "Tukey"))
summary(postHocs)

emmeans(ancova_model, ~ Group) # estimated marginal means

## Effect size
eta_squared(ancova_model)

########## AUC ##########
## Select relevant data
main2AUC <- dplyr::select(main2, ID, Group, Age, GenderBinary, AUC)

## Remove participants with missing data
main2AUC <- na.omit(main2AUC) # remove participants with missing data

## Analysis
ancova_model <- aov(AUC ~ Group + Age + GenderBinary, data = main2AUC)
summary(ancova_model)
postHocs <- glht(ancova_model, linfct = mcp(Group = "Tukey"))
summary(postHocs)

emmeans(ancova_model, ~ Group) # estimated marginal means

## Effect size
eta_squared(ancova_model)
