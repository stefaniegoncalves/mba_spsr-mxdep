####################################### Cleaning/Transformation  ############################ 
setwd("~/Documents/Research/MBA/SPSR-MxDep Paper")

##### Packages #####
library(dplyr)

##### Get and Format Datasets #####
MBA <- read.csv('MBA_Dep-MxFeat-Mot_DataFINALFINAL.csv') # Study 1
PEBS <- read.csv('PEBS_T2FINAL.csv') # Study 2

## Combine Study 1 (MBA) and Study 2 (PEBS) datasets
library(dplyr)
main <- full_join(PEBS, MBA)

## Factor binary/categorical variables
main$GenderBinary <- factor(main$GenderBinary,
                            levels = c(0, 1),
                            labels = c("Cisboys", "Other"))

main$IncomeHigh <- factor(main$IncomeHigh,
                            levels = c(0, 1),
                            labels = c("Under100-105k", "Over100-105k"))

main$RaceWhite <- factor(main$RaceWhite,
                          levels = c(0, 1),
                          labels = c("Non-White", "White"))

main$Ethnicity <- factor(main$Ethnicity,
                         levels = c(0, 1),
                         labels = c("NotHispanic", "Hispanic"))

## Select relevant variables
main2 <- dplyr::select(main, ID, Group, Age, Race,RaceWhite,
                       Ethnicity, HouseholdIncome,IncomeHigh,
                       GenderBinary, BISBAS_C01_Score_BAS_Drive,
                       BISBAS_C01_Score_BAS_FunSeeking, BISBAS_C01_Score_BAS_Reward,
                       BISBAS_C01_Score_BIS, BISBAS_P01_Score_BIS, BISBAS_P01_Score_BAS_Drive,
                       BISBAS_P01_Score_BAS_Reward, BISBAS_P01_Score_BAS_FunSeeking,
                       SNAP_IV_P01_Score_ADHD_In, SNAP_IV_P01_Score_ADHD_HIm, 
                       R_SPSR_C01_Mean_Anxiety, R_SPSR_C01_Mean_Drive, R_SPSR_C01_Mean_Fearshy,
                       R_SPSR_C01_Mean_Impulse, R_SPSR_C01_Mean_Response,
                       R_SPSR_P01_Mean_Anxiety, R_SPSR_P01_Mean_Drive, R_SPSR_P01_Mean_Fearshy,
                       R_SPSR_P01_Mean_Impulse, R_SPSR_P01_Mean_Response, AUC,
                       tippingpoint,MFQ_P01_Score_Total, MFQ_C01_Score_Total,
                       CMRS_C01_Score_Total_Curr, CMRS_P01_Score_Total_Curr,
                       Q008a_Curr_Sv, Q008b_Curr_Sv,Q004_Curr_Sv, Q005a_Curr_Sv,
                       Q001_Curr_Sv, Q006_Curr_Sv, Q007_Curr_Sv, Q009_Curr_Sv, Q003_Curr_Sv, 
                       Q010_Curr_Sv, Q013_Curr_Sv, 
                       Q008a_MSP_Dep_Sv, Q008b_MSP_Dep_Sv, 
                       Q004_MSP_Dep_Sv, Q005a_MSP_Dep_Sv, 
                       Q001_MSP_Dep_Sv, Q006_MSP_Dep_Sv, Q007_MSP_Dep_Sv, Q009_MSP_Dep_Sv, 
                       Q003_MSP_Dep_Sv, Q010_MSP_Dep_Sv, Q013_MSP_Dep_Sv,
                       Q008a_MSP_Dep_wMixedFea_Sv, Q008b_MSP_Dep_wMixedFea_Sv,
                       Q004_MSP_Dep_wMixedFea_Sv, Q005a_MSP_Dep_wMixedFea_Sv,
                       Q001_MSP_Dep_wMixedFea_Sv, Q006_MSP_Dep_wMixedFea_Sv, 
                       Q007_MSP_Dep_wMixedFea_Sv, Q009_MSP_Dep_wMixedFea_Sv, 
                       Q003_MSP_Dep_wMixedFea_Sv, Q010_MSP_Dep_wMixedFea_Sv, 
                       Q013_MSP_Dep_wMixedFea_Sv)

###### Create Depression/Healthy Control Groups ######
## Use highest score as the representative score
main2 <- main2 %>%
  mutate(thoughts_ideas_Curr = pmax(Q008a_Curr_Sv, Q008b_Curr_Sv, na.rm = TRUE)) 

main2 <- main2 %>%
  mutate(energetic_goal_Curr = pmax(Q004_Curr_Sv, Q005a_Curr_Sv, na.rm = TRUE)) 

main2 <- main2 %>%
  mutate(thoughts_ideas_MSP_Dep = pmax(Q008a_MSP_Dep_Sv, Q008b_MSP_Dep_Sv, na.rm = TRUE)) 

main2 <- main2 %>%
  mutate(energetic_goal_MSP_Dep = pmax(Q004_MSP_Dep_Sv, Q005a_MSP_Dep_Sv, na.rm = TRUE)) 

main2 <- main2 %>%
  mutate(thoughts_ideas_MSP_Dep_wMixedFea = pmax(Q008a_MSP_Dep_wMixedFea_Sv, Q008b_MSP_Dep_wMixedFea_Sv, na.rm = TRUE)) 

main2 <- main2 %>%
  mutate(energetic_goal_MSP_Dep_wMixedFea = pmax(Q004_MSP_Dep_wMixedFea_Sv, Q005a_MSP_Dep_wMixedFea_Sv, na.rm = TRUE)) 

## Add up number of symptoms with a score of 3 or more
main2$CURRnumsymDSM <- rowSums(main2[, c("thoughts_ideas_Curr", "energetic_goal_Curr",
                                         "Q001_Curr_Sv", "Q006_Curr_Sv", "Q007_Curr_Sv", 
                                         "Q009_Curr_Sv", "Q003_Curr_Sv")] >= 3) #DSM manic criteria

main2$CURRnumsymDSMplus <- rowSums(main2[, c("thoughts_ideas_Curr", "energetic_goal_Curr",
                                             "Q001_Curr_Sv", "Q006_Curr_Sv", "Q007_Curr_Sv", 
                                             "Q009_Curr_Sv", "Q003_Curr_Sv", 
                                             "Q010_Curr_Sv", "Q013_Curr_Sv")] >= 3) #DSM manic criteria + mood lability and distractibility 

main2$MSPnumsymDSM <- rowSums(main2[, c("thoughts_ideas_MSP_Dep", "energetic_goal_MSP_Dep",
                                        "Q001_MSP_Dep_Sv", "Q006_MSP_Dep_Sv", "Q007_MSP_Dep_Sv",
                                        "Q009_MSP_Dep_Sv", "Q003_MSP_Dep_Sv")] >= 3) #DSM manic criteria

main2$MSPnumsymDSMplus <- rowSums(main2[, c("thoughts_ideas_MSP_Dep", "energetic_goal_MSP_Dep",
                                            "Q001_MSP_Dep_Sv", "Q006_MSP_Dep_Sv", "Q007_MSP_Dep_Sv",
                                            "Q009_MSP_Dep_Sv", "Q003_MSP_Dep_Sv", "Q010_MSP_Dep_Sv",
                                            "Q013_MSP_Dep_Sv")] >= 3) #DSM manic criteria + mood lability and distractibility 

main2$MSPMxnumsymDSM <- rowSums(main2[, c("thoughts_ideas_MSP_Dep_wMixedFea", "energetic_goal_MSP_Dep_wMixedFea",
                                          "Q001_MSP_Dep_wMixedFea_Sv", "Q006_MSP_Dep_wMixedFea_Sv", 
                                          "Q007_MSP_Dep_wMixedFea_Sv", "Q009_MSP_Dep_wMixedFea_Sv", 
                                          "Q003_MSP_Dep_wMixedFea_Sv")] >= 3) #DSM manic criteria

main2$MSPMxnumsymDSMplus <- rowSums(main2[, c("thoughts_ideas_MSP_Dep_wMixedFea", "energetic_goal_MSP_Dep_wMixedFea",
                                              "Q001_MSP_Dep_wMixedFea_Sv", "Q006_MSP_Dep_wMixedFea_Sv", 
                                              "Q007_MSP_Dep_wMixedFea_Sv", "Q009_MSP_Dep_wMixedFea_Sv", 
                                              "Q003_MSP_Dep_wMixedFea_Sv", "Q010_MSP_Dep_wMixedFea_Sv", 
                                              "Q013_MSP_Dep_wMixedFea_Sv")] >= 3) #DSM manic criteria + mood lability and distractibility 

## Label participant as depressed with mixed features (2) if 3+ symptoms with 3+ score 
## Label participant as depressed without mixed features (1) if not
main2$groupsDSM <- ifelse(main2$CURRnumsymDSM >= 3 | main2$MSPnumsymDSM >= 3 |
                            main2$MSPMxnumsymDSM >= 3, 3, 2)

main2$groupsDSMplus <- ifelse(main2$CURRnumsymDSMplus >= 3 | main2$MSPnumsymDSMplus >= 3 |
                                main2$MSPMxnumsymDSMplus >= 3, 3, 2)

## Change participant label to healthy control (0) if participant has Type = 1
main2$groupsDSM[main2$Group == '1'] <- "1"
main2$groupsDSMplus[main2$Group == '1'] <- "1"

main2$Group <- main2$groupsDSMplus

main2$Group <- factor(main2$Group,
                      levels = c(1, 2, 3),
                      labels = c("Healthy Control", "Dep", "MxDep"))
