##################################### Descriptive Statistics ###############################
##### Packages #####
library(rstatix)
library(dplyr)

##### Frequencies, Means, SD #####
##  Mean and SD by group
main2 %>%
  group_by(Group) %>%
  get_summary_stats(Age,
                    MFQ_C01_Score_Total, SNAP_IV_P01_Score_ADHD_In, 
                    SNAP_IV_P01_Score_ADHD_HIm, CMRS_P01_Score_Total_Curr,
                    type = "mean_sd")

## Frequencies of binary variables
main2 %>%
  group_by(Group, RaceWhite) %>%
  summarize(Freq=n())

main2 %>%
  group_by(Group, IncomeHigh) %>%
  summarize(Freq=n())

main2 %>%
  group_by(Group, Ethnicity) %>%
  summarize(Freq=n())

main2 %>%
  group_by(Group, GenderBinary) %>%
  summarize(Freq=n())
