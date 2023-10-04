####################################### Main Analyses - Missing Data ####################################
## Relevant variables for main analyses
main2parmiss <- dplyr::select(main2, ID, Group, Age, GenderBinary, R_SPSR_P01_Mean_Drive,
                              R_SPSR_P01_Mean_Impulse, R_SPSR_P01_Mean_Response, 
                              R_SPSR_P01_Mean_Anxiety, R_SPSR_P01_Mean_Fearshy,
                              AUC, tippingpoint)

## Descriptive of missing data
colSums(is.na(main2parmiss)) # number of participants with missing data per variable
sum(complete.cases(main2parmiss)) # number of participants with all data
lapply(main2parmiss[,-1],function(x){main2parmiss[!complete.cases(x),'ID']}) # which participants are missing data
#main2parmiss[!complete.cases(main2parmiss),'ID'] # participants with missing data

## Create variable indicating participants with complete data (1) or missing data (0)
main2parmiss$missingdata <- as.numeric(complete.cases(main2parmiss))
main2parmiss$missingdata <- factor(main2parmiss$missingdata,
                                   levels = c(0, 1),
                                   labels = c("missing", "complete")) 

## Associations between missing data and independent variables
t.test(main2parmiss$Age ~ main2parmiss$missingdata, alternative = "two.sided", var.equal = FALSE)
chisq.test(main2parmiss$GenderBinary, main2parmiss$missingdata, correct=FALSE)
chisq.test(main2parmiss$Group, main2parmiss$missingdata, correct=FALSE)
