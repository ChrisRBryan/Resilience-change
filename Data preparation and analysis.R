#Clear the environment
rm(list = ls())
#install.packages("fBasics")
#Required Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, party, rio, tidyverse, fBasics, dplyr, kableExtra, graphics, gridExtra, reshape2, rstatix,
               psych, ltm, plyr, lavaan)
library(lavaan)
library(semTools)
install.packages("psych","ltm","plyr")
#install.packages("ltm")
library(psych)
library(reshape)
library(kableExtra)
library(rstatix)
library(lme4)
library(mice)
#set working directory
setwd("C:/Users/cbryan/Desktop/PhD Work Resilience Paper/June 2023/RAW data")

##Check for MCAR
install.packages("naniar")
install.packages("mice")
install.packages("BaylorEdPsych")
install.packages("MissMech")
library(MissMech)
library(BaylorEdPsych)
library(naniar)


#import raw merged dataset
(data<- import("reslong.csv", na.strings="") %>% as_tibble())


#Make a subset to check test variables
library(dplyr)

selected_vars <- c("DWUPT_", "DWHAST_","PRAT_", "REST_", "DHAST_", "DUPT_", "ABALT_", "id", "TIME", "CD25_1",	"CD25_2",	"CD25_3",	"CD25_4",	"CD25_5",	"CD25_6",	"CD25_7",	"CD25_8",	"CD25_9",	"CD25_10",	"CD25_11",	"CD25_12",	"CD25_13",	"CD25_14",	"CD25_15",	"CD25_16",	"CD25_17",	"CD25_18",	"CD25_19",	"CD25_20",	"CD25_21",	"CD25_22",	"CD25_23",	"CD25_24",	"CD25_25")


# Create a subset of your dataset with the specified columns
data_subset <- data[selected_vars]


# Check missing data pattern
md.pattern(data_subset)

# Perform Little's MCAR test on the subset
mcar_test <- TestMCARNormality(data_subset)


mcar_test <- test_MCAR_Little(data_subset)

# Print test results
print(mcar_test)

# Extract p-value from the test result
p_value <- mcar_test$p.value

##Data umputation for missing data
imputation_model <- mice(data = data_subset, method = "pmm", m = 5)
imputed_data <- complete(imputation_model)

#Put data in correct form and type
data_subset$TIME <- as.numeric(data_subset$TIME)
data_subset$REST_ <- as.numeric(data_subset$REST_)
data_subset$DHAST_ <- as.numeric(data_subset$DHAST_)
data_subset$DUPT_ <- as.numeric(data_subset$DUPT_)
data_subset$DWUPT_ <- as.numeric(data_subset$DWUPT_)
data_subset$DWHAT_ <- as.numeric(data_subset$DWHAT_)



##Create a wide datset also
library(reshape2)

#wide_data <- reshape(imputed_data, idvar = "id", timevar = "TIME", direction = "wide")

wide_data <- reshape(data_subset, idvar = "id", timevar = "TIME", direction = "wide")

library(data.table)

# Convert to data.table for fast reshaping
DT <- as.data.table(data_subset)

# Reshape using data.table::dcast
wide_data <- dcast(
  DT,
  id ~ TIME,                      # reshape by id and TIME
  value.var = setdiff(names(DT), c("id", "TIME"))
)

# Now rename the columns to have the ".1", ".2", etc. suffix pattern
# Assuming TIME is numeric or ordered like 1, 2, 3, 4

# Extract variable base names (all except id and TIME)
vars <- setdiff(names(data_subset), c("id", "TIME"))

timepoints <- sort(unique(data_subset$TIME))

# Create new names in var.time format (e.g., var1.1, var1.2)
new_names <- c("id", as.vector(sapply(vars, function(v) paste0(v, ".", timepoints))))

# Rename columns
setnames(wide_data, names(wide_data), new_names)

# Result: wide_data with id, var1.1, var1.2, ..., varN.T

write.csv(wide_data, file = "widedataWork.csv")

###############################################Compute the intercept and the slope######################################################################
##Create a variable for individual resilience intercept slope

library(dplyr)
library(broom)

#Creat variables for Slope and intercept by id
library(lme4)

# Assuming your data is stored in a data frame called 'data'
# 'id' represents the individual's identifier, and REST_.1 to REST_.4 are the repeated measures

# Fit a linear mixed-effects model
model <- lmer(REST_ ~ TIME + (1 + TIME | id), data = data_subset)

# Replace 'RESPONSE_VARIABLE' and 'TIME_VARIABLE' with your actual variable names.

# Extract the fixed effects (intercept and slope) for each individual
individual_effects <- coef(model)$id

# Add 'id' column to individual_effects
individual_effects$id <- unique(data$id)

# The first column will give you the intercept, and the second column will give you the slope

#Rewrite the variable names

#Create new function to rename variables
rename_variables <- function(data, old_names, new_names) {
  if (length(old_names) != length(new_names)) {
    stop("The number of old_names must match the number of new_names.")
  }
  
  for (i in seq_along(old_names)) {
    col_index <- match(old_names[i], names(data))
    if (!is.na(col_index)) {
      names(data)[col_index] <- new_names[i]
    }
  }
  
  return(data)
}

#Rename variables
individual_effects <- rename_variables(individual_effects, c("(Intercept)", "TIME"), c("Intercept", "Slope"))

#Create a variable of Slope * intercept
# Compute a new variable as the product of Variable1 and Variable2
individual_effects$'SxI' <- individual_effects$Intercept * individual_effects$Slope


# Merge individual_effects into wide_data by 'id'
wide_data <- merge(wide_data, individual_effects, by = "id", all.x = TRUE)

write.csv(wide_data, file = "reswide.csv")

##Merege back the demographic varibles

# Load dplyr if not already loaded
library(dplyr)


# Step 3: If all column names are correct, run the extraction
demographics <- data %>%
  as.data.frame() %>%
  dplyr::select(id, Age, MarStat, Role, Gender) %>%
  distinct(id, .keep_all = TRUE)

# Step 4: Merge with wide_data
wide_data_merged <- wide_data %>%
  left_join(demographics, by = "id")

write.csv(wide_data_merged, file = "widedemog.csv")

#Convert Back to Long format as well

library(dplyr)
library(tidyr)

long_data <- wide_data_merged %>%
  pivot_longer(
    cols = matches("\\.\\d$"),           # matches columns ending in .1, .2, etc.
    names_to = c(".value", "Time"),      # splits into base variable and timepoint
    names_sep = "\\."
  ) %>%
  mutate(
    TIME = as.numeric(Time)              # convert extracted time to numeric
  ) %>%
  dplyr::select(-Time)                   # remove the old character column
# creates numeric TIME variable



############Investigating Hypothesis 1: Employee resilience capacities  will change over time  
#AND Investigating Hypothesis 2: Higher individual initial values of resilience (intercepts) will be negatively be related with resileince change (random slopes) over time 
####Using a Stepwise Approach- Bliese and Ployhart 2002- Growth_Modeling_Using_Random_Coefficient_Models################################################################
library(nlme)
#Step 1: Baseline model
model.1<-gls(REST_~TIME, data= imputed_data)
summary (model.1)

#Need to control for both Age and Role 

model.1B<-gls(REST_~TIME + Age + Role, data= long_data, na.action = na.omit)
summary (model.1B)

#Continue to run additional models with no covariates as an added comparision

#Step 2: Allow random intercepts
model.2<-lme(REST_~TIME, random=~1|id, data= imputed_data)
summary (model.2)

model.2B<-lme(REST_~TIME + Age + Role, random=~1|id, data= long_data)
summary (model.2B)


##Contrast both models using an ANOVA
anova(model.1B, model.2B)

#Step 3: Slope variation amongst Participants
model.3<-lme(REST_~TIME, random=~1+TIME|id, data= imputed_data)
summary(model.3)

model.3b<-lme(REST_~TIME + Age + Role, random=~1+TIME|id, data= long_data, na.action = na.omit)
summary(model.3b)


##Contrast both models using an ANOVA
anova(model.2B, model.3b)
VarCorr(model.3)

#Step 4: Do the residuals show evidence of heteroscedasticity and autocorrelation?
#Short hand: model.4a<-update(model.3, correlation=corAR1())
model.4<-lme(REST_~TIME, random=~1+TIME|id, correlation=corAR1(),data= imputed_data)
summary(model.4, weights=varExp(form=~TIME))
anova(model.3, model.4)

model.4b<-lme(REST_~TIME + Age + Role, random=~1+TIME|id, correlation=corAR1(),data= long_data, na.action = na.omit)
summary(model.4b, weights=varExp(form=~TIME))
anova(model.3b, model.4b)

####Model fit did not improve therefore no further complexity added


##########COMBINED MODEL SEM- Resilience slop AND intercept will have a significant mediation effect of daily Hassles and uplifts relationship on Proactivity and Affect
#Hypotheses 3 & 4
# Define the SEM model for Hassles and Uplifts  
#Include controls of Age and Role as controls for Intercept

#import merged dataset
(mergeddata<- import("widedemog.csv", na.strings="") %>% as_tibble())

#Check Overall Hassles and Uplifts

SEM3 <- '
  # Define regression paths
  ABALT_.4 ~ a1*DHAST_.1 + a2*DUPT_.1 + a3*Slope  +a5*Intercept 
 PRAT_.4 ~ a1.2*DHAST_.1 + a2.2*DUPT_.1 + a3.2*Slope  +a5.2*Intercept

  # Define mediation paths
    Slope ~ d1*DHAST_.1 + d2*DUPT_.1
    Intercept ~ f1*DHAST_.1 + f2*DUPT_.1 + Age + Role

  # Define residual variances and covariances
  Intercept~~Slope
  ABALT_.4~~PRAT_.4
  
#effects of Hassles on AFF through slope and intercept
dir_HAFF := a1
ind_HAFFslope := d1*a3
ind_HAFFintercept := f1*a5
tot_HAFF := a1 + d1*a3 + f1*a5

#effects of Uplifts on AFF through slope and intercept
dir_UAFF := a2
ind_UAFFslope := d2*a3
ind_UAFFintercept := f2*a5
tot_UAFF := a2 + d2*a3 +f2*a5

#effects of Hassles on PRA through slope and intercept
dir_HPRA := a1.2
ind_HPRAslope := d1*a3.2
ind_HPRAintercept := f1*a5.2
tot_HPRA := a1.2 + d1*a3.2 + f1*a5.2

#effects of Uplifts on PRA through slope and intercept
dir_UPRA := a2.2
ind_UPRAslope := d2*a3.2
ind_UPRAintercept := f2*a5.2
tot_UPRA := a2.2 + d2*a3.2 +f2*a5.2


'

# Estimate the SEM model
fit3 <- sem(SEM3, data=mergeddata,
            meanstructure=TRUE, std.lv=TRUE)

# Summarize the SEM model results
summary(fit3, fit.measures=TRUE, standardized=TRUE)

modindices(fit3, sort.=TRUE, minimum.value=10)

fit_indices3 <- fitMeasures(fit3)
summary(fit_indices3)


#Check Work only Hassles and Uplifts
SEM4 <- '
  # Define regression paths
  ABALT_.4 ~ a1*DWHAST_.1 + a2*DWUPT_.1 + a3*Slope  +a5*Intercept 
 PRAT_.4 ~ a1.2*DWHAST_.1 + a2.2*DWUPT_.1 + a3.2*Slope  +a5.2*Intercept

  # Define mediation paths
    Slope ~ d1*DWHAST_.1 + d2*DWUPT_.1
    Intercept ~ f1*DWHAST_.1 + f2*DWUPT_.1 + Age + Role

  # Define residual variances and covariances
  Intercept~~Slope
  ABALT_.4~~PRAT_.4
  
#effects of Hassles on AFF through slope and intercept
dir_HAFF := a1
ind_HAFFslope := d1*a3
ind_HAFFintercept := f1*a5
tot_HAFF := a1 + d1*a3 + f1*a5

#effects of Uplifts on AFF through slope and intercept
dir_UAFF := a2
ind_UAFFslope := d2*a3
ind_UAFFintercept := f2*a5
tot_UAFF := a2 + d2*a3 +f2*a5

#effects of Hassles on PRA through slope and intercept
dir_HPRA := a1.2
ind_HPRAslope := d1*a3.2
ind_HPRAintercept := f1*a5.2
tot_HPRA := a1.2 + d1*a3.2 + f1*a5.2

#effects of Uplifts on PRA through slope and intercept
dir_UPRA := a2.2
ind_UPRAslope := d2*a3.2
ind_UPRAintercept := f2*a5.2
tot_UPRA := a2.2 + d2*a3.2 +f2*a5.2


'

# Estimate the SEM model
fit4 <- sem(SEM4, data=mergeddata,
            meanstructure=TRUE, std.lv=TRUE)

# Summarize the SEM model results
summary(fit4, fit.measures=TRUE, standardized=TRUE)

modindices(fit3, sort.=TRUE, minimum.value=10)

fit_indices3 <- fitMeasures(fit3)
summary(fit_indices3)

##ROBSUTNESS CHECK 1
##########COMBINED MODEL- Resilience slop AND intercept will have a significant mediation effect of daily Hassles and uplifts relationship on Proactivity and Affect
# CHECKING REVERSE CAUSATION MODELS
#1. Swapping ABAL and PRAT and hassles/uplifts
SEM3 <- '
  # Define regression paths
  ABALT_.4 ~ a1*DHAST_.1 + a2*DUPT_.1 + a3*SlopeSQRT  +a5*Intercept 
 PRAT_.4 ~ a1.2*DHAST_.1 + a2.2*DUPT_.1 + a3.2*SlopeSQRT  +a5.2*Intercept

  # Define mediation paths
    SlopeSQRT ~ d1*DHAST_.1 + d2*DUPT_.1
    Intercept ~ f1*DHAST_.1 + f2*DUPT_.1

  # Define residual variances and covariances
   DHAST_.1~~DUPT_.1
   ABALT_.4~~PRAT_.4
  #Intercept~~SlopeSQRT
  
#effects of Hassles on AFF through slope and intercept
dir_HAFF := a1
ind_HAFFslope := d1*a3
ind_HAFFintercept := f1*a5
tot_HAFF := a1 + d1*a3 + f1*a5

#effects of Uplifts on AFF through slope and intercept
dir_UAFF := a2
ind_UAFFslope := d2*a3
ind_UAFFintercept := f2*a5
tot_UAFF := a2 + d2*a3 +f2*a5

#effects of Hassles on PRA through slope and intercept
dir_HPRA := a1.2
ind_HPRAslope := d1*a3.2
ind_HPRAintercept := f1*a5.2
tot_HPRA := a1.2 + d1*a3.2 + f1*a5.2

#effects of Uplifts on PRA through slope and intercept
dir_UPRA := a2.2
ind_UPRAslope := d2*a3.2
ind_UPRAintercept := f2*a5.2
tot_UPRA := a2.2 + d2*a3.2 +f2*a5.2

'

# Estimate the SEM model
fit3 <- sem(SEM3, data=wide_data2,
            meanstructure=TRUE, std.lv=TRUE)

# Summarize the SEM model results
summary(fit3, fit.measures=TRUE, standardized=TRUE)

modindices(fit2, sort.=TRUE, minimum.value=10)

fit_indices2 <- fitMeasures(fit2)
summary(fit_indices2)

#ROBUSTNESS CHECK 2
##########COMBINED MODEL- Resilience slop AND intercept will have a significant mediation effect of daily Hassles and uplifts relationship on Proactivity and Affect
#CHECK: Changing timepoints as needed
#2. Swapping HAST_.1 for DHAST_.4 and DUPT_.1 for DUPT_.4

SEM3 <- '
  # Define regression paths
  ABALT_.4 ~ a1*DHAST_.2 + a2*DUPT_.2 + a3*Slope  +a5*Intercept 
 PRAT_.4 ~ a1.2*DHAST_.2 + a2.2*DUPT_.2 + a3.2*Slope  +a5.2*Intercept

  # Define mediation paths
    Slope ~ d1*DHAST_.2 + d2*DUPT_.2
    Intercept ~ f1*DHAST_.2 + f2*DUPT_.2

  # Define residual variances and covariances
   #DHAST_.2~~DUPT_.2
   ABALT_.4~~PRAT_.4
  Intercept~~Slope
  
#effects of Hassles on AFF through slope and intercept
dir_HAFF := a1
ind_HAFFslope := d1*a3
ind_HAFFintercept := f1*a5
tot_HAFF := a1 + d1*a3 + f1*a5

#effects of Uplifts on AFF through slope and intercept
dir_UAFF := a2
ind_UAFFslope := d2*a3
ind_UAFFintercept := f2*a5
tot_UAFF := a2 + d2*a3 +f2*a5

#effects of Hassles on PRA through slope and intercept
dir_HPRA := a1.2
ind_HPRAslope := d1*a3.2
ind_HPRAintercept := f1*a5.2
tot_HPRA := a1.2 + d1*a3.2 + f1*a5.2

#effects of Uplifts on PRA through slope and intercept
dir_UPRA := a2.2
ind_UPRAslope := d2*a3.2
ind_UPRAintercept := f2*a5.2
tot_UPRA := a2.2 + d2*a3.2 +f2*a5.2


'

# Estimate the SEM model
fit3 <- sem(SEM3, data=wide_data,
            meanstructure=TRUE, std.lv=TRUE)

# Summarize the SEM model results
summary(fit3, fit.measures=TRUE, standardized=TRUE)

modindices(fit2, sort.=TRUE, minimum.value=10)

fit_indices2 <- fitMeasures(fit2)
summary(fit_indices2)
