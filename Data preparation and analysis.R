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

####Import data
(data<- import("reslongshort.csv", na.strings="") %>% as_tibble())

##Data umputation for missing data
imputation_model <- mice(data = data, method = "pmm", m = 5)
imputed_data <- complete(imputation_model)

#Put data in correct form and type
imputed_data$TIME <- as.numeric(imputed_data$TIME)
imputed_data$REST_ <- as.numeric(imputed_data$REST_)
imputed_data$DHAST_ <- as.numeric(imputed_data$DHAST_)
imputed_data$DUPT_ <- as.numeric(imputed_data$DUPT_)
imputed_data$DSUPT_ <- as.numeric(imputed_data$DUPT_)
imputed_data$DSHAT_ <- as.numeric(imputed_data$DUPT_)



##Create a wide datset also
library(reshape2)

wide_data <- reshape(imputed_data, idvar = "id", timevar = "TIME", direction = "wide")


###############################################Compute the intercept and the slope######################################################################
##Create a variable for individual resilience intercept slope

library(dplyr)
library(broom)

#Creat variables for Slope and intercept by id
library(lme4)

# Assuming your data is stored in a data frame called 'data'
# 'id' represents the individual's identifier, and REST_.1 to REST_.4 are the repeated measures

# Fit a linear mixed-effects model
model <- lmer(REST_ ~ TIME + (1 + TIME | id), data = imputed_data)

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


###Correlation comparison of coefficients of linear vesus cubic


correlation <- cor(wide_data2$Slope.x, wide_data2$Slope.y)
correlation <- cor(wide_data2$Intercept.x, wide_data2$Intercept.y)

# Print the correlation coefficient
print(correlation)


###########################################################################
############Investigating Hypothesis 1: Employee resilience capacities  will change over time  
#AND Investigating Hypothesis 2: Higher individual initial values of resilience (intercepts) will be negatively be related with resileince change (random slopes) over time 
####Using a Stepwise Approach- Bliese and Ployhart 2002- Growth_Modeling_Using_Random_Coefficient_Models################################################################
library(nlme)
#Step 1: Baseline model
model.1<-gls(REST_~TIME, data= imputed_data)
summary (model.1)

#Step 2: Allow random intercepts
model.2<-lme(REST_~TIME, random=~1|id, data= imputed_data)
summary (model.2)

##Contrast both moels using an ANOVA
anova(model.1, model.2)

#Step 3: Slope variation amongst Participants
model.3<-lme(REST_~TIME, random=~1+TIME|id, data= imputed_data)
summary(model.3)

##Contrast both models using an ANOVA
anova(model.2, model.3)
VarCorr(model.3)

#Step 4: Do the residuals show evidence of heteroscedasticity and autocorrelation?
#Short hand: model.4a<-update(model.3, correlation=corAR1())
model.4<-lme(REST_~TIME, random=~1+TIME|id, correlation=corAR1(),data= imputed_data)
summary(model.4, weights=varExp(form=~TIME))
anova(model.3, model.4)

#Step 5: We can test further restrictions on the error-variance matrix by examining whether the errors associated with job satisfaction are homoscedastic across time.
model.5<-update(model.3, weights=varExp(form=~TIME))
#model.4a<-lme(REST_~TIME, random=~1+TIME|id, correlation=corAR1(),data= imputed_data)
summary(model.5, weights=varExp(form=~TIME))
anova(model.3, model.5)

#Step 6. Linear vs. quadratic growth
model.6<-update(model.3, weights=varExp(form=~TIME^2))
summary(model.6)
#model.6<-lme(REST_~TIME, random=~1+TIME|id, weights=varExp(form=~TIME^2), data= imputed_data)
anova(model.3, model.6)



#Step 7. Linear vs. Cubic growth
model.7<-update(model.3, weights=varExp(form=~TIME^3))
summary(model.7)
#model.7<-lme(REST_~TIME, random=~1+TIME|id, weights=varExp(form=~TIME^3), data= imputed_data)

anova(model.3, model.7)



anova(model.6, model.7)


#Recheck evidence of heteroscedasticity and autocorrelation?

model.4e<-lme(REST_~TIME, random=~1+TIME|id, weights=varExp(form=~TIME^3),correlation=corAR1(), data= imputed_data)
summary(model.4e)
anova(model.4d, model.4e)
#No imporved model fit!


##########COMBINED MODEL- Resilience slop AND intercept will have a significant mediation effect of daily Hassles and uplifts relationship on Proactivity and Affect
#Hypotheses 3 & 4
# Define the SEM model for Hassles and Uplifts
SEM3 <- '
  # Define regression paths
  ABALT_.4 ~ a1*DHAST_.1 + a2*DUPT_.1 + a3*Slope  +a5*Intercept 
 PRAT_.4 ~ a1.2*DHAST_.1 + a2.2*DUPT_.1 + a3.2*Slope  +a5.2*Intercept

  # Define mediation paths
    Slope ~ d1*DHAST_.1 + d2*DUPT_.1
    Intercept ~ f1*DHAST_.1 + f2*DUPT_.1

  # Define residual variances and covariances
   DHAST_.1~~DUPT_.1
   ABALT_.4~~PRAT_.4
  #Intercept~~Slope
  
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
   DHAST_.2~~DUPT_.2
   ABALT_.4~~PRAT_.4
  #Intercept~~Slope
  
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

