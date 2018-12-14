# Breast cancer screening compliance: paper SMMR
#
rm(list=ls())
gc()
#setwd('C:/Users/fa15/Dropbox/ats_paper_1') # office
setwd('C:/Users/Fede//Dropbox/ats_paper_1') # home

# packages
library(readr) # read
library(tidyverse) # process
library(lubridate) # for dates
library(lme4) # model
library(brms) # bayesian modelling with stan
library(merTools) # intervals & co
library(caret) # for cross-validation
  
# read in data & functions
source('Code/tools.R')
source('Code/data_in.R')

# select subcohort
source('Code/create_subcohort.R')

# define variables
source('Code/refining.R')
#save.image('data.RData')

# timeline plots
# timeline_plot(id_list=c(...), dframe=ss)

# reduced dataframe for analyses
d <- ss %>% 
  mutate(death=ifelse(is.na(data_mor),'No','Yes'),
         diagnosis=ifelse(is.na(incidenza),'No','Yes')) %>% 
  dplyr::select(id,italian,partnered,
                immigration,emigration,
                invitation_n,
                any_reminder_lag1,any_recent_lag1,any_special_lag1,
                any_cvd,any_diabetes,any_tumor,
                any_neuro,
                any_emigration,
                any_compliance,death,diagnosis)
d$any_compliance <- as.integer(d$any_compliance)
# 334587x17

#save.image('data_clean.RData')
