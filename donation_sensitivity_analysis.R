# import libraries
library(tweedie)
library(statmod)
library(cplm)
library(MASS)
library(lmtest)

# set wd
# getwd()
# setwd("~/Desktop/crowdfunding/code/")

# import data
master <- read.csv('../cf_data_for_regressions.csv')

# convert categorical variables to factors
master$d_cardiovascular_diseases <- as.factor(master$d_cardiovascular_diseases)
master$d_endocrine_diseases <- as.factor(master$d_endocrine_diseases)
master$d_gastrointestinal_diseases <- as.factor(master$d_gastrointestinal_diseases)
master$d_genitourinary_diseases <- as.factor(master$d_genitourinary_diseases)
master$d_infections <- as.factor(master$d_infections)
master$d_injuries_and_external_causes <- as.factor(master$d_injuries_and_external_causes)
master$d_mental_and_substance_use_disorders <- as.factor(master$d_mental_and_substance_use_disorders)
master$d_musculoskeletal_diseases <- as.factor(master$d_musculoskeletal_diseases)
master$d_neoplasms <- as.factor(master$d_neoplasms)
master$d_nervous_system_diseases <- as.factor(master$d_nervous_system_diseases)
master$d_respiratory_diseases <- as.factor(master$d_respiratory_diseases)

# relevel NDI quartile variable so that quartile 4 is reference group
master$ndi_quantile <- as.factor(master$ndi_quantile)
master$ndi_quantile <- relevel (master$ndi_quantile, ref="4")

# check data structure
str(master)

# Negative binomial model of donation count
# Using untransformed goal amount
nb_model_raw <- glm.nb(donation_count ~ d_cardiovascular_diseases+
               d_endocrine_diseases+ 
               d_gastrointestinal_diseases+ 
               d_genitourinary_diseases+ 
               d_infections+ 
               d_injuries_and_external_causes+ 
               d_mental_and_substance_use_disorders+
               d_musculoskeletal_diseases+ 
               d_neoplasms+ 
               d_nervous_system_diseases+ 
               d_respiratory_diseases+
               year_int+
               goal_amount +
               ndi_quantile,
             data = master, maxit=1000)
# Using box-cox transformed goal amount
nb_model_boxcox <- glm.nb(donation_count ~ d_cardiovascular_diseases+
                         d_endocrine_diseases+ 
                         d_gastrointestinal_diseases+ 
                         d_genitourinary_diseases+ 
                         d_infections+ 
                         d_injuries_and_external_causes+ 
                         d_mental_and_substance_use_disorders+
                         d_musculoskeletal_diseases+ 
                         d_neoplasms+ 
                         d_nervous_system_diseases+ 
                         d_respiratory_diseases+
                         year_int+
                         goal_amount_boxcox +
                         ndi_quantile,
                       data = master, maxit=1000)

summary(nb_model_raw)
summary(nb_model_boxcox)

# Compute poisson model of donation count
# Using untransformed goal amount
poisson_model_raw <- glm(donation_count ~ d_cardiovascular_diseases+
                                  d_endocrine_diseases+ 
                                  d_gastrointestinal_diseases+ 
                                  d_genitourinary_diseases+ 
                                  d_infections+ 
                                  d_injuries_and_external_causes+ 
                                  d_mental_and_substance_use_disorders+
                                  d_musculoskeletal_diseases+ 
                                  d_neoplasms+ 
                                  d_nervous_system_diseases+ 
                                  d_respiratory_diseases+
                                  year_int+
                                  goal_amount +
                                  ndi_quantile,
                                data = master, family="poisson")
# Using box-cox transformed goal amount
poisson_model_boxcox <- glm(donation_count ~ d_cardiovascular_diseases+
                           d_endocrine_diseases+ 
                           d_gastrointestinal_diseases+ 
                           d_genitourinary_diseases+ 
                           d_infections+ 
                           d_injuries_and_external_causes+ 
                           d_mental_and_substance_use_disorders+
                           d_musculoskeletal_diseases+ 
                           d_neoplasms+ 
                           d_nervous_system_diseases+ 
                           d_respiratory_diseases+
                           year_int+
                           goal_amount_boxcox +
                           ndi_quantile,
                         data = master, family="poisson")
summary(poisson_model_raw)
summary(poisson_model_boxcox)

# Likelihood ratio test between negative binomial and poisson models
# Raw
lrtest(poisson_model_raw, nb_model_raw)
# Boxcox
lrtest(poisson_model_boxcox, nb_model_boxcox)

#---------------------------------------------------------------------------
# For both raw and box-cox:
# likelihood is higher for NB
# likelihood ratio test is significant
#---------------------------------------------------------------------------

# Gamma model of mean donation amount
# Using untransformed goal amount
gamma_model_raw <- glm(donation_mean ~ d_cardiovascular_diseases+
                      d_endocrine_diseases+ 
                      d_gastrointestinal_diseases+ 
                      d_genitourinary_diseases+ 
                      d_infections+ 
                      d_injuries_and_external_causes+ 
                      d_mental_and_substance_use_disorders+
                      d_musculoskeletal_diseases+ 
                      d_neoplasms+ 
                      d_nervous_system_diseases+ 
                      d_respiratory_diseases+
                      year_int+
                      goal_amount +
                      ndi_quantile,
                    data = master[master$donation_mean > 0,], family=Gamma(link="log"))
# Using box-cox transformed goal amount
gamma_model_boxcox <- glm(donation_mean ~ d_cardiovascular_diseases+
                    d_endocrine_diseases+ 
                    d_gastrointestinal_diseases+ 
                    d_genitourinary_diseases+ 
                    d_infections+ 
                    d_injuries_and_external_causes+ 
                    d_mental_and_substance_use_disorders+
                    d_musculoskeletal_diseases+ 
                    d_neoplasms+ 
                    d_nervous_system_diseases+ 
                    d_respiratory_diseases+
                    year_int+
                    goal_amount_boxcox +
                    ndi_quantile,
                  data = master[master$donation_mean > 0,], family=Gamma(link="log"))

summary(gamma_model_raw)
summary(gamma_model_boxcox)
