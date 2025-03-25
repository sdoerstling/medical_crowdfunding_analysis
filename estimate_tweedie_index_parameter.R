# import libraries
library(tweedie)

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

# estimate Tweedie index parameter
# (Took about 15 minutes)
# profile <- tweedie.profile(current_amount ~ d_cardiovascular_diseases+
#                   d_endocrine_diseases+
#                   d_gastrointestinal_diseases+
#                   d_genitourinary_diseases+
#                   d_infections+
#                   d_injuries_and_external_causes+
#                   d_mental_and_substance_use_disorders+
#                   d_musculoskeletal_diseases+
#                   d_neoplasms+
#                   d_nervous_system_diseases+
#                   d_respiratory_diseases+
#                   year_int+
#                   goal_amount_boxcox +
#                   ndi_quantile,
#                 data = master,
#                 p.vec=seq(1.1,2,0.1), method='series', do.ci=F)

#Output for reference:

#$p.max
#[1] 1.718367

#$L
#[1]      -Inf      -Inf      -Inf -866009.4 -859734.2 -856060.0 -854430.5 -855324.5        NA

#$p
#[1] 1.1 1.2 1.3 1.4 1.5 1.6 1.7 1.8 1.9

#export plot
#pdf("estimating_p.pdf", height = 4, width = 5)
#plot(t$x, t$y, xlab="Index parameter (p)", ylab = "Log-likelihood", type="p")
#dev.off()


# Tweedie GLM of amount raised
#tweedie_model <- glm(current_amount ~ d_cardiovascular_diseases+
#                       d_endocrine_diseases+ 
#                       d_gastrointestinal_diseases+ 
#                       d_genitourinary_diseases+ 
#                       d_infections+ 
#                       d_injuries_and_external_causes+ 
#                       d_mental_and_substance_use_disorders+
#                       d_musculoskeletal_diseases+ 
#                       d_neoplasms+ 
#                       d_nervous_system_diseases+ 
#                       d_respiratory_diseases+
#                       year_int+
#                       goal_amount_boxcox +
#                       ndi_quantile,
#                     data = master,
#                     family = tweedie(var.power=1.72, link.power=0))

#summary(tweedie_model)