#import libraries
#library(tidyverse)
library(dplyr)
library(stats)

#import data
df <- read.csv('../data/census/all_vars_by_county.csv')

#select variables to include in pca
pca_vars <- c("unemployment_rate", "percent_poverty", "no_health_insurance",
              "home_owners", "percent_crowding", "no_car", "percent_vacant_units",
              "ed_percent_highschool", "has_internet", "percent_black",
              "percent_single_parent", "percent_less_35k", "percent_public_assist",
              "percent_mgmt_art_sci")

pca_df <- dplyr::select(df, pca_vars)

#convert percent estimates to actual percents
pca_df <- pca_df/100

pca_df.results <- prcomp(pca_df, center=TRUE, scale.=TRUE)
print(pca_df.results)

print(summary(pca_df.results))
f_loadings_full <-as.data.frame(pca_df.results$rotation)
f_loadings_full <- f_loadings_full['PC1'] * -1
result_full <- summary(pca_df.results)
print(result_full)
importance_full <- result_full$importance
colnames(f_loadings_full) <- c('factor_loadings_full')
variance_exp_full <- importance_full[3, 1]

#remove home_owners, percent_crowding, percent_vacant_units, percent_black,
#no_car, percent_public_assist, and percent_single_parent for low loadings
pca_df_2 <- select(pca_df, -c("home_owners", "percent_crowding", "no_car", "percent_vacant_units",
                              "percent_black","percent_public_assist","percent_single_parent"))


pca_df_2.results <- prcomp(pca_df_2, center=TRUE, scale.=TRUE)

factor_loadings <- as.data.frame(pca_df_2.results$rotation)
#transform so that higher scores == more deprivation
factor_loadings <- factor_loadings['PC1'] * -1
colnames(factor_loadings) <- c('factor_loadings_reduced')


result <- summary(pca_df_2.results)
print(result)
importance <- result$importance
variance_exp <- importance[3, 1]

##Save results of full and reduced PCA
sink('../data/census/PCA_results.txt')
cat('Full PCA\n')
f_loadings_full
cat('Full PCA percent of variance explained: ')
variance_exp_full

cat('\nReduced PCA\n')
factor_loadings
cat('\nPercent of variance explained: ')
variance_exp
sink()

#### build dataframe with factor loadings/weights ####
new_df <- pca_df_2
new_df['state_county_fips'] <- df['state_county_fips']
new_colnames <- c()
vars <- c(colnames(pca_df_2))
for(i in(1:length(vars))){
  
  new_col <- paste('weighted_',vars[i], sep='')
  new_colnames[i] <- new_col
  print(factor_loadings[vars[i], 'factor_loadings_reduced'])
  new_df[new_col] <- new_df[vars[i]] * factor_loadings[vars[i], 'factor_loadings_reduced']
}
write.csv(new_df, '../data/census/census_w_factor_weights.csv')
