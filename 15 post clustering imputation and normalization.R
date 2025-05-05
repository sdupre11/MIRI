#All Kosovo clustering occurs prior to this step using the fmir dataframe at this point. This was a conscious decision to minimize the influence of 
#imputation on the clustering. This can be altered if desired and fmir_imputations_preprocessed used instead of this version of fmir for the clustering.

#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
### Voting data CAR imputation backwards interpolation####

KKU_CAR_means <- fmir %>% 
  filter(indicator_name == "sub_obj_1_3_iidea_voter_turnout") %>% 
  filter(mcp_grouping == "CARs" & (!(country %in% c("Turkmenistan", 
                                                    "Tajikistan")))) %>% 
  group_by(year) %>% 
  summarize(CAR_mean = mean(values))

print(KKU_CAR_means)

fmir <- fmir %>%
  mutate(
    values = case_when(
      indicator_name == "sub_obj_1_3_iidea_voter_turnout" & country %in% c("Turkmenistan", "Tajikistan") & year == 2018 ~ 64.0, 
      indicator_name == "sub_obj_1_3_iidea_voter_turnout" & country %in% c("Turkmenistan", "Tajikistan") & year == 2019 ~ 64.0,  
      indicator_name == "sub_obj_1_3_iidea_voter_turnout" & country %in% c("Turkmenistan", "Tajikistan") & year == 2020 ~ NA,  
      indicator_name == "sub_obj_1_3_iidea_voter_turnout" & country %in% c("Turkmenistan", "Tajikistan") & year == 2021 ~ NA,  
      indicator_name == "sub_obj_1_3_iidea_voter_turnout" & country %in% c("Turkmenistan", "Tajikistan") & year == 2022 ~ NA,  
      indicator_name == "sub_obj_1_3_iidea_voter_turnout" & country %in% c("Turkmenistan", "Tajikistan") & year == 2023 ~ NA,  
      TRUE ~ values
    )
  )

#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
### Starting basic last-value-forward and first-value-backwards imputation process ####
###
# Function to perform imputation on a single row
impute_row <- function(row) {
  row <- as.numeric(row)  # Convert to numeric
  non_na_indices <- which(!is.na(row))
  
  if (length(non_na_indices) == 0) {
    return(row)  # Return unchanged if all values are NA
  }
  
  # Backward-fill leading NAs
  if (min(non_na_indices) > 1) {
    row[1:(min(non_na_indices) - 1)] <- row[min(non_na_indices)]
  }
  
  # Forward-fill trailing NAs
  if (max(non_na_indices) < length(row)) {
    row[(max(non_na_indices) + 1):length(row)] <- row[max(non_na_indices)]
  }
  
  return(row)
}

###

fmir_imputations <- fmir %>%
  pivot_wider(
    names_from = year,
    values_from = values,
    names_sort = TRUE
  )


#Apply the imputation row by row
year_columns <- grep("^[0-9]{4}$", names(fmir_imputations))  # Select year columns
fmir_imputations[year_columns] <- t(apply(fmir_imputations[year_columns], 1, impute_row))

#Pivot back to narrow format
fmir_imputations <- fmir_imputations %>%
  pivot_longer(
    cols = starts_with("2"),  # Select year columns for pivoting
    names_to = "year",
    values_to = "values",
    values_drop_na = FALSE
  ) %>%
  mutate(year = as.numeric(year))  # Ensure year is numeric


# Count total number of NAs in the wider dataset
total_NAs <- sum(is.na(fmir))

# View the wider dataset and the count of NAs
print(fmir_imputations)
cat("Total number of NAs:", total_NAs, "\n")


fmir_imputations %>% 
  write_csv(file = "data/fmir_backup/fmir_imputations_backup.csv")


#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
### Check NAs and tallies ####

fmir_imputations %>% 
  # pivot_longer(cols = c(`2018`,`2019`,`2020`,`2021`,`2022`,`2023`),
  #              names_to = "year",
  #              values_to = "values") %>%
  filter(is.na(values)) %>% 
  group_by(indicator_name) %>%
  tally() %>%
  mutate(perc_missing = round(((n/258)*100),1)) %>%
  select(-n) %>%
  arrange(desc(perc_missing)) %>%
  ungroup() %>%
  print(n=50)  

#Missing percentage
#EUCOMs - 88.4% -> 60.5%
#Basel AML - 83.3-83.7% -> 2.3%
#KNOMAD - 83.3% -> 0%
#IREX - 52.3-75.6% -> 25.6-60.5%
#UN DESA - 75.6% -> 25.6%
#BTI - 67.4-74.8% -> 56.6-65.5%
#WBG External debt - 71.3% -> 65.1%


fmir_imputations %>%
  filter(is.na(values)) %>% 
  ungroup() %>%
  tally() %>%
  mutate(present = 1-(n/16770)) %>%
  print() 

#5,146 missing from 16,770 - 69.3% present -> 2,759 - 83.5%

one_or_two_year_data <- c("sub_obj_2_3_eucom_malign_media_penetration",
                          "sub_obj_2_3_eucom_western_media_penetration",
                          "sub_obj_4_2_basel_financial_transparency",
                          "sub_obj_4_3_knomad_bilateral_fmi_remittances",
                          "sub_obj_4_3_undesa_migrant_opportunities")

fmir_imputations %>%
  filter(!indicator_name %in% one_or_two_year_data) %>%
  ungroup() %>%
  tally()

fmir_imputations %>%
  filter(!indicator_name %in% one_or_two_year_data) %>%
  filter(is.na(values)) %>% 
  ungroup() %>%
  tally() %>%
  mutate(present = 1-(n/14964)) %>%
  print() 

#3,634 missings from 14,964 - 75.7% present without those -> 2,375 - 84.1%

#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
### Linear interpolation ####

fmir_imputations_linear_imputation <- fmir_imputations %>%
  pivot_wider(names_from = year,
              values_from = values)

fmir_imputations_linear_imputation[, c("2018", "2019", "2020", "2021", "2022", "2023")] <- t(apply(fmir_imputations_linear_imputation[, c("2018", "2019", "2020", "2021", "2022", "2023")], 1, function(x) {
  na.approx(x, na.rm = FALSE)  # Linear interpolation, keeping NAs if there is no value to interpolate
}))

fmir_imputations_preprocessed <- fmir_imputations_linear_imputation

#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
### Check NAs and tallies ####

fmir_imputations_preprocessed %>% 
  pivot_longer(cols = c(`2018`,`2019`,`2020`,`2021`,`2022`,`2023`),
               names_to = "year",
               values_to = "values") %>%
  filter(is.na(values)) %>% 
  group_by(indicator_name) %>%
  tally() %>%
  mutate(perc_missing = round(((n/258)*100),1)) %>%
  select(-n) %>%
  arrange(desc(perc_missing)) %>%
  ungroup() %>%
  print(n=50)  

#Missing percentage
#EUCOMs - 88.4% -> 60.5% -> 60.5%
#Basel AML - 83.3-83.7% -> 2.3% -> 2.3%
#KNOMAD - 83.3% -> 0% -> 0%
#IREX - 52.3-75.6% -> 25.6-60.5% -> 25.6-60.5%
#UN DESA - 75.6% -> 25.6% -> 25.6%
#BTI - 67.4-74.8% -> 56.6-65.5% -> 35.9-48.8%
#WBG External debt - 71.3% -> 65.1% -> 65.1%


fmir_imputations_preprocessed %>%
  pivot_longer(cols = c(`2018`,`2019`,`2020`,`2021`,`2022`,`2023`),
               names_to = "year",
               values_to = "values") %>%
  tally()

fmir_imputations_preprocessed %>%
  pivot_longer(cols = c(`2018`,`2019`,`2020`,`2021`,`2022`,`2023`),
               names_to = "year",
               values_to = "values") %>%
  filter(is.na(values)) %>% 
  ungroup() %>%
  tally() %>%
  mutate(present = 1-(n/16770)) %>%
  print() 

#5,146 missing from 16,770 - 69.3% present ->  2,759 - 83.5% -> 2,268 - 86.5%

one_or_two_year_data <- c("sub_obj_2_3_eucom_malign_media_penetration",
                          "sub_obj_2_3_eucom_western_media_penetration",
                          "sub_obj_4_2_basel_financial_transparency",
                          "sub_obj_4_3_knomad_bilateral_fmi_remittances",
                          "sub_obj_4_3_undesa_migrant_opportunities")

fmir_imputations_preprocessed %>%
  pivot_longer(cols = c(`2018`,`2019`,`2020`,`2021`,`2022`,`2023`),
               names_to = "year",
               values_to = "values") %>%
  filter(!indicator_name %in% one_or_two_year_data) %>%
  ungroup() %>%
  tally()

fmir_imputations_preprocessed %>%
  pivot_longer(cols = c(`2018`,`2019`,`2020`,`2021`,`2022`,`2023`),
               names_to = "year",
               values_to = "values") %>%
  filter(!indicator_name %in% one_or_two_year_data) %>%
  filter(is.na(values)) %>% 
  ungroup() %>%
  tally() %>%
  mutate(present = 1-(n/14964)) %>%
  print() 

#3,634 missings from 14,964 - 75.7% present without those -> 2,375 - 84.1% -> 1,884 - 87.4%

#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
### Regional imputation and Clustering ####

## Voting data CAR regional imputation ####

KKU_CAR_means <- fmir_imputations_preprocessed %>%
  pivot_longer(cols = `2018`:`2023`, names_to = "year", values_to = "values") %>%
  filter(indicator_name == "sub_obj_1_3_iidea_voter_turnout") %>%
  filter(mcp_grouping == "CARs" & (!(country %in% c("Turkmenistan", 
                                                    "Tajikistan")))) %>% 
  group_by(year) %>% 
  summarize(CAR_mean = mean(values))

print(KKU_CAR_means)

fmir_imputations_preprocessed <- fmir_imputations_preprocessed %>%
  pivot_longer(cols = `2018`:`2023`, names_to = "year", values_to = "values") %>%
  mutate(
    values = case_when(
      indicator_name == "sub_obj_1_3_iidea_voter_turnout" & country %in% c("Turkmenistan", "Tajikistan") & year == 2018 ~ values, 
      indicator_name == "sub_obj_1_3_iidea_voter_turnout" & country %in% c("Turkmenistan", "Tajikistan") & year == 2019 ~ values,  
      indicator_name == "sub_obj_1_3_iidea_voter_turnout" & country %in% c("Turkmenistan", "Tajikistan") & year == 2020 ~ 64.0,  
      indicator_name == "sub_obj_1_3_iidea_voter_turnout" & country %in% c("Turkmenistan", "Tajikistan") & year == 2021 ~ 57.7,  
      indicator_name == "sub_obj_1_3_iidea_voter_turnout" & country %in% c("Turkmenistan", "Tajikistan") & year == 2022 ~ 56.3,  
      indicator_name == "sub_obj_1_3_iidea_voter_turnout" & country %in% c("Turkmenistan", "Tajikistan") & year == 2023 ~ 54.4,  
      TRUE ~ values
    )
  )


regional <- fmir_imputations_preprocessed %>%
  # left_join(mcp_grouping,
  #           by="country") %>%
  # pivot_longer(cols = `2018`:`2023`, names_to = "year", values_to = "values") %>%
  filter(!is.na(values)) %>%
  group_by(indicator_name,
           year,
           mcp_grouping) %>%
  summarize(regional_average=mean(values))

regional_ready <- fmir_imputations_preprocessed %>%
  # left_join(mcp_grouping,
  #           by="country") %>%
  # pivot_longer(cols = `2018`:`2023`, names_to = "year", values_to = "values") %>%
  left_join(regional,
            by=c("indicator_name",
                 "year",
                 "mcp_grouping"))


#Clusters are derived in `13 clustering.R`
#Clusters defined using multivariate time-series clustering with 
#dynamic time warping and Ward's method. Standardization is achieved
#using z-score standardization following imputation of missing values
#with indicator-year means.

clusteredA <- c("Armenia",
                "BiH",
                #"Bulgaria", removed 1/27/25 following intro of new variables
                "Georgia",
                "Moldova")


clusteredB <- c("Albania",
                "Armenia",
                "Moldova",
                "Montenegro",
                "Ukraine" # new as of 1/27/25 following intro of new variables
)

clusteredC <- c("Montenegro",
                "Albania",
                "BiH",
                "N. Macedonia",
                "Serbia")

clusteredD <- c("Albania", 
                "BiH", 
                "Croatia",
                "Ireland", # new as of 1/27/25 following intro of new variables
                "Luxembourg", # new as of 1/27/25 following intro of new variables
                #"Montenegro", removed 1/27/25 following intro of new variables
                "N. Macedonia",
                "Serbia")

clusterA <- fmir_imputations_preprocessed %>%
  filter(country %in% clusteredA) %>%
  filter(!is.na(values)) %>%
  group_by(indicator_name,
           year) %>%
  summarize(clusterA_average=mean(values))

clusterB <- fmir_imputations_preprocessed %>%
  filter(country %in% clusteredB) %>%
  filter(!is.na(values)) %>%
  group_by(indicator_name,
           year) %>%
  summarize(clusterB_average=mean(values))

clusterC <- fmir_imputations_preprocessed %>%
  filter(country %in% clusteredC) %>%
  filter(!is.na(values)) %>%
  group_by(indicator_name,
           year) %>%
  summarize(clusterC_average=mean(values))

clusterD <- fmir_imputations_preprocessed %>%
  filter(country %in% clusteredD) %>%
  filter(!is.na(values)) %>%
  group_by(indicator_name,
           year) %>%
  summarize(clusterD_average=mean(values))


cluster_all <- clusterA %>%
  left_join(clusterB,
            by=c("indicator_name",
                 "year")) %>%
  left_join(clusterC,
            by=c("indicator_name",
                 "year")) %>%
  left_join(clusterD,
            by=c("indicator_name",
                 "year")) 

cluster_all$cluster <- "X"

cluster_all$cluster <- ifelse(grepl("^sub_obj_1", cluster_all$indicator_name), "A",
                              ifelse(grepl("^sub_obj_2", cluster_all$indicator_name), "B",
                                     ifelse(grepl("^sub_obj_3", cluster_all$indicator_name), "C",
                                            ifelse(grepl("^sub_obj_4", cluster_all$indicator_name), "D", "OTHER"))))


cluster_ready <- cluster_all %>%
  mutate(
    cluster_average = case_when(
      cluster == "A" ~ clusterA_average,
      cluster == "B" ~ clusterB_average,
      cluster == "C" ~ clusterC_average,
      cluster == "D" ~ clusterD_average,
      TRUE ~ NA
    )
  ) %>%
  select(c("indicator_name",
           "year",
           "cluster_average"))

comparison_ready <- regional_ready %>%
  group_by(year,
           indicator_name,
           mcp_grouping) %>%
  summarize(regional_average = mean(regional_average), na.rm = TRUE) %>%
  left_join(cluster_ready,
            by = c("indicator_name",
                   "year"))

comparison_ready$cluster <- "X"

comparison_ready$cluster <- ifelse(grepl("^sub_obj_1", comparison_ready$indicator_name), "A",
                                   ifelse(grepl("^sub_obj_2", comparison_ready$indicator_name), "B",
                                          ifelse(grepl("^sub_obj_3", comparison_ready$indicator_name), "C",
                                                 ifelse(grepl("^sub_obj_4", comparison_ready$indicator_name), "D", "OTHER"))))

#/////////////////////////

plot(comparison_ready$regional_average, comparison_ready$cluster_average, 
     main = "Scatterplot of Regional Average vs Cluster Average",
     xlab = "Regional Average", 
     ylab = "Cluster Average", 
     pch = 19,        # Point style
     col = "blue")    # Point color


abline(lm(cluster_average ~ regional_average, data = comparison_ready), col = "red")

#/////////////////////////

colors <- as.factor(comparison_ready$mcp_grouping)
plot(comparison_ready$regional_average, comparison_ready$cluster_average, 
     col = colors, 
     main = "Scatterplot Colored by MCP Grouping with Regression Lines",
     xlab = "Regional Average", 
     ylab = "Cluster Average", 
     pch = 19)

# Split the data by mcp_grouping and add a regression line for each group with matching colors
lapply(split(comparison_ready, comparison_ready$mcp_grouping), function(group) {
  color <- as.numeric(as.factor(group$mcp_grouping)[1])
  abline(lm(cluster_average ~ regional_average, data = group), col = color)
})

# Add a legend to identify the mcp_grouping colors
legend("topright", legend = levels(as.factor(comparison_ready$mcp_grouping)), 
       col = 1:length(levels(as.factor(comparison_ready$mcp_grouping))), 
       pch = 19)

#/////////////////////////

colors <- as.factor(comparison_ready$cluster)
plot(comparison_ready$regional_average, comparison_ready$cluster_average, 
     col = colors, 
     main = "Scatterplot Colored by Cluster with Regression Lines",
     xlab = "Regional Average", 
     ylab = "Cluster Average", 
     pch = 19)

# Split the data by cluster and add a regression line for each group with matching colors, handling missing data
lapply(split(comparison_ready, comparison_ready$cluster), function(group) {
  if (nrow(group[!is.na(group$regional_average) & !is.na(group$cluster_average),]) > 1) {
    color <- as.numeric(as.factor(group$cluster)[1])
    abline(lm(cluster_average ~ regional_average, data = group), col = color)
  }
})

# Add a legend to identify the cluster colors
legend("topright", legend = levels(as.factor(comparison_ready$cluster)), 
       col = 1:length(levels(as.factor(comparison_ready$cluster))), 
       pch = 19)


#/////////////////////////


imputation_testing_calculations <- comparison_ready %>%
  select(-c("na.rm",
            "cluster"
  ))

imputation_testing_calculations2 <- fmir_imputations_preprocessed %>%
  left_join(imputation_testing_calculations,
            by=c("indicator_name",
                 "year",
                 "mcp_grouping")) %>%
  rename(
    original_values = values
  ) %>%
  mutate(
    values = case_when(
      mcp_grouping %in% c("EU-15") & is.na(original_values) ~ original_values,
      country == "Kosovo" & is.na(original_values) & !is.na(cluster_average) ~ cluster_average,
      (!(mcp_grouping  %in% c("EU-15"))) & country != "Kosovo" & is.na(original_values) & !is.na(regional_average) ~ regional_average,
      TRUE ~ original_values
    )
  )

fmir_imputed <- imputation_testing_calculations2 %>%
  select(-c("regional_average",
            "original_values",
            "cluster_average"
  )) %>%
  mutate(year = as.numeric(year))


#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
fmir_backup <- fmir

fmir <- fmir_imputed


#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
### Check NAs and tallies ####

fmir %>% 
  filter(is.na(values)) %>% 
  group_by(indicator_name) %>%
  tally() %>%
  mutate(perc_missing = round(((n/258)*100),1)) %>%
  select(-n) %>%
  arrange(desc(perc_missing)) %>%
  ungroup() %>%
  print(n=50)  

#Missing percentage
#EUCOMs - 88.4% -> 60.5% -> 60.5% -> 46.5%
#Basel AML - 83.3-83.7% -> 2.3% -> 2.3% -> 0%
#KNOMAD - 83.3% -> 0% -> 0% -> 0%
#IREX - 52.3-75.6% -> 25.6-60.5% -> 25.6-60.5% -> 34.9%
#UN DESA - 75.6% -> 25.6% -> 25.6% -> 11.6%
#BTI - 67.4-74.8% -> 56.6-65.5% -> 35.9-48.8% -> 34.9%
#WBG External debt - 71.3% -> 65.1% -> 65.1% -> 34.9%

#It's all EU-15s missing, with the exception of EUCOM data for the CAR countries

#As of 1/27/25 variable addition, we also have at this stage:
#FHI - 34.9% (EU-15)
#RISE - 2.3% (Luxembourg)
#Atlas - 2.3% (Luxembourg)


fmir %>%
  tally()

fmir %>%
  filter(is.na(values)) %>% 
  ungroup() %>%
  tally() %>%
  mutate(present = 1-(n/16770)) %>%
  print() 

#5,146 missing from 16,770 - 69.3% present ->  2,759 - 83.5% -> 2,268 - 86.5% -> 1,458 - 91.3%

#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////

fmir <- fmir %>%
  mutate(values = case_when(
    country == "Belarus" & indicator_name == "sub_obj_2_1_irex_professional_journalism" ~ 0,
    TRUE ~ values
  ))


#Checking that all country-year-indicator combinations have full representation

fmir %>% 
  group_by(indicator_name) %>% 
  tally() %>% 
  filter(n!=258)

#For cases where NAs remain, these should be uniformly CARs for EUCOM data and EU-15 countries (and Belarus for media quality). 
#These will be set to the annual indicator mean for standardization and then replaced with the sub-objective mean
#for that sub-objective for that country.

remainingNAs <- fmir %>%
  filter(is.na(values)) %>%
  group_by(country, indicator_name) %>%
  tally() %>%
  select(-n)

indicator_means <- fmir %>%
  filter(!is.na(values)) %>%
  group_by(indicator_name,
           year) %>%
  summarize(indicator_mean = mean(values))

update_rows <- fmir %>%
  inner_join(remainingNAs, by = c("country",
                                  "indicator_name")) %>%
  left_join(indicator_means, by = c("indicator_name",
                                    "year"))

fmir <- fmir %>%
  left_join(update_rows %>%
              select(country, year, indicator_name, indicator_mean),
            by = c("country", "year", "indicator_name")) %>%
  mutate(values = ifelse(is.na(values), indicator_mean, values)) %>%
  select(-indicator_mean) 



#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
### Check NAs and tallies ####

fmir %>% 
  filter(is.na(values)) %>% 
  group_by(indicator_name) %>%
  tally() %>%
  mutate(perc_missing = round(((n/258)*100),1)) %>%
  select(-n) %>%
  arrange(desc(perc_missing)) %>%
  ungroup() %>%
  print(n=50)  

#Missing percentage
#EUCOMs - 88.4% -> 60.5% -> 60.5% -> 46.5% -> 0%
#Basel AML - 83.3-83.7% -> 2.3% -> 2.3% -> 0% -> 0%
#KNOMAD - 83.3% -> 0% -> 0% -> 0% -> 0%
#IREX - 52.3-75.6% -> 25.6-60.5% -> 25.6-60.5% -> 34.9% -> 0%
#UN DESA - 75.6% -> 25.6% -> 25.6% -> 11.6% -> 0%
#BTI - 67.4-74.8% -> 56.6-65.5% -> 35.9-48.8% -> 34.9% -> 0%
#WBG External debt - 71.3% -> 65.1% -> 65.1% -> 34.9% -> 0%

#As of 1/27/25...

#FHI - . . . 34.9% -> 0%
#RISE - . . . 2.3% -> 0%
#Atlas - . . . 2.3% -> 0%

#No remaining NAs in the data


#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
### get indicator_standardized_values ####

backup <- fmir

fmir <- fmir %>% 
  group_by(indicator_name) %>%
  mutate(indicator_max = max(values, na.rm = TRUE),
         indicator_min = min(values, na.rm = TRUE),
         indicator_good_frontier_flag = case_when(high_value_is_good_outcome_flag == 1 & values == indicator_max ~ 1,
                                                  high_value_is_good_outcome_flag == 0 & values == indicator_min ~ 1,
                                                  TRUE ~ 0),
         indicator_bad_frontier_flag = case_when(high_value_is_good_outcome_flag == 1 & values == indicator_min ~ 1,
                                                 high_value_is_good_outcome_flag == 0 & values == indicator_max ~ 1,
                                                 TRUE ~ 0),
         indicator_mean = mean(values, na.rm = TRUE),
         indicator_sd = sd(values, na.rm = TRUE),
         indicator_median = median(values, na.rm = TRUE),
         indicator_iqr = IQR(values, na.rm = TRUE),
         indicator_midpoint = ((indicator_max - indicator_min) / 2) + indicator_min,
         values_z_std = (values - indicator_mean) / indicator_sd,
         values_rob_std = (values - indicator_median) / (indicator_iqr + .0001),
         indicator_standardized_values = case_when(
           high_value_is_good_outcome_flag == 1 ~ (values_z_std - (-1.5)) / (1.5 - (-1.5)),
           high_value_is_good_outcome_flag == 0 ~ 1 - (values_z_std - (-1.5)) / (1.5 - (-1.5))),
         indicator_standardized_values = case_when(indicator_standardized_values > 1 ~ 1,
                                                   indicator_standardized_values < 0 ~ 0,
                                                   TRUE ~ indicator_standardized_values),
         indicator_rob_standardized_values = case_when(
           high_value_is_good_outcome_flag == 1 ~ (values_rob_std - (-1.5)) / (1.5 - (-1.5)),
           high_value_is_good_outcome_flag == 0 ~ 1 - (values_rob_std - (-1.5)) / (1.5 - (-1.5))),
         indicator_rob_standardized_values = case_when(indicator_rob_standardized_values > 1 ~ 1,
                                                       indicator_rob_standardized_values < 0 ~ 0,
                                                       TRUE ~ indicator_rob_standardized_values),
         indicator_normalized_values = case_when(high_value_is_good_outcome_flag == 1 ~ (values - indicator_min) /
                                                   (indicator_max - indicator_min),
                                                 high_value_is_good_outcome_flag == 0 ~ 1 - (values - indicator_min) /
                                                   (indicator_max - indicator_min))) %>%
  ungroup()


#////////////////////////


# inspect
fmir 
fmir %>% glimpse()
fmir %>% nrow() # 35604 / 17759 new / 22463 newer
fmir %>% ncol() # 48 / 49


# check indicator_standardized_values
fmir %>% group_by(indicator_name) %>% slice(1) %>%
  ungroup() %>%
  select(high_value_is_good_outcome_flag, 
         values, indicator_mean, indicator_sd, values_z_std, indicator_standardized_values, 
         indicator_rob_standardized_values, indicator_normalized_values) %>%
  print(n = nrow(.))

# test that manually calculated z_standardization is same as using scale()
expect_equal(object = fmir %>% pull(values_z_std),
             expected = fmir %>% group_by(indicator_name) %>%
               mutate(values_z_std = scale(values, center = TRUE, scale = TRUE)[ , 1]) %>%
               pull(values_z_std))


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////




# skim
fmir %>% skim(values_z_std, indicator_standardized_values)

# skim values_z_std by indicator
fmir %>% group_by(indicator_name) %>% 
  skim(values_z_std) %>% 
  ungroup() %>%
  as_tibble() %>%
  select(indicator_name, 
         # n_missing, complete_rate, 
         numeric.mean, numeric.sd, numeric.p0,
         numeric.p25, numeric.p50, numeric.p75, numeric.p100) %>%
  mutate(max_abs_std_values = case_when(abs(numeric.p0) > numeric.p100 ~ abs(numeric.p0),
                                        TRUE ~ numeric.p100)) %>%
  arrange(desc(max_abs_std_values)) %>%
  # skim(max_abs_std_values)
  print(n = nrow(.))


# skim indicator_standardized_values by indicator
fmir %>% group_by(indicator_name) %>% 
  skim(indicator_standardized_values) %>% 
  ungroup() %>%
  as_tibble() %>%
  select(indicator_name, 
         # n_missing, complete_rate, 
         numeric.mean, numeric.sd, numeric.p0,
         numeric.p25, numeric.p50, numeric.p75, numeric.p100) %>%
  arrange(desc(numeric.p100)) %>%
  # arrange(numeric.p0) %>%
  print(n = nrow(.))

# skim inv, isv, and irv
fmir %>% select(indicator_normalized_values, indicator_rob_standardized_values, indicator_standardized_values) %>%
  pivot_longer(cols = everything(), names_to = "var", values_to = "score") %>%
  group_by(var) %>% skim(score)

# check abs(values_z_std) > 1.5 that got bucketed

# current method where abs(values_z_std) > 1.5 was used
# result: about ~9.3% of all observations had abs(values_z_std) > 1.5, and so were bucketed as 0/1
# this ~9.3% is split roughly equally between upper (4.4%) and lower (4.9%) tails
# for each indicator, the share of obs that get bucketed ranges from .21 to 0; p25 is 6%, p50 is .10; p75 is 12%
# this upper/lower tail split pretty much holds for just ee_presence as well; 5.6% < -1.5; 4.7% > 1.5; 

# old method where abs(values_z_std) > 1 was used
# result: about ~25% of all observations had abs(values_z_std) > 1, and so were bucketed as 0/1
# this ~25% is split roughly equally between upper (12.5%) and lower (12.6%) tails
# for each indicator, the share of obs that get bucketed ranges from .67 to .03; p25 is 15%, p50 is .29; p75 is 33%
# looking at just ee_presence, it's ~26% of obs with abs(values_z_std) > 1; more are < -1 (.17%) than are > 1 (9.5%)

# 1) alex has consistently been asking explicitly for more visually-detectable variation, which is a legitimate issue since
# the goal of the index is to give policymakers a simple and visually-detectable understanding 
# he specifically cited the development of freedom house NIT, saying it almost got killed because there was little variation and
# policymakers were like "well what's the point?"; at the end of the day, this has to be useful to policymakers, not just data people
# along the same lines, he specifically asked for a way to adjust the y axis to make the change more visually-detectable
# which is effectively what scaling values_z_std -1 to 1 to isr 0 to 1 does, it zooms in on the central distribution to better show
# spread of majority of countries of interest
# 2) the drawback of using abs(values_z_std) > 1 instead of 1.5 is that more extreme observations get bucketed and made
# to look like less extreme observations, collapsing the plotted distance between most extreme and moderates
# 3) while there is a lot of bucketing, the interpretation that a isv = 0 is >= 3 std below a isv = 1 still doesn't seem outlandish

fmir %>% mutate(values_z_std_over_1 = case_when(values_z_std > 1.5 ~ 1,
                                                TRUE ~ 0),
                values_z_std_under_neg_1 = case_when(values_z_std < -1.5 ~ 1,
                                                     TRUE ~ 0)) %>%
  summarize(n = n(),
            num_over_1 = sum(values_z_std_over_1),
            pct_over_1 = num_over_1 / n,
            num_under_neg_1 = sum(values_z_std_under_neg_1),
            pct_under_neg_1 = num_under_neg_1 / n)

fmir %>% mutate(values_z_std_over_1 = case_when(values_z_std > 1.5 ~ 1,
                                                TRUE ~ 0),
                values_z_std_under_neg_1 = case_when(values_z_std < -1.5 ~ 1,
                                                     TRUE ~ 0),
                ee_presence_flag = case_when(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia") ~ 1,
                                             TRUE ~ 0)) %>%
  group_by(ee_presence_flag) %>%
  summarize(n = n(),
            num_over_1 = sum(values_z_std_over_1),
            pct_over_1 = num_over_1 / n,
            num_under_neg_1 = sum(values_z_std_under_neg_1),
            pct_under_neg_1 = num_under_neg_1 / n)

fmir %>% ggplot(data = ., mapping = aes(x = values_z_std)) + geom_density() +
  geom_segment(mapping = aes(x = -1.5, xend = -1.5, y = 0, yend = .5), color = "red") +
  geom_segment(mapping = aes(x = 1.5, xend = 1.5, y = 0, yend = .5), color = "red") +
  scale_x_continuous(breaks = seq(from = -5, to = 10, by = 1))
fmir %>% filter(abs(values_z_std) > 1) %>% ggplot(data = ., mapping = aes(x = values_z_std)) + geom_density() +
  scale_x_continuous(breaks = seq(from = -5, to = 10, by = 1))
fmir %>% filter(abs(values_z_std) > 1) %>% count(indicator_standardized_values)
fmir %>% mutate(abs_values_z_std_over_1_flag = case_when(abs(values_z_std) > 1 ~ 1,
                                                         TRUE ~ 0)) %>%
  summarize(pct_abs_values_z_std_over_1 = mean(abs_values_z_std_over_1_flag))

fmir %>% mutate(abs_values_z_std_over_1_flag = case_when(abs(values_z_std) > 1 ~ 1,
                                                         TRUE ~ 0)) %>%
  group_by(indicator_name) %>%
  summarize(pct_abs_values_z_std_over_1 = mean(abs_values_z_std_over_1_flag)) %>%
  ungroup() %>%
  arrange(desc(pct_abs_values_z_std_over_1)) %>% print(n = nrow(.))

fmir %>% mutate(abs_values_z_std_over_1_flag = case_when(abs(values_z_std) > 1 ~ 1,
                                                         TRUE ~ 0)) %>%
  group_by(indicator_name) %>%
  summarize(pct_abs_values_z_std_over_1 = mean(abs_values_z_std_over_1_flag)) %>%
  ungroup() %>%
  skim(pct_abs_values_z_std_over_1)


#//////////////////////




# plot distribution of values_rob_std and values_z_std
fmir %>% select(values_z_std, values_rob_std) %>%
  pivot_longer(cols = everything(), names_to = "var", values_to = "values_std") %>%
  filter(values_std < 10000) %>%
  ggplot(data = ., mapping = aes(x = values_std, color = var)) + geom_density()

# plot distribution of inv, isv, and irv
fmir %>% select(indicator_normalized_values, indicator_rob_standardized_values, indicator_standardized_values) %>%
  pivot_longer(cols = everything(), names_to = "var", values_to = "score") %>%
  ggplot(data = ., mapping = aes(x = score, color = var)) + geom_density()

# plot distribution of individual indicators
fmir %>% 
  filter(indicator_name == "sub_obj_3_2_iea_natural_gas_imports_from_russia_as_share_of_natural_gas_imports") %>%
  select(indicator_name, values, indicator_max, indicator_min,
         indicator_mean, indicator_median, 
  ) %>%
  mutate(indicator_midpoint = ((indicator_max - indicator_min) / 2) + indicator_min) %>%
  
  ggplot(data = ., mapping = aes(x = values)) + geom_histogram(bins = 100) +
  geom_segment(mapping = aes(x = indicator_midpoint, y = 0, xend = indicator_midpoint, yend = 50), color = "red") +
  geom_segment(mapping = aes(x = indicator_mean, y = 0, xend = indicator_mean, yend = 50), color = "blue") +
  geom_segment(mapping = aes(x = indicator_median, y = 0, xend = indicator_median, yend = 50), color = "green")


# plot distributions of indicator_standardized_values 
# (and indicator_rob_standardized_values and indicator_normalized_values for comparison)
# note that this code has an option to plot raw values instead of indicator_standardized_values, but
# because faceting the plots puts them on the same x axis scale, indicators with raw values between 0-1 will be compressed
# into a small amount of buckets if compared with an indicator with raw values between say 1-100 (eg sub_obj_3_1)
# note that sub_obj_4_1 highlights very clearly the incompatibility of normalized scores
# where fdi_from_russia and exports_to_russia are both very skewed (see skim above), so
# a country that is just a tiny bit above the center of distribution gets inv 100, and 
# the midpoint getting inv 50 gets pulled far out by outliers so that it inv 50 is an outlier, not "average/middle of the distribution"
# whereas a more normally distributed indicator requires like fdi_from_eu15 or atlas_eci requires substantial outlier to get isv 100
# and isv 50 is mathematically the median of the distribution, or "middle of the pack"
# note that sub_obj_1_1 highlights why rob_std is (marginally) better than z_std, 
# because for vdem_leg and vdem_judicial, z_std has isv = 50 midway onto the tail of the distro
# and also because for fh_natl_democratic, the values are nearly uniformly distributed, iqr is large, and isv doesn't have large values
# but inv still gives out inv 0 and inv 100 scores, but isv does not, because the values are not out on a tail 
# this reflects the principle that you want isv 0 or 11 to reward/punish extreme values out on a tail
fmir %>% 
  filter(sub_obj_num == "sub_obj_1_3") %>%
  
  select(indicator_name, values, values_rob_std, indicator_standardized_values, 
         indicator_rob_standardized_values, indicator_normalized_values) %>%
  pivot_longer(cols = c(indicator_standardized_values, indicator_rob_standardized_values, indicator_normalized_values), 
               names_to = "var", values_to = "score") %>%
  
  ggplot(data = ., mapping = aes(x = score, color = var), alpha = .8) + geom_density() +
  
  facet_wrap(facets = vars(indicator_name))



#//////////////////


# inspect individual indicators
# note that 3.1 net enery imports and 3.2 natural gas imports are good examples of how values_std work
# both indicators have raw share values ranging from 0 to 1, but because 3.1 has more of a normal distro, the min/max values
# are more outliers in the sense of being further from the standardized distribution mean, and so
# 3.1 has a greater range of values_std and thus indicator_standardized_values
# practically, this will mean that having the worst scores on high-standard deviation indicators like 3.2 natural gas won't
# have the same downward drag on a country's score as it did when using normalized indicators that reliably scored 0 for worst country
# so a country like ukraine that has made improvements in energy over time won't have such a dramatic increase in sub_obj score
# because the high-standard deviation indicator isnt moving from isv = 0 to isv = 1, but instead maybe isv = .3 to isv = .7
# (see plot for ukraine over time below)
# but this loss in within-country accentuated inv variation is the price for better more legitimate comparisons across sub_obj/obj;
# the benefit of within-country accentuated inv variation is really an illusion at the aggregate level,
# because of the incompatibility of inv scores 
# eg a very low standard deviation inv scores can have an apparent spike from inv 0 to inv 100, but only move a negligible amount


indicator_list <- "sub_obj_1_1_fh_national_democratic_gov"

values_distribution_plot <- fmir %>%
  filter(indicator_name %in% indicator_list) %>%
  select(indicator_name, values, values_rob_std, indicator_standardized_values) %>%
  ggplot(data = ., mapping = aes(x = values, color = indicator_name)) + geom_density() +
  theme(legend.position = "bottom")

values_rob_std_distribution_plot <- fmir %>%
  filter(indicator_name %in% indicator_list) %>%
  select(indicator_name, values, values_rob_std, indicator_standardized_values) %>%
  ggplot(data = ., mapping = aes(x = values_rob_std, color = indicator_name)) + geom_density() +
  theme(legend.position = "bottom")

isv_distribution_plot <- fmir %>%
  filter(indicator_name %in% indicator_list) %>%
  select(indicator_name, values, values_rob_std, indicator_standardized_values) %>%
  ggplot(data = ., mapping = aes(x = indicator_standardized_values, color = indicator_name)) + geom_density() +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .25), limits = c(0, 1)) +
  theme(legend.position = "bottom")

inv_distribution_plot <- fmir %>%
  filter(indicator_name %in% indicator_list) %>%
  select(indicator_name, values, indicator_normalized_values) %>%
  ggplot(data = ., mapping = aes(x = indicator_normalized_values, color = indicator_name)) + geom_density() +
  theme(legend.position = "bottom")

values_distribution_plot + values_rob_std_distribution_plot + isv_distribution_plot + inv_distribution_plot

#///////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////


# add concept_avg ####
fmir <- fmir %>% group_by(country, year, concept) %>% mutate(concept_avg = mean(indicator_standardized_values, na.rm = TRUE)) %>%
  ungroup()


#/////////////////////


# inspect
fmir 
fmir %>% glimpse()
fmir %>% nrow() # 35604 / 17759 new / 22463 newer
fmir %>% ncol() # 64 / 49 new / 50 newer

# check country/year
fmir %>% count(country) %>% nrow() # 43
fmir %>% count(year) # 11 / 7 new
fmir %>% count(obj_num) # 5

# check concept_avg
fmir %>% arrange(country, year, sub_obj_num, concept, indicator_name) %>%
  select(country, year, obj_num, sub_obj_num, concept, indicator_name,  
         indicator_standardized_values, concept_avg) %>%
  # print(n = 20) %>%
  identity()
fmir %>% skim(concept_avg)


#///////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////

backup <- fmir


fmir <- fmir %>% distinct(country, year, sub_obj_num, concept, concept_avg) %>%
  arrange(country, year, sub_obj_num, concept) %>%
  group_by(country, year, sub_obj_num) %>% 
  mutate(sub_obj_avg = mean(concept_avg, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(country, year, sub_obj_num, sub_obj_avg) %>%
  left_join(fmir, ., by = c("country", "year", "sub_obj_num"))


#/////////////////////


# inspect
fmir 
fmir %>% glimpse()
fmir %>% nrow() # 35604 / 17759 new / 22463 newer / 21672 newest
fmir %>% ncol() # 65 / 50 new / 51 newer / 51 newest

# check country/year
fmir %>% count(country) %>% nrow() # 43
fmir %>% count(year) # 11 / 7 new
fmir %>% count(obj_num) # 5
fmir %>% distinct(sub_obj_num, country, year, sub_obj_avg) %>% nrow() # 5676 (43 countries * 11 years * 12 sub_obj) / 6622 new (43 * 7 * 22) (or 6020?)


# check sub_obj_avg
fmir %>% arrange(country, year, sub_obj_num, concept, indicator_name) %>%
  select(country, year, obj_num, sub_obj_num, concept, indicator_name,  
         indicator_standardized_values, concept_avg, sub_obj_avg) %>%
  print(n = 20) %>%
  identity() 

fmir %>% skim(sub_obj_avg)

#///////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////
fmir_backup <- fmir

update_rows2 <- fmir %>%
  inner_join(remainingNAs, by = c("country", "indicator_name")) %>%
  mutate(replace_flag = 1)


fmir <- fmir %>% 
  left_join(update_rows2 %>% 
              select(country, indicator_name, year, replace_flag),
            by = c("country", "indicator_name", "year")) %>%
  mutate(
    indicator_standardized_values = case_when(
      replace_flag == 1 ~ sub_obj_avg,
      TRUE ~ indicator_standardized_values
    )) %>%
  select(-replace_flag)


#///////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////
fmir_backup <- fmir


# ROUND 2 add concept_avg ####
fmir <- fmir %>%
  select(-c("concept_avg",
            "sub_obj_avg")) %>%
  group_by(country, year, concept) %>% mutate(concept_avg = mean(indicator_standardized_values, na.rm = TRUE)) %>%
  ungroup()


#/////////////////////


# inspect
fmir 
fmir %>% glimpse()
fmir %>% nrow() # 35604 / 17759 new / 22463 newer
fmir %>% ncol() # 64 / 49 new / 50 newer

# check country/year
fmir %>% count(country) %>% nrow() # 43
fmir %>% count(year) # 11 / 7 new
fmir %>% count(obj_num) # 5

# check concept_avg
fmir %>% arrange(country, year, sub_obj_num, concept, indicator_name) %>%
  select(country, year, obj_num, sub_obj_num, concept, indicator_name,  
         indicator_standardized_values, concept_avg) %>%
  identity()
fmir %>% skim(concept_avg)


#///////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////

backup <- fmir

# add sub_obj_avg ####

fmir <- fmir %>% distinct(country, year, sub_obj_num, concept, concept_avg) %>%
  arrange(country, year, sub_obj_num, concept) %>%
  group_by(country, year, sub_obj_num) %>% 
  mutate(sub_obj_avg = mean(concept_avg, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(country, year, sub_obj_num, sub_obj_avg) %>%
  left_join(fmir, ., by = c("country", "year", "sub_obj_num"))


#/////////////////////
# inspect
fmir 
fmir %>% glimpse()
fmir %>% nrow() # 35604 / 17759 new / 22463 newer / 21672 newest
fmir %>% ncol() # 65 / 50 new / 51 newer / 51 newest

# check country/year
fmir %>% count(country) %>% nrow() # 43
fmir %>% count(year) # 11 / 7 new
fmir %>% count(obj_num) # 5
fmir %>% distinct(sub_obj_num, country, year, sub_obj_avg) %>% nrow() # 5676 (43 countries * 11 years * 12 sub_obj) / 6622 new (43 * 7 * 22) (or 6020?)

# check sub_obj_avg
fmir %>% arrange(country, year, sub_obj_num, concept, indicator_name) %>%
  select(country, year, obj_num, sub_obj_num, concept, indicator_name,  
         indicator_standardized_values, concept_avg, sub_obj_avg) %>%
  print(n = 20) %>%
  identity() 
fmir %>% 
  skim(sub_obj_avg)

#///////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////
fmir_backup <- fmir

# add obj_avg ####
fmir <- fmir %>% 
  # filter(!(indicator_name %in% cyber_indicators)) %>% 
  distinct(country, year, obj_num, sub_obj_num, sub_obj_avg) %>%
  arrange(country, year, obj_num, sub_obj_num) %>%
  group_by(country, year, obj_num) %>%
  mutate(obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
  ungroup() %>% select(country, year, obj_num, obj_avg) %>% distinct() %>%
  arrange(country, year, obj_num) %>%
  left_join(fmir, ., by = c("country", "year", "obj_num"))


# inspect
fmir
fmir %>% glimpse()
fmir %>% nrow() # 35604 / 17759 new / 22463 newer / 21672 newest
fmir %>% ncol() # 66 / 51 new / 52 newer

# check country/year
fmir %>% count(country) %>% nrow() # 43
fmir %>% count(year) # 11 / 7 new
fmir %>% count(obj_num) # 5
fmir %>% distinct(obj_num, country, year, obj_avg) %>% nrow() # 2365 (43 countries * 11 years * 5 obj) 1505 (43 * 7 * 5)

# check obj_avg
fmir %>% arrange(country, year, obj_num, sub_obj_num, concept, indicator_name) %>%
  select(country, year, obj_num, sub_obj_num, concept, indicator_name,  
         indicator_standardized_values, concept_avg, sub_obj_avg, obj_avg) %>%
  print(n = 20) %>%
  identity() 
fmir %>% skim(obj_avg)


#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////

backup <- fmir 

# add miri_avg ####
fmir <- fmir %>%
  mutate(weighted_obj_avg =
           case_when(
             obj_num == "obj_1" ~ obj_avg * 0.20,
             obj_num == "obj_2" ~ obj_avg * 0.20,
             obj_num == "obj_3" ~ obj_avg * 0.30,
             obj_num == "obj_4" ~ obj_avg * 0.30,
             TRUE ~ NA 
           )) %>%
  group_by(country, year, obj_num) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(country, year) %>%
  summarize(miri_avg = sum(weighted_obj_avg)) %>%
  ungroup() %>% 
  select(country, year, miri_avg) %>% 
  distinct() %>%
  left_join(fmir, ., by = c("country", "year"))

#///////////////////////////
# inspect
fmir
fmir %>% glimpse()
fmir %>% nrow() # 35604 / 17759 new / 22463 newer
fmir %>% ncol() # 67 / 52 new / 53 newer

# check country/year
fmir %>% count(country) %>% nrow() # 43
fmir %>% count(year) # 11 / 7 new
fmir %>% count(obj_num) # 5
fmir %>% distinct(obj_num, country, year, obj_avg) %>% nrow() # 2365 (43 countries * 11 years * 5 obj) / 1505 (43 * 7 * 5)

# check miri_avg
fmir %>% arrange(country, year, obj_num, sub_obj_num, concept, indicator_name) %>%
  select(country, year, obj_num, sub_obj_num, concept, indicator_name,  
         indicator_standardized_values, concept_avg, sub_obj_avg, obj_avg, miri_avg) %>%
  print(n = 80) %>%
  identity() 
fmir %>% arrange(country, year, obj_num, sub_obj_num, concept, indicator_name) %>%
  distinct(country, year, obj_num, obj_avg, miri_avg) %>%
  identity() 
fmir %>% skim(miri_avg)


#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
backup <- fmir

# read/write foreign relations data
foreign_relations_data %>% write.xlsx(file = "data/fmir/frelations_20241114.xlsx")
foreign_relations_data <- read_excel(path = "data/fmir/frelations_20241114.xlsx")

#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////

# read/write fmir ####
fmir %>% write.xlsx(file = "data/fmir/fmir_20250123.xlsx")
fmir <- read_excel(path = "data/fmir/fmir_20250123.xlsx")

# inspect
fmir 
fmir %>% glimpse()
fmir %>% nrow() # 35604 / 17759 new / 22463 newer / 17802 newest
fmir %>% ncol() # 67 / 52 new / 53 newer / 53 newest

#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
# read/write ri_data ####
fmir %>% select(country, mcp_grouping, year, obj, obj_short_name, obj_num,
                sub_obj, sub_obj_short_name, sub_obj_num, concept, indicator_name,
                description, high_value_is_good_outcome_flag,
                values, indicator_standardized_values, concept_avg, sub_obj_avg, obj_avg, miri_avg) %>%
  rename(regional_grouping = mcp_grouping, 
         indicator_raw_values = values,
         high_indicator_raw_value_is_good_outcome_flag = high_value_is_good_outcome_flag) %>%
  write.xlsx(file = "data/fmir/fmir_data_20241114.xlsx", sheetName = "data")


fmir_data <- read_excel(path = "data/fmir/fmir_data_20241114.xlsx", sheet = "data")

# inspect
fmir_data
fmir_data %>% glimpse()
fmir_data %>% nrow() # 17759 / / 22463 newer
fmir_data %>% ncol() # 21

#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
# read/write ri_obj_data ####
fmir_data %>% distinct(country, regional_grouping, year, obj_short_name, obj_num, obj_avg, miri_avg) %>%
  select(country, regional_grouping, year, obj_short_name, obj_num, obj_avg, miri_avg) %>%
  pivot_longer(cols = c(miri_avg, obj_avg), names_to = "obj_type", values_to = "obj_avg") %>%
  mutate(obj_short_name = case_when(obj_type == "miri_avg" ~ "Resilience Index",
                                    TRUE ~ obj_short_name)) %>%
  select(country, regional_grouping, year, obj_short_name, obj_avg) %>%
  distinct() %>%
  pivot_wider(id_cols = c(country, regional_grouping, year), 
              names_from = obj_short_name,
              values_from = obj_avg) %>%
  write.xlsx(file = "data/fmir/ri_obj_data_20241114.xlsx")

ri_obj_data <- read_excel(path = "data/fmir/ri_obj_data_20241114.xlsx") %>% as_tibble()

# inspect
ri_obj_data
ri_obj_data %>% glimpse()
ri_obj_data %>% nrow() # 473 (43 countries * 6 years) / 258 new
ri_obj_data %>% ncol() # 9

# check
expect_equal(object = ri_obj_data %>% 
               rowwise() %>%
               mutate(ri_avg = mean(c_across(cols = tidyselect::matches(match = "resilience i", 
                                                                        ignore.case = TRUE))),
                      ri_avg_rounded = round(ri_avg, digits = 3),
                      resilience_index_rounded = round(`Resilience Index`, digits = 3),
                      ri_avg_match_flag = case_when(ri_avg_rounded == resilience_index_rounded ~ 1,
                                                    TRUE ~ 0)) %>% 
               select(-c(`Democratic Institutions and Processes`, 
                         `Information Space`,
                         `Energy and Infrastructure`,
                         `The Economy`
               )) %>%
               distinct(ri_avg_match_flag) %>% pull(ri_avg_match_flag),
             expected = 1)


#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////


# read/write ri_data_long ####

# get fmir_indicator_long
fmir_indicator_long <- fmir %>% select(country, year, mcp_grouping, indicator_name, 
                                       # high_value_is_good_outcome_flag, values, imputation_condition, imputed_flag
                                       indicator_standardized_values) %>%
  pivot_longer(cols = c(indicator_standardized_values),
               names_to = "var_type", values_to = "var_values") %>%
  rename(var_name = indicator_name) %>%
  relocate(var_type, .before = var_name) %>%
  relocate(var_values, .after = var_name) %>%
  mutate(var_name = str_replace(string = var_name, pattern = regex("^sub_obj_", ignore_case = TRUE), 
                                replacement = "indicator_"))


#////////////////////
# inspect
fmir_indicator_long 
fmir_indicator_long %>% glimpse()
fmir_indicator_long %>% nrow() # 30745 (43 countries * 11 years * 65 indicators * 1 var_types) / #17759 / # 22463
fmir_indicator_long %>% ncol() # 6


#//////////////////////////////////////////////////////////////////////////////////////////////////////
# get fmir_concept_long
fmir_concept_long <- fmir %>%
  distinct(country, year, mcp_grouping, sub_obj_num, concept, concept_avg) %>%
  rename(var_name = concept, 
         var_values = concept_avg) %>%
  unite(col = "var_name", sub_obj_num, var_name, sep = " ") %>%
  mutate(var_type = "concept_avg",
         var_name = str_replace(string = var_name, pattern = regex("^sub_obj_", ignore_case = TRUE), 
                                replacement = "concept_")) %>%
  select(country, year, mcp_grouping, var_type, var_name, var_values)


#////////////////////
# inspect
fmir_concept_long
fmir_concept_long %>% glimpse()
fmir_concept_long %>% nrow() # 8987 (43 countries * 11 years * 19 concepts) 12642 (43 * 7 * 42) / 13279
fmir_concept_long %>% ncol() # 6


#//////////////////////////////////////////////////////////////////////////////////////////////////////
# get fmir_sub_obj_long
fmir_sub_obj_long <- fmir %>%
  distinct(country, year, mcp_grouping, sub_obj_short_name, sub_obj_avg) %>%
  rename(var_name = sub_obj_short_name, var_values = sub_obj_avg) %>%
  mutate(var_type = "sub_obj_avg") %>%
  select(country, year, mcp_grouping, var_type, var_name, var_values)


#////////////////////      
# inspect
fmir_sub_obj_long 
fmir_sub_obj_long %>% glimpse()
fmir_sub_obj_long %>% nrow() # 5676 (43 countries * 11 years * 12 sub_obj) / 6020 / 6391
fmir_sub_obj_long %>% ncol() # 6


#//////////////////////////////////////////////////////////////////////////////////////////////////////
# get fmir_obj_long
fmir_obj_long <- fmir %>%
  distinct(country, year, mcp_grouping, obj_short_name, obj_avg) %>%
  rename(var_name = obj_short_name, var_values = obj_avg) %>%
  mutate(var_type = "obj_avg") %>%
  select(country, year, mcp_grouping, var_type, var_name, var_values)


#////////////////////      
# inspect
fmir_obj_long 
fmir_obj_long %>% glimpse()
fmir_obj_long %>% nrow() # 2365 (43 countries * 11 years * 5 obj) / 1505
fmir_obj_long %>% ncol() # 9 / 6

#//////////////////////////////////////////////////////////////////////////////////////////////////////
# get fmir_index_long
fmir_index_long <- fmir %>%
  distinct(country, year, mcp_grouping, miri_avg) %>%
  mutate(miri_short_name = "FMI Resilience Index") %>%
  rename(var_name = miri_short_name, var_values = miri_avg) %>%
  mutate(var_type = "miri_avg") %>%
  select(country, year, mcp_grouping, var_type, var_name, var_values)


#////////////////////      
# inspect
fmir_index_long 
fmir_index_long %>% glimpse()
fmir_index_long %>% nrow() # 473 (43 countries * 11 years) / 301
fmir_index_long %>% ncol() # 9 / 6


#//////////////////////////////////////////////////////////////////////////////////////////////////////
# combine and save
fmir_indicator_long %>% 
  bind_rows(., fmir_concept_long) %>%
  bind_rows(., fmir_sub_obj_long) %>%
  bind_rows(., fmir_obj_long) %>%
  bind_rows(., fmir_index_long) %>%
  write.xlsx(file = "data/fmir/ri_data_long_20241114.xlsx")

fmir_long <- read_excel(path = "data/fmir/ri_data_long_20241114.xlsx") %>% as_tibble()

# inspect
fmir_long
fmir_long %>% glimpse()
fmir_long %>% nrow() # 48246 / 43939
fmir_long %>% ncol() # 9 / 6

fmir_long %>% count(var_name) %>% print(n = nrow(.))
fmir_long %>% count(var_type)

# check
expect_equal(object = sum(fmir_indicator_long %>% nrow(), 
                          fmir_concept_long %>% nrow(),
                          fmir_sub_obj_long %>% nrow(),
                          fmir_obj_long %>% nrow(),
                          fmir_index_long %>% nrow()),
             expected = fmir_long %>% nrow())


#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
# read/write ri_data_wide ####

# get ri_data_wide
ri_data_wide <- fmir %>% distinct(country, mcp_grouping, year, miri_avg) %>%
  mutate(miri_short_name = "Resilience Index") %>%
  pivot_wider(id_cols = c(country, mcp_grouping, year), names_from = miri_short_name, values_from = miri_avg) %>%
  left_join(., fmir %>% distinct(country, mcp_grouping, year, obj_short_name, obj_avg) %>%
              mutate(obj_short_name = str_c("Obj: ", obj_short_name)) %>%
              pivot_wider(id_cols = c(country, mcp_grouping, year), names_from = obj_short_name, values_from = obj_avg),
            by = c("country", "mcp_grouping", "year")) %>%
  left_join(., fmir %>% distinct(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>%
              mutate(sub_obj_short_name = str_c("Sub-obj: ", sub_obj_short_name)) %>%
              pivot_wider(id_cols = c(country, mcp_grouping, year), names_from = sub_obj_short_name, values_from = sub_obj_avg),
            by = c("country", "mcp_grouping", "year")) %>%
  left_join(., fmir %>% distinct(country, mcp_grouping, year, concept, concept_avg) %>%
              mutate(concept = str_c("Concept: ", concept)) %>%
              pivot_wider(id_cols = c(country, mcp_grouping, year), names_from = concept, values_from = concept_avg),
            by = c("country", "mcp_grouping", "year")) %>%
  left_join(., fmir %>% select(country, mcp_grouping, year, indicator_name, indicator_standardized_values) %>%
              pivot_wider(id_cols = c(country, mcp_grouping, year), 
                          names_from = indicator_name, values_from = indicator_standardized_values),
            by = c("country", "mcp_grouping", "year")) 

# read/write
ri_data_wide %>% write.xlsx(file = "data/fmir/ri_data_wide_20241114.xlsx")
ri_data_wide <- read_excel(path = "data/fmir/ri_data_wide_20241114.xlsx")


#///////////////////
# inspect

ri_data_wide
ri_data_wide %>% glimpse()
ri_data_wide %>% nrow() # 473 (43 countries * 11 years) / 301
ri_data_wide %>% ncol() # 105 / 150


#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////


# compare current version of fmir to previous version of fmir ####
# 
# previous_fmir <- read_excel(path = "data/fmir/round 5/previous_fmir_20221012.xlsx")
# 
# # inspect previous_fmir
# previous_fmir %>% glimpse()
# previous_fmir %>% nrow() # 30272
# previous_fmir %>% ncol() # 66
# previous_fmir %>% count(indicator_name) # 64
# previous_fmir %>% count(country) # 43
# 
# # inspect fmir
# fmir %>% glimpse()
# fmir %>% nrow() # 34572
# fmir %>% ncol() # 71
# fmir %>% count(indicator_name) # 67
# fmir %>% count(country) # 43


#/////////////////////////

all_objects <- ls()
non_functions <- all_objects[!sapply(mget(all_objects, envir = .GlobalEnv), is.function)]
rm(list = non_functions)
rm(all_objects, non_functions)
