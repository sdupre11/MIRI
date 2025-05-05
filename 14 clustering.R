### Clustering ####
## Hierarchical clustering with Ward's method and Dynamic Time Warping
#NOTE: Must run the early portions of script 14 to create fmir first

library(dplyr)
library(tidyr)
library(TSclust)
library(cluster)

kosovo_na_combinations <- fmir %>%
  filter(country == "Kosovo" & is.na(values)) %>%
  select(year, indicator_name) %>%
  distinct()

# Step 2: Remove rows for these combinations across all countries
cleaned_fmir_obj1 <- fmir %>%
  anti_join(kosovo_na_combinations, by = c("year", "indicator_name")) %>%
  filter(obj_num == "obj_1") %>%
  select(c("year", "country", "indicator_name", "values")) %>%
  pivot_wider(names_from = c(year, indicator_name),
              values_from = values) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

cleaned_fmir_obj2 <- fmir %>%
  anti_join(kosovo_na_combinations, by = c("year", "indicator_name")) %>%
  filter(obj_num == "obj_2") %>%
  select(c("year", "country", "indicator_name", "values")) %>%
  pivot_wider(names_from = c(year, indicator_name),
              values_from = values) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

cleaned_fmir_obj3 <- fmir %>%
  anti_join(kosovo_na_combinations, by = c("year", "indicator_name")) %>%
  filter(obj_num == "obj_3") %>%
  select(c("year", "country", "indicator_name", "values")) %>%
  pivot_wider(names_from = c(year, indicator_name),
              values_from = values) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

cleaned_fmir_obj4 <- fmir %>%
  anti_join(kosovo_na_combinations, by = c("year", "indicator_name")) %>%
  filter(obj_num == "obj_4") %>%
  select(c("year", "country", "indicator_name", "values")) %>%
  pivot_wider(names_from = c(year, indicator_name),
              values_from = values) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))


scaled_obj1 <- scale(cleaned_fmir_obj1[,-1])
scaled_obj2 <- scale(cleaned_fmir_obj2[,-1])
scaled_obj3 <- scale(cleaned_fmir_obj3[,-1])
scaled_obj4 <- scale(cleaned_fmir_obj4[,-1])

scaled_obj1_distmatrix <- diss(scaled_obj1,
                               METHOD = "DTW")

scaled_obj2_distmatrix <- diss(scaled_obj2,
                               METHOD = "DTW")

scaled_obj3_distmatrix <- diss(scaled_obj3,
                               METHOD = "DTW")

scaled_obj4_distmatrix <- diss(scaled_obj4,
                               METHOD = "DTW")

hc1 <- hclust(scaled_obj1_distmatrix, method = "ward.D2")
hc2 <- hclust(scaled_obj2_distmatrix, method = "ward.D2")
hc3 <- hclust(scaled_obj3_distmatrix, method = "ward.D2")
hc4 <- hclust(scaled_obj4_distmatrix, method = "ward.D2")

plot(hc1, labels = cleaned_fmir_obj1$country, main = "Dendrogram for Multivariate Time-Series Clustering, obj1")
#Kosovo groups with Armenia, Bosnia, Georgia, and Moldova. Moldova is the closest. Removed Bulgaria from the first round of this (1/27/25)
plot(hc2, labels = cleaned_fmir_obj2$country, main = "Dendrogram for Multivariate Time-Series Clustering, obj2")
#Kosovo groups with Albania, Armenia, Moldova, Montenegro, and Ukraine. Moldova is the closest. Added Ukraine after first round of this (1/27/25)
plot(hc3, labels = cleaned_fmir_obj3$country, main = "Dendrogram for Multivariate Time-Series Clustering, obj3")
#Kosovo groups with Montenegro, Albania, Bosnia, N. Macedonia, and Serbia. Closest with Albania and Montenegro. Unchanged in second round of this (1/27/25)
plot(hc4, labels = cleaned_fmir_obj4$country, main = "Dendrogram for Multivariate Time-Series Clustering, obj4")
#Kosovo groups with Albania, Bosnia, Croatia, Ireland, Luxembourg, N. Macedonia, and Serbia. Montenegro removed, while Ireland and Luxembourg added for second round of this (1/27/25)


rm(list = ls(pattern = "^(scaled_|cleaned|hc)"))
