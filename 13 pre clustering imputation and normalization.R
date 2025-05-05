library(VIM)
library(cluster)
library(FNN)

# combine sub_obj datasets to get fmir ####
# get var_list, using this particular .csv/variable is arbitrary

sub_obj_1_1_vdem_free_elections <- read_csv(file = "data/fmir/sub_obj_1_1_vdem_free_elections.csv", 
                                            lazy = FALSE)

var_list <- syms(c(sub_obj_1_1_vdem_free_elections %>% names()))
var_list


fmir <- dir_ls("data/fmir") %>%
  tibble(path = as.character(.)) %>%
  select(path) %>%
  filter(str_detect(string = path, pattern = "data/fmir/sub_obj_")) %>%
  pull(path) %>%
  map(.x = ., .f = function(.x) {
    print(.x)
    df <- read.csv(file = .x) %>% as_tibble()
    
    # Ensure var_list is a character vector
    var_list <- as.character(var_list)
    
    # Select only columns that exist in the dataframe
    valid_columns <- intersect(var_list, names(df))
    
    # If no valid columns, return an empty dataframe with appropriate structure
    if (length(valid_columns) == 0) {
      return(tibble())
    } else {
      return(df %>% select(all_of(valid_columns)))
    }
  }) %>%
  bind_rows() %>%
  select(-c(obj_num, obj, obj_short_name, sub_obj_num, sub_obj, sub_obj_short_name,
            concept, indicator, description)) %>%
  left_join(fmir_framework, ., by = "indicator_name", multiple = "all") %>%
  filter(year >= 2018, !(country %in% c("U.S.", "Russia")))

fmir <- fmir %>% 
  mutate(obj_num = case_when(sub_obj_num %in% c("sub_obj_FR_1",
                                                "sub_obj_FR_2",
                                                "sub_obj_FR_3",
                                                "sub_obj_FR_4") ~ "obj_fr",
                             TRUE ~ obj_num))


foreign_relations_data <- fmir %>%
  filter(obj_num == "obj_fr")

fmir <- fmir %>%
  filter(!(obj_num %in% c("obj_fr")))


#////////////////////////////


# inspect
fmir
fmir %>% glimpse()
fmir %>% nrow() # 17,802 (43 countries * 69 indicators * 6 years = 17,802)
fmir %>% ncol() # 35

#Check where missing entries are
fmir %>% 
  group_by(indicator_name) %>% 
  tally() %>%
  filter(n != 301)

### check where NAs are
fmir %>% 
  filter(is.na(values)) %>% #5,538
  group_by(country, indicator) %>%
  tally() %>% 
  View()

ee_grads <- c("Bulgaria",
              "Croatia",
              "Czechia",
              "Estonia",
              "Hungary",
              "Latvia",
              "Lithuania",
              "Montenegro",
              "Poland",
              "Romania",
              "Slovakia",
              "Slovenia")

eu_15 <- c("Austria",
           "Belgium",
           "Denmark",
           "Finland",
           "France",
           "Germany",
           "Greece",
           "Ireland",
           "Italy",
           "Luxembourg",
           "Netherlands",
           "Portugal",
           "Spain",
           "Sweden",
           "U.K.")


##ee_grads
fmir %>%  
  filter(is.na(values) & country %in% ee_grads) %>% 
  group_by(country, indicator_name) %>%
  tally()

fmir %>%  #490 of 5,538 are missing entirely from ee_grads
  filter(is.na(values) & country %in% ee_grads) %>% 
  group_by(country, indicator_name) %>%
  tally() %>%
  filter(n==7) 

fmir %>% 
  filter(is.na(values) & country %in% ee_grads) %>% 
  group_by(country, indicator_name) %>%
  tally() %>%
  filter(n==7) %>%
  ungroup() %>%
  group_by(indicator_name) %>%
  tally()

##EU-15
fmir %>%  
  filter(is.na(values) & country %in% eu_15) %>% 
  group_by(country, indicator) %>%
  tally() 

fmir %>%  #1,778 of 5,538 are missing entirely from EU-15
  filter(is.na(values) & country %in% eu_15) %>% 
  group_by(country, indicator) %>%
  tally() %>%
  filter(n==7)

fmir %>% 
  filter(is.na(values) & country %in% eu_15) %>% 
  group_by(country, indicator_name) %>%
  tally() %>%
  filter(n==7) %>%
  ungroup() %>%
  group_by(indicator_name) %>%
  tally() %>%
  print(n=25)





fmir %>% count(obj_short_name) %>% print(n = nrow(.))
fmir %>% count(sub_obj_short_name) %>% print(n = nrow(.))
fmir %>% count(concept) %>% print(n = nrow(.))
fmir %>% count(indicator_name) %>% print(n = nrow(.))
fmir %>% count(high_value_is_good_outcome_flag)
fmir %>% count(country) %>% print(n = nrow(.))
fmir %>% count(mcp_grouping)
fmir %>% distinct(country) %>% nrow() # 43
fmir %>% count(year)



#//////////////////////////


# inspections for fmir_assembly_tests

# test that each country/year/indicator combo has only one record
fmir %>% count(country, year, indicator_name, values) %>% distinct(n) # 1
fmir %>% count(country, year, indicator_name, values) %>% filter(n > 1) %>% select(country, year, indicator_name) %>% 
  print(n = nrow(.))
fmir %>% filter(country == "Kosovo", year == 2021, indicator_name == "sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi") %>%
  select(country, year, indicator_name, values)

# test that each sub_obj/indicator combo has 258 records (6 years * 43 countries)
fmir %>% count(sub_obj_short_name, indicator_name) %>% print(n = nrow(.))


#/////////////////////////

### Check NAs and initial tallies ####

fmir %>% 
  filter(is.na(values)) %>% 
  group_by(indicator_name) %>%
  tally() %>%
  mutate(perc_missing = round(((n/258)*100),1)) %>%
  select(-n) %>%
  arrange(desc(perc_missing)) %>%
  print(n=50)
 
#Missing percentage
#EUCOMs - 88.4%
#Basel AML - 83.3-83.7%
#KNOMAD - 83.3%
#IREX - 52.3-75.6%
#UN DESA - 75.6%
#BTI - 67.4-74.8%
#WBG External debt - 71.3%

## Check which have only 1 or 2 years of data
fmir %>% 
  filter(is.na(values)) %>% 
  group_by(indicator_name, year) %>% 
  tally() %>% 
  filter(n==43) %>% 
  group_by(indicator_name) %>% 
  tally() %>% 
  filter(n >= 4)

#EUCOM both only have 2023 and 2024
#Basel three only have 2023 data (due to be updated with 2024 data 12/2/24)
#KNOMAD only has 2021 data
#UN DESA only has 2019 and 2020 data

fmir %>%
  tally()

fmir %>%
  filter(is.na(values)) %>% 
  tally() %>%
  mutate(missing = 1-(n/16770)) %>%
  print() 

#5,146 missing from 16,770 - 69.3% present with

one_or_two_year_data <- c("sub_obj_2_3_eucom_malign_media_penetration",
                          "sub_obj_2_3_eucom_western_media_penetration",
                          "sub_obj_4_2_basel_financial_transparency",
                          "sub_obj_4_3_knomad_bilateral_fmi_remittances",
                          "sub_obj_4_3_undesa_migrant_opportunities")

fmir %>%
  filter(!indicator_name %in% one_or_two_year_data) %>%
  tally()

fmir %>%
  filter(!indicator_name %in% one_or_two_year_data) %>%
  filter(is.na(values)) %>% 
  tally() %>%
  mutate(present = 1-(n/14964)) %>%
  print() 

#3,634 missings from 14,964 - 75.7% present without those

