# load sub_obj_3_1_usaid_electric_grids ####

sub_obj_3_1_usaid_electric_grids <- read_csv(file = "data/memberships and categories/memberships_and_categories_toimport.csv",
                                lazy = FALSE) %>%
  filter(!is.na(year)) %>%
  select(-c(mcp_grouping,
            eu_status,
            nato_status,
            market_risk_category)) %>%
  mutate(high_value_is_good_outcome_flag = 1,
         indicator_name = "sub_obj_3_1_usaid_electric_grids",
         year = as.numeric(year)) %>%
  rename(values = grid_risk_category) %>%
  filter(year != 2024)
  
#/////////////////


# inspect
sub_obj_3_1_usaid_electric_grids
sub_obj_3_1_usaid_electric_grids %>% glimpse()
sub_obj_3_1_usaid_electric_grids %>% nrow() # 336
sub_obj_3_1_usaid_electric_grids %>% ncol() # 5

sub_obj_3_1_usaid_electric_grids %>% arrange(values) %>% distinct(country)
sub_obj_3_1_usaid_electric_grids %>% arrange(desc(values)) %>% distinct(country)
sub_obj_3_1_usaid_electric_grids %>% skim()

# inspect country names
sub_obj_3_1_usaid_electric_grids %>% anti_join(., country_crosswalk, by = c("country" = "country")) %>% 
  distinct(country) %>% arrange(country)
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_3_1_usaid_electric_grids, by = c("country" = "country")) %>% 
  distinct(country) %>% arrange(country)

sub_obj_3_1_usaid_electric_grids %>% 
  filter(str_detect(string = country, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_3_1_usaid_electric_grids <- sub_obj_3_1_usaid_electric_grids %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country", "year" = "year")) %>%
  left_join(., fmir_framework, by = "indicator_name") 


#/////////////////


# inspect
sub_obj_3_1_usaid_electric_grids
sub_obj_3_1_usaid_electric_grids %>% glimpse()
sub_obj_3_1_usaid_electric_grids %>% nrow() # 315
sub_obj_3_1_usaid_electric_grids %>% ncol() # 40

# check country/year
sub_obj_3_1_usaid_electric_grids %>% distinct(country) %>% nrow() # 45
sub_obj_3_1_usaid_electric_grids %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_3_1_usaid_electric_grids %>% count(indicator_name) # 900 (7 years * 45 countries = 315)
sub_obj_3_1_usaid_electric_grids %>% count(year) %>% print(n = nrow(.))

# check values
sub_obj_3_1_usaid_electric_grids %>% 
  filter(indicator_name == "sub_obj_3_1_usaid_electric_grids") %>% 
  skim(values)
sub_obj_3_1_usaid_electric_grids %>% group_by(year) %>% skim(values)


# plot
sub_obj_3_1_usaid_electric_grids %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# inspect summary stats on indicators
sub_obj_3_1_usaid_electric_grids %>% group_by(indicator_name) %>% 
  mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
         missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
         missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
  distinct(indicator_name, year_start, year_end, missing_pct)


#/////////////////


# read/write
sub_obj_3_1_usaid_electric_grids %>% write_csv(file = "data/fmir/sub_obj_3_1_usaid_electric_grids.csv")
sub_obj_3_1_usaid_electric_grids <- read_csv(file = "data/fmir/sub_obj_3_1_usaid_electric_grids.csv", 
                                                          lazy = FALSE)




#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# load sub_obj_3_1_usaid_electricity_markets ####

sub_obj_3_1_usaid_electricity_markets <- read_csv(file = "data/memberships and categories/memberships_and_categories_toimport.csv",
                                             lazy = FALSE) %>%
  filter(!is.na(year)) %>%
  select(-c(mcp_grouping,
            eu_status,
            nato_status,
            grid_risk_category)) %>%
  mutate(high_value_is_good_outcome_flag = 1,
         indicator_name = "sub_obj_3_1_usaid_electricity_markets",
         year = as.numeric(year)) %>%
  rename(values = market_risk_category) %>%
  filter(year != 2024)

#/////////////////


# inspect
sub_obj_3_1_usaid_electricity_markets
sub_obj_3_1_usaid_electricity_markets %>% glimpse()
sub_obj_3_1_usaid_electricity_markets %>% nrow() # 1074
sub_obj_3_1_usaid_electricity_markets %>% ncol() # 5

var_info("v2x_jucon")
sub_obj_3_1_usaid_electricity_markets %>% arrange(values) %>% distinct(country)
sub_obj_3_1_usaid_electricity_markets %>% arrange(desc(values)) %>% distinct(country)
sub_obj_3_1_usaid_electricity_markets %>% skim()

# inspect country names
sub_obj_3_1_usaid_electricity_markets %>% anti_join(., country_crosswalk, by = c("country" = "country")) %>% 
  distinct(country) %>% arrange(country)
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_3_1_usaid_electricity_markets, by = c("country" = "country")) %>% 
  distinct(country) %>% arrange(country)

sub_obj_3_1_usaid_electricity_markets %>% 
  filter(str_detect(string = country, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_3_1_usaid_electricity_markets <- sub_obj_3_1_usaid_electricity_markets %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country", "year" = "year")) %>%
  left_join(., fmir_framework, by = "indicator_name") 


#/////////////////


# inspect
sub_obj_3_1_usaid_electricity_markets
sub_obj_3_1_usaid_electricity_markets %>% glimpse()
sub_obj_3_1_usaid_electricity_markets %>% nrow() # 315
sub_obj_3_1_usaid_electricity_markets %>% ncol() # 39

# check country/year
sub_obj_3_1_usaid_electricity_markets %>% distinct(country) %>% nrow() # 45
sub_obj_3_1_usaid_electricity_markets %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_3_1_usaid_electricity_markets %>% count(indicator_name) # 900 (7 years * 45 countries = 315)
sub_obj_3_1_usaid_electricity_markets %>% count(year) %>% print(n = nrow(.))

# check values
sub_obj_3_1_usaid_electricity_markets %>% 
  filter(indicator_name == "sub_obj_3_1_usaid_electricity_markets") %>% 
  skim(values)
sub_obj_3_1_usaid_electricity_markets %>% group_by(year) %>% skim(values)


# plot
sub_obj_3_1_usaid_electricity_markets %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# inspect summary stats on indicators
sub_obj_3_1_usaid_electricity_markets %>% group_by(indicator_name) %>% 
  mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
         missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
         missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
  distinct(indicator_name, year_start, year_end, missing_pct)


#/////////////////


# read/write
sub_obj_3_1_usaid_electricity_markets %>% write_csv(file = "data/fmir/sub_obj_3_1_usaid_electricity_markets.csv")
sub_obj_3_1_usaid_electricity_markets <- read_csv(file = "data/fmir/sub_obj_3_1_usaid_electricity_markets.csv", 
                                                          lazy = FALSE)




#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# load sub_obj_fr_1_nato_membership ####

sub_obj_fr_1_nato_membership <- read_csv(file = "data/memberships and categories/memberships_and_categories_toimport.csv",
                                                  lazy = FALSE) %>%
  filter(!is.na(year)) %>%
  select(-c(mcp_grouping,
            eu_status,
            market_risk_category,
            grid_risk_category)) %>%
  mutate(high_value_is_good_outcome_flag = 1,
         indicator_name = "sub_obj_fr_1_nato_membership",
         year = as.numeric(year)) %>%
  rename(values = nato_status) %>%
  filter(year != 2024)

#/////////////////


# inspect
sub_obj_fr_1_nato_membership
sub_obj_fr_1_nato_membership %>% glimpse()
sub_obj_fr_1_nato_membership %>% nrow() # 1074
sub_obj_fr_1_nato_membership %>% ncol() # 5

var_info("v2x_jucon")
sub_obj_fr_1_nato_membership %>% arrange(values) %>% distinct(country)
sub_obj_fr_1_nato_membership %>% arrange(desc(values)) %>% distinct(country)
sub_obj_fr_1_nato_membership %>% skim()

# inspect country names
sub_obj_fr_1_nato_membership %>% anti_join(., country_crosswalk, by = c("country" = "country")) %>% 
  distinct(country) %>% arrange(country)
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_fr_1_nato_membership, by = c("country" = "country")) %>% 
  distinct(country) %>% arrange(country)

sub_obj_fr_1_nato_membership %>% 
  filter(str_detect(string = country, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_fr_1_nato_membership <- sub_obj_fr_1_nato_membership %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country", "year" = "year")) %>%
  left_join(., fmir_framework, by = "indicator_name") 


#/////////////////


# inspect
sub_obj_fr_1_nato_membership
sub_obj_fr_1_nato_membership %>% glimpse()
sub_obj_fr_1_nato_membership %>% nrow() # 315
sub_obj_fr_1_nato_membership %>% ncol() # 39

# check country/year
sub_obj_fr_1_nato_membership %>% distinct(country) %>% nrow() # 45
sub_obj_fr_1_nato_membership %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_fr_1_nato_membership %>% count(indicator_name) # 900 (7 years * 45 countries = 315)
sub_obj_fr_1_nato_membership %>% count(year) %>% print(n = nrow(.))

# check values
sub_obj_fr_1_nato_membership %>% 
  filter(indicator_name == "sub_obj_fr_1_nato_membership") %>% 
  skim(values)
sub_obj_fr_1_nato_membership %>% group_by(year) %>% skim(values)


# plot
sub_obj_fr_1_nato_membership %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# inspect summary stats on indicators
sub_obj_fr_1_nato_membership %>% group_by(indicator_name) %>% 
  mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
         missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
         missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
  distinct(indicator_name, year_start, year_end, missing_pct)


#/////////////////


# read/write
sub_obj_fr_1_nato_membership %>% write_csv(file = "data/fmir/sub_obj_fr_1_nato_membership.csv")
sub_obj_fr_1_nato_membership <- read_csv(file = "data/fmir/sub_obj_fr_1_nato_membership.csv", 
                                                          lazy = FALSE)




#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# load sub_obj_fr_2_eu_membership ####

sub_obj_fr_2_eu_membership <- read_csv(file = "data/memberships and categories/memberships_and_categories_toimport.csv",
                                                  lazy = FALSE) %>%
  filter(!is.na(year)) %>%
  select(-c(mcp_grouping,
            market_risk_category,
            nato_status,
            grid_risk_category)) %>%
  mutate(high_value_is_good_outcome_flag = 1,
         indicator_name = "sub_obj_fr_2_eu_membership",
         year = as.numeric(year)) %>%
  rename(values = eu_status) %>%
  filter(year != 2024)

#/////////////////


# inspect
sub_obj_fr_2_eu_membership
sub_obj_fr_2_eu_membership %>% glimpse()
sub_obj_fr_2_eu_membership %>% nrow() # 1074
sub_obj_fr_2_eu_membership %>% ncol() # 5

var_info("v2x_jucon")
sub_obj_fr_2_eu_membership %>% arrange(values) %>% distinct(country)
sub_obj_fr_2_eu_membership %>% arrange(desc(values)) %>% distinct(country)
sub_obj_fr_2_eu_membership %>% skim()

# inspect country names
sub_obj_fr_2_eu_membership %>% anti_join(., country_crosswalk, by = c("country" = "country")) %>% 
  distinct(country) %>% arrange(country)
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_fr_2_eu_membership, by = c("country" = "country")) %>% 
  distinct(country) %>% arrange(country)

sub_obj_fr_2_eu_membership %>% 
  filter(str_detect(string = country, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_fr_2_eu_membership <- sub_obj_fr_2_eu_membership %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country", "year" = "year")) %>%
  left_join(., fmir_framework, by = "indicator_name") 


#/////////////////


# inspect
sub_obj_fr_2_eu_membership
sub_obj_fr_2_eu_membership %>% glimpse()
sub_obj_fr_2_eu_membership %>% nrow() # 315
sub_obj_fr_2_eu_membership %>% ncol() # 39

# check country/year
sub_obj_fr_2_eu_membership %>% distinct(country) %>% nrow() # 45
sub_obj_fr_2_eu_membership %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_fr_2_eu_membership %>% count(indicator_name) # 900 (7 years * 45 countries = 315)
sub_obj_fr_2_eu_membership %>% count(year) %>% print(n = nrow(.))

# check values
sub_obj_fr_2_eu_membership %>% 
  filter(indicator_name == "sub_obj_fr_2_eu_membership") %>% 
  skim(values)
sub_obj_fr_2_eu_membership %>% group_by(year) %>% skim(values)


# plot
sub_obj_fr_2_eu_membership %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# inspect summary stats on indicators
sub_obj_fr_2_eu_membership %>% group_by(indicator_name) %>% 
  mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
         missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
         missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
  distinct(indicator_name, year_start, year_end, missing_pct)


#/////////////////


# read/write
sub_obj_fr_2_eu_membership %>% write_csv(file = "data/fmir/sub_obj_fr_2_eu_membership.csv")
sub_obj_fr_2_eu_membership <- read_csv(file = "data/fmir/sub_obj_fr_2_eu_membership.csv", 
                                                          lazy = FALSE)




#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
# load sub_obj_fr_3_csto_membership ####

CSTO_members_pre2024 <- c("Armenia",
                          "Belarus",
                          "Kazakhstan",
                          "Kyrgyzstan",
                          "Russia",
                          "Tajikistan")

CSTO_members_2024 <- c("Belarus",
                       "Kazakhstan",
                       "Kyrgyzstan",
                       "Russia",
                       "Tajikistan")

CSTO_observers <- c("Serbia")


sub_obj_fr_3_csto_membership <- read_csv(file = "data/memberships and categories/memberships_and_categories_toimport.csv",
                                      lazy = FALSE) %>%
  filter(!is.na(year)) %>%
  select(c(country,
           year)) %>%
  mutate(
    values = case_when(
      (country %in% CSTO_members_pre2024) & (year < 2024) ~ 1,
      (country %in% CSTO_members_2024) & (year == 2024) ~ 1,
      (country %in% CSTO_observers) ~ 0.5,
      TRUE ~ 0
    )
  ) %>%
mutate(high_value_is_good_outcome_flag = 0,
       indicator_name = "sub_obj_fr_3_csto_membership",
       year = as.numeric(year)) %>%
  filter(year != 2024)

#/////////////////


# inspect
sub_obj_fr_3_csto_membership
sub_obj_fr_3_csto_membership %>% glimpse()
sub_obj_fr_3_csto_membership %>% nrow() # 1074
sub_obj_fr_3_csto_membership %>% ncol() # 5

var_info("v2x_jucon")
sub_obj_fr_3_csto_membership %>% arrange(values) %>% distinct(country)
sub_obj_fr_3_csto_membership %>% arrange(desc(values)) %>% distinct(country)
sub_obj_fr_3_csto_membership %>% skim()

# inspect country names
sub_obj_fr_3_csto_membership %>% anti_join(., country_crosswalk, by = c("country" = "country")) %>% 
  distinct(country) %>% arrange(country)
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_fr_3_csto_membership, by = c("country" = "country")) %>% 
  distinct(country) %>% arrange(country)

sub_obj_fr_3_csto_membership %>% 
  filter(str_detect(string = country, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_fr_3_csto_membership <- sub_obj_fr_3_csto_membership %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country", "year" = "year")) %>%
  left_join(., fmir_framework, by = "indicator_name") 


#/////////////////


# inspect
sub_obj_fr_3_csto_membership
sub_obj_fr_3_csto_membership %>% glimpse()
sub_obj_fr_3_csto_membership %>% nrow() # 315
sub_obj_fr_3_csto_membership %>% ncol() # 39

# check country/year
sub_obj_fr_3_csto_membership %>% distinct(country) %>% nrow() # 45
sub_obj_fr_3_csto_membership %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_fr_3_csto_membership %>% count(indicator_name) # 900 (7 years * 45 countries = 315)
sub_obj_fr_3_csto_membership %>% count(year) %>% print(n = nrow(.))

# check values
sub_obj_fr_3_csto_membership %>% 
  filter(indicator_name == "sub_obj_fr_3_csto_membership") %>% 
  skim(values)
sub_obj_fr_3_csto_membership %>% group_by(year) %>% skim(values)


# plot
sub_obj_fr_3_csto_membership %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# inspect summary stats on indicators
sub_obj_fr_3_csto_membership %>% group_by(indicator_name) %>% 
  mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
         missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
         missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
  distinct(indicator_name, year_start, year_end, missing_pct)


#/////////////////


# read/write
sub_obj_fr_3_csto_membership %>% write_csv(file = "data/fmir/sub_obj_fr_3_csto_membership.csv")
sub_obj_fr_3_csto_membership <- read_csv(file = "data/fmir/sub_obj_fr_3_csto_membership.csv", 
                                                          lazy = FALSE)




#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# load sub_obj_fr_4_eaeu_membership ####

EAEU_members  <- c("Armenia",
                   "Belarus",
                   "Kazakhstan",
                   "Kyrgyzstan",
                   "Russia")

EAEU_observers_pre2021 <- c("Moldova")

EAEU_observers_2021 <- c("Moldova",
                         "Uzbekistan")

sub_obj_fr_4_eaeu_membership <- read_csv(file = "data/memberships and categories/memberships_and_categories_toimport.csv",
                                        lazy = FALSE) %>%
  filter(!is.na(year)) %>%
  select(c(country,
           year)) %>%
  mutate(
    values = case_when(
      (country %in% EAEU_members) ~ 1,
      (country %in% EAEU_observers_pre2021) & (year < 2021) ~ 0.5,
      (country %in% EAEU_observers_2021) & (year >= 2021) ~ 0.5,
      TRUE ~ 0
    )
  ) %>%
  mutate(high_value_is_good_outcome_flag = 0,
         indicator_name = "sub_obj_fr_4_eaeu_membership",
         year = as.numeric(year)) %>%
  filter(year != 2024)

#/////////////////


# inspect
sub_obj_fr_4_eaeu_membership
sub_obj_fr_4_eaeu_membership %>% glimpse()
sub_obj_fr_4_eaeu_membership %>% nrow() # 1074
sub_obj_fr_4_eaeu_membership %>% ncol() # 5

var_info("v2x_jucon")
sub_obj_fr_4_eaeu_membership %>% arrange(values) %>% distinct(country)
sub_obj_fr_4_eaeu_membership %>% arrange(desc(values)) %>% distinct(country)
sub_obj_fr_4_eaeu_membership %>% skim()

# inspect country names
sub_obj_fr_4_eaeu_membership %>% anti_join(., country_crosswalk, by = c("country" = "country")) %>% 
  distinct(country) %>% arrange(country)
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_fr_4_eaeu_membership, by = c("country" = "country")) %>% 
  distinct(country) %>% arrange(country)

sub_obj_fr_4_eaeu_membership %>% 
  filter(str_detect(string = country, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_fr_4_eaeu_membership <- sub_obj_fr_4_eaeu_membership %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country", "year" = "year")) %>%
  left_join(., fmir_framework, by = "indicator_name") 


#/////////////////


# inspect
sub_obj_fr_4_eaeu_membership
sub_obj_fr_4_eaeu_membership %>% glimpse()
sub_obj_fr_4_eaeu_membership %>% nrow() # 315
sub_obj_fr_4_eaeu_membership %>% ncol() # 39

# check country/year
sub_obj_fr_4_eaeu_membership %>% distinct(country) %>% nrow() # 45
sub_obj_fr_4_eaeu_membership %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_fr_4_eaeu_membership %>% count(indicator_name) # 900 (7 years * 45 countries = 315)
sub_obj_fr_4_eaeu_membership %>% count(year) %>% print(n = nrow(.))

# check values
sub_obj_fr_4_eaeu_membership %>% 
  filter(indicator_name == "sub_obj_fr_4_eaeu_membership") %>% 
  skim(values)
sub_obj_fr_4_eaeu_membership %>% group_by(year) %>% skim(values)


# plot
sub_obj_fr_4_eaeu_membership %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# inspect summary stats on indicators
sub_obj_fr_4_eaeu_membership %>% group_by(indicator_name) %>% 
  mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
         missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
         missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
  distinct(indicator_name, year_start, year_end, missing_pct)


#/////////////////


# read/write
sub_obj_fr_4_eaeu_membership %>% write_csv(file = "data/fmir/sub_obj_fr_4_eaeu_membership.csv")
sub_obj_fr_4_eaeu_membership <- read_csv(file = "data/fmir/sub_obj_fr_4_eaeu_membership.csv", 
                                        lazy = FALSE)

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////

rm(list = ls(pattern = "^(CSTO|current|EAEU|exclude|FMI_actors)"))
