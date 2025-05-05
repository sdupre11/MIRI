# load sub_obj_4_1_wef_industrial_internal_sufficiency ####

sub_obj_4_1_wef_industrial_internal_sufficiency <- read_excel(path = "data/wef/eos/EOS Data Share - USAID  - 2024-08-26.xlsx") %>%
  filter(eos_title == "State of cluster development" & edition >= 2018) %>%
  select(-c("eos_title",
            "eos_id",
            "country_iso3")) %>%
  rename(year=edition,
         values=value) %>%
  pivot_wider(id_cols = country_name, 
              names_from = year, 
              values_from = values) %>%
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "values") %>%
  mutate(indicator_name = "sub_obj_4_1_wef_industrial_internal_sufficiency",
         high_value_is_good_outcome_flag = 1,
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Republic of North Macedonia" ~ "N. Macedonia",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States" ~ "U.S.",
                                  country_name == "USA" ~ "U.S.",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Slovak Republic" ~ "Slovakia",
                                  country_name == "Russian Federation" ~ "Russia",
                                  TRUE ~ country_name),
         year = as.numeric(year))

#//////////////////////////


# inspect
sub_obj_4_1_wef_industrial_internal_sufficiency
sub_obj_4_1_wef_industrial_internal_sufficiency %>% glimpse()
sub_obj_4_1_wef_industrial_internal_sufficiency %>% nrow() # 1015
sub_obj_4_1_wef_industrial_internal_sufficiency %>% ncol() # 5

# chack values
sub_obj_4_1_wef_industrial_internal_sufficiency %>% skim()
sub_obj_4_1_wef_industrial_internal_sufficiency %>% group_by(year) %>% skim()
sub_obj_4_1_wef_industrial_internal_sufficiency %>% count(year)
sub_obj_4_1_wef_industrial_internal_sufficiency %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_4_1_wef_industrial_internal_sufficiency %>% arrange(values) %>% distinct(country_name)


# inspect country names
# missing Belarus, Kyrgyzstan, and Turkmenistan
sub_obj_4_1_wef_industrial_internal_sufficiency %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_4_1_wef_industrial_internal_sufficiency, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country) %>% print(n = nrow(.))

sub_obj_4_1_wef_industrial_internal_sufficiency %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_4_1_wef_industrial_internal_sufficiency <- sub_obj_4_1_wef_industrial_internal_sufficiency %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_4_1_wef_industrial_internal_sufficiency",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_4_1_wef_industrial_internal_sufficiency 
sub_obj_4_1_wef_industrial_internal_sufficiency %>% glimpse()
sub_obj_4_1_wef_industrial_internal_sufficiency %>% nrow() # 315
sub_obj_4_1_wef_industrial_internal_sufficiency %>% ncol() # 39

sub_obj_4_1_wef_industrial_internal_sufficiency %>% distinct(country) %>% nrow() # 45

# check values
# range from 2006-2020
# Missing all values (2006-2020) for Kosovo
sub_obj_4_1_wef_industrial_internal_sufficiency %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_4_1_wef_industrial_internal_sufficiency %>% filter(is.na(values), year >= 2006) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_4_1_wef_industrial_internal_sufficiency %>% skim(values)
sub_obj_4_1_wef_industrial_internal_sufficiency %>% group_by(year) %>% skim(values)
sub_obj_4_1_wef_industrial_internal_sufficiency %>% filter(year >= 2006) %>% skim(values)
sub_obj_4_1_wef_industrial_internal_sufficiency %>% filter(year >= 2006) %>% group_by(country) %>% skim(values)

sub_obj_4_1_wef_industrial_internal_sufficiency %>% filter(is.na(values),
                                                     mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                                     year >= 2007) %>%
  select(country, mcp_grouping, year, values)
sub_obj_4_1_wef_industrial_internal_sufficiency %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_4_1_wef_industrial_internal_sufficiency %>% filter(year >= 2007) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_4_1_wef_industrial_internal_sufficiency %>% 
  # filter(mcp_grouping == "E&E Balkans") %>%
  filter(mcp_grouping == "E&E Eurasia") %>%
  # filter(mcp_grouping == "E&E graduates") %>%
  # filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# #/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_4_1_wef_industrial_internal_sufficiency %>% write_csv(file = "data/fmir/sub_obj_4_1_wef_industrial_internal_sufficiency.csv")
sub_obj_4_1_wef_industrial_internal_sufficiency <- read.csv(file = "data/fmir/sub_obj_4_1_wef_industrial_internal_sufficiency.csv") %>% 
  as_tibble()


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# load sub_obj_3_2_wef_market_dominance ####


sub_obj_3_2_wef_market_dominance <- read_excel(path = "data/wef/eos/EOS Data Share - USAID  - 2024-08-26.xlsx") %>%
  filter(eos_title == "Extent of market dominance" & edition >= 2018) %>%
  select(-c("eos_title",
            "eos_id",
            "country_iso3")) %>%
  rename(year=edition,
         values=value) %>%
  pivot_wider(id_cols = country_name, 
              names_from = year, 
              values_from = values) %>%
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "values") %>%
  mutate(indicator_name = "sub_obj_3_2_wef_market_dominance",
         high_value_is_good_outcome_flag = 1,
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Republic of North Macedonia" ~ "N. Macedonia",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States" ~ "U.S.",
                                  country_name == "USA" ~ "U.S.",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Slovak Republic" ~ "Slovakia",
                                  country_name == "Russian Federation" ~ "Russia",
                                  TRUE ~ country_name),
         year = as.numeric(year))

#//////////////////////////


# inspect
sub_obj_3_2_wef_market_dominance
sub_obj_3_2_wef_market_dominance %>% glimpse()
sub_obj_3_2_wef_market_dominance %>% nrow() # 1015
sub_obj_3_2_wef_market_dominance %>% ncol() # 5

# chack values
sub_obj_3_2_wef_market_dominance %>% skim()
sub_obj_3_2_wef_market_dominance %>% group_by(year) %>% skim()
sub_obj_3_2_wef_market_dominance %>% count(year)
sub_obj_3_2_wef_market_dominance %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_3_2_wef_market_dominance %>% arrange(values) %>% distinct(country_name)


# inspect country names
# missing Belarus, Kyrgyzstan, and Turkmenistan
sub_obj_3_2_wef_market_dominance %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_3_2_wef_market_dominance, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country) %>% print(n = nrow(.))

sub_obj_3_2_wef_market_dominance %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_3_2_wef_market_dominance <- sub_obj_3_2_wef_market_dominance %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_3_2_wef_market_dominance",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_3_2_wef_market_dominance 
sub_obj_3_2_wef_market_dominance %>% glimpse()
sub_obj_3_2_wef_market_dominance %>% nrow() # 315
sub_obj_3_2_wef_market_dominance %>% ncol() # 39

sub_obj_3_2_wef_market_dominance %>% distinct(country) %>% nrow() # 45

# check values
# range from 2006-2020
# Missing all values (2006-2020) for Kosovo
sub_obj_3_2_wef_market_dominance %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_3_2_wef_market_dominance %>% filter(is.na(values), year >= 2006) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_3_2_wef_market_dominance %>% skim(values)
sub_obj_3_2_wef_market_dominance %>% group_by(year) %>% skim(values)
sub_obj_3_2_wef_market_dominance %>% filter(year >= 2006) %>% skim(values)
sub_obj_3_2_wef_market_dominance %>% filter(year >= 2006) %>% group_by(country) %>% skim(values)

sub_obj_3_2_wef_market_dominance %>% filter(is.na(values),
                                                           mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                                           year >= 2007) %>%
  select(country, mcp_grouping, year, values)
sub_obj_3_2_wef_market_dominance %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_3_2_wef_market_dominance %>% filter(year >= 2007) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_3_2_wef_market_dominance %>% 
  # filter(mcp_grouping == "E&E Balkans") %>%
  filter(mcp_grouping == "E&E Eurasia") %>%
  # filter(mcp_grouping == "E&E graduates") %>%
  # filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# #/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_3_2_wef_market_dominance %>% write_csv(file = "data/fmir/sub_obj_3_2_wef_market_dominance.csv")
sub_obj_3_2_wef_market_dominance <- read.csv(file = "data/fmir/sub_obj_3_2_wef_market_dominance.csv") %>% 
  as_tibble()


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
# load sub_obj_4_3_wdi_vuln_employment ####

sub_obj_4_3_wdi_vuln_employment <- read_excel(path = "data/wbg/wdi/WDIEXCEL.xlsx",
                                              sheet = "Data") %>%
  filter(`Indicator Code` == "SL.EMP.VULN.ZS") %>%
  select(-(`1960`:`2017`)) %>%
  select(-c("Indicator Code",
            "Indicator Name",
            "Country Code")) %>%
  rename(country_name=`Country Name`) %>%
  mutate(`2023` = NA) %>%
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "values") %>%
  mutate(indicator_name = "sub_obj_4_3_wdi_vuln_employment",
         high_value_is_good_outcome_flag = 0,
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Republic of North Macedonia" ~ "N. Macedonia",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States" ~ "U.S.",
                                  country_name == "USA" ~ "U.S.",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Slovak Republic" ~ "Slovakia",
                                  country_name == "Russian Federation" ~ "Russia",
                                  country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                  TRUE ~ country_name),
         year = as.numeric(year))

#//////////////////////////


# inspect
sub_obj_4_3_wdi_vuln_employment
sub_obj_4_3_wdi_vuln_employment %>% glimpse()
sub_obj_4_3_wdi_vuln_employment %>% nrow() # 1862
sub_obj_4_3_wdi_vuln_employment %>% ncol() # 5

# chack values
sub_obj_4_3_wdi_vuln_employment %>% skim()
sub_obj_4_3_wdi_vuln_employment %>% group_by(year) %>% skim()
sub_obj_4_3_wdi_vuln_employment %>% count(year)
sub_obj_4_3_wdi_vuln_employment %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_4_3_wdi_vuln_employment %>% arrange(values) %>% distinct(country_name)


# inspect country names
# missing Belarus, Kyrgyzstan, and Turkmenistan
sub_obj_4_3_wdi_vuln_employment %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_4_3_wdi_vuln_employment, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country) %>% print(n = nrow(.))

sub_obj_4_3_wdi_vuln_employment %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_4_3_wdi_vuln_employment <- sub_obj_4_3_wdi_vuln_employment %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_4_3_wdi_vuln_employment") %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_4_3_wdi_vuln_employment 
sub_obj_4_3_wdi_vuln_employment %>% glimpse()
sub_obj_4_3_wdi_vuln_employment %>% nrow() # 315
sub_obj_4_3_wdi_vuln_employment %>% ncol() # 39

sub_obj_4_3_wdi_vuln_employment %>% distinct(country) %>% nrow() # 45

# check values
# range from 2006-2020
# Missing all values (2006-2020) for Kosovo
sub_obj_4_3_wdi_vuln_employment %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_4_3_wdi_vuln_employment %>% filter(is.na(values), year >= 2006) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_4_3_wdi_vuln_employment %>% skim(values)
sub_obj_4_3_wdi_vuln_employment %>% group_by(year) %>% skim(values)
sub_obj_4_3_wdi_vuln_employment %>% filter(year >= 2006) %>% skim(values)
sub_obj_4_3_wdi_vuln_employment %>% filter(year >= 2006) %>% group_by(country) %>% skim(values)

sub_obj_4_3_wdi_vuln_employment %>% filter(is.na(values),
                                            mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                            year >= 2007) %>%
  select(country, mcp_grouping, year, values)
sub_obj_4_3_wdi_vuln_employment %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_4_3_wdi_vuln_employment %>% filter(year >= 2007) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_4_3_wdi_vuln_employment %>% 
  # filter(mcp_grouping == "E&E Balkans") %>%
  filter(mcp_grouping == "E&E Eurasia") %>%
  # filter(mcp_grouping == "E&E graduates") %>%
  # filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# #/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_4_3_wdi_vuln_employment %>% write_csv(file = "data/fmir/sub_obj_4_3_wdi_vuln_employment.csv")
sub_obj_4_3_wdi_vuln_employment <- read.csv(file = "data/fmir/sub_obj_4_3_wdi_vuln_employment.csv") %>% 
  as_tibble()


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
# load sub_obj_4_2_wdi_gdp_per_capita ####

sub_obj_4_2_wdi_gdp_per_capita <- read_excel(path = "data/wbg/wdi/WDIEXCEL.xlsx",
                                              sheet = "Data") %>%
  filter(`Indicator Code` == "NY.GDP.PCAP.CD") %>%
  select(-(`1960`:`2017`)) %>%
  select(-c("Indicator Code",
            "Indicator Name",
            "Country Code")) %>%
  rename(country_name=`Country Name`) %>%
  mutate(`2023` = NA) %>%
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "values") %>%
  mutate(indicator_name = "sub_obj_4_2_wdi_gdp_per_capita",
         high_value_is_good_outcome_flag = 1,
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Republic of North Macedonia" ~ "N. Macedonia",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States" ~ "U.S.",
                                  country_name == "USA" ~ "U.S.",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Slovak Republic" ~ "Slovakia",
                                  country_name == "Russian Federation" ~ "Russia",
                                  country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                  TRUE ~ country_name),
         year = as.numeric(year))

#//////////////////////////


# inspect
sub_obj_4_2_wdi_gdp_per_capita
sub_obj_4_2_wdi_gdp_per_capita %>% glimpse()
sub_obj_4_2_wdi_gdp_per_capita %>% nrow() # 1862
sub_obj_4_2_wdi_gdp_per_capita %>% ncol() # 5

# chack values
sub_obj_4_2_wdi_gdp_per_capita %>% skim()
sub_obj_4_2_wdi_gdp_per_capita %>% group_by(year) %>% skim()
sub_obj_4_2_wdi_gdp_per_capita %>% count(year)
sub_obj_4_2_wdi_gdp_per_capita %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_4_2_wdi_gdp_per_capita %>% arrange(values) %>% distinct(country_name)


# inspect country names
# missing Belarus, Kyrgyzstan, and Turkmenistan
sub_obj_4_2_wdi_gdp_per_capita %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_4_2_wdi_gdp_per_capita, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country) %>% print(n = nrow(.))

sub_obj_4_2_wdi_gdp_per_capita %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_4_2_wdi_gdp_per_capita <- sub_obj_4_2_wdi_gdp_per_capita %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_4_2_wdi_gdp_per_capita") %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_4_2_wdi_gdp_per_capita 
sub_obj_4_2_wdi_gdp_per_capita %>% glimpse()
sub_obj_4_2_wdi_gdp_per_capita %>% nrow() # 315
sub_obj_4_2_wdi_gdp_per_capita %>% ncol() # 39

sub_obj_4_2_wdi_gdp_per_capita %>% distinct(country) %>% nrow() # 45

# check values
# range from 2006-2020
# Missing all values (2006-2020) for Kosovo
sub_obj_4_2_wdi_gdp_per_capita %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_4_2_wdi_gdp_per_capita %>% filter(is.na(values), year >= 2006) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_4_2_wdi_gdp_per_capita %>% skim(values)
sub_obj_4_2_wdi_gdp_per_capita %>% group_by(year) %>% skim(values)
sub_obj_4_2_wdi_gdp_per_capita %>% filter(year >= 2006) %>% skim(values)
sub_obj_4_2_wdi_gdp_per_capita %>% filter(year >= 2006) %>% group_by(country) %>% skim(values)

sub_obj_4_2_wdi_gdp_per_capita %>% filter(is.na(values),
                                           mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                           year >= 2007) %>%
  select(country, mcp_grouping, year, values)
sub_obj_4_2_wdi_gdp_per_capita %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_4_2_wdi_gdp_per_capita %>% filter(year >= 2007) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_4_2_wdi_gdp_per_capita %>% 
  # filter(mcp_grouping == "E&E Balkans") %>%
  filter(mcp_grouping == "E&E Eurasia") %>%
  # filter(mcp_grouping == "E&E graduates") %>%
  # filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# #/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_4_2_wdi_gdp_per_capita %>% write_csv(file = "data/fmir/sub_obj_4_2_wdi_gdp_per_capita.csv")
sub_obj_4_2_wdi_gdp_per_capita <- read.csv(file = "data/fmir/sub_obj_4_2_wdi_gdp_per_capita.csv") %>% 
  as_tibble()


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
# load sub_obj_2_3_wb_voice_and_accountability ####

sub_obj_2_3_wb_voice_and_accountability <- read_excel(path = "data/wbg/wgi/wgidataset_cleaned_11_7_2024.xlsx",
                                              sheet = "VoiceandAccountability") %>%
  select(-`2024`) %>%
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "values") %>%
  mutate(indicator_name = "sub_obj_2_3_wb_voice_and_accountability",
         high_value_is_good_outcome_flag = 1,
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Republic of North Macedonia" ~ "N. Macedonia",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States" ~ "U.S.",
                                  country_name == "USA" ~ "U.S.",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Slovak Republic" ~ "Slovakia",
                                  country_name == "Russian Federation" ~ "Russia",
                                  country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                  TRUE ~ country_name),
         year = as.numeric(year))

#//////////////////////////


# inspect
sub_obj_2_3_wb_voice_and_accountability
sub_obj_2_3_wb_voice_and_accountability %>% glimpse()
sub_obj_2_3_wb_voice_and_accountability %>% nrow() # 1498
sub_obj_2_3_wb_voice_and_accountability %>% ncol() # 5

# chack values
sub_obj_2_3_wb_voice_and_accountability %>% skim()
sub_obj_2_3_wb_voice_and_accountability %>% group_by(year) %>% skim()
sub_obj_2_3_wb_voice_and_accountability %>% count(year)
sub_obj_2_3_wb_voice_and_accountability %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_2_3_wb_voice_and_accountability %>% arrange(values) %>% distinct(country_name)


# inspect country names
# missing Belarus, Kyrgyzstan, and Turkmenistan
sub_obj_2_3_wb_voice_and_accountability %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_2_3_wb_voice_and_accountability, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country) %>% print(n = nrow(.))

sub_obj_2_3_wb_voice_and_accountability %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_2_3_wb_voice_and_accountability <- sub_obj_2_3_wb_voice_and_accountability %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_2_3_wb_voice_and_accountability") %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_2_3_wb_voice_and_accountability 
sub_obj_2_3_wb_voice_and_accountability %>% glimpse()
sub_obj_2_3_wb_voice_and_accountability %>% nrow() # 315
sub_obj_2_3_wb_voice_and_accountability %>% ncol() # 39

sub_obj_2_3_wb_voice_and_accountability %>% distinct(country) %>% nrow() # 45

# check values
# range from 2006-2020
# Missing all values (2006-2020) for Kosovo
sub_obj_2_3_wb_voice_and_accountability %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_2_3_wb_voice_and_accountability %>% filter(is.na(values), year >= 2006) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_2_3_wb_voice_and_accountability %>% skim(values)
sub_obj_2_3_wb_voice_and_accountability %>% group_by(year) %>% skim(values)
sub_obj_2_3_wb_voice_and_accountability %>% filter(year >= 2006) %>% skim(values)
sub_obj_2_3_wb_voice_and_accountability %>% filter(year >= 2006) %>% group_by(country) %>% skim(values)

sub_obj_2_3_wb_voice_and_accountability %>% filter(is.na(values),
                                           mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                           year >= 2007) %>%
  select(country, mcp_grouping, year, values)
sub_obj_2_3_wb_voice_and_accountability %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_2_3_wb_voice_and_accountability %>% filter(year >= 2007) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_2_3_wb_voice_and_accountability %>% 
  # filter(mcp_grouping == "E&E Balkans") %>%
  filter(mcp_grouping == "E&E Eurasia") %>%
  # filter(mcp_grouping == "E&E graduates") %>%
  # filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# #/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_2_3_wb_voice_and_accountability %>% write_csv(file = "data/fmir/sub_obj_2_3_wb_voice_and_accountability.csv")
sub_obj_2_3_wb_voice_and_accountability <- read.csv(file = "data/fmir/sub_obj_2_3_wb_voice_and_accountability.csv") %>% 
  as_tibble()


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
# load sub_obj_3_2_rise_energy_efficiency_regulation_rank ####


# https://rise.worldbank.org/scores
# note historical data was sent directly from RIS team by email 2/15/2023 1210pm by Daron Bedrosyan <dbedrosyan@worldbank.org>

rise_energy_efficiency_regulation_rank_2021 <- read_excel(path = "data/wbg/rise/EE Full Time Series RISE 2022.xlsx",
                                                                      sheet = "ee_website_data_2021") %>%
  select(Country, `EE Overall Score`) %>% 
  rename(country_name = Country, values = `EE Overall Score`) %>%
  slice(-1) %>% mutate(values = as.numeric(values),
                       year = 2021)

rise_energy_efficiency_regulation_rank_2020 <- read_excel(path = "data/wbg/rise/EE Full Time Series RISE 2022.xlsx",
                                                                      sheet = "ee_website_data_2020") %>%
  select(Country, `EE Overall Score`) %>% 
  rename(country_name = Country, values = `EE Overall Score`) %>%
  slice(-1) %>% mutate(values = as.numeric(values),
                       year = 2020)

rise_energy_efficiency_regulation_rank_2019 <- read_excel(path = "data/wbg/rise/EE Full Time Series RISE 2022.xlsx",
                                                                      sheet = "ee_website_data_2019") %>%
  select(Country, `EE Overall Score`) %>% 
  rename(country_name = Country, values = `EE Overall Score`) %>%
  slice(-1) %>% mutate(values = as.numeric(values),
                       year = 2019)

rise_energy_efficiency_regulation_rank_2018 <- read_excel(path = "data/wbg/rise/EE Full Time Series RISE 2022.xlsx",
                                                                      sheet = "ee_website_data_2018") %>%
  select(Country, `EE Overall Score`) %>% 
  rename(country_name = Country, values = `EE Overall Score`) %>%
  slice(-1) %>% mutate(values = as.numeric(values),
                       year = 2018)


#////////////////////


# combine years to get sub_obj_3_2_rise_energy_efficiency_regulation_rank

# also, data for georgia and moldova was missing in 2021-report year release, but added in 2022-report year release
# though manual inspection shows really weird values for georgia and moldova 
# georgia gets all 0 ISV, close to afghanistan, and moldova has extreme rise in ISV
# will manually set moldova and georgia back to missing to be imputed, like last year
sub_obj_3_2_rise_energy_efficiency_regulation_rank <- rise_energy_efficiency_regulation_rank_2018 %>%
  bind_rows(., rise_energy_efficiency_regulation_rank_2019) %>%
  bind_rows(., rise_energy_efficiency_regulation_rank_2020) %>%
  bind_rows(., rise_energy_efficiency_regulation_rank_2021) %>%
  pivot_wider(id_cols = country_name, 
              names_from = year, 
              values_from = values) %>%
  mutate(`2022` = NA,
         `2023` = NA) %>%
  pivot_longer(cols = c(`2018`, `2019`, `2020`, `2021`, `2022`, `2023`), 
               names_to = "year", 
               values_to = "values") %>%
  mutate(country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Russian Federation" ~ "Russia",
                                  country_name == "Slovak Republic" ~ "Slovakia",
                                  country_name == "United States" ~ "U.S.",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  TRUE ~ country_name),
         indicator_name = "sub_obj_3_2_rise_energy_efficiency_regulation_rank",
         high_value_is_good_outcome_flag = 0,
         overall_rise_score = values) %>%
  group_by(year) %>% arrange(desc(overall_rise_score)) %>% 
  mutate(values = as.numeric(row_number())) %>%
  ungroup() %>%
  mutate(values = case_when(country_name %in% c("Georgia", "Moldova") ~ NA_real_,
                            TRUE ~ values),
         values = case_when(is.na(overall_rise_score) ~ NA_real_,
                            TRUE ~ values),
         year = as.numeric(year))


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////


# inspect 
sub_obj_3_2_rise_energy_efficiency_regulation_rank
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% glimpse()
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% nrow() # 994
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% ncol() # 6


sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% count(year)
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% select(country_name, year, values, overall_rise_score) %>%
  arrange(year, desc(overall_rise_score)) %>% print(n = 50)
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% arrange(values) %>% distinct(country_name)
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% arrange(year, values) %>% print(n = 100)

# inspect country names
# note rise does not have data for estonia, latvia, lithuania, luxembourg, slovenia
# also, data for georgia and moldova was missing in 2021-report year release, but added in 2022-report year release
# though manual inspection shows really weird values for georgia and moldova 
# georgia gets all 0 ISV, close to afghanistan, and moldova has extreme rise in ISV
# will manually set moldova and georgia back to missing to be imputed, like last year
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_3_2_rise_energy_efficiency_regulation_rank, by = c("country" = "country_name")) %>% select(country)

sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% 
  filter(str_detect(string = country_name, pattern = regex("United", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#////////////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# note need to updated indicator_name to make sure it has value for kosovo, which was not in raw data but was in crosswalk join
sub_obj_3_2_rise_energy_efficiency_regulation_rank <- sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_3_2_rise_energy_efficiency_regulation_rank",
         high_value_is_good_outcome_flag = 0) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////////


# inspect
sub_obj_3_2_rise_energy_efficiency_regulation_rank
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% glimpse()
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% nrow() # 315
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% ncol() # 40

# check country/year
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% distinct(country) %>% nrow() # 45
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% count(indicator_name) # 315 (7 years * 45 countries = 315)

# check values
# Missing all values for Estonia, Georgia, Latvia, Lithuania, Luxembourg, Moldova, Slovenia
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% filter(is.na(values), year >= 2010, year <= 2019) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% skim(values)
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% group_by(year) %>% skim(values)
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% filter(year >= 2009) %>% skim(values)
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% filter(year >= 2009) %>% group_by(country) %>% skim(values)

# check rank
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% 
  filter(year >= 2010, year <= 2019) %>%
  filter(mcp_grouping %in% c("U.S.", "E&E graduates")) %>%
  filter(year %in% c(2010, 2019)) %>%
  select(country, year, values, overall_rise_score) %>% arrange(year, values) %>% print(n = 100)
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% filter(year == 2019, values < 14) %>%
  select(country, year, values, overall_rise_score)

# plot
# note that georgia scores very low, and moldova very high, but i confirmed this on RISE interactive website and raw data
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% 
  # filter(mcp_grouping == "E&E Balkans") %>%
  filter(mcp_grouping == "E&E Eurasia") %>%
  # filter(mcp_grouping == "E&E graduates") %>%
  # filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  # filter(mcp_grouping == "U.S.") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# inspect summary stats on indicators
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% group_by(indicator_name) %>% 
  mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
         missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
         missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
  distinct(indicator_name, year_start, year_end, missing_pct)

sub_obj_3_2_rise_energy_efficiency_regulation_rank <- sub_obj_3_2_rise_energy_efficiency_regulation_rank %>%
  select(-overall_rise_score)



# read/write
sub_obj_3_2_rise_energy_efficiency_regulation_rank %>% write_csv(file = "data/fmir/sub_obj_3_2_rise_energy_efficiency_regulation_rank.csv")
sub_obj_3_2_rise_energy_efficiency_regulation_rank <- read_csv(file = "data/fmir/sub_obj_3_2_rise_energy_efficiency_regulation_rank.csv")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# load sub_obj_3_2_rise_renewable_energy_regulation_rank ####

# https://rise.worldbank.org/scores
# note historical data was sent directly from RIS team by email 2/15/2023 1210pm by Daron Bedrosyan <dbedrosyan@worldbank.org>

# note that for this renewable energy spreadsheet there's an error where US and Uzbekistan have two duplicate rows for each year
# will call distinct on each year data to drop duplicate records
sub_obj_3_2_rise_renewable_energy_regulation_rank_2021 <- read_excel(path = "data/wbg/rise/RE Full Time Series RISE 2022.xlsx",
                                                                     sheet = "re_website_data_2021") %>%
  select(Country, `RE Overall Score`) %>% 
  rename(country_name = Country, values = `RE Overall Score`) %>%
  slice(-1) %>% mutate(values = as.numeric(values),
                       year = 2021) %>%
  distinct()

sub_obj_3_2_rise_renewable_energy_regulation_rank_2020 <- read_excel(path = "data/wbg/rise/RE Full Time Series RISE 2022.xlsx",
                                                                     sheet = "re_website_data_2020") %>%
  select(Country, `RE Overall Score`) %>% 
  rename(country_name = Country, values = `RE Overall Score`) %>%
  slice(-1) %>% mutate(values = as.numeric(values),
                       year = 2020) %>%
  distinct()

sub_obj_3_2_rise_renewable_energy_regulation_rank_2019 <- read_excel(path = "data/wbg/rise/RE Full Time Series RISE 2022.xlsx",
                                                                     sheet = "re_website_data_2019") %>%
  select(Country, `RE Overall Score`) %>% 
  rename(country_name = Country, values = `RE Overall Score`) %>%
  slice(-1) %>% mutate(values = as.numeric(values),
                       year = 2019) %>%
  distinct()

sub_obj_3_2_rise_renewable_energy_regulation_rank_2018 <- read_excel(path = "data/wbg/rise/RE Full Time Series RISE 2022.xlsx",
                                                                     sheet = "re_website_data_2018") %>%
  select(Country, `RE Overall Score`) %>% 
  rename(country_name = Country, values = `RE Overall Score`) %>%
  slice(-1) %>% mutate(values = as.numeric(values),
                       year = 2018) %>%
  distinct()


#////////////////////


# combine years to get sub_obj_3_2_rise_renewable_energy_regulation_rank

# also, data for georgia and moldova was missing in 2021-report year release, but added in 2022-report year release
# though manual inspection shows really weird values for georgia and moldova 
# georgia gets all 0 ISV, close to afghanistan, and moldova has extreme rise in ISV
# will manually set moldova and georgia back to missing to be imputed, like last year
sub_obj_3_2_rise_renewable_energy_regulation_rank <- sub_obj_3_2_rise_renewable_energy_regulation_rank_2018 %>%
  bind_rows(., sub_obj_3_2_rise_renewable_energy_regulation_rank_2019) %>%
  bind_rows(., sub_obj_3_2_rise_renewable_energy_regulation_rank_2020) %>%
  bind_rows(., sub_obj_3_2_rise_renewable_energy_regulation_rank_2021) %>%
  pivot_wider(id_cols = country_name, 
              names_from = year, 
              values_from = values) %>%
  mutate(`2022` = NA,
         `2023` = NA) %>%
  pivot_longer(cols = c(`2018`, `2019`, `2020`, `2021`, `2022`, `2023`), 
               names_to = "year", 
               values_to = "values") %>%
  mutate(country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Russian Federation" ~ "Russia",
                                  country_name == "Slovak Republic" ~ "Slovakia",
                                  country_name == "United States" ~ "U.S.",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  TRUE ~ country_name),
         indicator_name = "sub_obj_3_2_rise_renewable_energy_regulation_rank",
         high_value_is_good_outcome_flag = 0,
         overall_rise_score = values) %>%
  group_by(year) %>% arrange(desc(overall_rise_score)) %>% mutate(values = as.numeric(row_number())) %>%
  ungroup() %>%
  mutate(values = case_when(country_name %in% c("Georgia", "Moldova") ~ NA_real_,
                            TRUE ~ values),
         values = case_when(is.na(overall_rise_score) ~ NA_real_,
                            TRUE ~ values),
         year = as.numeric(year))


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////


# inspect 
sub_obj_3_2_rise_renewable_energy_regulation_rank
sub_obj_3_2_rise_renewable_energy_regulation_rank %>% glimpse()
sub_obj_3_2_rise_renewable_energy_regulation_rank %>% nrow() # 87
sub_obj_3_2_rise_renewable_energy_regulation_rank %>% ncol() # 6


sub_obj_3_2_rise_renewable_energy_regulation_rank %>% arrange(year, values) %>% print(n = 100)
sub_obj_3_2_rise_renewable_energy_regulation_rank %>% select(country_name, year, values, overall_rise_score) %>%
  arrange(year, desc(overall_rise_score)) %>% print(n = 50)

# inspect country names
# note rise does not have data for estonia, latvia, lithuania, luxembourg, slovenia
# also, data for georgia and moldova was missing in 2021-report year release, but added in 2022-report year release
# though manual inspection shows really weird values for georgia and moldova 
# georgia gets all 0 ISV, close to afghanistan, and moldova has extreme rise in ISV
# will manually set moldova and georgia back to missing to be imputed, like last year
sub_obj_3_2_rise_renewable_energy_regulation_rank %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_3_2_rise_renewable_energy_regulation_rank, by = c("country" = "country_name")) %>% select(country)

sub_obj_3_2_rise_renewable_energy_regulation_rank %>% 
  filter(str_detect(string = country_name, pattern = regex("United", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#////////////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# note need to updated indicator_name to make sure it has value for kosovo, which was not in raw data but was in crosswalk join
sub_obj_3_2_rise_renewable_energy_regulation_rank <- sub_obj_3_2_rise_renewable_energy_regulation_rank %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_3_2_rise_renewable_energy_regulation_rank",
         high_value_is_good_outcome_flag = 0) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////////


# inspect
sub_obj_3_2_rise_renewable_energy_regulation_rank
sub_obj_3_2_rise_renewable_energy_regulation_rank %>% glimpse()
sub_obj_3_2_rise_renewable_energy_regulation_rank %>% nrow() # 945
sub_obj_3_2_rise_renewable_energy_regulation_rank %>% ncol() # 35

# check country/year
sub_obj_3_2_rise_renewable_energy_regulation_rank %>% distinct(country) %>% nrow() # 45
sub_obj_3_2_rise_renewable_energy_regulation_rank %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_3_2_rise_renewable_energy_regulation_rank %>% count(indicator_name) # 900 (20 years * 45 countries = 900)

# check values
# Missing all values (2010-2019) for Estonia, Georgia, Latvia, Lithuania, Luxembourg, Moldova, Slovenia
sub_obj_3_2_rise_renewable_energy_regulation_rank %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_3_2_rise_renewable_energy_regulation_rank %>% filter(is.na(values), year >= 2010) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_3_2_rise_renewable_energy_regulation_rank %>% skim(values)
sub_obj_3_2_rise_renewable_energy_regulation_rank %>% group_by(year) %>% skim(values)
sub_obj_3_2_rise_renewable_energy_regulation_rank %>% filter(year >= 2009) %>% skim(values)
sub_obj_3_2_rise_renewable_energy_regulation_rank %>% filter(year >= 2009) %>% group_by(country) %>% skim(values)

# check rank
sub_obj_3_2_rise_renewable_energy_regulation_rank %>% 
  filter(year >= 2010, year <= 2019) %>%
  filter(mcp_grouping %in% c("U.S.", "E&E graduates")) %>%
  filter(year %in% c(2010, 2019)) %>%
  select(country, year, values, overall_rise_score) %>% arrange(year, values) %>% print(n = 100)

# plot
# note that georgia scores very low, and moldova very high, but i confirmed this on RISE interactive website and raw data
sub_obj_3_2_rise_renewable_energy_regulation_rank %>% 
  filter(year >= 2010) %>%
  filter(mcp_grouping == "E&E Balkans") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# inspect summary stats on indicators
sub_obj_3_2_rise_renewable_energy_regulation_rank %>% group_by(indicator_name) %>% 
  mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
         missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
         missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
  distinct(indicator_name, year_start, year_end, missing_pct)

sub_obj_3_2_rise_renewable_energy_regulation_rank <- sub_obj_3_2_rise_renewable_energy_regulation_rank %>%
  select(-overall_rise_score)

# read/write
sub_obj_3_2_rise_renewable_energy_regulation_rank %>% write_csv(file = "data/fmir/sub_obj_3_2_rise_renewable_energy_regulation_rank.csv")
sub_obj_3_2_rise_renewable_energy_regulation_rank <- read_csv(file = "data/fmir/sub_obj_3_2_rise_renewable_energy_regulation_rank.csv")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
# load sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt ####

# https://www.worldbank.org/en/programs/debt-statistics/ids
# https://datahelpdesk.worldbank.org/knowledgebase/topics/19287-external-debt

# note that clicking on "DataBank" brings you to data download menu where parameters are set for bulk download
# by clicking on specific country it brings you to data explorer for specific country - can be useful for quick viewing/checking
# view old spreadsheet to see parameters 
# click checkbox for all countries, counterparts are just russia/china/world, a few indicators, 2000-current


# detailed definition of "external debt" here: https://data.worldbank.org/indicator/DT.DOD.DECT.CD
# Total external debt is debt owed to nonresidents repayable in currency, goods, or services. 
# Total external debt is the sum of public, publicly guaranteed, and private nonguaranteed long-term debt, 
# use of IMF credit, and short-term debt. Short-term debt includes all debt having an original maturity of one year 
# or less and interest in arrears on long-term debt. Data are in current U.S. dollars.

# from debt statistics faq: https://pubdocs.worldbank.org/en/342211633111359690/Debt-Statistics-FAQ.pdf
# "DRS classifies official creditors as bilateral agencies and multilateral institutions. Bilateral creditors
# comprise these agencies that make loans on behalf of the government:
# . Development aid agencies e.g. European Bank for Reconstruction and Development
# . Official Export credit agencies e.g. Export Import Banks
# . Central banks and state monetary authorities
# . Other official bilateral agencies

# DRS classifies a creditor entity as private based on legal status the entity. The categories of these
# creditors are:
#         . Exporter: supplier of goods, such as manufacturer or trading company who extends credits
# for the purchase of the goods directly
# . Private bank: an institution engaged in commercial banking activities, whether the
# ownership of the bank is public or private
# . Bond holder: a security with a promise to pay a specified amount of money at a fixed date
# and income at periodic dates until maturity. It includes publicly placed bonds and privately
# placed bonds
# . Other financial institutions: any private entity other than exporters, private banks or
# bondholders

# remember to manually delete the "Data from database: International Debt Statistics" and "Last Updated: mm/dd/yyyy" records on
# the downloaded file from databank

# note that stock of external debt is better indicator for index than flow,
# because flow could have negative values, which would get a very low score when standardized, and
# would lead to more jumpy shifts, whereas stocks are more stable over time, which is what we want for the index

# note that as of 20220228 download, there were no values for 2021-2022
sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt <- read_excel(path = "data/wbg/ids/P_Data_Extract_From_International_Debt_Statistics_9_9_2024.xlsx", 
                                                                           na = "..") %>%
  rename(country_name = `Country Name`, 
         counterpart = `Counterpart-Area Name`,
         variable = `Series Name`) %>%
  select(-c(`Country Code`, `Counterpart-Area Code`, `Series Code`)) %>%
  pivot_longer(cols = -c(country_name, counterpart, variable), names_to = "year", values_to = "values") %>%
  mutate(year = as.numeric(str_sub(string = year, start = 1, end = 4)),
         high_value_is_good_outcome_flag = 0,
         indicator_name = "sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt",
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Russian Federation" ~ "Russia",
                                  country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                  TRUE ~ country_name),
         counterpart = case_when(counterpart == "Russian Federation" ~ "Russia",
                                 counterpart == "Turkiye" ~ "Turkey",
                                 counterpart == "Iran, Islamic Republic Of" ~ "Iran",
                                 TRUE ~ counterpart)) %>%
  arrange(country_name, year) %>%
  filter(counterpart != "Turkey")

world <- sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt %>%
  filter(counterpart == "World") %>%
  select(-variable) %>%
  rename(world_values = values)
  
fmi <-sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt %>%
  filter(counterpart != "World")

fmi[is.na(fmi)] <- 0

fmi <- fmi %>%
  group_by(country_name,
           indicator_name,
           year,
           high_value_is_good_outcome_flag) %>%
  summarize(fmi_values = sum(values)) %>%
  mutate(counterpart = "FMI")

world_fmi <- full_join(world, 
                       fmi, 
                       by = c("country_name", 
                              "year")) %>%
  select(-c("counterpart.x",
            "counterpart.y",
            "high_value_is_good_outcome_flag.x",
            "indicator_name.x")) %>%
  rename(high_value_is_good_outcome_flag = high_value_is_good_outcome_flag.y,
         indicator_name = indicator_name.y) %>%
  mutate(values = ((fmi_values/world_values)*100)) %>%
  select(-c("fmi_values",
            "world_values")) %>%
  pivot_wider(id_cols = c("country_name",
                          "high_value_is_good_outcome_flag",
                          "indicator_name"),
              names_from = year, 
              values_from = values) %>%
  mutate(`2023` = NA
         ) %>%
  select(-`2024`) %>%
  pivot_longer(cols = `2018`:`2023`,
               names_to = "year",
               values_to = "values") %>%
  mutate(
    values = case_when(
      values == 0.000000000 ~ NA,
      TRUE ~ values
    ),
    year = as.numeric(year)
  )

sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt <- world_fmi

rm(world)
rm(fmi)
rm(world_fmi)

#/////////////////


# inspect
sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt
sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt %>% glimpse()
sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt %>% nrow() # 952
sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt %>% ncol() # 5

# inspect country names
# note that 26 of 45 countries do not have a world bank IDS record because they arent' required to report external debt as
# a condition of any assistance package; will analyze missing data further below
sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt %>% 
  anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country) %>% print(n = nrow(.))

sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt %>% 
  filter(str_detect(string = country_name, pattern = regex("United", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#//////////////////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt <- sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt",
         high_value_is_good_outcome_flag = 0) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////////////////


# inspect
sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt
sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt %>% glimpse()
sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt %>% nrow() # 315
sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt %>% ncol() # 39

# check country/year
sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt %>% count(country) # 45
sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt %>% count(mcp_grouping)
sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt %>% count(year) # 20

# check values
#sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt %>% skim(ext_debt_to_russia_as_share_of_total_ext_debt, values)

#This note is from Steve, but still applies
# inspect 25 countries that do not have any records in world_bank IDS because they aren't required to report as condition of assistance
# missing records for all years for US, all EU-15 countries, and 9 of the 12 graduates 
# (excluding bulgaria, montenegro, and romania)
sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt %>% count(indicator_name)

# inspect missing values among those country/years that do have a record
# note that excluding countries w/o records, ext_debt_to_world is only missing 1.2% of records; ext_debt_to_russia is missing ~49%
# for ext_debt_to_world, only montenegro doesn't have 100% complete_rate, which is missing for 2001-2005
# for ext_debt_to_russia, BiH, kazakhstan, kosovo, macedonia, romania, russia, and turkmenistan have 0% complete_rate
# only six countries had 100% complete_rate for ext_debt_to_russia (albania, belarus, georgia, moldova, serbia, ukraine)

# note that for those countries with at least one non-NA ext_debt_to_russia, there is odd timing in the end of NA reporting
# e.g., some countries reported non-NA ext_debt_to_russia values for new years only 
# azerbaijan (2019-2020) 

# e.g., others have the reverse problem, where they reported non-NA ext_debt_to_russia values for old years only
# bulgaria (2000-2008), tajikistan (2000-2008), kyrgyzstan (2000-2018)

# e.g., others are missing ext_debt_to_russia sporadically armenia (2007-2008, 2014), uzbkeistan (2017-2018)

# note there are some country/year observations with a values of zero, as opposed to NA, so it's unclear whether 
# the NA values should be interpreted as zero or NA (and therefore imputed to non-zero value)
# e.g., in the case of kazakhstan, it seems more likely to be lack of reporting as opposed to true zero
# e.g., in the case of kosovo, it seems plausible that it's a true zero as opposed to lack of reporting
# result: since many countries have non-NA values interspersed with NA values (eg armenia), and we do want to impute those NAs,
# then the best choice is to leave all NAs and impute them, and so the only zero values will be those actually reported as zero
# note that the balkans with non-NA values are all close to zero (under 3%),
# graduates only have montenegro and bulgaria with non-NA values both <1%, so imputation for missing grads and EU is negligible
# so the regional_avg used to impute for those countries missing all values will
# be close to zero, and therefore not much difference for them
sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt %>%
  filter(year >= 2010) %>%
  group_by(country, mcp_grouping) %>%
  skim(values) %>%
  as_tibble() %>% select(skim_variable, country, mcp_grouping, n_missing, complete_rate) %>%
  arrange(desc(n_missing)) %>% 
  print(n = nrow(.))
sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt %>%
  filter(year >= 2010) %>%
  group_by(country, mcp_grouping) %>%
  skim(values) %>%
  as_tibble() %>% select(skim_variable, country, mcp_grouping, n_missing, complete_rate) %>%
  count(mcp_grouping, n_missing) %>%
  arrange(desc(n_missing))

#//////////////////


# plot
sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt %>% 
  filter(mcp_grouping == "E&E graduates") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

#//////////////////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt %>%
         write_csv(file = "data/fmir/sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt.csv")
sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt <- read.csv(file = "data/fmir/sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt.csv") %>%
  as_tibble()

# inspect
sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt
sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt %>% glimpse()
sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt %>% nrow() # 315
sub_obj_4_2_wb_ext_debt_to_fmi_as_share_of_total_ext_debt %>% ncol() # 39

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
# load sub_obj_4_3_wb_remittance_as_share_of_gdp ####

sub_obj_4_3_wb_remittance_as_share_of_gdp <- read_excel(path = "data/wbg/wdi/remittance_data_11072024.xls",
                                                      sheet = "Data",
                                                      skip = 3) %>%
  select(c("Country Name", "2018", "2019", "2020", "2021", "2022", "2023")) %>%
  rename(country_name = `Country Name`) %>%
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "values") %>%
  mutate(indicator_name = "sub_obj_4_3_wb_remittance_as_share_of_gdp",
         high_value_is_good_outcome_flag = 0,
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Republic of North Macedonia" ~ "N. Macedonia",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States" ~ "U.S.",
                                  country_name == "USA" ~ "U.S.",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Slovak Republic" ~ "Slovakia",
                                  country_name == "Russian Federation" ~ "Russia",
                                  country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                  TRUE ~ country_name),
         year = as.numeric(year))

#//////////////////////////


# inspect
sub_obj_4_3_wb_remittance_as_share_of_gdp
sub_obj_4_3_wb_remittance_as_share_of_gdp %>% glimpse()
sub_obj_4_3_wb_remittance_as_share_of_gdp %>% nrow() # 1498
sub_obj_4_3_wb_remittance_as_share_of_gdp %>% ncol() # 5

# chack values
sub_obj_4_3_wb_remittance_as_share_of_gdp %>% skim()
sub_obj_4_3_wb_remittance_as_share_of_gdp %>% group_by(year) %>% skim()
sub_obj_4_3_wb_remittance_as_share_of_gdp %>% count(year)
sub_obj_4_3_wb_remittance_as_share_of_gdp %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_4_3_wb_remittance_as_share_of_gdp %>% arrange(values) %>% distinct(country_name)


# inspect country names
# missing Belarus, Kyrgyzstan, and Turkmenistan
sub_obj_4_3_wb_remittance_as_share_of_gdp %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_4_3_wb_remittance_as_share_of_gdp, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country) %>% print(n = nrow(.))

sub_obj_4_3_wb_remittance_as_share_of_gdp %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_4_3_wb_remittance_as_share_of_gdp <- sub_obj_4_3_wb_remittance_as_share_of_gdp %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_4_3_wb_remittance_as_share_of_gdp") %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_4_3_wb_remittance_as_share_of_gdp 
sub_obj_4_3_wb_remittance_as_share_of_gdp %>% glimpse()
sub_obj_4_3_wb_remittance_as_share_of_gdp %>% nrow() # 315
sub_obj_4_3_wb_remittance_as_share_of_gdp %>% ncol() # 36

sub_obj_4_3_wb_remittance_as_share_of_gdp %>% distinct(country) %>% nrow() # 45

# check values
# range from 2006-2020
# Missing all values (2006-2020) for Kosovo
sub_obj_4_3_wb_remittance_as_share_of_gdp %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_4_3_wb_remittance_as_share_of_gdp %>% filter(is.na(values), year >= 2006) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_4_3_wb_remittance_as_share_of_gdp %>% skim(values)
sub_obj_4_3_wb_remittance_as_share_of_gdp %>% group_by(year) %>% skim(values)
sub_obj_4_3_wb_remittance_as_share_of_gdp %>% filter(year >= 2006) %>% skim(values)
sub_obj_4_3_wb_remittance_as_share_of_gdp %>% filter(year >= 2006) %>% group_by(country) %>% skim(values)

sub_obj_4_3_wb_remittance_as_share_of_gdp %>% filter(is.na(values),
                                                   mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                                   year >= 2007) %>%
  select(country, mcp_grouping, year, values)
sub_obj_4_3_wb_remittance_as_share_of_gdp %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_4_3_wb_remittance_as_share_of_gdp %>% filter(year >= 2007) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_4_3_wb_remittance_as_share_of_gdp %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# #/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_4_3_wb_remittance_as_share_of_gdp %>% write_csv(file = "data/fmir/sub_obj_4_3_wb_remittance_as_share_of_gdp.csv")
sub_obj_4_3_wb_remittance_as_share_of_gdp <- read.csv(file = "data/fmir/sub_obj_4_3_wb_remittance_as_share_of_gdp.csv") %>% 
  as_tibble()



rm(list = ls(pattern = "rank_20"))
