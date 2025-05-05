
# load sub_obj_1_2_fhi_civil_society ####

sub_obj_1_2_fhi_civil_society <- read_excel(path = "data/fhi/nit/All_Data_Nations_in_Transit_NIT_2005-2024_For_website.xlsx",
                                          sheet = "NIT 2005-2022") %>% 
  select(c("Country",
           "Year",
           "Civil Society")) %>%
  rename(country_name = Country,
         year = Year,
         values = `Civil Society`) %>%
  filter(year>=2018 & year != 2024) %>%
  mutate(indicator_name = "sub_obj_1_2_fhi_civil_society",
         high_value_is_good_outcome_flag = 1,
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States" ~ "U.S.",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Slovak Republic" ~ "Slovakia",
                                  TRUE ~ country_name),
         year = as.numeric(year))

#//////////////////////////


# inspect
sub_obj_1_2_fhi_civil_society
sub_obj_1_2_fhi_civil_society %>% glimpse()
sub_obj_1_2_fhi_civil_society %>% nrow() # 203
sub_obj_1_2_fhi_civil_society %>% ncol() # 5

# chack values
sub_obj_1_2_fhi_civil_society %>% skim()
sub_obj_1_2_fhi_civil_society %>% group_by(year) %>% skim()
sub_obj_1_2_fhi_civil_society %>% count(year)
sub_obj_1_2_fhi_civil_society %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_1_2_fhi_civil_society %>% arrange(values) %>% distinct(country_name)


# inspect country names
# many are missing for EU (UK and US too)
sub_obj_1_2_fhi_civil_society %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_1_2_fhi_civil_society, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country) %>% print(n = nrow(.))

sub_obj_1_2_fhi_civil_society %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_1_2_fhi_civil_society <- sub_obj_1_2_fhi_civil_society %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_1_2_fhi_civil_society",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_1_2_fhi_civil_society 
sub_obj_1_2_fhi_civil_society %>% glimpse()
sub_obj_1_2_fhi_civil_society %>% nrow() # 315
sub_obj_1_2_fhi_civil_society %>% ncol() # 40

sub_obj_1_2_fhi_civil_society %>% distinct(country) %>% nrow() # 45

# check values
sub_obj_1_2_fhi_civil_society %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_1_2_fhi_civil_society %>% filter(is.na(values), year >= 2006) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_1_2_fhi_civil_society %>% skim(values)
sub_obj_1_2_fhi_civil_society %>% group_by(year) %>% skim(values)
sub_obj_1_2_fhi_civil_society %>% filter(year >= 2006) %>% skim(values)
sub_obj_1_2_fhi_civil_society %>% filter(year >= 2006) %>% group_by(country) %>% skim(values)

sub_obj_1_2_fhi_civil_society %>% filter(is.na(values),
                                       mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                       year >= 2007) %>%
  select(country, mcp_grouping, year, values)
sub_obj_1_2_fhi_civil_society %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_1_2_fhi_civil_society %>% filter(year >= 2007) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_1_2_fhi_civil_society %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 


# #/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_1_2_fhi_civil_society %>% write_csv(file = "data/fmir/sub_obj_1_2_fhi_civil_society.csv")
sub_obj_1_2_fhi_civil_society <- read.csv(file = "data/fmir/sub_obj_1_2_fhi_civil_society.csv") %>% 
  as_tibble()




#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
# load sub_obj_1_2_fhi_csosi_overall ####

#Taking in 2013 data and onwards because of the CAR countries. Their data is missing for 2014 onwards. Little apparent
#volatility in their numbers is present.


sub_obj_1_2_fhi_csosi_overall <- read_excel(path = "data/fhi/fhi360/CSOSI-Scores.xlsx",
                                            sheet = "Overall") %>% 
  select(c("Country",
           `2013.0`,
           `2014.0`,
           `2015.0`,
           `2016.0`,
           `2017.0`,
           `2018.0`,
           `2019.0`,
           `2020.0`,
           `2021.0`,
           `2022.0`,
           `2023.0`)) %>%
  rename(country_name = Country,
         `2013` = `2013.0`,
         `2014` =`2014.0`,
         `2015` =`2015.0`,
         `2016` =`2016.0`,
         `2017` =`2017.0`,
         `2018` =`2018.0`,
         `2019` =`2019.0`,
         `2020` =`2020.0`,
         `2021` =`2021.0`,
         `2022` =`2022.0`,
         `2023` =`2023.0`) %>%
  mutate( #This is to extend the most-recent CAR data out to 2018
    `2018` = case_when(
      is.na(`2018`) ~ `2013`,
      TRUE ~ `2018`)
  ) %>%
  dplyr::select(!(`2013`:`2017`)) %>%
  pivot_longer(cols = c(`2018`,`2019`,`2020`,`2021`,`2022`,`2023`), 
               names_to = "year", 
               values_to = "values") %>%
  mutate(indicator_name = "sub_obj_1_2_fhi_csosi_overall",
         high_value_is_good_outcome_flag = 0,
         country_name = case_when(country_name == "Bosnia-Herzegovina" ~ "BiH",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States" ~ "U.S.",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Slovak Republic" ~ "Slovakia",
                                  TRUE ~ country_name),
         year = as.numeric(year))

#//////////////////////////


# inspect
sub_obj_1_2_fhi_csosi_overall
sub_obj_1_2_fhi_csosi_overall %>% glimpse()
sub_obj_1_2_fhi_csosi_overall %>% nrow() # 574
sub_obj_1_2_fhi_csosi_overall %>% ncol() # 5

# chack values
sub_obj_1_2_fhi_csosi_overall %>% skim()
sub_obj_1_2_fhi_csosi_overall %>% group_by(year) %>% skim()
sub_obj_1_2_fhi_csosi_overall %>% count(year)
sub_obj_1_2_fhi_csosi_overall %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_1_2_fhi_csosi_overall %>% arrange(values) %>% distinct(country_name)


# inspect country names
# many are missing for EU (UK and US too)
sub_obj_1_2_fhi_csosi_overall %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_1_2_fhi_csosi_overall, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country) %>% print(n = nrow(.))

sub_obj_1_2_fhi_csosi_overall %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_1_2_fhi_csosi_overall <- sub_obj_1_2_fhi_csosi_overall %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_1_2_fhi_csosi_overall",
         high_value_is_good_outcome_flag = 0) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_1_2_fhi_csosi_overall 
sub_obj_1_2_fhi_csosi_overall %>% glimpse()
sub_obj_1_2_fhi_csosi_overall %>% nrow() # 315
sub_obj_1_2_fhi_csosi_overall %>% ncol() # 40

sub_obj_1_2_fhi_csosi_overall %>% distinct(country) %>% nrow() # 45

# check values
sub_obj_1_2_fhi_csosi_overall %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_1_2_fhi_csosi_overall %>% filter(is.na(values), year >= 2006) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_1_2_fhi_csosi_overall %>% skim(values)
sub_obj_1_2_fhi_csosi_overall %>% group_by(year) %>% skim(values)
sub_obj_1_2_fhi_csosi_overall %>% filter(year >= 2006) %>% skim(values)
sub_obj_1_2_fhi_csosi_overall %>% filter(year >= 2006) %>% group_by(country) %>% skim(values)

sub_obj_1_2_fhi_csosi_overall %>% filter(is.na(values),
                                                 mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                                 year >= 2007) %>%
  select(country, mcp_grouping, year, values)
sub_obj_1_2_fhi_csosi_overall %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_1_2_fhi_csosi_overall %>% filter(year >= 2007) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_1_2_fhi_csosi_overall %>%
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 


# #/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_1_2_fhi_csosi_overall %>% write_csv(file = "data/fmir/sub_obj_1_2_fhi_csosi_overall.csv")
sub_obj_1_2_fhi_csosi_overall <- read.csv(file = "data/fmir/sub_obj_1_2_fhi_csosi_overall.csv") %>% 
  as_tibble()





#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
# load sub_obj_1_1_fhi_electoral_process ####



sub_obj_1_1_fhi_electoral_process <- read_excel(path = "data/fhi/nit/All_Data_Nations_in_Transit_NIT_2005-2024_For_website.xlsx",
                                          sheet = "NIT 2005-2022") %>% 
  select(c("Country",
           "Year",
           "Electoral Process")) %>%
  rename(country_name = Country,
         year = Year,
         values = `Electoral Process`) %>%
  filter(year>=2018 & year != 2024) %>%
  mutate(indicator_name = "sub_obj_1_1_fhi_electoral_process",
         high_value_is_good_outcome_flag = 1,
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States" ~ "U.S.",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Slovak Republic" ~ "Slovakia",
                                  TRUE ~ country_name),
         year = as.numeric(year))

#//////////////////////////


# inspect
sub_obj_1_1_fhi_electoral_process
sub_obj_1_1_fhi_electoral_process %>% glimpse()
sub_obj_1_1_fhi_electoral_process %>% nrow() # 203
sub_obj_1_1_fhi_electoral_process %>% ncol() # 5

# chack values
sub_obj_1_1_fhi_electoral_process %>% skim()
sub_obj_1_1_fhi_electoral_process %>% group_by(year) %>% skim()
sub_obj_1_1_fhi_electoral_process %>% count(year)
sub_obj_1_1_fhi_electoral_process %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_1_1_fhi_electoral_process %>% arrange(values) %>% distinct(country_name)


# inspect country names
# many are missing for EU (UK and US too)
sub_obj_1_1_fhi_electoral_process %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_1_1_fhi_electoral_process, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country) %>% print(n = nrow(.))

sub_obj_1_1_fhi_electoral_process %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_1_1_fhi_electoral_process <- sub_obj_1_1_fhi_electoral_process %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_1_1_fhi_electoral_process",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_1_1_fhi_electoral_process 
sub_obj_1_1_fhi_electoral_process %>% glimpse()
sub_obj_1_1_fhi_electoral_process %>% nrow() # 315
sub_obj_1_1_fhi_electoral_process %>% ncol() # 40

sub_obj_1_1_fhi_electoral_process %>% distinct(country) %>% nrow() # 45

# check values
sub_obj_1_1_fhi_electoral_process %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_1_1_fhi_electoral_process %>% filter(is.na(values), year >= 2006) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_1_1_fhi_electoral_process %>% skim(values)
sub_obj_1_1_fhi_electoral_process %>% group_by(year) %>% skim(values)
sub_obj_1_1_fhi_electoral_process %>% filter(year >= 2006) %>% skim(values)
sub_obj_1_1_fhi_electoral_process %>% filter(year >= 2006) %>% group_by(country) %>% skim(values)

sub_obj_1_1_fhi_electoral_process %>% filter(is.na(values),
                                       mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                       year >= 2007) %>%
  select(country, mcp_grouping, year, values)
sub_obj_1_1_fhi_electoral_process %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_1_1_fhi_electoral_process %>% filter(year >= 2007) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_1_1_fhi_electoral_process %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 


# #/////////////////////////////////////////////////////////////////////////////////////////////

# read/write
sub_obj_1_1_fhi_electoral_process %>% write_csv(file = "data/fmir/sub_obj_1_1_fhi_electoral_process.csv")
sub_obj_1_1_fhi_electoral_process <- read.csv(file = "data/fmir/sub_obj_1_1_fhi_electoral_process.csv") %>% 
  as_tibble()

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
# load sub_obj_1_1_fhi_national_democratic_governance ####



sub_obj_1_1_fhi_national_democratic_governance <- read_excel(path = "data/fhi/nit/All_Data_Nations_in_Transit_NIT_2005-2024_For_website.xlsx",
                                                sheet = "NIT 2005-2022") %>% 
  select(c("Country",
           "Year",
           "National Democratic Governance")) %>%
  rename(country_name = Country,
         year = Year,
         values = `National Democratic Governance`) %>%
  filter(year>=2018 & year != 2024) %>%
  mutate(indicator_name = "sub_obj_1_1_fhi_national_democratic_governance",
         high_value_is_good_outcome_flag = 1,
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States" ~ "U.S.",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Slovak Republic" ~ "Slovakia",
                                  TRUE ~ country_name),
         year = as.numeric(year))

#//////////////////////////


# inspect
sub_obj_1_1_fhi_national_democratic_governance
sub_obj_1_1_fhi_national_democratic_governance %>% glimpse()
sub_obj_1_1_fhi_national_democratic_governance %>% nrow() # 203
sub_obj_1_1_fhi_national_democratic_governance %>% ncol() # 5

# chack values
sub_obj_1_1_fhi_national_democratic_governance %>% skim()
sub_obj_1_1_fhi_national_democratic_governance %>% group_by(year) %>% skim()
sub_obj_1_1_fhi_national_democratic_governance %>% count(year)
sub_obj_1_1_fhi_national_democratic_governance %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_1_1_fhi_national_democratic_governance %>% arrange(values) %>% distinct(country_name)


# inspect country names
# many are missing for EU (UK and US too)
sub_obj_1_1_fhi_national_democratic_governance %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_1_1_fhi_national_democratic_governance, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country) %>% print(n = nrow(.))

sub_obj_1_1_fhi_national_democratic_governance %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_1_1_fhi_national_democratic_governance <- sub_obj_1_1_fhi_national_democratic_governance %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_1_1_fhi_national_democratic_governance",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_1_1_fhi_national_democratic_governance 
sub_obj_1_1_fhi_national_democratic_governance %>% glimpse()
sub_obj_1_1_fhi_national_democratic_governance %>% nrow() # 315
sub_obj_1_1_fhi_national_democratic_governance %>% ncol() # 40

sub_obj_1_1_fhi_national_democratic_governance %>% distinct(country) %>% nrow() # 45

# check values
sub_obj_1_1_fhi_national_democratic_governance %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_1_1_fhi_national_democratic_governance %>% filter(is.na(values), year >= 2006) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_1_1_fhi_national_democratic_governance %>% skim(values)
sub_obj_1_1_fhi_national_democratic_governance %>% group_by(year) %>% skim(values)
sub_obj_1_1_fhi_national_democratic_governance %>% filter(year >= 2006) %>% skim(values)
sub_obj_1_1_fhi_national_democratic_governance %>% filter(year >= 2006) %>% group_by(country) %>% skim(values)

sub_obj_1_1_fhi_national_democratic_governance %>% filter(is.na(values),
                                             mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                             year >= 2007) %>%
  select(country, mcp_grouping, year, values)
sub_obj_1_1_fhi_national_democratic_governance %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_1_1_fhi_national_democratic_governance %>% filter(year >= 2007) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_1_1_fhi_national_democratic_governance %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 


# #/////////////////////////////////////////////////////////////////////////////////////////////

# read/write
sub_obj_1_1_fhi_national_democratic_governance %>% write_csv(file = "data/fmir/sub_obj_1_1_fhi_national_democratic_governance.csv")
sub_obj_1_1_fhi_national_democratic_governance <- read.csv(file = "data/fmir/sub_obj_1_1_fhi_national_democratic_governance.csv") %>% 
  as_tibble()

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
# load sub_obj_1_1_fhi_judicial_framework_and_independence ####

sub_obj_1_1_fhi_judicial_framework_and_independence <- read_excel(path = "data/fhi/nit/All_Data_Nations_in_Transit_NIT_2005-2024_For_website.xlsx",
                                                sheet = "NIT 2005-2022") %>% 
  select(c("Country",
           "Year",
           "Judicial Framework and Independence")) %>%
  rename(country_name = Country,
         year = Year,
         values = `Judicial Framework and Independence`) %>%
  filter(year>=2018 & year != 2024) %>%
  mutate(indicator_name = "sub_obj_1_1_fhi_judicial_framework_and_independence",
         high_value_is_good_outcome_flag = 1,
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States" ~ "U.S.",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Slovak Republic" ~ "Slovakia",
                                  TRUE ~ country_name),
         year = as.numeric(year))

#//////////////////////////


# inspect
sub_obj_1_1_fhi_judicial_framework_and_independence
sub_obj_1_1_fhi_judicial_framework_and_independence %>% glimpse()
sub_obj_1_1_fhi_judicial_framework_and_independence %>% nrow() # 203
sub_obj_1_1_fhi_judicial_framework_and_independence %>% ncol() # 5

# chack values
sub_obj_1_1_fhi_judicial_framework_and_independence %>% skim()
sub_obj_1_1_fhi_judicial_framework_and_independence %>% group_by(year) %>% skim()
sub_obj_1_1_fhi_judicial_framework_and_independence %>% count(year)
sub_obj_1_1_fhi_judicial_framework_and_independence %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_1_1_fhi_judicial_framework_and_independence %>% arrange(values) %>% distinct(country_name)


# inspect country names
# many are missing for EU (UK and US too)
sub_obj_1_1_fhi_judicial_framework_and_independence %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_1_1_fhi_judicial_framework_and_independence, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country) %>% print(n = nrow(.))

sub_obj_1_1_fhi_judicial_framework_and_independence %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_1_1_fhi_judicial_framework_and_independence <- sub_obj_1_1_fhi_judicial_framework_and_independence %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_1_1_fhi_judicial_framework_and_independence",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_1_1_fhi_judicial_framework_and_independence 
sub_obj_1_1_fhi_judicial_framework_and_independence %>% glimpse()
sub_obj_1_1_fhi_judicial_framework_and_independence %>% nrow() # 315
sub_obj_1_1_fhi_judicial_framework_and_independence %>% ncol() # 40

sub_obj_1_1_fhi_judicial_framework_and_independence %>% distinct(country) %>% nrow() # 45

# check values
sub_obj_1_1_fhi_judicial_framework_and_independence %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_1_1_fhi_judicial_framework_and_independence %>% filter(is.na(values), year >= 2006) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_1_1_fhi_judicial_framework_and_independence %>% skim(values)
sub_obj_1_1_fhi_judicial_framework_and_independence %>% group_by(year) %>% skim(values)
sub_obj_1_1_fhi_judicial_framework_and_independence %>% filter(year >= 2006) %>% skim(values)
sub_obj_1_1_fhi_judicial_framework_and_independence %>% filter(year >= 2006) %>% group_by(country) %>% skim(values)

sub_obj_1_1_fhi_judicial_framework_and_independence %>% filter(is.na(values),
                                             mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                             year >= 2007) %>%
  select(country, mcp_grouping, year, values)
sub_obj_1_1_fhi_judicial_framework_and_independence %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_1_1_fhi_judicial_framework_and_independence %>% filter(year >= 2007) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_1_1_fhi_judicial_framework_and_independence %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# #/////////////////////////////////////////////////////////////////////////////////////////////

# read/write
sub_obj_1_1_fhi_judicial_framework_and_independence %>% write_csv(file = "data/fmir/sub_obj_1_1_fhi_judicial_framework_and_independence.csv")
sub_obj_1_1_fhi_judicial_framework_and_independence <- read.csv(file = "data/fmir/sub_obj_1_1_fhi_judicial_framework_and_independence.csv") %>% 
  as_tibble()

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
# load sub_obj_1_3_iidea_voter_turnout ####

# Tajikistan and Turkmenistan data has strong data quality issues. In the imputation stage,
# they get imputed to the mean for CAR countries

sub_obj_1_3_iidea_voter_turnout <- read_excel(path = "data/iidea/export_table.xls",
                                              skip = 1) 

sub_obj_1_3_iidea_voter_turnout$year <- substr(sub_obj_1_3_iidea_voter_turnout$Date, 1, 4)

sub_obj_1_3_iidea_voter_turnout <- sub_obj_1_3_iidea_voter_turnout %>%
  select(-c("Date",
            "ISO2",
            "Country")) %>%
  filter(year != 2024) %>%
  mutate(
    across(everything(), ~na_if(.,"-"))
  ) %>%
  mutate(
    across(c(`Parliamentary>Voter Turnout`, 
             `Parliamentary>VAP Turnout`, 
             `Presidential>Voter Turnout`, 
             `Presidential>VAP Turnout`), as.numeric)
  ) %>%
  group_by(ISO3,
           year) %>%
  summarize(Parliamentary_Voter_Turnout = mean(`Parliamentary>Voter Turnout`, na.rm = TRUE),
            Parliamentary_VAP_Turnout = mean(`Parliamentary>VAP Turnout`, na.rm = TRUE),
            Presidential_Voter_Turnout = mean(`Presidential>Voter Turnout`, na.rm = TRUE),
            Presidential_VAP_Turnout = mean(`Presidential>VAP Turnout`, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    Parliament = ifelse(
      is.nan(rowMeans(
      cbind(Parliamentary_Voter_Turnout, Parliamentary_VAP_Turnout),
      na.rm = TRUE)), NA,
      rowMeans(
        cbind(Parliamentary_Voter_Turnout, Parliamentary_VAP_Turnout),
        na.rm = TRUE)),
    President = ifelse(
      is.nan(rowMeans(
      cbind(Presidential_Voter_Turnout, Presidential_VAP_Turnout),
      na.rm = TRUE)), NA,
      rowMeans(
        cbind(Presidential_Voter_Turnout, Presidential_VAP_Turnout),
        na.rm = TRUE))
  ) %>%
  select(c(
    "ISO3",
    "year",
    "Parliament",
    "President"
  )) %>%
  filter(year >= 2018) %>%
  pivot_longer(cols = c(Parliament,
                        President),
               names_to = "denominator_form",
               values_to = "values") %>%
  pivot_wider(id_cols = c(ISO3,
                          denominator_form),
              names_from = year,
              values_from = values) %>%
  dplyr::select(ISO3, 
                denominator_form,
                `2018`,
                `2019`,
                `2020`,
                `2021`,
                `2022`,
                `2023`) %>%
  mutate(across(`2018`:`2023`, as.numeric))

sub_obj_1_3_iidea_voter_turnout[ , 3:ncol(sub_obj_1_3_iidea_voter_turnout)] <- t(apply(sub_obj_1_3_iidea_voter_turnout[ , 3:ncol(sub_obj_1_3_iidea_voter_turnout)], 1, function(x) na.locf(x, na.rm = FALSE)))

sub_obj_1_3_iidea_voter_turnout <- sub_obj_1_3_iidea_voter_turnout %>%
  group_by(ISO3) %>%
  summarize(`2018` = mean(`2018`,
                          na.rm = TRUE),
            `2019` = mean(`2019`,
                          na.rm = TRUE),
            `2020` = mean(`2020`,
                          na.rm = TRUE),
            `2021` = mean(`2021`,
                          na.rm = TRUE),
            `2022` = mean(`2022`,
                          na.rm = TRUE),
            `2023` = mean(`2023`,
                          na.rm = TRUE)) %>%
  pivot_longer(cols = `2018`:`2023`,
               names_to = "year",
               values_to = "values") %>%
  mutate(values = case_when(
    is.nan(values) ~ NA,
    TRUE ~ as.numeric(values)),
    indicator_name = "sub_obj_1_3_iidea_voter_turnout",
    high_value_is_good_outcome_flag = 1,
    year = as.numeric(year)) %>%
  rename(iso3 = ISO3)

sub_obj_1_3_iidea_voter_turnout <- sub_obj_1_3_iidea_voter_turnout %>% 
  mutate(
    values = case_when(
      iso3 %in% c("TJK",
                  "TKM") ~ NA,
      TRUE ~ values
    ))

#//////////////////////////


# inspect
sub_obj_1_3_iidea_voter_turnout
sub_obj_1_3_iidea_voter_turnout %>% glimpse()
sub_obj_1_3_iidea_voter_turnout %>% nrow() # 1146
sub_obj_1_3_iidea_voter_turnout %>% ncol() # 5

# chack values
sub_obj_1_3_iidea_voter_turnout %>% skim()
sub_obj_1_3_iidea_voter_turnout %>% group_by(year) %>% skim()
sub_obj_1_3_iidea_voter_turnout %>% count(year)
sub_obj_1_3_iidea_voter_turnout %>% arrange(desc(values)) %>% distinct(iso3)
sub_obj_1_3_iidea_voter_turnout %>% arrange(values) %>% distinct(iso3)


# inspect country names
# Kosovo missing entirely
sub_obj_1_3_iidea_voter_turnout %>% anti_join(., country_crosswalk, by = c("iso3" = "iso3")) %>% 
  distinct(iso3) %>% arrange(iso3) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_1_3_iidea_voter_turnout, by = c("iso3" = "iso3")) %>% 
  distinct(country) %>% arrange(country) %>% print(n = nrow(.))

#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_1_3_iidea_voter_turnout <- sub_obj_1_3_iidea_voter_turnout %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("iso3" = "iso3", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_1_3_iidea_voter_turnout",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")

#/////////////

# inspect
sub_obj_1_3_iidea_voter_turnout 
sub_obj_1_3_iidea_voter_turnout %>% glimpse()
sub_obj_1_3_iidea_voter_turnout %>% nrow() # 315
sub_obj_1_3_iidea_voter_turnout %>% ncol() # 36

sub_obj_1_3_iidea_voter_turnout %>% distinct(country) %>% nrow() # 45

# check values
# Missing all values for Kosovo and Ukraine, many for France, some from Bulgaria, Albania, Germany, Netherlands, and occasional from others
sub_obj_1_3_iidea_voter_turnout %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_1_3_iidea_voter_turnout %>% filter(is.na(values), year >= 2006) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_1_3_iidea_voter_turnout %>% skim(values)
sub_obj_1_3_iidea_voter_turnout %>% group_by(year) %>% skim(values)
sub_obj_1_3_iidea_voter_turnout %>% filter(year >= 2006) %>% skim(values)
sub_obj_1_3_iidea_voter_turnout %>% filter(year >= 2006) %>% group_by(country) %>% skim(values)

sub_obj_1_3_iidea_voter_turnout %>% filter(is.na(values),
                                                 mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                                 year >= 2007) %>%
  select(country, mcp_grouping, year, values)
sub_obj_1_3_iidea_voter_turnout %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_1_3_iidea_voter_turnout %>% filter(year >= 2007) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_1_3_iidea_voter_turnout %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

#/////////////

# read/write
sub_obj_1_3_iidea_voter_turnout %>% write_csv(file = "data/fmir/sub_obj_1_3_iidea_voter_turnout.csv")
sub_obj_1_3_iidea_voter_turnout <- read.csv(file = "data/fmir/sub_obj_1_3_iidea_voter_turnout.csv") %>% 
  as_tibble()

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
