# load sub_obj_4_3_undesa_migrant_opportunities ####

fmi_2019 <- read_excel(path = "data/un desa/undesa_cleaned_2019.xlsx",
                       sheet = "Cleaned") %>%
  mutate(`2019` = ifelse(is.na(`2019`), 0, `2019`)) %>%
  filter(Destination != "WORLD") %>%
  group_by(Origin) %>%
  summarize(fmi_2019 = sum(`2019`))

fmi_2020 <- read_excel(path = "data/un desa/undesa_cleaned.xlsx",
                  sheet = "Cleaned") %>%
  mutate(`2020` = ifelse(is.na(`2020`), 0, `2020`)) %>%
  filter(Destination != "WORLD") %>%
  group_by(Origin) %>%
  summarize(fmi_2020 = sum(`2020`))

world_2019 <- read_excel(path = "data/un desa/undesa_cleaned_2019.xlsx",
                         sheet = "Cleaned") %>%
  mutate(`2019` = ifelse(is.na(`2019`), 0, `2019`)) %>%
  filter(Destination == "WORLD") %>%
  select(-Destination) %>%
  rename(all_2019 = `2019`)

world_2020 <- read_excel(path = "data/un desa/undesa_cleaned.xlsx",
                                                       sheet = "Cleaned") %>%
  filter(Destination == "WORLD") %>%
  mutate(`2020` = ifelse(is.na(`2020`), 0, `2020`)) %>%
  select(-Destination) %>%
  rename(all_2020 = `2020`)




sub_obj_4_3_undesa_migrant_opportunities <- left_join(fmi_2019, fmi_2020, by = "Origin") %>%
  left_join(world_2019, by = "Origin") %>%
  left_join(world_2020, by = "Origin") %>%
  filter((!is.na(all_2019)) & (!is.na(all_2020))) %>%
  mutate(percent_to_fmi_2019 = ((fmi_2019/all_2019)*100)) %>%
  mutate(percent_to_fmi_2020 = ((fmi_2020/all_2020)*100)) %>%
  select(c(Origin,
           percent_to_fmi_2019,
           percent_to_fmi_2020)) %>%
  rename(country_name = Origin,
         `2019` = percent_to_fmi_2019,
         `2020` = percent_to_fmi_2020) %>%
  mutate(`2018`=NA,
         `2021`=NA,
         `2022`=NA,
         `2023`=NA
         ) %>%
  pivot_longer(cols = c(`2018`,`2019`,`2020`,`2021`,`2022`,`2023`), 
               names_to = "year", 
               values_to = "values") %>%
  mutate(indicator_name = "sub_obj_4_3_undesa_migrant_opportunities",
         high_value_is_good_outcome_flag = 0,
         year = as.numeric(year),
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Republic of Moldova" ~ "Moldova",
                                  country_name == "Republic of Moldova" ~ "Moldova",
                                  country_name == "Moldova, Republic of" ~ "Moldova", 
                                  country_name == "Netherlands (Kingdom of the)" ~ "Netherlands",
                                  country_name == "United Kingdom of Great Britain and Northern Ireland" ~ "U.K.",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States of America" ~ "U.S.",
                                  country_name == "United States" ~ "U.S.",
                                  country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                  country_name == "Slovak Republic" ~ "Slovakia",
                                  country_name == "Russian Federation" ~ "Russia",
                                  str_detect(country_name, "Moldova") ~ "Moldova",
                                  str_detect(country_name, "United K") ~ "U.K.",
                                  str_detect(country_name, "States of America") ~ "U.S.",
                                  TRUE ~ country_name)) 

sub_obj_4_3_undesa_migrant_opportunities$country_name <- gsub("\\*", "", sub_obj_4_3_undesa_migrant_opportunities$country_name)

#//////////////////////////


# inspect
sub_obj_4_3_undesa_migrant_opportunities
sub_obj_4_3_undesa_migrant_opportunities %>% glimpse()
sub_obj_4_3_undesa_migrant_opportunities %>% nrow() # 1064
sub_obj_4_3_undesa_migrant_opportunities %>% ncol() # 5

# chack values
sub_obj_4_3_undesa_migrant_opportunities %>% skim()
sub_obj_4_3_undesa_migrant_opportunities %>% group_by(year) %>% skim()
sub_obj_4_3_undesa_migrant_opportunities %>% count(year)
sub_obj_4_3_undesa_migrant_opportunities %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_4_3_undesa_migrant_opportunities %>% arrange(values) %>% distinct(country_name)


# inspect country names
# many are missing
sub_obj_4_3_undesa_migrant_opportunities %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_4_3_undesa_migrant_opportunities, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country) %>% print(n = nrow(.))

#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_4_3_undesa_migrant_opportunities <- sub_obj_4_3_undesa_migrant_opportunities %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_4_3_undesa_migrant_opportunities",
         high_value_is_good_outcome_flag = 0) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_4_3_undesa_migrant_opportunities 
sub_obj_4_3_undesa_migrant_opportunities %>% glimpse()
sub_obj_4_3_undesa_migrant_opportunities %>% nrow() # 315
sub_obj_4_3_undesa_migrant_opportunities %>% ncol() # 39

sub_obj_4_3_undesa_migrant_opportunities %>% distinct(country) %>% nrow() # 45

# check values
# Missing all values for Kosovo and Turkmenistan
sub_obj_4_3_undesa_migrant_opportunities %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_4_3_undesa_migrant_opportunities %>% filter(is.na(values), year >= 2006) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_4_3_undesa_migrant_opportunities %>% skim(values)
sub_obj_4_3_undesa_migrant_opportunities %>% group_by(year) %>% skim(values)
sub_obj_4_3_undesa_migrant_opportunities %>% filter(year >= 2006) %>% skim(values)
sub_obj_4_3_undesa_migrant_opportunities %>% filter(year >= 2006) %>% group_by(country) %>% skim(values)

sub_obj_4_3_undesa_migrant_opportunities %>% filter(is.na(values),
                                                         mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                                         year >= 2007) %>%
  select(country, mcp_grouping, year, values)
sub_obj_4_3_undesa_migrant_opportunities %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_4_3_undesa_migrant_opportunities %>% filter(year >= 2007) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_4_3_undesa_migrant_opportunities %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 


# read/write
sub_obj_4_3_undesa_migrant_opportunities %>% write_csv(file = "data/fmir/sub_obj_4_3_undesa_migrant_opportunities.csv")
sub_obj_4_3_undesa_migrant_opportunities <- read.csv(file = "data/fmir/sub_obj_4_3_undesa_migrant_opportunities.csv") %>% 
  as_tibble()

rm(fmi_2019,
   fmi_2020,
   world_2019,
   world_2020)

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
# prepping for next two indicators as loading this file takes forever

pardee <- read_csv("data/pardee/fbic/Diplometrics_FBIC_Index_and_inputs_1960-2023_preliminary_update_20240722.csv")

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
# load sub_obj_4_1_pardee_fmi_economic_dependence ####

sub_obj_4_1_pardee_fmi_economic_dependence <- pardee

sub_obj_4_1_pardee_fmi_economic_dependence <- sub_obj_4_1_pardee_fmi_economic_dependence %>%
  select(c(countrya,
           countryb,
           year,
           iso3a,
           iso3b,
           economicdependence)) %>%
  mutate(
    countrya = case_when(
      countrya %in% c("China", "Iran", "Russia") ~ "FMI"
    )
  ) %>% 
  filter(countrya == "FMI" & year >= 2018) 

sub_obj_4_1_pardee_fmi_economic_dependence <- sub_obj_4_1_pardee_fmi_economic_dependence %>%
  select(countryb,
         year,
         iso3b,
         economicdependence) 

sub_obj_4_1_pardee_fmi_economic_dependence <- sub_obj_4_1_pardee_fmi_economic_dependence %>%
  group_by(countryb,
           year,
           iso3b) %>%
  summarize(economicdependence = sum(economicdependence))

sub_obj_4_1_pardee_fmi_economic_dependence <- sub_obj_4_1_pardee_fmi_economic_dependence %>%
  select(-countryb) %>%
  rename(iso3 = iso3b,
         values = economicdependence) %>%
  pivot_wider(id_cols = iso3, 
              names_from = year, 
              values_from = values) %>%
  # mutate(`2024` = `2023`) %>%
pivot_longer(cols = c(`2018`,`2019`,`2020`,`2021`,`2022`,`2023`), 
             names_to = "year", 
             values_to = "values") %>%
  mutate(indicator_name = "sub_obj_4_1_pardee_fmi_economic_dependence",
         high_value_is_good_outcome_flag = 0,
         year = as.numeric(year))

#//////////////////////////


# inspect
sub_obj_4_1_pardee_fmi_economic_dependence
sub_obj_4_1_pardee_fmi_economic_dependence %>% glimpse()
sub_obj_4_1_pardee_fmi_economic_dependence %>% nrow() # 1407
sub_obj_4_1_pardee_fmi_economic_dependence %>% ncol() # 5

# chack values
sub_obj_4_1_pardee_fmi_economic_dependence %>% skim()
sub_obj_4_1_pardee_fmi_economic_dependence %>% group_by(year) %>% skim()
sub_obj_4_1_pardee_fmi_economic_dependence %>% count(year)
sub_obj_4_1_pardee_fmi_economic_dependence %>% arrange(desc(values)) %>% distinct(iso3)
sub_obj_4_1_pardee_fmi_economic_dependence %>% arrange(values) %>% distinct(iso3)


# inspect country names
# many are missing
sub_obj_4_1_pardee_fmi_economic_dependence %>% anti_join(., country_crosswalk, by = c("iso3" = "iso3")) %>% 
  distinct(iso3) %>% arrange(iso3) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_4_1_pardee_fmi_economic_dependence, by = c("iso3" = "iso3")) %>% 
  distinct(iso3) %>% arrange(iso3) %>% print(n = nrow(.))

sub_obj_4_1_pardee_fmi_economic_dependence %>% 
  filter(str_detect(string = iso3, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(iso3)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_4_1_pardee_fmi_economic_dependence <- sub_obj_4_1_pardee_fmi_economic_dependence %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("iso3" = "iso3", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_4_1_pardee_fmi_economic_dependence",
         high_value_is_good_outcome_flag = 0) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_4_1_pardee_fmi_economic_dependence 
sub_obj_4_1_pardee_fmi_economic_dependence %>% glimpse()
sub_obj_4_1_pardee_fmi_economic_dependence %>% nrow() # 315
sub_obj_4_1_pardee_fmi_economic_dependence %>% ncol() # 39

sub_obj_4_1_pardee_fmi_economic_dependence %>% distinct(country) %>% nrow() # 45

# check values
# Missing all values for Kosovo
sub_obj_4_1_pardee_fmi_economic_dependence %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_4_1_pardee_fmi_economic_dependence %>% filter(is.na(values), year >= 2006) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_4_1_pardee_fmi_economic_dependence %>% skim(values)
sub_obj_4_1_pardee_fmi_economic_dependence %>% group_by(year) %>% skim(values)
sub_obj_4_1_pardee_fmi_economic_dependence %>% filter(year >= 2006) %>% skim(values)
sub_obj_4_1_pardee_fmi_economic_dependence %>% filter(year >= 2006) %>% group_by(country) %>% skim(values)

sub_obj_4_1_pardee_fmi_economic_dependence %>% filter(is.na(values),
                                                  mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                                  year >= 2007) %>%
  select(country, mcp_grouping, year, values)
sub_obj_4_1_pardee_fmi_economic_dependence %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_4_1_pardee_fmi_economic_dependence %>% filter(year >= 2007) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_4_1_pardee_fmi_economic_dependence %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 


# read/write
sub_obj_4_1_pardee_fmi_economic_dependence %>% write_csv(file = "data/fmir/sub_obj_4_1_pardee_fmi_economic_dependence.csv")
sub_obj_4_1_pardee_fmi_economic_dependence <- read.csv(file = "data/fmir/sub_obj_4_1_pardee_fmi_economic_dependence.csv") %>% 
  as_tibble()

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
# load sub_obj_4_1_pardee_eu_economic_dependence ####

EU_countries_list <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")

sub_obj_4_1_pardee_eu_economic_dependence <- pardee

sub_obj_4_1_pardee_eu_economic_dependence <- sub_obj_4_1_pardee_eu_economic_dependence %>%
  select(c(countrya,
           countryb,
           year,
           iso3a,
           iso3b,
           economicdependence)) %>%
  mutate(
    countrya = case_when(
      countrya %in% EU_countries_list ~ "EU"
    )
  ) %>% 
  filter(countrya == "EU" & year >= 2018) 

sub_obj_4_1_pardee_eu_economic_dependence <- sub_obj_4_1_pardee_eu_economic_dependence %>%
  select(countryb,
         year,
         iso3b,
         economicdependence) 

sub_obj_4_1_pardee_eu_economic_dependence <- sub_obj_4_1_pardee_eu_economic_dependence %>%
  group_by(countryb,
           year,
           iso3b) %>%
  summarize(economicdependence = sum(economicdependence))

sub_obj_4_1_pardee_eu_economic_dependence <- sub_obj_4_1_pardee_eu_economic_dependence %>%
  select(-countryb) %>%
  rename(iso3 = iso3b,
         values = economicdependence) %>%
  pivot_wider(id_cols = iso3, 
              names_from = year, 
              values_from = values) %>%
  # mutate(`2024` = `2023`) %>%
  pivot_longer(cols = c(`2018`,`2019`,`2020`,`2021`,`2022`,`2023`), 
               names_to = "year", 
               values_to = "values") %>%
  mutate(indicator_name = "sub_obj_4_1_pardee_eu_economic_dependence",
         high_value_is_good_outcome_flag = 1,
         year = as.numeric(year))

#//////////////////////////


# inspect
sub_obj_4_1_pardee_eu_economic_dependence
sub_obj_4_1_pardee_eu_economic_dependence %>% glimpse()
sub_obj_4_1_pardee_eu_economic_dependence %>% nrow() # 1407
sub_obj_4_1_pardee_eu_economic_dependence %>% ncol() # 5

# chack values
sub_obj_4_1_pardee_eu_economic_dependence %>% skim()
sub_obj_4_1_pardee_eu_economic_dependence %>% group_by(year) %>% skim()
sub_obj_4_1_pardee_eu_economic_dependence %>% count(year)
sub_obj_4_1_pardee_eu_economic_dependence %>% arrange(desc(values)) %>% distinct(iso3)
sub_obj_4_1_pardee_eu_economic_dependence %>% arrange(values) %>% distinct(iso3)


# inspect country names
# many are missing
sub_obj_4_1_pardee_eu_economic_dependence %>% anti_join(., country_crosswalk, by = c("iso3" = "iso3")) %>% 
  distinct(iso3) %>% arrange(iso3) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_4_1_pardee_eu_economic_dependence, by = c("iso3" = "iso3")) %>% 
  distinct(iso3) %>% arrange(iso3) %>% print(n = nrow(.))

sub_obj_4_1_pardee_eu_economic_dependence %>% 
  filter(str_detect(string = iso3, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(iso3)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_4_1_pardee_eu_economic_dependence <- sub_obj_4_1_pardee_eu_economic_dependence %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("iso3" = "iso3", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_4_1_pardee_eu_economic_dependence",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_4_1_pardee_eu_economic_dependence 
sub_obj_4_1_pardee_eu_economic_dependence %>% glimpse()
sub_obj_4_1_pardee_eu_economic_dependence %>% nrow() # 315
sub_obj_4_1_pardee_eu_economic_dependence %>% ncol() # 39

sub_obj_4_1_pardee_eu_economic_dependence %>% distinct(country) %>% nrow() # 45

# check values
# Missing all values for Kosovo
sub_obj_4_1_pardee_eu_economic_dependence %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_4_1_pardee_eu_economic_dependence %>% filter(is.na(values), year >= 2006) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_4_1_pardee_eu_economic_dependence %>% skim(values)
sub_obj_4_1_pardee_eu_economic_dependence %>% group_by(year) %>% skim(values)
sub_obj_4_1_pardee_eu_economic_dependence %>% filter(year >= 2006) %>% skim(values)
sub_obj_4_1_pardee_eu_economic_dependence %>% filter(year >= 2006) %>% group_by(country) %>% skim(values)

sub_obj_4_1_pardee_eu_economic_dependence %>% filter(is.na(values),
                                                      mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                                      year >= 2007) %>%
  select(country, mcp_grouping, year, values)
sub_obj_4_1_pardee_eu_economic_dependence %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_4_1_pardee_eu_economic_dependence %>% filter(year >= 2007) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_4_1_pardee_eu_economic_dependence %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 


# read/write
sub_obj_4_1_pardee_eu_economic_dependence %>% write_csv(file = "data/fmir/sub_obj_4_1_pardee_eu_economic_dependence.csv")
sub_obj_4_1_pardee_eu_economic_dependence <- read.csv(file = "data/fmir/sub_obj_4_1_pardee_eu_economic_dependence.csv") %>% 
  as_tibble()

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////

rm(pardee)
rm(EU_countries_list)
