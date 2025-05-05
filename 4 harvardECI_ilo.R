# load sub_obj_4_1_atlas_eci ####

# https://atlas.cid.harvard.edu/rankings
# https://atlas.cid.harvard.edu/glossary

#they seem to be releasing on a 2-year lag. 2022 data was updated in Dataverse in Sept 2024, though the main ECI website still only includes data
#up to 2021 in their visualizations. I've emailed the Growth Lab to confirm this inference, but no response yet (as of 11/13/2024). 

#####
#test new data for correlation between HS and SITC methods

locations <- read.csv("data/harvard/atlas_of_economic_complexity/location_country.csv")

test_eci <- read_dta("data/harvard/atlas_of_economic_complexity/rankings.dta") %>% 
  full_join(locations,
            by = "country_id") %>%
  filter(year >=2018)


test_countries <- country_crosswalk %>%
  filter(!is.na(mcp_grouping)) %>%
  select(iso3) %>%
  rename(iso3_code = iso3) %>%
  left_join(test_eci,
            by = "iso3_code") %>%
  filter(!is.na(year)) %>%
  mutate(mean = ((sitc_eci + hs_eci)/2))

cor.test(test_countries$sitc_eci, test_countries$hs_eci)
cor.test(as.numeric(test_countries$sitc_eci_rank), as.numeric(test_countries$hs_eci_rank))
plot(test_countries$sitc_eci, test_countries$hs_eci)
plot(as.numeric(test_countries$sitc_eci_rank), as.numeric(test_countries$hs_eci_rank))

cor.test(test_countries$sitc_eci, test_countries$mean)
cor.test(test_countries$hs_eci, test_countries$mean)
plot(test_countries$sitc_eci, test_countries$mean)
plot(test_countries$hs_eci, test_countries$mean)

rm(locations,
   test_eci,
   test_countries)


#####
# start actual import here

eci_locations <- read.csv("data/harvard/atlas_of_economic_complexity/location_country.csv")
miri_locations <- country_crosswalk %>%
  filter(!is.na(mcp_grouping)) %>%
  select(c("iso3",
           "country"))

sub_obj_4_1_atlas_eci <- read_dta("data/harvard/atlas_of_economic_complexity/rankings.dta") %>%
  full_join(eci_locations,
            by = "country_id") %>%
  filter(year >=2018) %>%
  rename(iso3 = iso3_code) %>%
  mutate(values = ((sitc_eci + hs_eci)/2)) %>%
  select(c("values", "iso3", "year")) %>%
  full_join(miri_locations,
            by = "iso3") %>%
  filter(!is.na(country) & !is.na(year)) %>%
  pivot_wider(names_from = year,
              values_from = values) %>%
  mutate(`2023` = NA) %>%
  pivot_longer(cols = c(`2018`, `2019`, `2020`, `2021`, `2022`, `2023`), 
               names_to = "year", 
               values_to = "values") %>%
  mutate(indicator_name = "sub_obj_4_1_atlas_eci",
         high_value_is_good_outcome_flag = 1) %>%
  rename(country_name = country)
  

rm(eci_locations,
   miri_locations)

sub_obj_4_1_atlas_eci$year <- sub_obj_4_1_atlas_eci$year %>% as.numeric()

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# inspect
sub_obj_4_1_atlas_eci
sub_obj_4_1_atlas_eci %>% glimpse()
sub_obj_4_1_atlas_eci %>% nrow() # 252
sub_obj_4_1_atlas_eci %>% ncol() # 4

sub_obj_4_1_atlas_eci %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_4_1_atlas_eci %>% arrange(values) %>% distinct(country_name)
sub_obj_4_1_atlas_eci %>% skim()

# inspect country names
# note that atlas does not have kosovo, montenegro, or luxembourg (only has 134 countries total)
sub_obj_4_1_atlas_eci %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name)
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_4_1_atlas_eci, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country)

sub_obj_4_1_atlas_eci %>% 
  filter(str_detect(string = country_name, pattern = regex("mont", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
sub_obj_4_1_atlas_eci <- sub_obj_4_1_atlas_eci %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_4_1_atlas_eci",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name") %>%
  select(-iso3.x)


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# inspect
sub_obj_4_1_atlas_eci
sub_obj_4_1_atlas_eci %>% glimpse()
sub_obj_4_1_atlas_eci %>% nrow() # 270
sub_obj_4_1_atlas_eci %>% ncol() # 36

# check country/year
sub_obj_4_1_atlas_eci %>% distinct(country) %>% nrow() # 45
sub_obj_4_1_atlas_eci %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_4_1_atlas_eci %>% count(indicator_name) # 270 (6 years * 45 countries = 270)
sub_obj_4_1_atlas_eci %>% count(year)

# check values
# missing all values (2018-2022) for kosovo, luxembourg, montenegro
sub_obj_4_1_atlas_eci %>% filter(indicator_name == "sub_obj_4_1_atlas_eci") %>% skim(values)
sub_obj_4_1_atlas_eci  %>% filter(is.na(values), year <= 2023) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_4_1_atlas_eci  %>% filter(is.na(values), year <= 2023) %>%
  count(country, year) %>% print(n = nrow(.))

sub_obj_4_1_atlas_eci  %>% skim(values)
sub_obj_4_1_atlas_eci  %>% group_by(year) %>% skim(values)
sub_obj_4_1_atlas_eci  %>% filter(year <= 2023) %>% skim(values)
sub_obj_4_1_atlas_eci  %>% filter(year <= 2023) %>% group_by(country) %>% skim(values)

# plot
sub_obj_4_1_atlas_eci %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# inspect summary stats on indicators
sub_obj_4_1_atlas_eci %>% group_by(indicator_name) %>% 
  mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
         missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
         missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
  distinct(indicator_name, year_start, year_end, missing_pct)



#//////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_4_1_atlas_eci %>% write_csv(file = "data/fmir/sub_obj_4_1_atlas_eci.csv")
sub_obj_4_1_atlas_eci <- read_csv(file = "data/fmir/sub_obj_4_1_atlas_eci.csv")



#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
# load sub_obj_4_3_ilo_youth_neet ####
sub_obj_4_3_ilo_youth_neet <- read_excel(path = "data/ilo/ilostat/ilostat2024_neet.xlsx") %>%
  filter(sex.label == "Sex: Total") %>%
  filter(time %in% 2018:2023) %>%
  dplyr::select(ref_area.label,
                time,
                obs_value) %>%
  rename(country_name=ref_area.label,
         year=time,
         values=obs_value) %>%
  pivot_wider(id_cols = country_name, 
              names_from = year, 
              values_from = values) %>%
  pivot_longer(cols = c(`2018`, `2019`, `2020`, `2021`, `2022`, `2023`), 
               names_to = "year", 
               values_to = "values") %>%
  mutate(high_value_is_good_outcome_flag = 0,
         indicator_name = "sub_obj_4_3_ilo_youth_neet",
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Republic of Moldova" ~ "Moldova",
                                  country_name == "United Kingdom of Great Britain and Northern Ireland" ~ "U.K.",
                                  country_name == "United States of America" ~ "U.S.",
                                  TRUE ~ country_name),
         year = as.numeric(year))


# inspect
sub_obj_4_3_ilo_youth_neet
sub_obj_4_3_ilo_youth_neet %>% glimpse()
sub_obj_4_3_ilo_youth_neet %>% nrow() # 1680
sub_obj_4_3_ilo_youth_neet %>% ncol() # 5
sub_obj_4_3_ilo_youth_neet %>% skim()
sub_obj_4_3_ilo_youth_neet %>% group_by(year) %>% skim()

#missing Kosovo
sub_obj_4_3_ilo_youth_neet %>% filter(country_name == "Kosovo") %>% arrange(year)
sub_obj_4_3_ilo_youth_neet %>% arrange(values)
sub_obj_4_3_ilo_youth_neet %>% arrange(desc(values))
sub_obj_4_3_ilo_youth_neet %>% filter(is.na(values)) %>% count(country_name) %>% arrange(desc(n))
sub_obj_4_3_ilo_youth_neet %>%
  filter(country_name %in%
           (country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S.") %>%
              distinct(country) %>% pull(country))) %>%
  filter(is.na(values)) %>% count(country_name)

# inspect country names
sub_obj_4_3_ilo_youth_neet %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>%
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_4_3_ilo_youth_neet <- sub_obj_4_3_ilo_youth_neet %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(high_value_is_good_outcome_flag = 0,
         indicator_name = "sub_obj_4_3_ilo_youth_neet") %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_4_3_ilo_youth_neet 
sub_obj_4_3_ilo_youth_neet %>% glimpse()
sub_obj_4_3_ilo_youth_neet %>% nrow() # 315
sub_obj_4_3_ilo_youth_neet %>% ncol() # 39
sub_obj_4_3_ilo_youth_neet %>% distinct(country) %>% nrow() # 45

sub_obj_4_3_ilo_youth_neet %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_4_3_ilo_youth_neet %>% filter(is.na(values), year >= 2018) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_4_3_ilo_youth_neet %>% skim(values)
sub_obj_4_3_ilo_youth_neet %>% group_by(year) %>% skim(values)
sub_obj_4_3_ilo_youth_neet %>% filter(year >= 2018) %>% skim(values)
sub_obj_4_3_ilo_youth_neet %>% filter(year >= 2018) %>% group_by(country) %>% skim(values)


# inspect interpolation
sub_obj_4_3_ilo_youth_neet %>% filter(year >= 2018) %>%
  select(country, year, values) %>% print(n = 50)

# Missing values for all EU-15 and US
sub_obj_4_3_ilo_youth_neet %>% filter(is.na(values), year >= 2018) %>% 
  count(country, mcp_grouping) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
sub_obj_4_3_ilo_youth_neet %>% filter(is.na(values),
                                             mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                             year >= 2018) %>%
  select(country, mcp_grouping, year, values) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
sub_obj_4_3_ilo_youth_neet %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_4_3_ilo_youth_neet %>% filter(year >= 2018) %>% group_by(mcp_grouping) %>% skim(values)


sub_obj_4_3_ilo_youth_neet$year <- as.numeric(sub_obj_4_3_ilo_youth_neet$year)

# plot
sub_obj_4_3_ilo_youth_neet %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = jitter(values, 1), color = country)) + geom_line(size = 1)


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_4_3_ilo_youth_neet %>% write_csv(file = "data/fmir/sub_obj_4_3_ilo_youth_neet.csv")
sub_obj_4_3_ilo_youth_neet <- read_csv(file = "data/fmir/sub_obj_4_3_ilo_youth_neet.csv")



#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
