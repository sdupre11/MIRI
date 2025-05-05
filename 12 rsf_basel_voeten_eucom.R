# load sub_obj_4_2_basel_financial_transparency ####
#Here we currently only take the most recent year available due to the time consuming aspect of pulling out data from the individual PDFs and
#their changing methodology over time

sub_obj_4_2_basel_financial_transparency <- read_excel(path = "data/basel/basel-aml-index-expertedition_2024-09-27.xlsx",
                                                    sheet = "Expert Edition") %>%
  slice(-c(2,3)) %>%
  rename(country_name = `Expert Edition 2024`,
         `2023` = `Financial Transparency & Standards`) %>%
  select(c("country_name",
           `2023`)) %>%
  slice(-1)

sub_obj_4_2_basel_financial_transparency$`2018` <- NA
sub_obj_4_2_basel_financial_transparency$`2019` <- NA
sub_obj_4_2_basel_financial_transparency$`2020` <- NA
sub_obj_4_2_basel_financial_transparency$`2021` <- NA
sub_obj_4_2_basel_financial_transparency$`2022` <- NA

sub_obj_4_2_basel_financial_transparency <- sub_obj_4_2_basel_financial_transparency %>%
  pivot_longer(cols = c(`2018`, `2019`, `2020`, `2021`, `2022`, `2023`), 
               names_to = "year", 
               values_to = "values") %>%
  mutate(indicator_name = "sub_obj_4_2_basel_financial_transparency",
         high_value_is_good_outcome_flag = 0,
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "Burma/Myanmar" ~ "Burma",
                                  country_name == "Cape Verde" ~ "Cabo Verde",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Democratic Republic of the Congo" ~ "Congo (Kinshasa)",
                                  country_name == "Hong Kong" ~ "Hong Kong SAR, China",
                                  country_name == "Ivory Coast" ~ "Cote d'Ivoire",
                                  country_name == "North Korea" ~ "Korea, North",
                                  country_name == "Macedonia" ~ "N. Macedonia",
                                  country_name == "Palestine/West Bank" ~ "West Bank and Gaza",
                                  country_name == "Papal States" ~ "Holy See",
                                  country_name == "Republic of the Congo" ~ "Congo (Brazzaville)",
                                  country_name == "Republic of Vietnam" ~ "Vietnam",
                                  country_name == "South Korea" ~ "Korea, South",
                                  country_name == "The Gambia" ~ "Gambia, The",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States" ~ "U.S.",
                                  TRUE ~ country_name),
         year = as.numeric(year)) 


#//////////////////////////


# inspect
sub_obj_4_2_basel_financial_transparency
sub_obj_4_2_basel_financial_transparency %>% glimpse()
sub_obj_4_2_basel_financial_transparency %>% nrow() # 1421
sub_obj_4_2_basel_financial_transparency %>% ncol() # 5

# chack values
sub_obj_4_2_basel_financial_transparency %>% skim()
sub_obj_4_2_basel_financial_transparency %>% group_by(year) %>% skim()
sub_obj_4_2_basel_financial_transparency %>% count(year)
sub_obj_4_2_basel_financial_transparency %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_4_2_basel_financial_transparency %>% arrange(values) %>% distinct(country_name)


# inspect country names
# many are missing
sub_obj_4_2_basel_financial_transparency %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | iso3 == "USA") %>% 
  anti_join(., sub_obj_4_2_basel_financial_transparency, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_4_2_basel_financial_transparency <- sub_obj_4_2_basel_financial_transparency %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_4_2_basel_financial_transparency",
         high_value_is_good_outcome_flag = 0) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_4_2_basel_financial_transparency 
sub_obj_4_2_basel_financial_transparency %>% glimpse()
sub_obj_4_2_basel_financial_transparency %>% nrow() # 315
sub_obj_4_2_basel_financial_transparency %>% ncol() # 36

sub_obj_4_2_basel_financial_transparency %>% distinct(country) %>% nrow() # 45

# check values
#Only 1 year per country, which is what we expected
sub_obj_4_2_basel_financial_transparency %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_4_2_basel_financial_transparency %>% filter(is.na(values), year >= 2006) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_4_2_basel_financial_transparency %>% skim(values)
sub_obj_4_2_basel_financial_transparency %>% group_by(year) %>% skim(values)
sub_obj_4_2_basel_financial_transparency %>% filter(year >= 2006) %>% skim(values)
sub_obj_4_2_basel_financial_transparency %>% filter(year >= 2006) %>% group_by(country) %>% skim(values)

sub_obj_4_2_basel_financial_transparency %>% filter(is.na(values),
                                                 mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                                 year >= 2007) %>%
  select(country, mcp_grouping, year, values)
sub_obj_4_2_basel_financial_transparency %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_4_2_basel_financial_transparency %>% filter(year >= 2007) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_4_2_basel_financial_transparency %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# #/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_4_2_basel_financial_transparency %>% write_csv(file = "data/fmir/sub_obj_4_2_basel_financial_transparency.csv")
sub_obj_4_2_basel_financial_transparency <- read.csv(file = "data/fmir/sub_obj_4_2_basel_financial_transparency.csv") %>% 
  as_tibble()

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
# load sub_obj_1_3_voeten_unga_voting ####

sub_obj_1_3_voeten_unga_voting <- read_csv("data/voeten/unga/IdealpointestimatesAll_Jun2024.csv") %>%
  mutate(year = session+1945) %>%
  filter(year >= 2018) %>%
  select(c("iso3c",
           "Q50%All",
           "year",
           "USAgree")) %>%
  rename(iso3 = iso3c,
         values = `Q50%All`) %>%
  select(-USAgree) %>%
  pivot_wider(id_cols = iso3, 
              names_from = year, 
              values_from = values) %>%
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "values") %>%
  mutate(indicator_name = "sub_obj_1_3_voeten_unga_voting",
         high_value_is_good_outcome_flag = 1,
         year = as.numeric(year),
         iso3 = case_when(
           iso3 == "YUG" ~ "SRB",
           TRUE ~ iso3
         ))

#//////////////////////////


# inspect
sub_obj_1_3_voeten_unga_voting
sub_obj_1_3_voeten_unga_voting %>% glimpse()
sub_obj_1_3_voeten_unga_voting %>% nrow() # 1351
sub_obj_1_3_voeten_unga_voting %>% ncol() # 5

# chack values
sub_obj_1_3_voeten_unga_voting %>% skim()
sub_obj_1_3_voeten_unga_voting %>% group_by(year) %>% skim()
sub_obj_1_3_voeten_unga_voting %>% count(year)
sub_obj_1_3_voeten_unga_voting %>% arrange(desc(values)) %>% distinct(iso3)
sub_obj_1_3_voeten_unga_voting %>% arrange(values) %>% distinct(iso3)


# inspect country names
# many are missing
sub_obj_1_3_voeten_unga_voting %>% anti_join(., country_crosswalk, by = c("iso3" = "iso3")) %>% 
  distinct(iso3) %>% arrange(iso3) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | iso3 == "iso3") %>% 
  anti_join(., sub_obj_1_3_voeten_unga_voting, by = c("iso3" = "iso3")) %>% 
  distinct(iso3) %>% arrange(iso3) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_1_3_voeten_unga_voting <- sub_obj_1_3_voeten_unga_voting %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("iso3" = "iso3", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_1_3_voeten_unga_voting",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_1_3_voeten_unga_voting 
sub_obj_1_3_voeten_unga_voting %>% glimpse()
sub_obj_1_3_voeten_unga_voting %>% nrow() # 315
sub_obj_1_3_voeten_unga_voting %>% ncol() # 36

sub_obj_1_3_voeten_unga_voting %>% distinct(country) %>% nrow() # 45

# check values
#Only 1 year per country, which is what we expected
sub_obj_1_3_voeten_unga_voting %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_1_3_voeten_unga_voting %>% filter(is.na(values), year >= 2006) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_1_3_voeten_unga_voting %>% skim(values)
sub_obj_1_3_voeten_unga_voting %>% group_by(year) %>% skim(values)
sub_obj_1_3_voeten_unga_voting %>% filter(year >= 2006) %>% skim(values)
sub_obj_1_3_voeten_unga_voting %>% filter(year >= 2006) %>% group_by(country) %>% skim(values)

sub_obj_1_3_voeten_unga_voting %>% filter(is.na(values),
                                                    mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                                    year >= 2007) %>%
  select(country, mcp_grouping, year, values)
sub_obj_1_3_voeten_unga_voting %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_1_3_voeten_unga_voting %>% filter(year >= 2007) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_1_3_voeten_unga_voting %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# #/////////////////////////////////////////////////////////////////////////////////////////////

# read/write
sub_obj_1_3_voeten_unga_voting %>% write_csv(file = "data/fmir/sub_obj_1_3_voeten_unga_voting.csv")
sub_obj_1_3_voeten_unga_voting <- read.csv(file = "data/fmir/sub_obj_1_3_voeten_unga_voting.csv") %>% 
  as_tibble()

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
# load sub_obj_2_3_eucom_malign_media_penetration ####

#Data collection happens at the very start of each year, applying the data to the preceding year 
#as the answers are largely pertaining to opinions from the preceding year

sub_obj_2_3_eucom_malign_media_penetration <- read_excel(path = "data/eucom/vmfi.xlsx") %>%
  filter(indicator == "sub_obj_2_3_eucom_malign_media_penetration") %>%
  mutate(`2022` = `2023`,
         `2023` = `2024`) %>%
  select(-`2024`) %>%
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "values") %>%
  select(-indicator) %>%
  mutate(indicator_name = "sub_obj_2_3_eucom_malign_media_penetration",
         high_value_is_good_outcome_flag = 0,
         year = as.numeric(year))

#//////////////////////////


# inspect
sub_obj_2_3_eucom_malign_media_penetration
sub_obj_2_3_eucom_malign_media_penetration %>% glimpse()
sub_obj_2_3_eucom_malign_media_penetration %>% nrow() # 154
sub_obj_2_3_eucom_malign_media_penetration %>% ncol() # 5

# chack values
sub_obj_2_3_eucom_malign_media_penetration %>% skim()
sub_obj_2_3_eucom_malign_media_penetration %>% group_by(year) %>% skim()
sub_obj_2_3_eucom_malign_media_penetration %>% count(year)
sub_obj_2_3_eucom_malign_media_penetration %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_2_3_eucom_malign_media_penetration %>% arrange(values) %>% distinct(country_name)


# inspect country names
# many are missing
sub_obj_2_3_eucom_malign_media_penetration %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | iso3 == "iso3") %>% 
  anti_join(., sub_obj_2_3_eucom_malign_media_penetration, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_2_3_eucom_malign_media_penetration <- sub_obj_2_3_eucom_malign_media_penetration %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_2_3_eucom_malign_media_penetration",
         high_value_is_good_outcome_flag = 0) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_2_3_eucom_malign_media_penetration 
sub_obj_2_3_eucom_malign_media_penetration %>% glimpse()
sub_obj_2_3_eucom_malign_media_penetration %>% nrow() # 315
sub_obj_2_3_eucom_malign_media_penetration %>% ncol() # 36

sub_obj_2_3_eucom_malign_media_penetration %>% distinct(country) %>% nrow() # 45

# check values
#Only 1 year per country, which is what we expected
sub_obj_2_3_eucom_malign_media_penetration %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_2_3_eucom_malign_media_penetration %>% filter(is.na(values), year >= 2006) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_2_3_eucom_malign_media_penetration %>% skim(values)
sub_obj_2_3_eucom_malign_media_penetration %>% group_by(year) %>% skim(values)
sub_obj_2_3_eucom_malign_media_penetration %>% filter(year >= 2006) %>% skim(values)
sub_obj_2_3_eucom_malign_media_penetration %>% filter(year >= 2006) %>% group_by(country) %>% skim(values)

sub_obj_2_3_eucom_malign_media_penetration %>% filter(is.na(values),
                                          mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                          year >= 2007) %>%
  select(country, mcp_grouping, year, values)
sub_obj_2_3_eucom_malign_media_penetration %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_2_3_eucom_malign_media_penetration %>% filter(year >= 2007) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_2_3_eucom_malign_media_penetration %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# #/////////////////////////////////////////////////////////////////////////////////////////////

# read/write
sub_obj_2_3_eucom_malign_media_penetration %>% write_csv(file = "data/fmir/sub_obj_2_3_eucom_malign_media_penetration.csv")
sub_obj_2_3_eucom_malign_media_penetration <- read.csv(file = "data/fmir/sub_obj_2_3_eucom_malign_media_penetration.csv") %>% 
  as_tibble()

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////

# load sub_obj_2_3_eucom_western_media_penetration ####

#Data collection happens at the very start of each year, applying the data to the preceding year 
#as the answers are largely pertaining to opinions from the preceding year

sub_obj_2_3_eucom_western_media_penetration <- read_excel(path = "data/eucom/vmfi.xlsx") %>%
  filter(indicator == "sub_obj_2_3_eucom_western_media_penetration") %>%
  mutate(`2022` = `2023`,
         `2023` = `2024`) %>%
  select(-`2024`) %>%
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "values") %>%
  select(-indicator) %>%
  mutate(indicator_name = "sub_obj_2_3_eucom_western_media_penetration",
         high_value_is_good_outcome_flag = 1,
         year = as.numeric(year))

#//////////////////////////


# inspect
sub_obj_2_3_eucom_western_media_penetration
sub_obj_2_3_eucom_western_media_penetration %>% glimpse()
sub_obj_2_3_eucom_western_media_penetration %>% nrow() # 154
sub_obj_2_3_eucom_western_media_penetration %>% ncol() # 5

# chack values
sub_obj_2_3_eucom_western_media_penetration %>% skim()
sub_obj_2_3_eucom_western_media_penetration %>% group_by(year) %>% skim()
sub_obj_2_3_eucom_western_media_penetration %>% count(year)
sub_obj_2_3_eucom_western_media_penetration %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_2_3_eucom_western_media_penetration %>% arrange(values) %>% distinct(country_name)


# inspect country names
# many are missing
sub_obj_2_3_eucom_western_media_penetration %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | iso3 == "iso3") %>% 
  anti_join(., sub_obj_2_3_eucom_western_media_penetration, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_2_3_eucom_western_media_penetration <- sub_obj_2_3_eucom_western_media_penetration %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_2_3_eucom_western_media_penetration",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_2_3_eucom_western_media_penetration 
sub_obj_2_3_eucom_western_media_penetration %>% glimpse()
sub_obj_2_3_eucom_western_media_penetration %>% nrow() # 315
sub_obj_2_3_eucom_western_media_penetration %>% ncol() # 36

sub_obj_2_3_eucom_western_media_penetration %>% distinct(country) %>% nrow() # 45

# check values
#Only 1 year per country, which is what we expected
sub_obj_2_3_eucom_western_media_penetration %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_2_3_eucom_western_media_penetration %>% filter(is.na(values), year >= 2006) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_2_3_eucom_western_media_penetration %>% skim(values)
sub_obj_2_3_eucom_western_media_penetration %>% group_by(year) %>% skim(values)
sub_obj_2_3_eucom_western_media_penetration %>% filter(year >= 2006) %>% skim(values)
sub_obj_2_3_eucom_western_media_penetration %>% filter(year >= 2006) %>% group_by(country) %>% skim(values)

sub_obj_2_3_eucom_western_media_penetration %>% filter(is.na(values),
                                                      mcp_grouping %in% c("E&E Balkans", 
                                                                          "E&E Eurasia", 
                                                                          "CARs", 
                                                                          "E&E graduates", 
                                                                          "Russia"),
                                                      year >= 2007) %>%
  select(country, mcp_grouping, year, values)
sub_obj_2_3_eucom_western_media_penetration %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_2_3_eucom_western_media_penetration %>% filter(year >= 2007) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_2_3_eucom_western_media_penetration %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# #/////////////////////////////////////////////////////////////////////////////////////////////

# read/write
sub_obj_2_3_eucom_western_media_penetration %>% write_csv(file = "data/fmir/sub_obj_2_3_eucom_western_media_penetration.csv")
sub_obj_2_3_eucom_western_media_penetration <- read.csv(file = "data/fmir/sub_obj_2_3_eucom_western_media_penetration.csv") %>% 
  as_tibble()

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////