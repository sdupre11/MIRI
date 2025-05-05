
# prep for all VIBE Data

# for reference, the new VIBE dataset will replace MSI, but VIBE only has 2020-2021 condition_year data
# below is the crosswalk of old MSI dimensions to new VICE dimensions from VIBE's report (also in methodology section on dashboard)
# https://vibe.irex.org/
# https://www.irex.org/sites/default/files/pdf/vibrant-information-barometer-2021.pdf
# MSI Overall scores - VIBE Overall scores
# Freedom of Speech - VIBE Principle 2 (Multiple Channels - How Info Flows)
# Professional Journalism - VIBE Principle 1 (Information Quality)
# Plurality of News - VIBE Indicator 4 (The body of content overall is inclusive and diverse.)
# Business Management - VIBE Indicator 5 (Content production is sufficiently resourced.)
# Supporting Institutions - no direct mapping (Supporting Institutions indicators are spread across VIBE Indicators)
# VIBE Principal 3 (Info Consumption and Engagement) has no MSI corollary
# VIBE Principal 4 (Transformative Action) has no MSI corollary

# note that shannon mcguire confirmed that the year variable listed in MSI and VIBE data is "report year", 
# and data refer to prior year; see email from shannon Feb 14, 2022, 4:25 PM

# note that MSI scoring ranges from 0 to 4; VIBE scoring ranges from 0 to 40
# will multiply MSI 0-4 scores by 10 to convert to VIBE 0-40 scale 
# https://www.irex.org/sites/default/files/pdf/media-sustainability-index-europe-eurasia-2019-full.pdf
# https://vibe.irex.org/

# note that MSI had 21 countries, VIBE had 13 in condition_year 2020, and 18 in condition_year 2021

# note that raw irex_msi data was copy/pasted together

# Edited the VIBE data by hand to make it more machine readable

VIBE_2020 <- read_excel("data/irex/vibe/vibe-scores-2021 - 2024.xlsx", 
                        sheet = "VIBE 2021") %>%
  mutate(year = 2020)

VIBE_2021 <- read_excel("data/irex/vibe/vibe-scores-2021 - 2024.xlsx", 
                        sheet = "VIBE 2022") %>%
  mutate(year = 2021)

VIBE_2022 <- read_excel("data/irex/vibe/vibe-scores-2021 - 2024.xlsx", 
                        sheet = "VIBE 2023") %>%
  mutate(year = 2022)

VIBE_2023 <- read_excel("data/irex/vibe/vibe-scores-2021 - 2024.xlsx", 
                        sheet = "VIBE 2024") %>%
  mutate(year = 2023)

VIBE_data <- VIBE_2020 %>%
  bind_rows(VIBE_2021,
            VIBE_2022,
            VIBE_2023) %>%
  rename(country_name = `Country Name`,
         professional_journalism = `Principle 1`,
         information_flows = `Principle 2`,
         digital_freedoms = `Indicator 11`,
         media_literacy = `Indicator 12`,
         media_engagement = `Indicator 13`,
         information_use = `Principle 4`) %>%
  select(-c(`Principle 3`,
            Overall))

columns_to_remove <- grep("^Indicator", names(VIBE_data), value = TRUE)
VIBE_data <- VIBE_data[ , !(names(VIBE_data) %in% columns_to_remove)]

MSI_data <- read_excel(path = "data/irex/msi/irex_msi_data_cleaned.xlsx", sheet = "Sheet1") %>%
  filter(year >= 2018) %>%
  rename(country_name = COUNTRY, 
         information_flows = `Obj. 1`,
         professional_journalism = `Obj. 2`) %>% 
  select(country_name, 
         year, 
         information_flows,
         professional_journalism) %>%
  mutate(information_flows = information_flows*10,
         professional_journalism = professional_journalism*10)

IREX_data <- VIBE_data %>%
  bind_rows(MSI_data) %>%
  mutate(high_value_is_good_outcome_flag = 1,
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "Bosnia & Herzegovina" ~ "BiH",
                                  country_name == "Macedonia" ~ "N. Macedonia",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Kazahkstan" ~ "Kazakhstan",
                                  TRUE ~ country_name))
#/////////////////
# impute missing 2019 values (due to jump from 2018 MSI to 2020 VIBE) for information_flows and for professional_journalism and load all
# NOTE Belarus professional journalism values are based largely on media in-exile and so are unrealistically high per Alex. They get replaced with
# NAs here (and then again prior to the imputation step with CAR EUCOM data and missing EU data), they then get later replaced with sub-objective
# mean scores. The inciting notice to this was taken when Alex and co. noted that this would put Serbia and Belarus at parity for information space
# resilience overall. 

#impute/load sub_obj_2_1_irex_professional_journalism ####  

sub_obj_2_1_irex_professional_journalism <- IREX_data %>%
  select(-c(information_flows,
            media_literacy,
            media_engagement,
            digital_freedoms,
            information_use)) %>%
  pivot_wider(id_cols = c("country_name", "high_value_is_good_outcome_flag"), names_from = "year", values_from = "professional_journalism") %>%
  pivot_longer(cols = -c("country_name", "high_value_is_good_outcome_flag"), names_to = "year", values_to = "professional_journalism") %>%
  rename(values = professional_journalism) %>%
  mutate(indicator_name = "sub_obj_2_1_irex_professional_journalism")  

sub_obj_2_1_irex_professional_journalism <- sub_obj_2_1_irex_professional_journalism %>% #Belarus scores are based heavily on media in-exile, overly inflated. Will be imputed.
  mutate(values = case_when(
    country_name == "Belarus" ~ NA,
    TRUE ~ values)
  )

#impute/load sub_obj_2_2_irex_media_literacy ####
sub_obj_2_2_irex_media_literacy <- IREX_data %>%
  select(-c(professional_journalism,
            information_flows,
            media_engagement,
            digital_freedoms,
            information_use)) %>%
  pivot_wider(id_cols = c("country_name", "high_value_is_good_outcome_flag"), names_from = "year", values_from = "media_literacy") %>%
  pivot_longer(cols = -c("country_name", "high_value_is_good_outcome_flag"), names_to = "year", values_to = "media_literacy") %>%
  rename(values = media_literacy) %>%
  mutate(indicator_name = "sub_obj_2_2_irex_media_literacy")


#impute/load sub_obj_2_2_irex_media_engagement ####
sub_obj_2_2_irex_media_engagement <- IREX_data %>%
  select(-c(professional_journalism,
            information_flows,
            media_literacy,
            digital_freedoms,
            information_use)) %>%
  pivot_wider(id_cols = c("country_name", "high_value_is_good_outcome_flag"), names_from = "year", values_from = "media_engagement") %>%
  pivot_longer(cols = -c("country_name", "high_value_is_good_outcome_flag"), names_to = "year", values_to = "media_engagement") %>%
  rename(values = media_engagement) %>%
  mutate(indicator_name = "sub_obj_2_2_irex_media_engagement")

#impute/load sub_obj_2_3_irex_information_flows ####
sub_obj_2_3_irex_information_flows <- IREX_data %>%
  select(-c(professional_journalism,
            media_literacy,
            media_engagement,
            digital_freedoms,
            information_use)) %>%
  pivot_wider(id_cols = c("country_name", "high_value_is_good_outcome_flag"), names_from = "year", values_from = "information_flows") %>%
  pivot_longer(cols = -c("country_name", "high_value_is_good_outcome_flag"), names_to = "year", values_to = "information_flows") %>%
  rename(values = information_flows) %>%
  mutate(indicator_name = "sub_obj_2_3_irex_information_flows")

#impute/load sub_obj_2_3_irex_information_use ####
sub_obj_2_3_irex_information_use <- IREX_data %>%
  select(-c(professional_journalism,
            information_flows,
            media_literacy,
            digital_freedoms,
            media_engagement)) %>%
  pivot_wider(id_cols = c("country_name", "high_value_is_good_outcome_flag"), names_from = "year", values_from = "information_use") %>%
  pivot_longer(cols = -c("country_name", "high_value_is_good_outcome_flag"), names_to = "year", values_to = "information_use") %>%
  rename(values = information_use) %>%
  mutate(indicator_name = "sub_obj_2_3_irex_information_use")

#Next steps ####


sub_obj_2_1_irex_professional_journalism$year <- sub_obj_2_1_irex_professional_journalism$year %>% as.numeric()
sub_obj_2_3_irex_information_flows$year <- sub_obj_2_3_irex_information_flows$year %>% as.numeric()
sub_obj_2_3_irex_information_use$year <- sub_obj_2_3_irex_information_use$year %>% as.numeric()
sub_obj_2_2_irex_media_engagement$year <- sub_obj_2_2_irex_media_engagement$year %>% as.numeric()
sub_obj_2_2_irex_media_literacy$year <- sub_obj_2_2_irex_media_literacy$year %>% as.numeric()

#/////////////////

#rest of processing for sub_obj_2_1_irex_professional_journalism

# inspect
# note msi has 20 countries for 2001-2007, then 21 countries for 2008-2019
sub_obj_2_1_irex_professional_journalism
sub_obj_2_1_irex_professional_journalism %>% glimpse()
sub_obj_2_1_irex_professional_journalism %>% nrow() # 132
sub_obj_2_1_irex_professional_journalism %>% ncol() # 5

sub_obj_2_1_irex_professional_journalism %>% count(country_name) # 21
sub_obj_2_1_irex_professional_journalism %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_2_1_irex_professional_journalism %>% arrange(values) %>% distinct(country_name)
sub_obj_2_1_irex_professional_journalism %>% skim()

# inspect country names
sub_obj_2_1_irex_professional_journalism %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name)
sub_obj_2_1_irex_professional_journalism %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_2_1_irex_professional_journalism <- sub_obj_2_1_irex_professional_journalism %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_2_1_irex_professional_journalism",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////////


# inspect
sub_obj_2_1_irex_professional_journalism
sub_obj_2_1_irex_professional_journalism %>% glimpse()
sub_obj_2_1_irex_professional_journalism %>% nrow() # 945
sub_obj_2_1_irex_professional_journalism %>% ncol() # 35

# check country/year
sub_obj_2_1_irex_professional_journalism %>% count(country) %>% print(n = nrow(.))
sub_obj_2_1_irex_professional_journalism %>% distinct(country) %>% nrow() # 45
sub_obj_2_1_irex_professional_journalism %>% count(year)
sub_obj_2_1_irex_professional_journalism %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))


#/////////////////


# Missing MSI 2001-2018 for US, EU-15, Czechia, Estonia, Hungary, Latvia, Lithuania, Poland, Slovakia, Slovenia, and
# Turkmenistan (2001-2007)
# Missing VIBE 2020 for US, EU-15, CARs, and all graduates except Montenegro
# Missing 2019 for all countries because of gap between MSI ending in 2018 and VIBE beginning in 2020
# will impute 2019 as the midpoint between 2018 and 2020 values
sub_obj_2_1_irex_professional_journalism %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_2_1_irex_professional_journalism %>% filter(is.na(values), mcp_grouping == "EU-15") %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))

# check missing for CARs
sub_obj_2_1_irex_professional_journalism %>% filter(is.na(values), mcp_grouping == "CARs") %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_2_1_irex_professional_journalism %>% filter(is.na(values), mcp_grouping == "CARs") %>% count(country, year) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))

# check missing for graduates
sub_obj_2_1_irex_professional_journalism %>% filter(is.na(values), mcp_grouping == "E&E graduates") %>% 
  count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_2_1_irex_professional_journalism %>% filter(is.na(values), mcp_grouping == "E&E graduates") %>% 
  count(country, year) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))

# check missing for ee_presence
sub_obj_2_1_irex_professional_journalism %>% filter(is.na(values), 
                                                    mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
  count(country, year) %>% print(n = nrow(.))

# skim values
sub_obj_2_1_irex_professional_journalism %>% skim(values)
sub_obj_2_1_irex_professional_journalism %>% group_by(year) %>% skim(values)
sub_obj_2_1_irex_professional_journalism %>% group_by(country) %>% skim(values)


#/////////////////////


# plot
sub_obj_2_1_irex_professional_journalism %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  filter(year >= 2010) %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) +
  scale_x_continuous(breaks = seq(from = 2010, to = 2020, by = 1))


#//////////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_2_1_irex_professional_journalism %>% write_csv(file = "data/fmir/sub_obj_2_1_irex_professional_journalism.csv")
sub_obj_2_1_irex_professional_journalism <- read_csv(file = "data/fmir/sub_obj_2_1_irex_professional_journalism.csv")




#rest of processing for sub_obj_2_2_irex_media_literacy

# inspect
# note msi has 20 countries for 2001-2007, then 21 countries for 2008-2019
sub_obj_2_2_irex_media_literacy
sub_obj_2_2_irex_media_literacy %>% glimpse()
sub_obj_2_2_irex_media_literacy %>% nrow() # 403
sub_obj_2_2_irex_media_literacy %>% ncol() # 6

sub_obj_2_2_irex_media_literacy %>% count(country_name) # 21
sub_obj_2_2_irex_media_literacy %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_2_2_irex_media_literacy %>% arrange(values) %>% distinct(country_name)
sub_obj_2_2_irex_media_literacy %>% skim()

# inspect country names
sub_obj_2_2_irex_media_literacy %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name)
sub_obj_2_2_irex_media_literacy %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_2_2_irex_media_literacy <- sub_obj_2_2_irex_media_literacy %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_2_2_irex_media_literacy",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")

#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_2_2_irex_media_literacy <- sub_obj_2_2_irex_media_literacy %>% 
  mutate(values = case_when(
    mcp_grouping == "EU-15" & year == 2020 ~ 19,
    mcp_grouping == "EU-15" & year == 2021 ~ 21,
    mcp_grouping == "EU-15" & year == 2022 ~ 21,
    mcp_grouping == "EU-15" & year == 2023 ~ 22,
    TRUE ~ values
  ))

#/////////////////


# inspect
sub_obj_2_2_irex_media_literacy
sub_obj_2_2_irex_media_literacy %>% glimpse()
sub_obj_2_2_irex_media_literacy %>% nrow() # 945
sub_obj_2_2_irex_media_literacy %>% ncol() # 35

# check country/year
sub_obj_2_2_irex_media_literacy %>% count(country) %>% print(n = nrow(.))
sub_obj_2_2_irex_media_literacy %>% distinct(country) %>% nrow() # 45
sub_obj_2_2_irex_media_literacy %>% count(year)
sub_obj_2_2_irex_media_literacy %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))


#/////////////////


# Missing MSI 2001-2018 for US, EU-15, Czechia, Estonia, Hungary, Latvia, Lithuania, Poland, Slovakia, Slovenia, and
# Turkmenistan (2001-2007)
# Missing VIBE 2020 for US, EU-15, CARs, and all graduates except Montenegro
# Missing 2019 for all countries because of gap between MSI ending in 2018 and VIBE beginning in 2020
# will impute 2019 as the midpoint between 2018 and 2020 values
sub_obj_2_2_irex_media_literacy %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_2_2_irex_media_literacy %>% filter(is.na(values), mcp_grouping == "EU-15") %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))

# check missing for CARs
sub_obj_2_2_irex_media_literacy %>% filter(is.na(values), mcp_grouping == "CARs") %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_2_2_irex_media_literacy %>% filter(is.na(values), mcp_grouping == "CARs") %>% count(country, year) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))

# check missing for graduates
sub_obj_2_2_irex_media_literacy %>% filter(is.na(values), mcp_grouping == "E&E graduates") %>% 
  count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_2_2_irex_media_literacy %>% filter(is.na(values), mcp_grouping == "E&E graduates") %>% 
  count(country, year) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))

# check missing for ee_presence
sub_obj_2_2_irex_media_literacy %>% filter(is.na(values), 
                                           mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
  count(country, year) %>% print(n = nrow(.))

# skim values
sub_obj_2_2_irex_media_literacy %>% skim(values)
sub_obj_2_2_irex_media_literacy %>% group_by(year) %>% skim(values)
sub_obj_2_2_irex_media_literacy %>% group_by(country) %>% skim(values)


#/////////////////////


# plot
sub_obj_2_2_irex_media_literacy %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  filter(year >= 2010) %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) +
  scale_x_continuous(breaks = seq(from = 2010, to = 2023, by = 1))


#//////////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_2_2_irex_media_literacy %>% write_csv(file = "data/fmir/sub_obj_2_2_irex_media_literacy.csv")
sub_obj_2_2_irex_media_literacy <- read_csv(file = "data/fmir/sub_obj_2_2_irex_media_literacy.csv")



#rest of processing for sub_obj_2_2_irex_media_engagement

# inspect
# note msi has 20 countries for 2001-2007, then 21 countries for 2008-2019
sub_obj_2_2_irex_media_engagement
sub_obj_2_2_irex_media_engagement %>% glimpse()
sub_obj_2_2_irex_media_engagement %>% nrow() # 403
sub_obj_2_2_irex_media_engagement %>% ncol() # 6

sub_obj_2_2_irex_media_engagement %>% count(country_name) # 21
sub_obj_2_2_irex_media_engagement %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_2_2_irex_media_engagement %>% arrange(values) %>% distinct(country_name)
sub_obj_2_2_irex_media_engagement %>% skim()

# inspect country names
sub_obj_2_2_irex_media_engagement %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name)
sub_obj_2_2_irex_media_engagement %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_2_2_irex_media_engagement <- sub_obj_2_2_irex_media_engagement %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_2_2_irex_media_engagement",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////////

sub_obj_2_2_irex_media_engagement <- sub_obj_2_2_irex_media_engagement %>% 
  mutate(values = case_when(
    mcp_grouping == "EU-15" & year == 2020 ~ 27,
    mcp_grouping == "EU-15" & year == 2021 ~ 25,
    mcp_grouping == "EU-15" & year == 2022 ~ 28,
    mcp_grouping == "EU-15" & year == 2023 ~ 28,
    TRUE ~ values
  ))
#/////////////////


# inspect
sub_obj_2_2_irex_media_engagement
sub_obj_2_2_irex_media_engagement %>% glimpse()
sub_obj_2_2_irex_media_engagement %>% nrow() # 945
sub_obj_2_2_irex_media_engagement %>% ncol() # 35

# check country/year
sub_obj_2_2_irex_media_engagement %>% count(country) %>% print(n = nrow(.))
sub_obj_2_2_irex_media_engagement %>% distinct(country) %>% nrow() # 45
sub_obj_2_2_irex_media_engagement %>% count(year)
sub_obj_2_2_irex_media_engagement %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))


#/////////////////


# Missing MSI 2001-2018 for US, EU-15, Czechia, Estonia, Hungary, Latvia, Lithuania, Poland, Slovakia, Slovenia, and
# Turkmenistan (2001-2007)
# Missing VIBE 2020 for US, EU-15, CARs, and all graduates except Montenegro
# Missing 2019 for all countries because of gap between MSI ending in 2018 and VIBE beginning in 2020
# will impute 2019 as the midpoint between 2018 and 2020 values
sub_obj_2_2_irex_media_engagement %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_2_2_irex_media_engagement %>% filter(is.na(values), mcp_grouping == "EU-15") %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))

# check missing for CARs
sub_obj_2_2_irex_media_engagement %>% filter(is.na(values), mcp_grouping == "CARs") %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_2_2_irex_media_engagement %>% filter(is.na(values), mcp_grouping == "CARs") %>% count(country, year) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))

# check missing for graduates
sub_obj_2_2_irex_media_engagement %>% filter(is.na(values), mcp_grouping == "E&E graduates") %>% 
  count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_2_2_irex_media_engagement %>% filter(is.na(values), mcp_grouping == "E&E graduates") %>% 
  count(country, year) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))

# check missing for ee_presence
sub_obj_2_2_irex_media_engagement %>% filter(is.na(values), 
                                             mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
  count(country, year) %>% print(n = nrow(.))

# skim values
sub_obj_2_2_irex_media_engagement %>% skim(values)
sub_obj_2_2_irex_media_engagement %>% group_by(year) %>% skim(values)
sub_obj_2_2_irex_media_engagement %>% group_by(country) %>% skim(values)


#/////////////////////


# plot
sub_obj_2_2_irex_media_engagement %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  filter(year >= 2010) %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) +
  scale_x_continuous(breaks = seq(from = 2010, to = 2020, by = 1))


#//////////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_2_2_irex_media_engagement %>% write_csv(file = "data/fmir/sub_obj_2_2_irex_media_engagement.csv")
sub_obj_2_2_irex_media_engagement <- read_csv(file = "data/fmir/sub_obj_2_2_irex_media_engagement.csv")



#rest of processing for sub_obj_2_3_irex_information_flows

# inspect
# note msi has 20 countries for 2001-2007, then 21 countries for 2008-2019
sub_obj_2_3_irex_information_flows
sub_obj_2_3_irex_information_flows %>% glimpse()
sub_obj_2_3_irex_information_flows %>% nrow() # 403
sub_obj_2_3_irex_information_flows %>% ncol() # 6

sub_obj_2_3_irex_information_flows %>% count(country_name) # 21
sub_obj_2_3_irex_information_flows %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_2_3_irex_information_flows %>% arrange(values) %>% distinct(country_name)
sub_obj_2_3_irex_information_flows %>% skim()

# inspect country names
sub_obj_2_3_irex_information_flows %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name)
sub_obj_2_3_irex_information_flows %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_2_3_irex_information_flows <- sub_obj_2_3_irex_information_flows %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_2_3_irex_information_flows",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////////


# inspect
sub_obj_2_3_irex_information_flows
sub_obj_2_3_irex_information_flows %>% glimpse()
sub_obj_2_3_irex_information_flows %>% nrow() # 945
sub_obj_2_3_irex_information_flows %>% ncol() # 35

# check country/year
sub_obj_2_3_irex_information_flows %>% count(country) %>% print(n = nrow(.))
sub_obj_2_3_irex_information_flows %>% distinct(country) %>% nrow() # 45
sub_obj_2_3_irex_information_flows %>% count(year)
sub_obj_2_3_irex_information_flows %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))


#/////////////////


# Missing MSI 2001-2018 for US, EU-15, Czechia, Estonia, Hungary, Latvia, Lithuania, Poland, Slovakia, Slovenia, and
# Turkmenistan (2001-2007)
# Missing VIBE 2020 for US, EU-15, CARs, and all graduates except Montenegro
# Missing 2019 for all countries because of gap between MSI ending in 2018 and VIBE beginning in 2020
# will impute 2019 as the midpoint between 2018 and 2020 values
sub_obj_2_3_irex_information_flows %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_2_3_irex_information_flows %>% filter(is.na(values), mcp_grouping == "EU-15") %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))

# check missing for CARs
sub_obj_2_3_irex_information_flows %>% filter(is.na(values), mcp_grouping == "CARs") %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_2_3_irex_information_flows %>% filter(is.na(values), mcp_grouping == "CARs") %>% count(country, year) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))

# check missing for graduates
sub_obj_2_3_irex_information_flows %>% filter(is.na(values), mcp_grouping == "E&E graduates") %>% 
  count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_2_3_irex_information_flows %>% filter(is.na(values), mcp_grouping == "E&E graduates") %>% 
  count(country, year) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))

# check missing for ee_presence
sub_obj_2_3_irex_information_flows %>% filter(is.na(values), 
                                              mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
  count(country, year) %>% print(n = nrow(.))

# skim values
sub_obj_2_3_irex_information_flows %>% skim(values)
sub_obj_2_3_irex_information_flows %>% group_by(year) %>% skim(values)
sub_obj_2_3_irex_information_flows %>% group_by(country) %>% skim(values)


#/////////////////////


# plot
sub_obj_2_3_irex_information_flows %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  filter(year >= 2010) %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) +
  scale_x_continuous(breaks = seq(from = 2010, to = 2020, by = 1))


#//////////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_2_3_irex_information_flows %>% write_csv(file = "data/fmir/sub_obj_2_3_irex_information_flows.csv")
sub_obj_2_3_irex_information_flows <- read_csv(file = "data/fmir/sub_obj_2_3_irex_information_flows.csv")



#rest of processing for sub_obj_2_3_irex_information_use

# inspect
# note msi has 20 countries for 2001-2007, then 21 countries for 2008-2019
sub_obj_2_3_irex_information_use
sub_obj_2_3_irex_information_use %>% glimpse()
sub_obj_2_3_irex_information_use %>% nrow() # 403
sub_obj_2_3_irex_information_use %>% ncol() # 6

sub_obj_2_3_irex_information_use %>% count(country_name) # 21
sub_obj_2_3_irex_information_use %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_2_3_irex_information_use %>% arrange(values) %>% distinct(country_name)
sub_obj_2_3_irex_information_use %>% skim()

# inspect country names
sub_obj_2_3_irex_information_use %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name)
sub_obj_2_3_irex_information_use %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_2_3_irex_information_use <- sub_obj_2_3_irex_information_use %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_2_3_irex_information_use",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////////


# inspect
sub_obj_2_3_irex_information_use
sub_obj_2_3_irex_information_use %>% glimpse()
sub_obj_2_3_irex_information_use %>% nrow() # 945
sub_obj_2_3_irex_information_use %>% ncol() # 35

# check country/year
sub_obj_2_3_irex_information_use %>% count(country) %>% print(n = nrow(.))
sub_obj_2_3_irex_information_use %>% distinct(country) %>% nrow() # 45
sub_obj_2_3_irex_information_use %>% count(year)
sub_obj_2_3_irex_information_use %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))


#/////////////////


# Missing MSI 2001-2018 for US, EU-15, Czechia, Estonia, Hungary, Latvia, Lithuania, Poland, Slovakia, Slovenia, and
# Turkmenistan (2001-2007)
# Missing VIBE 2020 for US, EU-15, CARs, and all graduates except Montenegro
# Missing 2019 for all countries because of gap between MSI ending in 2018 and VIBE beginning in 2020
# will impute 2019 as the midpoint between 2018 and 2020 values
sub_obj_2_3_irex_information_use %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_2_3_irex_information_use %>% filter(is.na(values), mcp_grouping == "EU-15") %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))

# check missing for CARs
sub_obj_2_3_irex_information_use %>% filter(is.na(values), mcp_grouping == "CARs") %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_2_3_irex_information_use %>% filter(is.na(values), mcp_grouping == "CARs") %>% count(country, year) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))

# check missing for graduates
sub_obj_2_3_irex_information_use %>% filter(is.na(values), mcp_grouping == "E&E graduates") %>% 
  count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_2_3_irex_information_use %>% filter(is.na(values), mcp_grouping == "E&E graduates") %>% 
  count(country, year) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))

# check missing for ee_presence
sub_obj_2_3_irex_information_use %>% filter(is.na(values), 
                                            mcp_grouping %in% c("E&E Balkans", "E&E Eurasia")) %>%
  count(country, year) %>% print(n = nrow(.))

# skim values
sub_obj_2_3_irex_information_use %>% skim(values)
sub_obj_2_3_irex_information_use %>% group_by(year) %>% skim(values)
sub_obj_2_3_irex_information_use %>% group_by(country) %>% skim(values)


#/////////////////////


# plot
sub_obj_2_3_irex_information_use %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  filter(year >= 2010) %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) +
  scale_x_continuous(breaks = seq(from = 2010, to = 2020, by = 1))


#//////////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_2_3_irex_information_use %>% write_csv(file = "data/fmir/sub_obj_2_3_irex_information_use.csv")
sub_obj_2_3_irex_information_use <- read_csv(file = "data/fmir/sub_obj_2_3_irex_information_use.csv")

# clean this space
rm(IREX_data)
rm(VIBE_data)
rm(MSI_data)
rm(VIBE_2023)
rm(VIBE_2022)
rm(VIBE_2021)
rm(VIBE_2020)
rm(columns_to_remove)
