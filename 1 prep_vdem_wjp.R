extrafont::loadfonts(device="win") 
library(tidyverse)
library(lubridate)
library(readxl)
library(skimr)
library(rlang)
library(haven)
library(fs)
library(officer)
library(janitor)
library(ggridges)
library(devEMF)
library(ggrepel)
library(rvest)
library(scales)
library(testthat)
library(patchwork)
library(vdemdata)
library(naniar)
library(corrr)
library(GGally)
library(ggcorrplot)
library(viridis)
library(bazar)
library(openxlsx)
library(tibble)
library(scales)
library(tidyverse)
library(WDI)
library(zoo)
library(data.table)


setwd("C:/Users/sdupre/Desktop/usaid/mcp/malign_influence")
options(scipen = 999)


#//////////////////////////////////////


# load add_group_index()
current_wd <- getwd()
setwd("C:/Users/sdupre/Desktop/usaid/mcp/code/assorted_helper_scripts")
source("add_group_index.R")
setwd(current_wd)


#//////////////////////////////////////////////////////////////////////////////////////////////////////

# create custom color_palette ####
color_palette <- tibble(hex = c("#083D7F", "#2474B6", "#8BBFD0",
                                "#CBCBCB", "#7D7D7D",
                                "#99ba78", "#35B779FF", "#006629", 
                                "#E4DC68", "#FDA159", "#EF6712", "#CE1B1E",
                                "#8B008B", "#DA70D6"))
color_palette
color_palette %>% pull(hex) %>% show_col()

# color_palette supports 11 colors, plus possible extensions via fill/line type
show_col(color_palette %>% slice(1, 3) %>% pull(hex)) # 2 colors
show_col(color_palette %>% slice(1, 2, 3) %>% pull(hex)) # 3 colors
show_col(color_palette %>% slice(1, 2, 3, 4) %>% pull(hex)) # 4 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5) %>% pull(hex)) # 5 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6) %>% pull(hex)) # 6 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6, 7) %>% pull(hex)) # 7 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6, 7, 8) %>% pull(hex)) # 8 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6, 7, 8, 9) %>% pull(hex)) # 9 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) %>% pull(hex)) # 10 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) %>% pull(hex)) # 11 colors


#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////


# read country_crosswalk and get country_crosswalk_expanded ####
# state_independent_flag refers to being an independent nation state
# ee_region_flag refers to being an E&E country
current_wd <- getwd()
setwd("C:/Users/sdupre/Desktop/usaid/mcp/useful_info/country_crosswalk")
country_crosswalk <- read_csv("country_crosswalk.csv", lazy = FALSE)
setwd(current_wd)


#/////////////////


# inspect
country_crosswalk
country_crosswalk %>% glimpse()
country_crosswalk %>% nrow() # 219
country_crosswalk %>% ncol() # 14
country_crosswalk %>% count(country) %>% arrange(desc(n))


#//////////////////////////////////////////////////////////////////////////////////////////////////


# get country_crosswalk_expanded to have a country record for each year
country_crosswalk_expanded <- expand_grid(country = country_crosswalk %>% pull(country), 
                                          year = seq(from = 2018, to = 2023, by = 1)) %>%
  left_join(., country_crosswalk, by = "country")


#/////////////////

# inspect
country_crosswalk_expanded
country_crosswalk_expanded %>% glimpse()
country_crosswalk_expanded %>% nrow() # 1533
country_crosswalk_expanded %>% ncol() # 19
country_crosswalk_expanded %>% count(country) %>% distinct(n) #7
country_crosswalk_expanded %>% skim()

country_crosswalk %>% glimpse()
country_crosswalk %>% print(n = nrow(.))
country_crosswalk %>% skim()
country_crosswalk %>% count(mcp_grouping)


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# read in fmir_framework ####
fmir_framework <- read_excel(path = "data/fmir/framework/foreign_malign_influence_resilience_framework_20250123.xlsx",
                             sheet = "Sheet1", skip = 4) %>%
  select(-c("Aspect")) %>%
  rename(obj = Objective,
         obj_short_name = `Objective short name`,
         sub_obj = `Sub-objective`,
         sub_obj_short_name = `Sub-objective short name`,
         concept = Concept,
         indicator = Indicator,
         indicator_name = `Indicator name`,
         description = Description#,
         # data_documentation = `Data documentation`,
         # data_source = `Data source`,
         # notes = Notes
         ) %>%
  mutate(#note_flag = case_when(is.na(notes) ~ 0, TRUE ~ 1),
         obj_num = case_when(str_detect(string = obj, pattern = "Objective 1:") ~ "obj_1",
                             str_detect(string = obj, pattern = "Objective 2:") ~ "obj_2",
                             str_detect(string = obj, pattern = "Objective 3:") ~ "obj_3",
                             str_detect(string = obj, pattern = "Objective 4:") ~ "obj_4",
                             str_detect(string = obj, pattern = "Associated Objective: Foreign Relations Resilience") ~ "obj_fr"), #was obj_c
         sub_obj_num = case_when(str_detect(string = sub_obj, pattern = "Sub-objective 1.1:") ~ "sub_obj_1_1",
                                 str_detect(string = sub_obj, pattern = "Sub-objective 1.2:") ~ "sub_obj_1_2",
                                 str_detect(string = sub_obj, pattern = "Sub-objective 1.3:") ~ "sub_obj_1_3",
                                 str_detect(string = sub_obj, pattern = "Sub-objective 2.1:") ~ "sub_obj_2_1",
                                 str_detect(string = sub_obj, pattern = "Sub-objective 2.2:") ~ "sub_obj_2_2",
                                 str_detect(string = sub_obj, pattern = "Sub-objective 2.3:") ~ "sub_obj_2_3",
                                 str_detect(string = sub_obj, pattern = "Sub-objective 3.1:") ~ "sub_obj_3_1",
                                 str_detect(string = sub_obj, pattern = "Sub-objective 3.2:") ~ "sub_obj_3_2",
                                 str_detect(string = sub_obj, pattern = "Sub-objective 4.1:") ~ "sub_obj_4_1",
                                 str_detect(string = sub_obj, pattern = "Sub-objective 4.2:") ~ "sub_obj_4_2",
                                 str_detect(string = sub_obj, pattern = "Sub-objective 4.3:") ~ "sub_obj_4_3",
                                 str_detect(string = sub_obj, pattern = "Sub-objective FR.1:") ~ "sub_obj_FR_1",
                                 str_detect(string = sub_obj, pattern = "Sub-objective FR.2:") ~ "sub_obj_FR_2",
                                 str_detect(string = sub_obj, pattern = "Sub-objective FR.3:") ~ "sub_obj_FR_3",
                                 str_detect(string = sub_obj, pattern = "Sub-objective FR.4:") ~ "sub_obj_FR_4",
                                 TRUE ~ NA_character_)) %>%
  relocate(obj_num) %>% relocate(sub_obj_num, .before = sub_obj) %>% arrange(sub_obj_num) %>%
  filter(!(is.na(obj)))


# inspect
fmir_framework
fmir_framework %>% glimpse()
fmir_framework %>% nrow() # 69 same as in v1
fmir_framework %>% ncol() # 19

fmir_framework %>% count(obj, obj_num)
fmir_framework %>% count(sub_obj, sub_obj_num) %>% print(n=30)
fmir_framework %>% count(sub_obj_short_name, concept) %>% print(n=60)
fmir_framework %>% count(indicator_name, indicator) %>% print(n = nrow(.))
fmir_framework %>% count(indicator_name, concept) %>% print(n = nrow(.))

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# load sub_obj_1_1_vdem_judicial_constraints_on_exec ####
sub_obj_1_1_vdem_judicial_constraints_on_exec <- vdem %>% select(country_name, year, v2x_jucon) %>%
  filter(year >= 2018) %>%
  rename(sub_obj_1_1_vdem_judicial_constraints_on_exec = "v2x_jucon") %>%
  pivot_longer(cols = sub_obj_1_1_vdem_judicial_constraints_on_exec, names_to = "indicator_name", values_to = "values") %>%
  mutate(high_value_is_good_outcome_flag = 1,
         indicator_name = "sub_obj_1_1_vdem_judicial_constraints_on_exec",
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "Burma/Myanmar" ~ "Burma",
                                  country_name == "Cape Verde" ~ "Cabo Verde",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Democratic Republic of the Congo" ~ "Congo (Kinshasa)",
                                  country_name == "Hong Kong" ~ "Hong Kong SAR, China",
                                  country_name == "Ivory Coast" ~ "Cote d'Ivoire",
                                  country_name == "North Korea" ~ "Korea, North",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Palestine/West Bank" ~ "West Bank and Gaza",
                                  country_name == "Papal States" ~ "Holy See",
                                  country_name == "Republic of the Congo" ~ "Congo (Brazzaville)",
                                  country_name == "Republic of Vietnam" ~ "Vietnam",
                                  country_name == "South Korea" ~ "Korea, South",
                                  country_name == "The Gambia" ~ "Gambia, The",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States of America" ~ "U.S.",
                                  TRUE ~ country_name)) %>%
  pivot_wider(names_from = year, values_from = values) %>%
  pivot_longer(cols = -c(country_name, high_value_is_good_outcome_flag, indicator_name), 
               names_to = "year", values_to = "values") %>%
  mutate(year = as.numeric(year))


#/////////////////


# inspect
sub_obj_1_1_vdem_judicial_constraints_on_exec
sub_obj_1_1_vdem_judicial_constraints_on_exec %>% glimpse()
sub_obj_1_1_vdem_judicial_constraints_on_exec %>% nrow() # 1074
sub_obj_1_1_vdem_judicial_constraints_on_exec %>% ncol() # 5

var_info("v2x_jucon")
sub_obj_1_1_vdem_judicial_constraints_on_exec %>% arrange(values) %>% distinct(country_name)
sub_obj_1_1_vdem_judicial_constraints_on_exec %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_1_1_vdem_judicial_constraints_on_exec %>% skim()

# inspect country names
sub_obj_1_1_vdem_judicial_constraints_on_exec %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name)
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_1_1_vdem_judicial_constraints_on_exec, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country)

sub_obj_1_1_vdem_judicial_constraints_on_exec %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_1_1_vdem_judicial_constraints_on_exec <- sub_obj_1_1_vdem_judicial_constraints_on_exec %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  left_join(., fmir_framework, by = "indicator_name") 


#/////////////////


# inspect
sub_obj_1_1_vdem_judicial_constraints_on_exec
sub_obj_1_1_vdem_judicial_constraints_on_exec %>% glimpse()
sub_obj_1_1_vdem_judicial_constraints_on_exec %>% nrow() # 315
sub_obj_1_1_vdem_judicial_constraints_on_exec %>% ncol() # 39

# check country/year
sub_obj_1_1_vdem_judicial_constraints_on_exec %>% distinct(country) %>% nrow() # 45
sub_obj_1_1_vdem_judicial_constraints_on_exec %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_1_1_vdem_judicial_constraints_on_exec %>% count(indicator_name) # 900 (7 years * 45 countries = 315)
sub_obj_1_1_vdem_judicial_constraints_on_exec %>% count(year) %>% print(n = nrow(.))

# check values
sub_obj_1_1_vdem_judicial_constraints_on_exec %>% 
  filter(indicator_name == "sub_obj_1_1_vdem_judicial_constraints_on_exec") %>% 
  skim(values)
sub_obj_1_1_vdem_judicial_constraints_on_exec %>% group_by(year) %>% skim(values)


# plot
sub_obj_1_1_vdem_judicial_constraints_on_exec %>% 
  # filter(mcp_grouping == "E&E Balkans") %>%
  filter(mcp_grouping == "E&E Eurasia") %>%
  # filter(mcp_grouping == "E&E graduates") %>%
  # filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# inspect summary stats on indicators
sub_obj_1_1_vdem_judicial_constraints_on_exec %>% group_by(indicator_name) %>% 
  mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
         missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
         missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
  distinct(indicator_name, year_start, year_end, missing_pct)


#/////////////////


# read/write
sub_obj_1_1_vdem_judicial_constraints_on_exec %>% write_csv(file = "data/fmir/sub_obj_1_1_vdem_judicial_constraints_on_exec.csv")
sub_obj_1_1_vdem_judicial_constraints_on_exec <- read_csv(file = "data/fmir/sub_obj_1_1_vdem_judicial_constraints_on_exec.csv", 
                                                          lazy = FALSE)




#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////

# load sub_obj_1_1_vdem_legislative_constraints_on_exec ####
sub_obj_1_1_vdem_legislative_constraints_on_exec <- vdem %>% select(country_name, year, v2xlg_legcon) %>%
  filter(year >= 2018) %>%
  rename(sub_obj_1_1_vdem_legislative_constraints_on_exec = "v2xlg_legcon") %>%
  pivot_longer(cols = sub_obj_1_1_vdem_legislative_constraints_on_exec, names_to = "indicator_name", values_to = "values") %>%
  mutate(high_value_is_good_outcome_flag = 1,
         indicator_name = "sub_obj_1_1_vdem_legislative_constraints_on_exec",
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "Burma/Myanmar" ~ "Burma",
                                  country_name == "Cape Verde" ~ "Cabo Verde",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Democratic Republic of the Congo" ~ "Congo (Kinshasa)",
                                  country_name == "Hong Kong" ~ "Hong Kong SAR, China",
                                  country_name == "Ivory Coast" ~ "Cote d'Ivoire",
                                  country_name == "North Korea" ~ "Korea, North",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Palestine/West Bank" ~ "West Bank and Gaza",
                                  country_name == "Papal States" ~ "Holy See",
                                  country_name == "Republic of the Congo" ~ "Congo (Brazzaville)",
                                  country_name == "Republic of Vietnam" ~ "Vietnam",
                                  country_name == "South Korea" ~ "Korea, South",
                                  country_name == "The Gambia" ~ "Gambia, The",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States of America" ~ "U.S.",
                                  TRUE ~ country_name)) %>%
  pivot_wider(names_from = year, values_from = values) %>%
  pivot_longer(cols = -c(country_name, high_value_is_good_outcome_flag, indicator_name), 
               names_to = "year", values_to = "values") %>%
  mutate(year = as.numeric(year))


#/////////////////


# inspect
sub_obj_1_1_vdem_legislative_constraints_on_exec
sub_obj_1_1_vdem_legislative_constraints_on_exec %>% glimpse()
sub_obj_1_1_vdem_legislative_constraints_on_exec %>% nrow() # 1253
sub_obj_1_1_vdem_legislative_constraints_on_exec %>% ncol() # 5

var_info("v2xlg_legcon")
sub_obj_1_1_vdem_legislative_constraints_on_exec %>% arrange(values) %>% distinct(country_name)
sub_obj_1_1_vdem_legislative_constraints_on_exec %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_1_1_vdem_legislative_constraints_on_exec %>% skim()

# inspect country names
sub_obj_1_1_vdem_legislative_constraints_on_exec %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name)
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_1_1_vdem_legislative_constraints_on_exec, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country)

sub_obj_1_1_vdem_legislative_constraints_on_exec %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_1_1_vdem_legislative_constraints_on_exec <- sub_obj_1_1_vdem_legislative_constraints_on_exec %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  left_join(., fmir_framework, by = "indicator_name") 


#/////////////////


# inspect
sub_obj_1_1_vdem_legislative_constraints_on_exec
sub_obj_1_1_vdem_legislative_constraints_on_exec %>% glimpse()
sub_obj_1_1_vdem_legislative_constraints_on_exec %>% nrow() # 315
sub_obj_1_1_vdem_legislative_constraints_on_exec %>% ncol() # 36

# check country/year
sub_obj_1_1_vdem_legislative_constraints_on_exec %>% distinct(country) %>% nrow() # 45
sub_obj_1_1_vdem_legislative_constraints_on_exec %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_1_1_vdem_legislative_constraints_on_exec %>% count(indicator_name) # 900 (7 years * 45 countries = 315)
sub_obj_1_1_vdem_legislative_constraints_on_exec %>% count(year) %>% print(n = nrow(.))

# check values
sub_obj_1_1_vdem_legislative_constraints_on_exec %>% 
  filter(indicator_name == "sub_obj_1_1_vdem_legislative_constraints_on_exec") %>% 
  skim(values)
sub_obj_1_1_vdem_legislative_constraints_on_exec %>% group_by(year) %>% skim(values)


# plot
sub_obj_1_1_vdem_legislative_constraints_on_exec %>% 
  # filter(mcp_grouping == "E&E Balkans") %>%
  filter(mcp_grouping == "E&E Eurasia") %>%
  # filter(mcp_grouping == "E&E graduates") %>%
  # filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# inspect summary stats on indicators
sub_obj_1_1_vdem_legislative_constraints_on_exec %>% group_by(indicator_name) %>% 
  mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
         missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
         missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
  distinct(indicator_name, year_start, year_end, missing_pct)


#/////////////////


# read/write
sub_obj_1_1_vdem_legislative_constraints_on_exec %>% write_csv(file = "data/fmir/sub_obj_1_1_vdem_legislative_constraints_on_exec.csv")
sub_obj_1_1_vdem_legislative_constraints_on_exec <- read_csv(file = "data/fmir/sub_obj_1_1_vdem_legislative_constraints_on_exec.csv", 
                                                          lazy = FALSE)




#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////

# load sub_obj_1_1_wjp_govt_auditing ####

#Note, 2017/2018 are combined in this set. 2019-2023 are individual year. Treating 2017/2018 as 2018 for this import.
sub_obj_1_1_wjp_govt_auditing <- read_excel(path = "data/WJP/RoLI/2023_wjp_rule_of_law_index_HISTORICAL_DATA.xlsx", sheet = "Historical Data") %>%
  filter(as.numeric(Year) > 2017| Year == "2017-2018") %>%
  mutate(
    Year = case_when(
      Year == "2017-2018" ~ as.numeric(2018),
      TRUE ~ as.numeric(Year)
    )
  ) %>%
  select(c("Year", 
           "Country Code", 
           "1.3 Government powers are effectively limited by independent auditing and review")) %>%
  rename(iso3 = `Country Code`,
         year = Year,
         values = `1.3 Government powers are effectively limited by independent auditing and review`) %>%
  mutate(high_value_is_good_outcome_flag = 1,
         indicator_name = "sub_obj_1_1_wjp_govt_auditing") %>%
  pivot_wider(names_from = year, values_from = values) %>%
  pivot_longer(cols = -c(iso3, high_value_is_good_outcome_flag, indicator_name), 
               names_to = "year", values_to = "values") %>%
  mutate(year = as.numeric(year))


#/////////////////


# inspect
sub_obj_1_1_wjp_govt_auditing
sub_obj_1_1_wjp_govt_auditing %>% glimpse()
sub_obj_1_1_wjp_govt_auditing %>% nrow() # 994
sub_obj_1_1_wjp_govt_auditing %>% ncol() # 5

sub_obj_1_1_wjp_govt_auditing %>% arrange(values) %>% distinct(iso3)
sub_obj_1_1_wjp_govt_auditing %>% arrange(desc(values)) %>% distinct(iso3)
sub_obj_1_1_wjp_govt_auditing %>% skim()

#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_1_1_wjp_govt_auditing <- sub_obj_1_1_wjp_govt_auditing %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("iso3" = "iso3", "year" = "year")) %>%
  mutate(high_value_is_good_outcome_flag = 1,
         indicator_name = "sub_obj_1_1_wjp_govt_auditing") %>%
  left_join(., fmir_framework, by = "indicator_name") 

#/////////////////


# inspect
sub_obj_1_1_wjp_govt_auditing
sub_obj_1_1_wjp_govt_auditing %>% glimpse()
sub_obj_1_1_wjp_govt_auditing %>% nrow() # 315
sub_obj_1_1_wjp_govt_auditing %>% ncol() # 39

# check country/year
sub_obj_1_1_wjp_govt_auditing %>% distinct(country) %>% nrow() # 45
sub_obj_1_1_wjp_govt_auditing %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_1_1_wjp_govt_auditing %>% count(indicator_name) # 900 (7 years * 45 countries = 315)
sub_obj_1_1_wjp_govt_auditing %>% count(year) %>% print(n = nrow(.))

# check values
sub_obj_1_1_wjp_govt_auditing %>% 
  filter(indicator_name == "sub_obj_1_1_wjp_govt_auditing") %>% 
  skim(values)
sub_obj_1_1_wjp_govt_auditing %>% group_by(year) %>% skim(values)


# plot
sub_obj_1_1_wjp_govt_auditing %>% 
  # filter(mcp_grouping == "E&E Balkans") %>%
  filter(mcp_grouping == "E&E Eurasia") %>%
  # filter(mcp_grouping == "E&E graduates") %>%
  # filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# inspect summary stats on indicators
sub_obj_1_1_wjp_govt_auditing %>% group_by(indicator_name) %>% 
  mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
         missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
         missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
  distinct(indicator_name, year_start, year_end, missing_pct)


#/////////////////


# read/write
sub_obj_1_1_wjp_govt_auditing %>% write_csv(file = "data/fmir/sub_obj_1_1_wjp_govt_auditing.csv")
sub_obj_1_1_wjp_govt_auditing <- read_csv(file = "data/fmir/sub_obj_1_1_wjp_govt_auditing.csv", 
                                          lazy = FALSE)  

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////

# load sub_obj_1_1_wjp_officials_sanctioned ####

#Note, 2017/2018 are combined in this set. 2019-2023 are individual year. Treating 2017/2018 as 2018 for this import.
sub_obj_1_1_wjp_officials_sanctioned <- read_excel(path = "data/WJP/RoLI/2023_wjp_rule_of_law_index_HISTORICAL_DATA.xlsx", sheet = "Historical Data") %>%
  filter(as.numeric(Year) > 2017| Year == "2017-2018") %>%
  mutate(
    Year = case_when(
      Year == "2017-2018" ~ as.numeric(2018),
      TRUE ~ as.numeric(Year)
    )
  ) %>%
  select(c("Year", 
           "Country Code", 
           "1.4 Government officials are sanctioned for misconduct")) %>%
  rename(iso3 = `Country Code`,
         year = Year,
         values = `1.4 Government officials are sanctioned for misconduct`) %>%
  mutate(high_value_is_good_outcome_flag = 1,
         indicator_name = "sub_obj_1_1_wjp_officials_sanctioned") %>%
  pivot_wider(names_from = year, values_from = values) %>%
  pivot_longer(cols = -c(iso3, high_value_is_good_outcome_flag, indicator_name), 
               names_to = "year", values_to = "values") %>%
  mutate(year = as.numeric(year))


#/////////////////


# inspect
sub_obj_1_1_wjp_officials_sanctioned
sub_obj_1_1_wjp_officials_sanctioned %>% glimpse()
sub_obj_1_1_wjp_officials_sanctioned %>% nrow() # 994
sub_obj_1_1_wjp_officials_sanctioned %>% ncol() # 5

sub_obj_1_1_wjp_officials_sanctioned %>% arrange(values) %>% distinct(iso3)
sub_obj_1_1_wjp_officials_sanctioned %>% arrange(desc(values)) %>% distinct(iso3)
sub_obj_1_1_wjp_officials_sanctioned %>% skim()

#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_1_1_wjp_officials_sanctioned <- sub_obj_1_1_wjp_officials_sanctioned %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("iso3" = "iso3", "year" = "year")) %>%
  mutate(high_value_is_good_outcome_flag = 1,
         indicator_name = "sub_obj_1_1_wjp_officials_sanctioned") %>%
  left_join(., fmir_framework, by = "indicator_name") 

#/////////////////


# inspect
sub_obj_1_1_wjp_officials_sanctioned
sub_obj_1_1_wjp_officials_sanctioned %>% glimpse()
sub_obj_1_1_wjp_officials_sanctioned %>% nrow() # 315
sub_obj_1_1_wjp_officials_sanctioned %>% ncol() # 39

# check country/year
sub_obj_1_1_wjp_officials_sanctioned %>% distinct(country) %>% nrow() # 45
sub_obj_1_1_wjp_officials_sanctioned %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_1_1_wjp_officials_sanctioned %>% count(indicator_name) # 900 (7 years * 45 countries = 315)
sub_obj_1_1_wjp_officials_sanctioned %>% count(year) %>% print(n = nrow(.))

# check values
sub_obj_1_1_wjp_officials_sanctioned %>% 
  filter(indicator_name == "sub_obj_1_1_wjp_officials_sanctioned") %>% 
  skim(values)
sub_obj_1_1_wjp_officials_sanctioned %>% group_by(year) %>% skim(values)


# plot
sub_obj_1_1_wjp_officials_sanctioned %>% 
  # filter(mcp_grouping == "E&E Balkans") %>%
  filter(mcp_grouping == "E&E Eurasia") %>%
  # filter(mcp_grouping == "E&E graduates") %>%
  # filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# inspect summary stats on indicators
sub_obj_1_1_wjp_officials_sanctioned %>% group_by(indicator_name) %>% 
  mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
         missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
         missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
  distinct(indicator_name, year_start, year_end, missing_pct)


#/////////////////


# read/write
sub_obj_1_1_wjp_officials_sanctioned %>% write_csv(file = "data/fmir/sub_obj_1_1_wjp_officials_sanctioned.csv")
sub_obj_1_1_wjp_officials_sanctioned <- read_csv(file = "data/fmir/sub_obj_1_1_wjp_officials_sanctioned.csv", 
                                                 lazy = FALSE)  



#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# load sub_obj_1_1_vdem_rule_of_law ####
sub_obj_1_1_vdem_rule_of_law <- vdem %>% select(country_name, year, v2x_rule) %>%
  filter(year >= 2018) %>%
  rename(sub_obj_1_1_vdem_rule_of_law = "v2x_rule") %>%
  pivot_longer(cols = sub_obj_1_1_vdem_rule_of_law, names_to = "indicator_name", values_to = "values") %>%
  mutate(high_value_is_good_outcome_flag = 1,
         indicator_name = "sub_obj_1_1_vdem_rule_of_law",
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "Burma/Myanmar" ~ "Burma",
                                  country_name == "Cape Verde" ~ "Cabo Verde",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Democratic Republic of the Congo" ~ "Congo (Kinshasa)",
                                  country_name == "Hong Kong" ~ "Hong Kong SAR, China",
                                  country_name == "Ivory Coast" ~ "Cote d'Ivoire",
                                  country_name == "North Korea" ~ "Korea, North",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Palestine/West Bank" ~ "West Bank and Gaza",
                                  country_name == "Papal States" ~ "Holy See",
                                  country_name == "Republic of the Congo" ~ "Congo (Brazzaville)",
                                  country_name == "Republic of Vietnam" ~ "Vietnam",
                                  country_name == "South Korea" ~ "Korea, South",
                                  country_name == "The Gambia" ~ "Gambia, The",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States of America" ~ "U.S.",
                                  TRUE ~ country_name)) %>%
  pivot_wider(names_from = year, values_from = values) %>%
  pivot_longer(cols = -c(country_name, high_value_is_good_outcome_flag, indicator_name), 
               names_to = "year", values_to = "values") %>%
  mutate(year = as.numeric(year))


#/////////////////


# inspect
sub_obj_1_1_vdem_rule_of_law
sub_obj_1_1_vdem_rule_of_law %>% glimpse()
sub_obj_1_1_vdem_rule_of_law %>% nrow() # 1074
sub_obj_1_1_vdem_rule_of_law %>% ncol() # 5

var_info("v2x_rule")
sub_obj_1_1_vdem_rule_of_law %>% arrange(values) %>% distinct(country_name)
sub_obj_1_1_vdem_rule_of_law %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_1_1_vdem_rule_of_law %>% skim()

# inspect country names
sub_obj_1_1_vdem_rule_of_law %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name)
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_1_1_vdem_rule_of_law, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country)

sub_obj_1_1_vdem_rule_of_law %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_1_1_vdem_rule_of_law <- sub_obj_1_1_vdem_rule_of_law %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  left_join(., fmir_framework, by = "indicator_name") 


#/////////////////


# inspect
sub_obj_1_1_vdem_rule_of_law
sub_obj_1_1_vdem_rule_of_law %>% glimpse()
sub_obj_1_1_vdem_rule_of_law %>% nrow() # 315
sub_obj_1_1_vdem_rule_of_law %>% ncol() # 39

# check country/year
sub_obj_1_1_vdem_rule_of_law %>% distinct(country) %>% nrow() # 45
sub_obj_1_1_vdem_rule_of_law %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_1_1_vdem_rule_of_law %>% count(indicator_name) # 900 (7 years * 45 countries = 315)
sub_obj_1_1_vdem_rule_of_law %>% count(year) %>% print(n = nrow(.))

# check values
sub_obj_1_1_vdem_rule_of_law %>% 
  filter(indicator_name == "sub_obj_1_1_vdem_rule_of_law") %>% 
  skim(values)
sub_obj_1_1_vdem_rule_of_law %>% group_by(year) %>% skim(values)


# plot
sub_obj_1_1_vdem_rule_of_law %>% 
  # filter(mcp_grouping == "E&E Balkans") %>%
  filter(mcp_grouping == "E&E Eurasia") %>%
  # filter(mcp_grouping == "E&E graduates") %>%
  # filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# inspect summary stats on indicators
sub_obj_1_1_vdem_rule_of_law %>% group_by(indicator_name) %>% 
  mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
         missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
         missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
  distinct(indicator_name, year_start, year_end, missing_pct)


#/////////////////


# read/write
sub_obj_1_1_vdem_rule_of_law %>% write_csv(file = "data/fmir/sub_obj_1_1_vdem_rule_of_law.csv")
sub_obj_1_1_vdem_rule_of_law <- read_csv(file = "data/fmir/sub_obj_1_1_vdem_rule_of_law.csv", 
                                         lazy = FALSE)






# load sub_obj_1_1_vdem_free_elections ####
sub_obj_1_1_vdem_free_elections <- vdem %>% select(country_name, year, v2xel_frefair) %>%
  filter(year >= 2018) %>%
  rename(sub_obj_1_1_vdem_free_elections = "v2xel_frefair") %>%
  pivot_longer(cols = sub_obj_1_1_vdem_free_elections, names_to = "indicator_name", values_to = "values") %>%
  mutate(high_value_is_good_outcome_flag = 1,
         indicator_name = "sub_obj_1_1_vdem_free_elections",
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "Burma/Myanmar" ~ "Burma",
                                  country_name == "Cape Verde" ~ "Cabo Verde",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Democratic Republic of the Congo" ~ "Congo (Kinshasa)",
                                  country_name == "Hong Kong" ~ "Hong Kong SAR, China",
                                  country_name == "Ivory Coast" ~ "Cote d'Ivoire",
                                  country_name == "North Korea" ~ "Korea, North",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Palestine/West Bank" ~ "West Bank and Gaza",
                                  country_name == "Papal States" ~ "Holy See",
                                  country_name == "Republic of the Congo" ~ "Congo (Brazzaville)",
                                  country_name == "Republic of Vietnam" ~ "Vietnam",
                                  country_name == "South Korea" ~ "Korea, South",
                                  country_name == "The Gambia" ~ "Gambia, The",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States of America" ~ "U.S.",
                                  TRUE ~ country_name)) %>%
  pivot_wider(names_from = year, values_from = values) %>%
  pivot_longer(cols = -c(country_name, high_value_is_good_outcome_flag, indicator_name), 
               names_to = "year", values_to = "values") %>%
  mutate(year = as.numeric(year))


#/////////////////


# inspect
sub_obj_1_1_vdem_free_elections
sub_obj_1_1_vdem_free_elections %>% glimpse()
sub_obj_1_1_vdem_free_elections %>% nrow() # 1253
sub_obj_1_1_vdem_free_elections %>% ncol() # 5

var_info("v2x_rule")
sub_obj_1_1_vdem_free_elections %>% arrange(values) %>% distinct(country_name)
sub_obj_1_1_vdem_free_elections %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_1_1_vdem_free_elections %>% skim()

# inspect country names
sub_obj_1_1_vdem_free_elections %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name)
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_1_1_vdem_free_elections, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country)

sub_obj_1_1_vdem_free_elections %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_1_1_vdem_free_elections <- sub_obj_1_1_vdem_free_elections %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  left_join(., fmir_framework, by = "indicator_name") 


#/////////////////


# inspect
sub_obj_1_1_vdem_free_elections
sub_obj_1_1_vdem_free_elections %>% glimpse()
sub_obj_1_1_vdem_free_elections %>% nrow() # 315
sub_obj_1_1_vdem_free_elections %>% ncol() # 39

# check country/year
sub_obj_1_1_vdem_free_elections %>% distinct(country) %>% nrow() # 45
sub_obj_1_1_vdem_free_elections %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_1_1_vdem_free_elections %>% count(indicator_name) # 315 (7 years * 45 countries = 315)
sub_obj_1_1_vdem_free_elections %>% count(year) %>% print(n = nrow(.))

# check values
sub_obj_1_1_vdem_free_elections %>% 
  filter(indicator_name == "sub_obj_1_1_vdem_free_elections") %>% 
  skim(values)
sub_obj_1_1_vdem_free_elections %>% group_by(year) %>% skim(values)


# plot
sub_obj_1_1_vdem_free_elections %>% 
  # filter(mcp_grouping == "E&E Balkans") %>%
  filter(mcp_grouping == "E&E Eurasia") %>%
  # filter(mcp_grouping == "E&E graduates") %>%
  # filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# inspect summary stats on indicators
sub_obj_1_1_vdem_free_elections %>% group_by(indicator_name) %>% 
  mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
         missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
         missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
  distinct(indicator_name, year_start, year_end, missing_pct)


#/////////////////


# read/write
sub_obj_1_1_vdem_free_elections %>% write_csv(file = "data/fmir/sub_obj_1_1_vdem_free_elections.csv")
sub_obj_1_1_vdem_free_elections <- read_csv(file = "data/fmir/sub_obj_1_1_vdem_free_elections.csv", 
                                            lazy = FALSE)








#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# load sub_obj_1_2_vdem_civil_society ####

sub_obj_1_2_vdem_civil_society <- vdem %>% select(country_name, year, v2xcs_ccsi) %>%
  filter(year >= 2018) %>%
  rename(sub_obj_1_2_vdem_civil_society = "v2xcs_ccsi") %>%
  pivot_longer(cols = sub_obj_1_2_vdem_civil_society, names_to = "indicator_name", values_to = "values") %>%
  mutate(high_value_is_good_outcome_flag = 1,
         indicator_name = "sub_obj_1_2_vdem_civil_society",
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "Burma/Myanmar" ~ "Burma",
                                  country_name == "Cape Verde" ~ "Cabo Verde",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Democratic Republic of the Congo" ~ "Congo (Kinshasa)",
                                  country_name == "Hong Kong" ~ "Hong Kong SAR, China",
                                  country_name == "Ivory Coast" ~ "Cote d'Ivoire",
                                  country_name == "North Korea" ~ "Korea, North",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Palestine/West Bank" ~ "West Bank and Gaza",
                                  country_name == "Papal States" ~ "Holy See",
                                  country_name == "Republic of the Congo" ~ "Congo (Brazzaville)",
                                  country_name == "Republic of Vietnam" ~ "Vietnam",
                                  country_name == "South Korea" ~ "Korea, South",
                                  country_name == "The Gambia" ~ "Gambia, The",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States of America" ~ "U.S.",
                                  TRUE ~ country_name)) %>%
  pivot_wider(names_from = year, values_from = values) %>%
  pivot_longer(cols = -c(country_name, high_value_is_good_outcome_flag, indicator_name), 
               names_to = "year", values_to = "values") %>%
  mutate(year = as.numeric(year))


#/////////////////


# inspect
sub_obj_1_2_vdem_civil_society
sub_obj_1_2_vdem_civil_society %>% glimpse()
sub_obj_1_2_vdem_civil_society %>% nrow() # 1253
sub_obj_1_2_vdem_civil_society %>% ncol() # 5

var_info("v2xcs_ccsi")
sub_obj_1_2_vdem_civil_society %>% arrange(values) %>% distinct(country_name)
sub_obj_1_2_vdem_civil_society %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_1_2_vdem_civil_society %>% skim()

# inspect country names
sub_obj_1_2_vdem_civil_society %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name)
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_1_2_vdem_civil_society, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country)

sub_obj_1_2_vdem_civil_society %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_1_2_vdem_civil_society <- sub_obj_1_2_vdem_civil_society %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  left_join(., fmir_framework, by = "indicator_name") 


#/////////////////


# inspect
sub_obj_1_2_vdem_civil_society
sub_obj_1_2_vdem_civil_society %>% glimpse()
sub_obj_1_2_vdem_civil_society %>% nrow() # 315
sub_obj_1_2_vdem_civil_society %>% ncol() # 39

# check country/year
sub_obj_1_2_vdem_civil_society %>% distinct(country) %>% nrow() # 45
sub_obj_1_2_vdem_civil_society %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_1_2_vdem_civil_society %>% count(indicator_name) # 315 (7 years * 45 countries = 315)
sub_obj_1_2_vdem_civil_society %>% count(year) %>% print(n = nrow(.))

# check values
sub_obj_1_2_vdem_civil_society %>% 
  filter(indicator_name == "sub_obj_1_2_vdem_civil_society") %>% 
  skim(values)
sub_obj_1_2_vdem_civil_society %>% group_by(year) %>% skim(values)


# plot
sub_obj_1_2_vdem_civil_society %>% 
  # filter(mcp_grouping == "E&E Balkans") %>%
  filter(mcp_grouping == "E&E Eurasia") %>%
  # filter(mcp_grouping == "E&E graduates") %>%
  # filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# inspect summary stats on indicators
sub_obj_1_2_vdem_civil_society %>% group_by(indicator_name) %>% 
  mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
         missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
         missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
  distinct(indicator_name, year_start, year_end, missing_pct)


#/////////////////


# read/write
sub_obj_1_2_vdem_civil_society %>% write_csv(file = "data/fmir/sub_obj_1_2_vdem_civil_society.csv")
sub_obj_1_2_vdem_civil_society <- read_csv(file = "data/fmir/sub_obj_1_2_vdem_civil_society.csv", 
                                                  lazy = FALSE)


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////

# load sub_obj_1_2_vdem_political_engagement ####
## This one behaves oddly. Little year-to-year variation for some, huge for others. Turkmenistan 
## especially, and Uzbekistan to a lesser degree, have terrible scores


sub_obj_1_2_vdem_political_engagement <- vdem %>% select(country_name, year, v2capolit) %>%
  filter(year >= 2018) %>%
  rename(sub_obj_1_2_vdem_political_engagement = "v2capolit") %>%
  pivot_longer(cols = sub_obj_1_2_vdem_political_engagement, names_to = "indicator_name", values_to = "values") %>%
  mutate(high_value_is_good_outcome_flag = 1,
         indicator_name = "sub_obj_1_2_vdem_political_engagement",
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "Burma/Myanmar" ~ "Burma",
                                  country_name == "Cape Verde" ~ "Cabo Verde",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Democratic Republic of the Congo" ~ "Congo (Kinshasa)",
                                  country_name == "Hong Kong" ~ "Hong Kong SAR, China",
                                  country_name == "Ivory Coast" ~ "Cote d'Ivoire",
                                  country_name == "North Korea" ~ "Korea, North",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Palestine/West Bank" ~ "West Bank and Gaza",
                                  country_name == "Papal States" ~ "Holy See",
                                  country_name == "Republic of the Congo" ~ "Congo (Brazzaville)",
                                  country_name == "Republic of Vietnam" ~ "Vietnam",
                                  country_name == "South Korea" ~ "Korea, South",
                                  country_name == "The Gambia" ~ "Gambia, The",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States of America" ~ "U.S.",
                                  TRUE ~ country_name)) %>%
  pivot_wider(names_from = year, values_from = values) %>%
  pivot_longer(cols = -c(country_name, high_value_is_good_outcome_flag, indicator_name), 
               names_to = "year", values_to = "values") %>%
  mutate(year = as.numeric(year),
         values = case_when(
           country_name == "Armenia" ~ NA,
           TRUE ~ values
         ))


#/////////////////


# inspect
sub_obj_1_2_vdem_political_engagement
sub_obj_1_2_vdem_political_engagement %>% glimpse()
sub_obj_1_2_vdem_political_engagement %>% nrow() # 1253
sub_obj_1_2_vdem_political_engagement %>% ncol() # 5

var_info("v2capolit")
sub_obj_1_2_vdem_political_engagement %>% arrange(values) %>% distinct(country_name)
sub_obj_1_2_vdem_political_engagement %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_1_2_vdem_political_engagement %>% skim()

# inspect country names
sub_obj_1_2_vdem_political_engagement %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name)
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_1_2_vdem_political_engagement, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country)

sub_obj_1_2_vdem_political_engagement %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_1_2_vdem_political_engagement <- sub_obj_1_2_vdem_political_engagement %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  left_join(., fmir_framework, by = "indicator_name") 


#/////////////////


# inspect
sub_obj_1_2_vdem_political_engagement
sub_obj_1_2_vdem_political_engagement %>% glimpse()
sub_obj_1_2_vdem_political_engagement %>% nrow() # 315
sub_obj_1_2_vdem_political_engagement %>% ncol() # 39

# check country/year
sub_obj_1_2_vdem_political_engagement %>% distinct(country) %>% nrow() # 45
sub_obj_1_2_vdem_political_engagement %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_1_2_vdem_political_engagement %>% count(indicator_name) # 315 (7 years * 45 countries = 315)
sub_obj_1_2_vdem_political_engagement %>% count(year) %>% print(n = nrow(.))

# check values
sub_obj_1_2_vdem_political_engagement %>% 
  filter(indicator_name == "sub_obj_1_2_vdem_political_engagement") %>% 
  skim(values)
sub_obj_1_2_vdem_political_engagement %>% group_by(year) %>% skim(values)


# plot
sub_obj_1_2_vdem_political_engagement %>% 
  # filter(mcp_grouping == "E&E Balkans") %>%
  filter(mcp_grouping == "E&E Eurasia") %>%
  # filter(mcp_grouping == "E&E graduates") %>%
  # filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# inspect summary stats on indicators
sub_obj_1_2_vdem_political_engagement %>% group_by(indicator_name) %>% 
  mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
         missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
         missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
  distinct(indicator_name, year_start, year_end, missing_pct)


#/////////////////


# read/write
sub_obj_1_2_vdem_political_engagement %>% write_csv(file = "data/fmir/sub_obj_1_2_vdem_political_engagement.csv")
sub_obj_1_2_vdem_political_engagement <- read_csv(file = "data/fmir/sub_obj_1_2_vdem_political_engagement.csv", 
                                                  lazy = FALSE)

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
# load sub_obj_2_1_vdem_media_bias ####


sub_obj_2_1_vdem_media_bias <- vdem %>% select(country_name, year, v2mebias) %>%
  filter(year >= 2018) %>%
  rename(sub_obj_2_1_vdem_media_bias = "v2mebias") %>%
  pivot_longer(cols = sub_obj_2_1_vdem_media_bias, names_to = "indicator_name", values_to = "values") %>%
  mutate(high_value_is_good_outcome_flag = 1,
         indicator_name = "sub_obj_2_1_vdem_media_bias",
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "Burma/Myanmar" ~ "Burma",
                                  country_name == "Cape Verde" ~ "Cabo Verde",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Democratic Republic of the Congo" ~ "Congo (Kinshasa)",
                                  country_name == "Hong Kong" ~ "Hong Kong SAR, China",
                                  country_name == "Ivory Coast" ~ "Cote d'Ivoire",
                                  country_name == "North Korea" ~ "Korea, North",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Palestine/West Bank" ~ "West Bank and Gaza",
                                  country_name == "Papal States" ~ "Holy See",
                                  country_name == "Republic of the Congo" ~ "Congo (Brazzaville)",
                                  country_name == "Republic of Vietnam" ~ "Vietnam",
                                  country_name == "South Korea" ~ "Korea, South",
                                  country_name == "The Gambia" ~ "Gambia, The",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States of America" ~ "U.S.",
                                  TRUE ~ country_name)) %>%
  pivot_wider(names_from = year, values_from = values) %>%
  pivot_longer(cols = -c(country_name, high_value_is_good_outcome_flag, indicator_name), 
               names_to = "year", values_to = "values") %>%
  mutate(year = as.numeric(year))


#/////////////////


# inspect
sub_obj_2_1_vdem_media_bias
sub_obj_2_1_vdem_media_bias %>% glimpse()
sub_obj_2_1_vdem_media_bias %>% nrow() # 1253
sub_obj_2_1_vdem_media_bias %>% ncol() # 5

var_info("v2mebias")
sub_obj_2_1_vdem_media_bias %>% arrange(values) %>% distinct(country_name)
sub_obj_2_1_vdem_media_bias %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_2_1_vdem_media_bias %>% skim()

# inspect country names
sub_obj_2_1_vdem_media_bias %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name)
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_2_1_vdem_media_bias, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country)

sub_obj_2_1_vdem_media_bias %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_2_1_vdem_media_bias <- sub_obj_2_1_vdem_media_bias %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  left_join(., fmir_framework, by = "indicator_name") 


#/////////////////


# inspect
sub_obj_2_1_vdem_media_bias
sub_obj_2_1_vdem_media_bias %>% glimpse()
sub_obj_2_1_vdem_media_bias %>% nrow() # 315
sub_obj_2_1_vdem_media_bias %>% ncol() # 39

# check country/year
sub_obj_2_1_vdem_media_bias %>% distinct(country) %>% nrow() # 45
sub_obj_2_1_vdem_media_bias %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_2_1_vdem_media_bias %>% count(indicator_name) # 315 (7 years * 45 countries = 315)
sub_obj_2_1_vdem_media_bias %>% count(year) %>% print(n = nrow(.))

# check values
sub_obj_2_1_vdem_media_bias %>% 
  filter(indicator_name == "sub_obj_2_1_vdem_media_bias") %>% 
  skim(values)
sub_obj_2_1_vdem_media_bias %>% group_by(year) %>% skim(values)


# plot
sub_obj_2_1_vdem_media_bias %>% 
  # filter(mcp_grouping == "E&E Balkans") %>%
  filter(mcp_grouping == "E&E Eurasia") %>%
  # filter(mcp_grouping == "E&E graduates") %>%
  # filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# inspect summary stats on indicators
sub_obj_2_1_vdem_media_bias %>% group_by(indicator_name) %>% 
  mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
         missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
         missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
  distinct(indicator_name, year_start, year_end, missing_pct)


#/////////////////


# read/write
sub_obj_2_1_vdem_media_bias %>% write_csv(file = "data/fmir/sub_obj_2_1_vdem_media_bias.csv")
sub_obj_2_1_vdem_media_bias <- read_csv(file = "data/fmir/sub_obj_2_1_vdem_media_bias.csv", 
                                        lazy = FALSE)

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# load sub_obj_2_1_vdem_media_corrupt ####


sub_obj_2_1_vdem_media_corrupt <- vdem %>% select(country_name, year, v2mecorrpt) %>%
  filter(year >= 2018) %>%
  rename(sub_obj_2_1_vdem_media_corrupt = "v2mecorrpt") %>%
  pivot_longer(cols = sub_obj_2_1_vdem_media_corrupt, names_to = "indicator_name", values_to = "values") %>%
  mutate(high_value_is_good_outcome_flag = 1,
         indicator_name = "sub_obj_2_1_vdem_media_corrupt",
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "Burma/Myanmar" ~ "Burma",
                                  country_name == "Cape Verde" ~ "Cabo Verde",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Democratic Republic of the Congo" ~ "Congo (Kinshasa)",
                                  country_name == "Hong Kong" ~ "Hong Kong SAR, China",
                                  country_name == "Ivory Coast" ~ "Cote d'Ivoire",
                                  country_name == "North Korea" ~ "Korea, North",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Palestine/West Bank" ~ "West Bank and Gaza",
                                  country_name == "Papal States" ~ "Holy See",
                                  country_name == "Republic of the Congo" ~ "Congo (Brazzaville)",
                                  country_name == "Republic of Vietnam" ~ "Vietnam",
                                  country_name == "South Korea" ~ "Korea, South",
                                  country_name == "The Gambia" ~ "Gambia, The",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States of America" ~ "U.S.",
                                  TRUE ~ country_name)) %>%
  pivot_wider(names_from = year, values_from = values) %>%
  pivot_longer(cols = -c(country_name, high_value_is_good_outcome_flag, indicator_name), 
               names_to = "year", values_to = "values") %>%
  mutate(year = as.numeric(year))


#/////////////////


# inspect
sub_obj_2_1_vdem_media_corrupt
sub_obj_2_1_vdem_media_corrupt %>% glimpse()
sub_obj_2_1_vdem_media_corrupt %>% nrow() # 1253
sub_obj_2_1_vdem_media_corrupt %>% ncol() # 5

var_info("v2mecorrpt")
sub_obj_2_1_vdem_media_corrupt %>% arrange(values) %>% distinct(country_name)
sub_obj_2_1_vdem_media_corrupt %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_2_1_vdem_media_corrupt %>% skim()

# inspect country names
sub_obj_2_1_vdem_media_corrupt %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name)
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_2_1_vdem_media_corrupt, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country)

sub_obj_2_1_vdem_media_corrupt %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_2_1_vdem_media_corrupt <- sub_obj_2_1_vdem_media_corrupt %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  left_join(., fmir_framework, by = "indicator_name") 


#/////////////////


# inspect
sub_obj_2_1_vdem_media_corrupt
sub_obj_2_1_vdem_media_corrupt %>% glimpse()
sub_obj_2_1_vdem_media_corrupt %>% nrow() # 315
sub_obj_2_1_vdem_media_corrupt %>% ncol() # 39

# check country/year
sub_obj_2_1_vdem_media_corrupt %>% distinct(country) %>% nrow() # 45
sub_obj_2_1_vdem_media_corrupt %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_2_1_vdem_media_corrupt %>% count(indicator_name) # 315 (7 years * 45 countries = 315)
sub_obj_2_1_vdem_media_corrupt %>% count(year) %>% print(n = nrow(.))

# check values
sub_obj_2_1_vdem_media_corrupt %>% 
  filter(indicator_name == "sub_obj_2_1_vdem_media_corrupt") %>% 
  skim(values)
sub_obj_2_1_vdem_media_corrupt %>% group_by(year) %>% skim(values)


# plot
sub_obj_2_1_vdem_media_corrupt %>% 
  # filter(mcp_grouping == "E&E Balkans") %>%
  filter(mcp_grouping == "E&E Eurasia") %>%
  # filter(mcp_grouping == "E&E graduates") %>%
  # filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# inspect summary stats on indicators
sub_obj_2_1_vdem_media_corrupt %>% group_by(indicator_name) %>% 
  mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
         missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
         missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
  distinct(indicator_name, year_start, year_end, missing_pct)


#/////////////////


# read/write
sub_obj_2_1_vdem_media_corrupt %>% write_csv(file = "data/fmir/sub_obj_2_1_vdem_media_corrupt.csv")
sub_obj_2_1_vdem_media_corrupt <- read_csv(file = "data/fmir/sub_obj_2_1_vdem_media_corrupt.csv", 
                                           lazy = FALSE)

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# load sub_obj_2_3_vdem_gov_dissem_false_info_domestically ####


sub_obj_2_3_vdem_gov_dissem_false_info_domestically <- vdem %>% select(country_name, year, v2smgovdom) %>%
  filter(year >= 2018) %>%
  rename(sub_obj_2_3_vdem_gov_dissem_false_info_domestically = "v2smgovdom") %>%
  pivot_longer(cols = sub_obj_2_3_vdem_gov_dissem_false_info_domestically, names_to = "indicator_name", values_to = "values") %>%
  mutate(high_value_is_good_outcome_flag = 1,
         indicator_name = "sub_obj_2_3_vdem_gov_dissem_false_info_domestically",
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "Burma/Myanmar" ~ "Burma",
                                  country_name == "Cape Verde" ~ "Cabo Verde",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Democratic Republic of the Congo" ~ "Congo (Kinshasa)",
                                  country_name == "Hong Kong" ~ "Hong Kong SAR, China",
                                  country_name == "Ivory Coast" ~ "Cote d'Ivoire",
                                  country_name == "North Korea" ~ "Korea, North",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Palestine/West Bank" ~ "West Bank and Gaza",
                                  country_name == "Papal States" ~ "Holy See",
                                  country_name == "Republic of the Congo" ~ "Congo (Brazzaville)",
                                  country_name == "Republic of Vietnam" ~ "Vietnam",
                                  country_name == "South Korea" ~ "Korea, South",
                                  country_name == "The Gambia" ~ "Gambia, The",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States of America" ~ "U.S.",
                                  TRUE ~ country_name)) %>%
  pivot_wider(names_from = year, values_from = values) %>%
  pivot_longer(cols = -c(country_name, high_value_is_good_outcome_flag, indicator_name), 
               names_to = "year", values_to = "values") %>%
  mutate(year = as.numeric(year))


#/////////////////


# inspect
sub_obj_2_3_vdem_gov_dissem_false_info_domestically
sub_obj_2_3_vdem_gov_dissem_false_info_domestically %>% glimpse()
sub_obj_2_3_vdem_gov_dissem_false_info_domestically %>% nrow() # 1253
sub_obj_2_3_vdem_gov_dissem_false_info_domestically %>% ncol() # 5

var_info("v2smgovdom")
sub_obj_2_3_vdem_gov_dissem_false_info_domestically %>% arrange(values) %>% distinct(country_name)
sub_obj_2_3_vdem_gov_dissem_false_info_domestically %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_2_3_vdem_gov_dissem_false_info_domestically %>% skim()

# inspect country names
sub_obj_2_3_vdem_gov_dissem_false_info_domestically %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name)
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_2_3_vdem_gov_dissem_false_info_domestically, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country)

sub_obj_2_3_vdem_gov_dissem_false_info_domestically %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_2_3_vdem_gov_dissem_false_info_domestically <- sub_obj_2_3_vdem_gov_dissem_false_info_domestically %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  left_join(., fmir_framework, by = "indicator_name") 


#/////////////////


# inspect
sub_obj_2_3_vdem_gov_dissem_false_info_domestically
sub_obj_2_3_vdem_gov_dissem_false_info_domestically %>% glimpse()
sub_obj_2_3_vdem_gov_dissem_false_info_domestically %>% nrow() # 315
sub_obj_2_3_vdem_gov_dissem_false_info_domestically %>% ncol() # 39

# check country/year
sub_obj_2_3_vdem_gov_dissem_false_info_domestically %>% distinct(country) %>% nrow() # 45
sub_obj_2_3_vdem_gov_dissem_false_info_domestically %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_2_3_vdem_gov_dissem_false_info_domestically %>% count(indicator_name) # 315 (7 years * 45 countries = 315)
sub_obj_2_3_vdem_gov_dissem_false_info_domestically %>% count(year) %>% print(n = nrow(.))

# check values
sub_obj_2_3_vdem_gov_dissem_false_info_domestically %>% 
  filter(indicator_name == "sub_obj_2_3_vdem_gov_dissem_false_info_domestically") %>% 
  skim(values)
sub_obj_2_3_vdem_gov_dissem_false_info_domestically %>% group_by(year) %>% skim(values)


# plot
sub_obj_2_3_vdem_gov_dissem_false_info_domestically %>% 
  # filter(mcp_grouping == "E&E Balkans") %>%
  filter(mcp_grouping == "E&E Eurasia") %>%
  # filter(mcp_grouping == "E&E graduates") %>%
  # filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# inspect summary stats on indicators
sub_obj_2_3_vdem_gov_dissem_false_info_domestically %>% group_by(indicator_name) %>% 
  mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
         missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
         missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
  distinct(indicator_name, year_start, year_end, missing_pct)


#/////////////////


# read/write
sub_obj_2_3_vdem_gov_dissem_false_info_domestically %>% write_csv(file = "data/fmir/sub_obj_2_3_vdem_gov_dissem_false_info_domestically.csv")
sub_obj_2_3_vdem_gov_dissem_false_info_domestically <- read_csv(file = "data/fmir/sub_obj_2_3_vdem_gov_dissem_false_info_domestically.csv", 
                                                                lazy = FALSE)

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# load sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically ####

sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically <- vdem %>% select(country_name, year, v2smpardom) %>%
  filter(year >= 2018) %>%
  rename(sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically = "v2smpardom") %>%
  pivot_longer(cols = sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically, names_to = "indicator_name", values_to = "values") %>%
  mutate(high_value_is_good_outcome_flag = 1,
         indicator_name = "sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically",
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "Burma/Myanmar" ~ "Burma",
                                  country_name == "Cape Verde" ~ "Cabo Verde",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Democratic Republic of the Congo" ~ "Congo (Kinshasa)",
                                  country_name == "Hong Kong" ~ "Hong Kong SAR, China",
                                  country_name == "Ivory Coast" ~ "Cote d'Ivoire",
                                  country_name == "North Korea" ~ "Korea, North",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Palestine/West Bank" ~ "West Bank and Gaza",
                                  country_name == "Papal States" ~ "Holy See",
                                  country_name == "Republic of the Congo" ~ "Congo (Brazzaville)",
                                  country_name == "Republic of Vietnam" ~ "Vietnam",
                                  country_name == "South Korea" ~ "Korea, South",
                                  country_name == "The Gambia" ~ "Gambia, The",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States of America" ~ "U.S.",
                                  TRUE ~ country_name)) %>%
  pivot_wider(names_from = year, values_from = values) %>%
  pivot_longer(cols = -c(country_name, high_value_is_good_outcome_flag, indicator_name), 
               names_to = "year", values_to = "values") %>%
  mutate(year = as.numeric(year))


#/////////////////


# inspect
sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically
sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically %>% glimpse()
sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically %>% nrow() # 1253
sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically %>% ncol() # 5

var_info("v2smpardom")
sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically %>% arrange(values) %>% distinct(country_name)
sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically %>% skim()

# inspect country names
sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name)
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country)

sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically <- sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  left_join(., fmir_framework, by = "indicator_name") 


#/////////////////


# inspect
sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically
sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically %>% glimpse()
sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically %>% nrow() # 315
sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically %>% ncol() # 39

# check country/year
sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically %>% distinct(country) %>% nrow() # 45
sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically %>% count(indicator_name) # 315 (7 years * 45 countries = 315)
sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically %>% count(year) %>% print(n = nrow(.))

# check values
sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically %>% 
  filter(indicator_name == "sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically") %>% 
  skim(values)
sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically %>% group_by(year) %>% skim(values)


# plot
sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically %>% 
  # filter(mcp_grouping == "E&E Balkans") %>%
  filter(mcp_grouping == "E&E Eurasia") %>%
  # filter(mcp_grouping == "E&E graduates") %>%
  # filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# inspect summary stats on indicators
sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically %>% group_by(indicator_name) %>% 
  mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
         missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
         missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
  distinct(indicator_name, year_start, year_end, missing_pct)


#/////////////////


# read/write
sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically %>% write_csv(file = "data/fmir/sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically.csv")
sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically <- read_csv(file = "data/fmir/sub_obj_2_3_vdem_political_parties_dissem_false_info_domestically.csv", 
                                                                              lazy = FALSE)

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# load sub_obj_2_3_vdem_foreign_gov_dissem_false_info ####


sub_obj_2_3_vdem_foreign_gov_dissem_false_info <- vdem %>% select(country_name, year, v2smfordom) %>%
  filter(year >= 2018) %>%
  rename(sub_obj_2_3_vdem_foreign_gov_dissem_false_info = "v2smfordom") %>%
  pivot_longer(cols = sub_obj_2_3_vdem_foreign_gov_dissem_false_info, names_to = "indicator_name", values_to = "values") %>%
  mutate(high_value_is_good_outcome_flag = 1,
         indicator_name = "sub_obj_2_3_vdem_foreign_gov_dissem_false_info",
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "Burma/Myanmar" ~ "Burma",
                                  country_name == "Cape Verde" ~ "Cabo Verde",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Democratic Republic of the Congo" ~ "Congo (Kinshasa)",
                                  country_name == "Hong Kong" ~ "Hong Kong SAR, China",
                                  country_name == "Ivory Coast" ~ "Cote d'Ivoire",
                                  country_name == "North Korea" ~ "Korea, North",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Palestine/West Bank" ~ "West Bank and Gaza",
                                  country_name == "Papal States" ~ "Holy See",
                                  country_name == "Republic of the Congo" ~ "Congo (Brazzaville)",
                                  country_name == "Republic of Vietnam" ~ "Vietnam",
                                  country_name == "South Korea" ~ "Korea, South",
                                  country_name == "The Gambia" ~ "Gambia, The",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States of America" ~ "U.S.",
                                  TRUE ~ country_name)) %>%
  pivot_wider(names_from = year, values_from = values) %>%
  pivot_longer(cols = -c(country_name, high_value_is_good_outcome_flag, indicator_name), 
               names_to = "year", values_to = "values") %>%
  mutate(year = as.numeric(year))


#/////////////////


# inspect
sub_obj_2_3_vdem_foreign_gov_dissem_false_info
sub_obj_2_3_vdem_foreign_gov_dissem_false_info %>% glimpse()
sub_obj_2_3_vdem_foreign_gov_dissem_false_info %>% nrow() # 1253
sub_obj_2_3_vdem_foreign_gov_dissem_false_info %>% ncol() # 5

var_info("v2smfordom")
sub_obj_2_3_vdem_foreign_gov_dissem_false_info %>% arrange(values) %>% distinct(country_name)
sub_obj_2_3_vdem_foreign_gov_dissem_false_info %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_2_3_vdem_foreign_gov_dissem_false_info %>% skim()

# inspect country names
sub_obj_2_3_vdem_foreign_gov_dissem_false_info %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name)
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_2_3_vdem_foreign_gov_dissem_false_info, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country)

sub_obj_2_3_vdem_foreign_gov_dissem_false_info %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_2_3_vdem_foreign_gov_dissem_false_info <- sub_obj_2_3_vdem_foreign_gov_dissem_false_info %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  left_join(., fmir_framework, by = "indicator_name") 


#/////////////////


# inspect
sub_obj_2_3_vdem_foreign_gov_dissem_false_info
sub_obj_2_3_vdem_foreign_gov_dissem_false_info %>% glimpse()
sub_obj_2_3_vdem_foreign_gov_dissem_false_info %>% nrow() # 315
sub_obj_2_3_vdem_foreign_gov_dissem_false_info %>% ncol() # 39

# check country/year
sub_obj_2_3_vdem_foreign_gov_dissem_false_info %>% distinct(country) %>% nrow() # 45
sub_obj_2_3_vdem_foreign_gov_dissem_false_info %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_2_3_vdem_foreign_gov_dissem_false_info %>% count(indicator_name) # 315 (7 years * 45 countries = 315)
sub_obj_2_3_vdem_foreign_gov_dissem_false_info %>% count(year) %>% print(n = nrow(.))

# check values
sub_obj_2_3_vdem_foreign_gov_dissem_false_info %>% 
  filter(indicator_name == "sub_obj_2_3_vdem_foreign_gov_dissem_false_info") %>% 
  skim(values)
sub_obj_2_3_vdem_foreign_gov_dissem_false_info %>% group_by(year) %>% skim(values)


# plot
sub_obj_2_3_vdem_foreign_gov_dissem_false_info %>% 
  # filter(mcp_grouping == "E&E Balkans") %>%
  filter(mcp_grouping == "E&E Eurasia") %>%
  # filter(mcp_grouping == "E&E graduates") %>%
  # filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# inspect summary stats on indicators
sub_obj_2_3_vdem_foreign_gov_dissem_false_info %>% group_by(indicator_name) %>% 
  mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
         missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
         missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
  distinct(indicator_name, year_start, year_end, missing_pct)


#/////////////////


# read/write
sub_obj_2_3_vdem_foreign_gov_dissem_false_info %>% write_csv(file = "data/fmir/sub_obj_2_3_vdem_foreign_gov_dissem_false_info.csv")
sub_obj_2_3_vdem_foreign_gov_dissem_false_info <- read_csv(file = "data/fmir/sub_obj_2_3_vdem_foreign_gov_dissem_false_info.csv", 
                                                                lazy = FALSE)

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# load sub_obj_2_3_vdem_gov_internet_filtering ####

sub_obj_2_3_vdem_gov_internet_filtering <- vdem %>% select(country_name, year, v2smgovfilprc) %>%
  filter(year >= 2018) %>%
  rename(sub_obj_2_3_vdem_gov_internet_filtering = "v2smgovfilprc") %>%
  pivot_longer(cols = sub_obj_2_3_vdem_gov_internet_filtering, names_to = "indicator_name", values_to = "values") %>%
  mutate(high_value_is_good_outcome_flag = 1,
         indicator_name = "sub_obj_2_3_vdem_gov_internet_filtering",
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "Burma/Myanmar" ~ "Burma",
                                  country_name == "Cape Verde" ~ "Cabo Verde",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Democratic Republic of the Congo" ~ "Congo (Kinshasa)",
                                  country_name == "Hong Kong" ~ "Hong Kong SAR, China",
                                  country_name == "Ivory Coast" ~ "Cote d'Ivoire",
                                  country_name == "North Korea" ~ "Korea, North",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Palestine/West Bank" ~ "West Bank and Gaza",
                                  country_name == "Papal States" ~ "Holy See",
                                  country_name == "Republic of the Congo" ~ "Congo (Brazzaville)",
                                  country_name == "Republic of Vietnam" ~ "Vietnam",
                                  country_name == "South Korea" ~ "Korea, South",
                                  country_name == "The Gambia" ~ "Gambia, The",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States of America" ~ "U.S.",
                                  TRUE ~ country_name)) %>%
  pivot_wider(names_from = year, values_from = values) %>%
  pivot_longer(cols = -c(country_name, high_value_is_good_outcome_flag, indicator_name), 
               names_to = "year", values_to = "values") %>%
  mutate(year = as.numeric(year))


#/////////////////


# inspect
sub_obj_2_3_vdem_gov_internet_filtering
sub_obj_2_3_vdem_gov_internet_filtering %>% glimpse()
sub_obj_2_3_vdem_gov_internet_filtering %>% nrow() # 1253
sub_obj_2_3_vdem_gov_internet_filtering %>% ncol() # 5

var_info("v2smgovfilprc")
sub_obj_2_3_vdem_gov_internet_filtering %>% arrange(values) %>% distinct(country_name)
sub_obj_2_3_vdem_gov_internet_filtering %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_2_3_vdem_gov_internet_filtering %>% skim()

# inspect country names
sub_obj_2_3_vdem_gov_internet_filtering %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name)
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_2_3_vdem_gov_internet_filtering, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country)

sub_obj_2_3_vdem_gov_internet_filtering %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_2_3_vdem_gov_internet_filtering <- sub_obj_2_3_vdem_gov_internet_filtering %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  left_join(., fmir_framework, by = "indicator_name") 


#/////////////////


# inspect
sub_obj_2_3_vdem_gov_internet_filtering
sub_obj_2_3_vdem_gov_internet_filtering %>% glimpse()
sub_obj_2_3_vdem_gov_internet_filtering %>% nrow() # 315
sub_obj_2_3_vdem_gov_internet_filtering %>% ncol() # 40

# check country/year
sub_obj_2_3_vdem_gov_internet_filtering %>% distinct(country) %>% nrow() # 45
sub_obj_2_3_vdem_gov_internet_filtering %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_2_3_vdem_gov_internet_filtering %>% count(indicator_name) # 315 (7 years * 45 countries = 315)
sub_obj_2_3_vdem_gov_internet_filtering %>% count(year) %>% print(n = nrow(.))

# check values
sub_obj_2_3_vdem_gov_internet_filtering %>% 
  filter(indicator_name == "sub_obj_2_3_vdem_gov_internet_filtering") %>% 
  skim(values)
sub_obj_2_3_vdem_gov_internet_filtering %>% group_by(year) %>% skim(values)


# plot
sub_obj_2_3_vdem_gov_internet_filtering %>% 
  # filter(mcp_grouping == "E&E Balkans") %>%
  filter(mcp_grouping == "E&E Eurasia") %>%
  # filter(mcp_grouping == "E&E graduates") %>%
  # filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# inspect summary stats on indicators
sub_obj_2_3_vdem_gov_internet_filtering %>% group_by(indicator_name) %>% 
  mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
         missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
         missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
  distinct(indicator_name, year_start, year_end, missing_pct)


#/////////////////


# read/write
sub_obj_2_3_vdem_gov_internet_filtering %>% write_csv(file = "data/fmir/sub_obj_2_3_vdem_gov_internet_filtering.csv")
sub_obj_2_3_vdem_gov_internet_filtering <- read_csv(file = "data/fmir/sub_obj_2_3_vdem_gov_internet_filtering.csv", 
                                            lazy = FALSE)

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////

# load sub_obj_2_3_vdem_gov_social_media_shutdown ####

sub_obj_2_3_vdem_gov_social_media_shutdown <- vdem %>% select(country_name, year, v2smgovsm) %>%
  filter(year >= 2018) %>%
  rename(sub_obj_2_3_vdem_gov_social_media_shutdown = "v2smgovsm") %>%
  pivot_longer(cols = sub_obj_2_3_vdem_gov_social_media_shutdown, names_to = "indicator_name", values_to = "values") %>%
  mutate(high_value_is_good_outcome_flag = 1,
         indicator_name = "sub_obj_2_3_vdem_gov_social_media_shutdown",
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "Burma/Myanmar" ~ "Burma",
                                  country_name == "Cape Verde" ~ "Cabo Verde",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Democratic Republic of the Congo" ~ "Congo (Kinshasa)",
                                  country_name == "Hong Kong" ~ "Hong Kong SAR, China",
                                  country_name == "Ivory Coast" ~ "Cote d'Ivoire",
                                  country_name == "North Korea" ~ "Korea, North",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Palestine/West Bank" ~ "West Bank and Gaza",
                                  country_name == "Papal States" ~ "Holy See",
                                  country_name == "Republic of the Congo" ~ "Congo (Brazzaville)",
                                  country_name == "Republic of Vietnam" ~ "Vietnam",
                                  country_name == "South Korea" ~ "Korea, South",
                                  country_name == "The Gambia" ~ "Gambia, The",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States of America" ~ "U.S.",
                                  TRUE ~ country_name)) %>%
  pivot_wider(names_from = year, values_from = values) %>%
  pivot_longer(cols = -c(country_name, high_value_is_good_outcome_flag, indicator_name), 
               names_to = "year", values_to = "values") %>%
  mutate(year = as.numeric(year))


#/////////////////


# inspect
sub_obj_2_3_vdem_gov_social_media_shutdown
sub_obj_2_3_vdem_gov_social_media_shutdown %>% glimpse()
sub_obj_2_3_vdem_gov_social_media_shutdown %>% nrow() # 1253
sub_obj_2_3_vdem_gov_social_media_shutdown %>% ncol() # 5

var_info("v2smgovsm")
sub_obj_2_3_vdem_gov_social_media_shutdown %>% arrange(values) %>% distinct(country_name)
sub_obj_2_3_vdem_gov_social_media_shutdown %>% arrange(desc(values)) %>% distinct(country_name)
sub_obj_2_3_vdem_gov_social_media_shutdown %>% skim()

# inspect country names
sub_obj_2_3_vdem_gov_social_media_shutdown %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name)
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_2_3_vdem_gov_social_media_shutdown, by = c("country" = "country_name")) %>% 
  distinct(country) %>% arrange(country)

sub_obj_2_3_vdem_gov_social_media_shutdown %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////


# join country_crosswalk and fmir_framework
sub_obj_2_3_vdem_gov_social_media_shutdown <- sub_obj_2_3_vdem_gov_social_media_shutdown %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  left_join(., fmir_framework, by = "indicator_name") 


#/////////////////


# inspect
sub_obj_2_3_vdem_gov_social_media_shutdown
sub_obj_2_3_vdem_gov_social_media_shutdown %>% glimpse()
sub_obj_2_3_vdem_gov_social_media_shutdown %>% nrow() # 315
sub_obj_2_3_vdem_gov_social_media_shutdown %>% ncol() # 40

# check country/year
sub_obj_2_3_vdem_gov_social_media_shutdown %>% distinct(country) %>% nrow() # 45
sub_obj_2_3_vdem_gov_social_media_shutdown %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_2_3_vdem_gov_social_media_shutdown %>% count(indicator_name) # 315 (7 years * 45 countries = 315)
sub_obj_2_3_vdem_gov_social_media_shutdown %>% count(year) %>% print(n = nrow(.))

# check values
sub_obj_2_3_vdem_gov_social_media_shutdown %>% 
  filter(indicator_name == "sub_obj_2_3_vdem_gov_social_media_shutdown") %>% 
  skim(values)
sub_obj_2_3_vdem_gov_social_media_shutdown %>% group_by(year) %>% skim(values)


# plot
sub_obj_2_3_vdem_gov_social_media_shutdown %>% 
  # filter(mcp_grouping == "E&E Balkans") %>%
  filter(mcp_grouping == "E&E Eurasia") %>%
  # filter(mcp_grouping == "E&E graduates") %>%
  # filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

# inspect summary stats on indicators
sub_obj_2_3_vdem_gov_social_media_shutdown %>% group_by(indicator_name) %>% 
  mutate(year_start = min(year, na.rm = TRUE), year_end = max(year, na.rm = TRUE),
         missing_flag = case_when(is.na(values) ~ 1, TRUE ~ 0),
         missing_pct = mean(missing_flag, na.rm = TRUE)) %>% 
  distinct(indicator_name, year_start, year_end, missing_pct)


#/////////////////


# read/write
sub_obj_2_3_vdem_gov_social_media_shutdown %>% write_csv(file = "data/fmir/sub_obj_2_3_vdem_gov_social_media_shutdown.csv")
sub_obj_2_3_vdem_gov_social_media_shutdown <- read_csv(file = "data/fmir/sub_obj_2_3_vdem_gov_social_media_shutdown.csv", 
                                            lazy = FALSE)

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
