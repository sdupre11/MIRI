# load sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi ####

# note that fdi data is a stock or total position, not a flow
# https://data.imf.org/?sk=40313609-F037-48C1-84B1-E1F1CE54D6D5&sId=1390288795525
# "2. Are CDIS data position or flow data?
# CDIS includes only direct investment position (stock) data"


# definition of IMF FDI:
# https://www.imf.org/external/pubs/ft/fandd/basics/20_direct-invest.htm
# "They can make a portfolio investment, buying stocks or bonds, say, often with the idea of making a short-term speculative financial gain 
# without becoming actively engaged in the day-to-day running of the enterprise in which they invest.
# Or they can choose the long-haul, hands-on approach-investing in an enterprise in another economy with the objective of gaining control or 
# exerting significant influence over management of the firm (which usually involves a stake of at least 10 percent of a company's stock). 
# In the most extreme case, investors may build new facilities from scratch, maintaining full control over operations.-"

# Direct investment takes different shapes and forms. A company may enter a foreign market through so-called greenfield direct investment, 
# in which the direct investor provides funds to build a new factory, distribution facility, or store, for example, to establish 
# its presence in the host country.-
# But a company might also choose brownfield direct investment. Instead of establishing a new presence the company invests in or 
# takes over an existing local company. Brownfield investment means acquiring existing facilities, suppliers, and operations-and often 
# the brand itself.-


# note that reported values are used, not mirror/derived values from counterparts
# so inward fdi from russia is taken as reported by receiving country
# picking one official perspective is how IMF reports a country's official exports/imports 
# to counterparties in their DOT statistics yearbook
# even though exports/imports have the same asymmetries with mirror/derived values reported by counterparts
# for exports/imports, atlas applies statistical cleaning to get better measure, and maybe the midpoint is at least a bit better
# but for simplicity and to keep with official reporting, will use the official non-derived values reported by 
# ee presence/CARS/graduates countries
# DOT yearbook: https://data.imf.org/?sk=9d6028d4-f14a-464c-a2f2-59b2cd424b85&sId=1488236767350
# https://data.imf.org/?sk=40313609-F037-48C1-84B1-E1F1CE54D6D5&sId=1410469360660

# note the default assumption would be values are reported in current dollars, 
# since that the docs don't seem to mention any adjustments
# note that values of C for censored are converted to NA

FMI_actors_CDIS <- c("China, P.R.: Hong Kong", "China, P.R.: Macao", "China, P.R.: Mainland", "Iran, Islamic Rep. of", "Russian Federation")

importCDIS <- function(file) {
  internal_df <- read_excel(path = file,
                            skip = 3,
                            na = "C") %>%
    rename(investor = "Investment from:") %>%
    filter(investor %in% FMI_actors_CDIS) %>%
    select(-`Total Investment`) %>%
    select(-investor) %>%
    rownames_to_column(var = "row_id") %>%   # Create a row identifier
    pivot_longer(cols = -row_id, names_to = "original_column", values_to = "value") %>%
    select(-row_id) %>%
    mutate(value = case_when(
      is.na(value) ~ 0,
      TRUE ~ value)) %>%
    group_by(original_column) %>%
    summarize(values=sum(value)) %>%
    ungroup() %>%
    rename(country_name = original_column)

  internal_world <- read_excel(path = file,
                               skip = 3,
                               na = "C") %>%
    rename(investor = "Investment from:") %>%
    filter(investor == "World") %>%
    select(-`Total Investment`) %>%
    select(-investor) %>%
    rownames_to_column(var = "row_id") %>%   # Create a row identifier
    pivot_longer(cols = -row_id, names_to = "original_column", values_to = "world_value") %>%
    select(-row_id) %>%
    mutate(world_value = case_when(
      is.na(world_value) ~ 0,
      TRUE ~ world_value)) %>%
    rename(country_name = original_column) %>%
    full_join(internal_df, 
              by = "country_name") %>%
    mutate(proportion = (values/world_value) * 100) %>%
    select(-c("values",
              "world_value")) %>%
    rename(values = proportion) %>%
    mutate(values = case_when(
      values == "NaN" ~ 0,
      TRUE ~ values
    ))



 

  
}

cdis_2018 <- importCDIS("data/imf/CDIS_Table_6_Direct_Investment_Posi 2018.xlsx") %>%
  rename(`2018` = values)
cdis_2019 <- importCDIS("data/imf/CDIS_Table_6_Direct_Investment_Posi 2019.xlsx") %>%
  rename(`2019` = values) %>%
  full_join(cdis_2018,
            by = "country_name")
cdis_2020 <- importCDIS("data/imf/CDIS_Table_6_Direct_Investment_Posi 2020.xlsx") %>%
  rename(`2020` = values) %>%
  full_join(cdis_2019,
            by = "country_name")
cdis_2021 <- importCDIS("data/imf/CDIS_Table_6_Direct_Investment_Posi 2021.xlsx") %>%
  rename(`2021` = values) %>%
  full_join(cdis_2020,
            by = "country_name")
cdis_2022 <- importCDIS("data/imf/CDIS_Table_6_Direct_Investment_Posi 2022.xlsx") %>%
  rename(`2022` = values) %>%
  full_join(cdis_2021,
            by = "country_name")

sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi <- cdis_2022 %>%
  mutate(`2023` = NA) %>%
  pivot_longer(cols = c("2018",
                        "2019",
                        "2020",
                        "2021",
                        "2022",
                        "2023"),
               names_to = "year",
               values_to = "values") %>%
  mutate(values = as.numeric(100-values),
         year = as.numeric(year),
         indicator_name = "sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi",
         high_value_is_good_outcome_flag = 1,
         country_name = case_when(country_name == "Armenia, Rep. of" ~ "Armenia",
                                  country_name == "Azerbaijan, Rep. of" ~ "Azerbaijan",
                                  country_name == "Belarus, Rep. of" ~ "Belarus",
                                  country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "Croatia, Rep. of" ~ "Croatia",
                                  country_name == "Czech Rep." ~ "Czechia",
                                  country_name == "Estonia, Rep. of" ~ "Estonia",
                                  country_name == "Kazakhstan, Rep. of" ~ "Kazakhstan",
                                  country_name == "Kosovo, Rep. of" ~ "Kosovo",
                                  country_name == "Kyrgyz Rep." ~ "Kyrgyzstan",
                                  country_name == "Moldova, Rep. of" ~ "Moldova",
                                  country_name == "Netherlands, The" ~ "Netherlands",
                                  country_name == "North Macedonia, Republic of" ~ "N. Macedonia",
                                  country_name == "Poland, Rep. of" ~ "Poland",
                                  country_name == "Russian Federation" ~ "Russia",
                                  country_name == "Serbia, Rep. of" ~ "Serbia",
                                  country_name == "Slovak Rep." ~ "Slovakia",
                                  country_name == "Slovenia, Rep. of" ~ "Slovenia",
                                  country_name == "Tajikistan, Rep. of" ~ "Tajikistan",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States" ~ "U.S.",
                                  country_name == "Uzbekistan, Rep. of" ~ "Uzbekistan",
                                  TRUE ~ country_name)) %>%
  add_row(country_name = "Turkmenistan", 
          year = 2018, 
          values = 65.4,
          indicator_name = "sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi",
          high_value_is_good_outcome_flag = 1) %>%
  add_row(country_name = "Turkmenistan", 
          year = 2019, 
          values = 87.8,
          indicator_name = "sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi",
          high_value_is_good_outcome_flag = 1) %>%
  add_row(country_name = "Turkmenistan", 
          year = 2020, 
          values = 72.2,
          indicator_name = "sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi",
          high_value_is_good_outcome_flag = 1) %>%
  add_row(country_name = "Turkmenistan", 
          year = 2021, 
          values = 75.5,
          indicator_name = "sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi",
          high_value_is_good_outcome_flag = 1) %>%
  add_row(country_name = "Uzbekistan", 
          year = 2018, 
          values = 48.5,
          indicator_name = "sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi",
          high_value_is_good_outcome_flag = 1) %>%
  add_row(country_name = "Uzbekistan", 
          year = 2019, 
          values = 48.9,
          indicator_name = "sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi",
          high_value_is_good_outcome_flag = 1) %>%
  add_row(country_name = "Uzbekistan", 
          year = 2020, 
          values = 23.2,
          indicator_name = "sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi",
          high_value_is_good_outcome_flag = 1) %>%
  add_row(country_name = "Uzbekistan", 
          year = 2021, 
          values = 25.4,
          indicator_name = "sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi",
          high_value_is_good_outcome_flag = 1)

 

#/////////////////////////////////////////////////////////////////////////////////////////////


# inspect
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi %>% nrow() # 774
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi %>% ncol() # 5
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi %>% glimpse()
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi %>% arrange(desc(values)) %>% distinct(country_name)

# check values
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi %>% skim()
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi %>% count(year)

# inspect country names
# note that turkmenistan and uzbekistan have zero records in raw data where category = inward/outward non-derived; only have derived
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
  anti_join(., sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi, by = c("country" = "country_name")) %>% select(country)

sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)

#/////////////////////////////////////////////////////////////////////////////////////////////


# join to country_crosswalk and fmir_framework
# need to update indicator_name and high_value_is_good_outcome_flag for records with NA values from country/year join
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi <- sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi %>%
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), .,
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////////


# inspect
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi %>% glimpse()
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi %>% nrow() # 315
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi %>% ncol() # 40

# check country/year
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi %>% distinct(country) %>% nrow() # 45
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi %>% count(year)

# check values (note missing values are checked below)
# note there are no negative values
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi %>% group_by(year) %>%
  skim(values)

# check values
# from 2010 onward, missing 32 country/years: 
# from E&E, missing serbia (2020-2021), albania(2010)
# also turkmenistan (2010-2021), uzbekistan (2010-2021), tajikistan (2010-2014), 
# because the missing country/years appear to reflect continuous periods of non-reporting, which might not be due to 
# actual zero values, these NAs will be left as NAs, and therefore imputed, rather than assign a zero value

sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi %>% 
  filter(is.na(values), year >= 2010) %>% count(country) %>% arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi %>% 
  filter(is.na(values), year >= 2010) %>% count(country, year) %>% 
  add_count(country, name = "country_count") %>% 
  arrange(desc(country_count)) %>% print(n = nrow(.))


#/////////////////////////


# plot
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi %>% 
  filter(mcp_grouping == "E&E Balkans") %>%
  filter(year >= 2010) %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1) 

#/////////////////////////////////////////////////////////////////////////////////////////////

# read/write
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi %>%
  write_csv(file = "data/fmir/sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi.csv")
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi <- read.csv(file = "data/fmir/sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi.csv") %>%
  as_tibble()

# inspect
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi %>% glimpse()
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi %>% nrow() # 315
sub_obj_4_2_cdis_non_fmi_fdi_as_share_of_total_fdi %>% ncol() # 36

rm(cdis_2018,
   cdis_2019,
   cdis_2020,
   cdis_2021,
   cdis_2022)

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
# load sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports ####

exports_World <- read_excel(path = "data/imf/DOTS_Imports_from_World_CIF.xlsx",
                            skip = 6) %>%
  rename(country_name = ...1) %>%
  select(c("country_name",
           "2018",
           "2019",
           "2020",
           "2021",
           "2022",
           "2023")) %>%
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "world")

exports_Iran <- read_excel(path = "data/imf/DOTS_Imports_from_Iran_CIF.xlsx",
                           skip = 6) %>%
  rename(country_name = ...1) 

exports_Iran$`2018` <- as.numeric(gsub(" e", 
                                       "",
                                       exports_Iran$`2018`))
exports_Iran$`2019` <- as.numeric(gsub(" e", 
                                       "",
                                       exports_Iran$`2019`))
exports_Iran$`2020` <- as.numeric(gsub(" e", 
                                       "",
                                       exports_Iran$`2020`))
exports_Iran$`2021` <- as.numeric(gsub(" e", 
                                       "",
                                       exports_Iran$`2021`))
exports_Iran$`2022` <- as.numeric(gsub(" e", 
                                       "",
                                       exports_Iran$`2022`))
exports_Iran$`2023` <- as.numeric(gsub(" e", 
                                       "",
                                       exports_Iran$`2023`))

exports_Iran <- exports_Iran %>%
  mutate(`2018` = as.numeric(`2018`))  %>%
  select(c("country_name",
           "2018",
           "2019",
           "2020",
           "2021",
           "2022",
           "2023")) %>%
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "Iran")

exports_Russia <- read_excel(path = "data/imf/DOTS_Imports_from_Russia_CIF.xlsx",
                             skip = 6) %>%
  rename(country_name = ...1)  %>%
  select(c("country_name",
           "2018",
           "2019",
           "2020",
           "2021",
           "2022",
           "2023")) %>%
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "Russia")

exports_PRC_HK <- read_excel(path = "data/imf/DOTS_Imports_from_PRC_HK_CIF.xlsx",
                             skip = 6) %>%
  rename(country_name = ...1)  %>%
  select(c("country_name",
           "2018",
           "2019",
           "2020",
           "2021",
           "2022",
           "2023")) %>%
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "PRC, Hong Kong")

exports_PRC_Macao <- read_excel(path = "data/imf/DOTS_Imports_from_PRC_Macao_CIF.xlsx",
                                skip = 6) %>%
  rename(country_name = ...1)  %>%
  select(c("country_name",
           "2018",
           "2019",
           "2020",
           "2021",
           "2022",
           "2023")) %>%
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "PRC, Macao")

exports_PRC_Mainland <- read_excel(path = "data/imf/DOTS_Imports_from_PRC_Mainland_CIF.xlsx",
                                   skip = 6) %>%
  rename(country_name = ...1)  %>%
  select(c("country_name",
           "2018",
           "2019",
           "2020",
           "2021",
           "2022",
           "2023")) %>%
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "PRC, Mainland")

exports_PRC_HK$`PRC, Hong Kong`[is.na(exports_PRC_HK$`PRC, Hong Kong`)] <- 0
exports_PRC_Macao$`PRC, Macao`[is.na(exports_PRC_Macao$`PRC, Macao`)] <- 0
exports_PRC_Mainland$`PRC, Mainland`[is.na(exports_PRC_Mainland$`PRC, Mainland`)] <- 0


exports_PRC <- full_join(exports_PRC_HK,
                         exports_PRC_Macao) %>%
  full_join(exports_PRC_Mainland) %>%
  mutate(`PRC, Hong Kong` = replace_na(`PRC, Hong Kong`,0),
         `PRC, Macao` = replace_na(`PRC, Macao`,0),
         `PRC, Mainland` = replace_na(`PRC, Mainland`,0),
         PRC = `PRC, Hong Kong` + `PRC, Macao` + `PRC, Mainland`) %>%
  select(c("country_name",
           "year",
           "PRC"))

#For now we're treating all NAs as zeroes. All relevant countries with missing data, that missing data is a small fraction of the total.
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports <- full_join(exports_PRC,
                                                                       exports_Iran) %>%
  full_join(exports_Russia) %>%
  full_join(exports_World) %>%
  mutate(PRC = replace_na(PRC, 0),
         Iran = replace_na(Iran, 0),
         Russia = replace_na(Russia, 0),
         world = replace_na(world, 0),
         FMIs = PRC + Iran + Russia,
         values = (FMIs/world)*100
  ) %>%
  select(c("country_name",
           "year",
           "values")) %>%
  mutate(country_name = case_when(country_name == "Armenia, Rep. of" ~ "Armenia",
                                  country_name == "Azerbaijan, Rep. of" ~ "Azerbaijan",
                                  country_name == "Belarus, Rep. of" ~ "Belarus",
                                  country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "Croatia, Rep. of" ~ "Croatia",
                                  country_name == "Czech Rep." ~ "Czechia",
                                  country_name == "Estonia, Rep. of" ~ "Estonia",
                                  country_name == "Kazakhstan, Rep. of" ~ "Kazakhstan",
                                  country_name == "Kosovo, Rep. of" ~ "Kosovo",
                                  country_name == "Kyrgyz Rep." ~ "Kyrgyzstan",
                                  country_name == "Moldova, Rep. of" ~ "Moldova",
                                  country_name == "Netherlands, The" ~ "Netherlands",
                                  country_name == "North Macedonia, Republic of" ~ "N. Macedonia",
                                  country_name == "Poland, Rep. of" ~ "Poland",
                                  country_name == "Russian Federation" ~ "Russia",
                                  country_name == "Serbia, Rep. of" ~ "Serbia",
                                  country_name == "Slovak Rep." ~ "Slovakia",
                                  country_name == "Slovenia, Rep. of" ~ "Slovenia",
                                  country_name == "Tajikistan, Rep. of" ~ "Tajikistan",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States" ~ "U.S.",
                                  country_name == "Uzbekistan, Rep. of" ~ "Uzbekistan",
                                  TRUE ~ country_name),
         year = as.numeric(year)) %>%
  pivot_wider(id_cols = country_name, 
              names_from = year, 
              values_from = values) %>%
  # mutate(`2024` = NA) %>% 
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "values") %>%
  mutate(indicator_name = "sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports",
         high_value_is_good_outcome_flag = 0,
         year = as.numeric(year))

import_objects <- ls(pattern = "^import")

# Remove each of these objects
rm(list = import_objects)
rm(import_objects)
#/////////////////////



# inspect
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports %>% glimpse()
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports %>% nrow() # 1278
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports %>% ncol() # 5


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# check
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports %>% count(year) %>% print(n = nrow(.))
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports %>% count(country_name) %>% print(n = nrow(.))

# inspect country names
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>%
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>%
  anti_join(., sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports, by = c("country" = "country_name")) %>% select(country)

sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports %>%
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join to country_crosswalk and fmir_framework
# need to update indicator_name and high_value_is_good_outcome_flag for records with NA values from country/year join
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports <- sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports %>%
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), .,
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports",
         high_value_is_good_outcome_flag = 0) %>%
  left_join(., fmir_framework, by = "indicator_name")


#///////////////////////////


# inspect
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports %>% glimpse()
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports %>% nrow() # 270
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports %>% ncol() # 36

# check country/year
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports %>% distinct(country) %>% nrow() # 45
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports %>% count(year) %>% print(n = nrow(.))
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports %>% count(country) %>% print(n = nrow(.))

# check missing
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports %>% skim(values)
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports %>% group_by(year) %>% skim(values)
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports %>% filter(is.na(values)) %>%
  select(country, year, values) %>% print(n = nrow(.))
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports %>% filter(is.na(values)) %>%
  select(country, year, values) %>% print(n = nrow(.))

sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports %>% filter(country == "Kosovo", year > 2016) %>%
  select(country, year, values)
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports %>% filter(country == "Turkmenistan", year > 2016) %>%
  select(country, year, values)




#///////////////////////


# plot
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1)

#/////////////////////////////////////////////////////////////////////////////////////////////////////////



# read/write
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports %>%
  write_csv(file = "data/fmir/sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports.csv")
sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports <- read.csv(file = "data/fmir/sub_obj_4_1_dots_exports_to_fmi_as_share_of_total_exports.csv") %>%
  as_tibble()


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
# load sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports ####



imports_World <- read_excel(path = "data/imf/DOTS_Exports_to_Counterpart_Countries_World.xlsx",
                            skip = 6) %>%
  rename(country_name = ...1) %>%
  select(-c("2014",
            "2015",
            "2016", 
            "2017")) %>% 
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "world")

imports_Iran <- read_excel(path = "data/imf/Exports_to_Counterpart_Countries_Iran.xlsx",
                            skip = 6) %>%
  rename(country_name = ...1) %>%
  select(-c("2014",
            "2015",
            "2016", 
            "2017"))

imports_Iran$`2018` <- as.numeric(gsub(" e", 
                                       "",
                                       imports_Iran$`2018`))
imports_Iran$`2019` <- as.numeric(gsub(" e", 
                                       "",
                                       imports_Iran$`2019`))
imports_Iran$`2020` <- as.numeric(gsub(" e", 
                                       "",
                                       imports_Iran$`2020`))
imports_Iran$`2021` <- as.numeric(gsub(" e", 
                                       "",
                                       imports_Iran$`2021`))
imports_Iran$`2022` <- as.numeric(gsub(" e", 
                                       "",
                                       imports_Iran$`2022`))
imports_Iran$`2023` <- as.numeric(gsub(" e", 
                                       "",
                                       imports_Iran$`2023`))

imports_Iran <- imports_Iran %>%
  mutate(`2018` = as.numeric(`2018`)) %>%
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "Iran")

imports_Russia <- read_excel(path = "data/imf/DOTS_Exports_to_Counterpart_Countries_Russia.xlsx",
                            skip = 6) %>%
  rename(country_name = ...1) %>%
  select(-c("2014",
            "2015",
            "2016", 
            "2017")) %>%
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "Russia")

imports_PRC_HK <- read_excel(path = "data/imf/Exports_to_Counterpart_Countries_PRC_HK.xlsx",
                            skip = 6) %>%
  rename(country_name = ...1) %>%
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "PRC, Hong Kong")

imports_PRC_Macao <- read_excel(path = "data/imf/Exports_to_Counterpart_Countries_PRC_Macao.xlsx",
                            skip = 6) %>%
  rename(country_name = ...1) %>%
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "PRC, Macao")

imports_PRC_Mainland <- read_excel(path = "data/imf/Exports_to_Counterpart_Countries_PRC_Mainland.xlsx",
                            skip = 6) %>%
  rename(country_name = ...1) %>%
  select(-c("2014",
            "2015",
            "2016", 
            "2017")) %>%
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "PRC, Mainland")

imports_PRC_HK$`PRC, Hong Kong`[is.na(imports_PRC_HK$`PRC, Hong Kong`)] <- 0
imports_PRC_Macao$`PRC, Macao`[is.na(imports_PRC_Macao$`PRC, Macao`)] <- 0
imports_PRC_Mainland$`PRC, Mainland`[is.na(imports_PRC_Mainland$`PRC, Mainland`)] <- 0


imports_PRC <- full_join(imports_PRC_HK,
                         imports_PRC_Macao) %>%
  full_join(imports_PRC_Mainland) %>%
  mutate(`PRC, Hong Kong` = replace_na(`PRC, Hong Kong`,0),
         `PRC, Macao` = replace_na(`PRC, Macao`,0),
         `PRC, Mainland` = replace_na(`PRC, Mainland`,0),
    PRC = `PRC, Hong Kong` + `PRC, Macao` + `PRC, Mainland`) %>%
  select(c("country_name",
           "year",
           "PRC"))

#For now we're treating all NAs as zeroes. All relevant countries with missing data, that missing data is a small fraction of the total.
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports <- full_join(imports_PRC,
                     imports_Iran) %>%
  full_join(imports_Russia) %>%
  full_join(imports_World) %>%
  mutate(PRC = replace_na(PRC, 0),
         Iran = replace_na(Iran, 0),
         Russia = replace_na(Russia, 0),
         world = replace_na(world, 0),
         FMIs = PRC + Iran + Russia,
         values = (FMIs/world)*100
         ) %>%
  select(c("country_name",
           "year",
           "values")) %>%
  mutate(country_name = case_when(country_name == "Armenia, Rep. of" ~ "Armenia",
                                  country_name == "Azerbaijan, Rep. of" ~ "Azerbaijan",
                                  country_name == "Belarus, Rep. of" ~ "Belarus",
                                  country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "Croatia, Rep. of" ~ "Croatia",
                                  country_name == "Czech Rep." ~ "Czechia",
                                  country_name == "Estonia, Rep. of" ~ "Estonia",
                                  country_name == "Kazakhstan, Rep. of" ~ "Kazakhstan",
                                  country_name == "Kosovo, Rep. of" ~ "Kosovo",
                                  country_name == "Kyrgyz Rep." ~ "Kyrgyzstan",
                                  country_name == "Moldova, Rep. of" ~ "Moldova",
                                  country_name == "Netherlands, The" ~ "Netherlands",
                                  country_name == "North Macedonia, Republic of" ~ "N. Macedonia",
                                  country_name == "Poland, Rep. of" ~ "Poland",
                                  country_name == "Russian Federation" ~ "Russia",
                                  country_name == "Serbia, Rep. of" ~ "Serbia",
                                  country_name == "Slovak Rep." ~ "Slovakia",
                                  country_name == "Slovenia, Rep. of" ~ "Slovenia",
                                  country_name == "Tajikistan, Rep. of" ~ "Tajikistan",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States" ~ "U.S.",
                                  country_name == "Uzbekistan, Rep. of" ~ "Uzbekistan",
                                  TRUE ~ country_name),
         year = as.numeric(year)) %>%
  pivot_wider(id_cols = country_name, 
              names_from = year, 
              values_from = values) %>%
  # mutate(`2024` = NA) %>% 
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "values") %>%
  mutate(indicator_name = "sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports",
         high_value_is_good_outcome_flag = 0,
         year = as.numeric(year))

import_objects <- ls(pattern = "^import")

# Remove each of these objects
rm(list = import_objects)
rm(import_objects)
#/////////////////////



# inspect
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports %>% glimpse()
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports %>% nrow() # 1477
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports %>% ncol() # 5


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# check
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports %>% count(year) %>% print(n = nrow(.))
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports %>% count(country_name) %>% print(n = nrow(.))

# inspect country names
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>%
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>%
  anti_join(., sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports, by = c("country" = "country_name")) %>% select(country)

sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports %>%
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join to country_crosswalk and fmir_framework
# need to update indicator_name and high_value_is_good_outcome_flag for records with NA values from country/year join
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports <- sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports %>%
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), .,
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports",
         high_value_is_good_outcome_flag = 0) %>%
  left_join(., fmir_framework, by = "indicator_name")


#///////////////////////////


# inspect
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports %>% glimpse()
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports %>% nrow() # 315
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports %>% ncol() # 36

# check country/year
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports %>% distinct(country) %>% nrow() # 45
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports %>% count(year) %>% print(n = nrow(.))
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports %>% count(country) %>% print(n = nrow(.))

# check missing
# note all the missing are from 2001-2006, so wont show up in final dataset ranging only from 2010-2020
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports %>% skim(values)
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports %>% group_by(year) %>% skim(values)
# note that kosovo is missing 2001-2006 
# montenegro is missing 2001-2005, serbia is missing 2001-2005,
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports %>% filter(is.na(values)) %>%
  select(country, year, values) %>% print(n = nrow(.))
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports %>% filter(is.na(values)) %>%
  select(country, year, values) %>% print(n = nrow(.))

sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports %>% filter(country == "Kosovo", year > 2016) %>%
  select(country, year, values)
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports %>% filter(country == "Turkmenistan", year > 2016) %>%
  select(country, year, values)




#///////////////////////


# plot
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1)

#/////////////////////////////////////////////////////////////////////////////////////////////////////////



# read/write
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports %>%
  write_csv(file = "data/fmir/sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports.csv")
sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports <- read.csv(file = "data/fmir/sub_obj_4_1_dots_imports_from_fmi_as_share_of_total_imports.csv") %>%
  as_tibble()


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
# load sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports ####

exports_World <- read_excel(path = "data/imf/DOTS_Imports_from_World_CIF.xlsx",
                            skip = 6) %>%
  rename(country_name = ...1) %>%
  select(c("country_name",
           "2018",
           "2019",
           "2020",
           "2021",
           "2022",
           "2023")) %>%
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "world")

exports_EU <- read_excel(path = "data/imf/DOTS_Exports_to_EU.xlsx",
                           skip = 6) %>%
  rename(country_name = ...1) 

exports_EU$`2018` <- as.numeric(gsub(" e", 
                                       "",
                                     exports_EU$`2018`))
exports_EU$`2019` <- as.numeric(gsub(" e", 
                                       "",
                                     exports_EU$`2019`))
exports_EU$`2020` <- as.numeric(gsub(" e", 
                                       "",
                                       exports_EU$`2020`))
exports_EU$`2021` <- as.numeric(gsub(" e", 
                                       "",
                                       exports_EU$`2021`))
exports_EU$`2022` <- as.numeric(gsub(" e", 
                                       "",
                                     exports_EU$`2022`))
exports_EU$`2023` <- as.numeric(gsub(" e", 
                                       "",
                                     exports_EU$`2023`))

exports_EU <- exports_EU %>%
  mutate(`2018` = as.numeric(`2018`))  %>%
  select(c("country_name",
           "2018",
           "2019",
           "2020",
           "2021",
           "2022",
           "2023")) %>%
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "EU")

#For now we're treating all NAs as zeroes. All relevant countries with missing data, that missing data is a small fraction of the total.
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports <- full_join(exports_EU,
                                                                      exports_World) %>%
  mutate(EU = replace_na(EU, 0),
         world = replace_na(world, 0),
         values = (EU/world)*100
  ) %>%
  select(c("country_name",
           "year",
           "values")) %>%
  mutate(country_name = case_when(country_name == "Armenia, Rep. of" ~ "Armenia",
                                  country_name == "Azerbaijan, Rep. of" ~ "Azerbaijan",
                                  country_name == "Belarus, Rep. of" ~ "Belarus",
                                  country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "Croatia, Rep. of" ~ "Croatia",
                                  country_name == "Czech Rep." ~ "Czechia",
                                  country_name == "Estonia, Rep. of" ~ "Estonia",
                                  country_name == "Kazakhstan, Rep. of" ~ "Kazakhstan",
                                  country_name == "Kosovo, Rep. of" ~ "Kosovo",
                                  country_name == "Kyrgyz Rep." ~ "Kyrgyzstan",
                                  country_name == "Moldova, Rep. of" ~ "Moldova",
                                  country_name == "Netherlands, The" ~ "Netherlands",
                                  country_name == "North Macedonia, Republic of" ~ "N. Macedonia",
                                  country_name == "Poland, Rep. of" ~ "Poland",
                                  country_name == "Russian Federation" ~ "Russia",
                                  country_name == "Serbia, Rep. of" ~ "Serbia",
                                  country_name == "Slovak Rep." ~ "Slovakia",
                                  country_name == "Slovenia, Rep. of" ~ "Slovenia",
                                  country_name == "Tajikistan, Rep. of" ~ "Tajikistan",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States" ~ "U.S.",
                                  country_name == "Uzbekistan, Rep. of" ~ "Uzbekistan",
                                  TRUE ~ country_name),
         year = as.numeric(year)) %>%
  pivot_wider(id_cols = country_name, 
              names_from = year, 
              values_from = values) %>%
  # mutate(`2024` = NA) %>% 
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "values") %>%
  mutate(indicator_name = "sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports",
         high_value_is_good_outcome_flag = 1,
         year = as.numeric(year))

import_objects <- ls(pattern = "^import")

# Remove each of these objects
rm(list = import_objects)
rm(import_objects)
#/////////////////////



# inspect
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports %>% glimpse()
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports %>% nrow() # 1278
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports %>% ncol() # 5


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# check
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports %>% count(year) %>% print(n = nrow(.))
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports %>% count(country_name) %>% print(n = nrow(.))

# inspect country names
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>%
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>%
  anti_join(., sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports, by = c("country" = "country_name")) %>% select(country)

sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports %>%
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join to country_crosswalk and fmir_framework
# need to update indicator_name and high_value_is_good_outcome_flag for records with NA values from country/year join
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports <- sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports %>%
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), .,
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")


#///////////////////////////


# inspect
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports %>% glimpse()
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports %>% nrow() # 270
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports %>% ncol() # 36

# check country/year
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports %>% distinct(country) %>% nrow() # 45
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports %>% count(year) %>% print(n = nrow(.))
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports %>% count(country) %>% print(n = nrow(.))

# check missing
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports %>% skim(values)
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports %>% group_by(year) %>% skim(values)
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports %>% filter(is.na(values)) %>%
  select(country, year, values) %>% print(n = nrow(.))
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports %>% filter(is.na(values)) %>%
  select(country, year, values) %>% print(n = nrow(.))

sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports %>% filter(country == "Kosovo", year > 2016) %>%
  select(country, year, values)
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports %>% filter(country == "Turkmenistan", year > 2016) %>%
  select(country, year, values)




#///////////////////////


# plot
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1)

#/////////////////////////////////////////////////////////////////////////////////////////////////////////



# read/write
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports %>%
  write_csv(file = "data/fmir/sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports.csv")
sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports <- read.csv(file = "data/fmir/sub_obj_4_1_dots_exports_to_eu_as_share_of_total_exports.csv") %>%
  as_tibble()


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////

# load sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports ####



imports_World <- read_excel(path = "data/imf/DOTS_Exports_to_Counterpart_Countries_World.xlsx",
                            skip = 6) %>%
  rename(country_name = ...1) %>%
  select(-c("2014",
            "2015",
            "2016", 
            "2017")) %>% 
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "world")

imports_EU <- read_excel(path = "data/imf/DOTS_Imports_from_EU.xlsx",
                           skip = 6) %>%
  rename(country_name = ...1) %>%
  select(-c("2014",
            "2015",
            "2016", 
            "2017"))

imports_EU$`2018` <- as.numeric(gsub(" e", 
                                       "",
                                     imports_EU$`2018`))
imports_EU$`2019` <- as.numeric(gsub(" e", 
                                       "",
                                     imports_EU$`2019`))
imports_EU$`2020` <- as.numeric(gsub(" e", 
                                       "",
                                     imports_EU$`2020`))
imports_EU$`2021` <- as.numeric(gsub(" e", 
                                       "",
                                     imports_EU$`2021`))
imports_EU$`2022` <- as.numeric(gsub(" e", 
                                       "",
                                       imports_EU$`2022`))
imports_EU$`2023` <- as.numeric(gsub(" e", 
                                       "",
                                       imports_EU$`2023`))

imports_EU <- imports_EU %>%
  mutate(`2018` = as.numeric(`2018`)) %>%
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "EU")

#For now we're treating all NAs as zeroes. All relevant countries with missing data, that missing data is a small fraction of the total.
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports <- full_join(imports_EU,
                                                                         imports_World) %>%
  mutate(EU = replace_na(EU, 0),
         world = replace_na(world, 0),
         values = (EU/world)*100
  ) %>%
  select(c("country_name",
           "year",
           "values")) %>%
  mutate(country_name = case_when(country_name == "Armenia, Rep. of" ~ "Armenia",
                                  country_name == "Azerbaijan, Rep. of" ~ "Azerbaijan",
                                  country_name == "Belarus, Rep. of" ~ "Belarus",
                                  country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "Croatia, Rep. of" ~ "Croatia",
                                  country_name == "Czech Rep." ~ "Czechia",
                                  country_name == "Estonia, Rep. of" ~ "Estonia",
                                  country_name == "Kazakhstan, Rep. of" ~ "Kazakhstan",
                                  country_name == "Kosovo, Rep. of" ~ "Kosovo",
                                  country_name == "Kyrgyz Rep." ~ "Kyrgyzstan",
                                  country_name == "Moldova, Rep. of" ~ "Moldova",
                                  country_name == "Netherlands, The" ~ "Netherlands",
                                  country_name == "North Macedonia, Republic of" ~ "N. Macedonia",
                                  country_name == "Poland, Rep. of" ~ "Poland",
                                  country_name == "Russian Federation" ~ "Russia",
                                  country_name == "Serbia, Rep. of" ~ "Serbia",
                                  country_name == "Slovak Rep." ~ "Slovakia",
                                  country_name == "Slovenia, Rep. of" ~ "Slovenia",
                                  country_name == "Tajikistan, Rep. of" ~ "Tajikistan",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States" ~ "U.S.",
                                  country_name == "Uzbekistan, Rep. of" ~ "Uzbekistan",
                                  TRUE ~ country_name),
         year = as.numeric(year)) %>%
  pivot_wider(id_cols = country_name, 
              names_from = year, 
              values_from = values) %>%
  pivot_longer(cols = `2018`:`2023`, 
               names_to = "year", 
               values_to = "values") %>%
  mutate(indicator_name = "sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports",
         high_value_is_good_outcome_flag = 1,
         year = as.numeric(year))

import_objects <- ls(pattern = "^import")

# Remove each of these objects
rm(list = import_objects)
rm(import_objects)
#/////////////////////



# inspect
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports %>% glimpse()
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports %>% nrow() # 1477
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports %>% ncol() # 5


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# check
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports %>% count(year) %>% print(n = nrow(.))
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports %>% count(country_name) %>% print(n = nrow(.))

# inspect country names
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>%
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>%
  anti_join(., sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports, by = c("country" = "country_name")) %>% select(country)

sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports %>%
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join to country_crosswalk and fmir_framework
# need to update indicator_name and high_value_is_good_outcome_flag for records with NA values from country/year join
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports <- sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports %>%
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), .,
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")


#///////////////////////////


# inspect
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports %>% glimpse()
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports %>% nrow() # 315
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports %>% ncol() # 36

# check country/year
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports %>% distinct(country) %>% nrow() # 45
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports %>% distinct(country, mcp_grouping, ee_region_flag) %>% print(n = nrow(.))
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports %>% count(year) %>% print(n = nrow(.))
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports %>% count(country) %>% print(n = nrow(.))

# check missing
# note all the missing are from 2001-2006, so wont show up in final dataset ranging only from 2010-2020
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports %>% skim(values)
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports %>% group_by(year) %>% skim(values)
# note that kosovo is missing 2001-2006 
# montenegro is missing 2001-2005, serbia is missing 2001-2005,
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports %>% filter(is.na(values)) %>%
  select(country, year, values) %>% print(n = nrow(.))
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports %>% filter(is.na(values)) %>%
  select(country, year, values) %>% print(n = nrow(.))

sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports %>% filter(country == "Kosovo", year > 2016) %>%
  select(country, year, values)
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports %>% filter(country == "Turkmenistan", year > 2016) %>%
  select(country, year, values)




#///////////////////////


# plot
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line(size = 1)

#/////////////////////////////////////////////////////////////////////////////////////////////////////////



# read/write
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports %>%
  write_csv(file = "data/fmir/sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports.csv")
sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports <- read.csv(file = "data/fmir/sub_obj_4_1_dots_imports_from_eu_as_share_of_total_imports.csv") %>%
  as_tibble()


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
# load sub_obj_3_1_iea_natural_gas_imports_from_FMI_as_share_of_natural_gas_imports ####

exclude <- c("Australia", "Canada", "Chile", "Colombia", "Israel", "Malta", "Mexico", "Switzerland", "Norway", "Korea", "Japan", "Russia", "Turkey")

ng_world <- read_excel(path = "data/iea/new/ImpData.xlsx") %>%
  filter(source == "World") %>%
  rename(World = values) %>%
  select(-source)
ng_fmi <- read_excel(path = "data/iea/new/ImpData.xlsx") %>%
  filter(source != "World") %>%
  group_by(year, country_name) %>%
  summarize(FMI = sum(values)) %>%
  ungroup() 
sub_obj_3_1_iea_natural_gas_imports_from_FMI_as_share_of_natural_gas_imports <- ng_fmi %>%
  full_join(ng_world,
            by = c("year",
                   "country_name")) %>%
  filter(!(country_name %in% exclude)) %>%
  mutate(FMI = case_when(
    is.na(FMI) ~ 0,
    TRUE ~ FMI),
    values = ((FMI/World)*100),
    values = case_when(
    values == "NaN" ~ 0,
    TRUE ~ values),
    indicator_name = "sub_obj_3_1_iea_natural_gas_imports_from_FMI_as_share_of_natural_gas_imports", 
    high_value_is_good_outcome_flag = 0) %>%
  select(-c("FMI",
            "World")) %>%
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  left_join(., fmir_framework, by = "indicator_name")


sub_obj_3_1_iea_natural_gas_imports_from_FMI_as_share_of_natural_gas_imports %>% anti_join(., country_crosswalk, by = c("country" = "country")) %>%
  distinct(country) %>% arrange(country) %>% print(n = nrow(.))

#////////////////////


# inspect
sub_obj_3_1_iea_natural_gas_imports_from_FMI_as_share_of_natural_gas_imports 
sub_obj_3_1_iea_natural_gas_imports_from_FMI_as_share_of_natural_gas_imports %>% glimpse()
sub_obj_3_1_iea_natural_gas_imports_from_FMI_as_share_of_natural_gas_imports %>% nrow() # 270
sub_obj_3_1_iea_natural_gas_imports_from_FMI_as_share_of_natural_gas_imports %>% ncol() # 36


# check missing
sub_obj_3_1_iea_natural_gas_imports_from_FMI_as_share_of_natural_gas_imports %>%
  skim(values)
sub_obj_3_1_iea_natural_gas_imports_from_FMI_as_share_of_natural_gas_imports %>%
  filter(is.na(values)) %>% select(country, year, values) %>% print(n = nrow(.))


# inspect values
sub_obj_3_1_iea_natural_gas_imports_from_FMI_as_share_of_natural_gas_imports %>% 
  filter(year > 2010) %>%
  filter(mcp_grouping == "E&E Balkans") %>%
  select(country, year,  
         values) %>%
  arrange(country, year) %>%
  print(n = nrow(.)) %>%
  identity()


# plot 
sub_obj_3_1_iea_natural_gas_imports_from_FMI_as_share_of_natural_gas_imports %>% 
  filter(year >= 2010) %>%
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + 
  geom_line(size = 1) 


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_3_1_iea_natural_gas_imports_from_FMI_as_share_of_natural_gas_imports %>%
         write_csv(file = "data/fmir/sub_obj_3_1_iea_natural_gas_imports_from_FMI_as_share_of_natural_gas_imports.csv")
sub_obj_3_1_iea_natural_gas_imports_from_FMI_as_share_of_natural_gas_imports <- read_csv(file = "data/fmir/sub_obj_3_1_iea_natural_gas_imports_from_FMI_as_share_of_natural_gas_imports.csv", lazy = FALSE)

# inspect
sub_obj_3_1_iea_natural_gas_imports_from_FMI_as_share_of_natural_gas_imports
sub_obj_3_1_iea_natural_gas_imports_from_FMI_as_share_of_natural_gas_imports %>% glimpse()
sub_obj_3_1_iea_natural_gas_imports_from_FMI_as_share_of_natural_gas_imports %>% nrow() # 315
sub_obj_3_1_iea_natural_gas_imports_from_FMI_as_share_of_natural_gas_imports %>% ncol() # 36


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////

# load sub_obj_4_2_imf_financial_markets_institutions ####

sub_obj_4_2_imf_financial_markets_institutions <- read_excel(path = "data/imf/Financial_Development_FD_edited.xlsx") %>%
  select(c(country_name,
           `2018`,
           `2019`,
           `2020`,
           `2021`,
           `2022`,
           `2023`)) %>%
  pivot_longer(cols = c(`2018`:`2023`), 
               names_to = "year", 
               values_to = "values") %>%
  mutate(high_value_is_good_outcome_flag = 1,
         indicator_name = "sub_obj_4_2_imf_financial_markets_institutions",
         year = as.numeric(year))


# inspect
sub_obj_4_2_imf_financial_markets_institutions
sub_obj_4_2_imf_financial_markets_institutions %>% glimpse()
sub_obj_4_2_imf_financial_markets_institutions %>% nrow() # 301
sub_obj_4_2_imf_financial_markets_institutions %>% ncol() # 5
sub_obj_4_2_imf_financial_markets_institutions %>% skim()
sub_obj_4_2_imf_financial_markets_institutions %>% group_by(year) %>% skim()

#missing Kosovo
#sub_obj_4_2_imf_financial_markets_institutions %>% filter(country_name == "Kosovo") %>% arrange(year)
sub_obj_4_2_imf_financial_markets_institutions %>% arrange(values)
sub_obj_4_2_imf_financial_markets_institutions %>% arrange(desc(values))
sub_obj_4_2_imf_financial_markets_institutions %>% filter(is.na(values)) %>% count(country_name) %>% arrange(desc(n))
sub_obj_4_2_imf_financial_markets_institutions %>%
  filter(country_name %in%
           (country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S.") %>%
              distinct(country) %>% pull(country))) %>%
  filter(is.na(values)) %>% count(country_name)

# inspect country names
sub_obj_4_2_imf_financial_markets_institutions %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>%
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_4_2_imf_financial_markets_institutions <- sub_obj_4_2_imf_financial_markets_institutions %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(high_value_is_good_outcome_flag = 1,
         indicator_name = "sub_obj_4_2_imf_financial_markets_institutions") %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_4_2_imf_financial_markets_institutions 
sub_obj_4_2_imf_financial_markets_institutions %>% glimpse()
sub_obj_4_2_imf_financial_markets_institutions %>% nrow() # 315
sub_obj_4_2_imf_financial_markets_institutions %>% ncol() # 40
sub_obj_4_2_imf_financial_markets_institutions %>% distinct(country) %>% nrow() # 45

sub_obj_4_2_imf_financial_markets_institutions %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_4_2_imf_financial_markets_institutions %>% filter(is.na(values), year >= 2018) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_4_2_imf_financial_markets_institutions %>% skim(values)
sub_obj_4_2_imf_financial_markets_institutions %>% group_by(year) %>% skim(values)
sub_obj_4_2_imf_financial_markets_institutions %>% filter(year >= 2018) %>% skim(values)
sub_obj_4_2_imf_financial_markets_institutions %>% filter(year >= 2018) %>% group_by(country) %>% skim(values)


# inspect interpolation
sub_obj_4_2_imf_financial_markets_institutions %>% filter(year >= 2018) %>%
  select(country, year, values) %>% print(n = 50)

sub_obj_4_2_imf_financial_markets_institutions %>% filter(is.na(values), year >= 2018) %>% 
  count(country, mcp_grouping) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
sub_obj_4_2_imf_financial_markets_institutions %>% filter(is.na(values),
                                                mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                                year >= 2018) %>%
  select(country, mcp_grouping, year, values) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
sub_obj_4_2_imf_financial_markets_institutions %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_4_2_imf_financial_markets_institutions %>% filter(year >= 2018) %>% group_by(mcp_grouping) %>% skim(values)


sub_obj_4_2_imf_financial_markets_institutions$year <- as.numeric(sub_obj_4_2_imf_financial_markets_institutions$year)

# plot
sub_obj_4_2_imf_financial_markets_institutions %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = jitter(values, 1), color = country)) + geom_line(size = 1)


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_4_2_imf_financial_markets_institutions %>% write_csv(file = "data/fmir/sub_obj_4_2_imf_financial_markets_institutions.csv")
sub_obj_4_2_imf_financial_markets_institutions <- read_csv(file = "data/fmir/sub_obj_4_2_imf_financial_markets_institutions.csv")

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////

rm(list = ls(pattern = "^(exports|ng)"))
