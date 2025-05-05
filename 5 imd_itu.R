# load sub_obj_3_1_imd_usaid_indigenous_energy_production ####
#Units are KTOE, not that it matters

sub_obj_3_1_imd_usaid_indigenous_energy_production <- fread("data/imd/iea_wes/WORLDBAL.txt") %>%
  filter(V3 >= 2018 & V2 == "TOTAL" & V3 != 2023) 

backup <- sub_obj_3_1_imd_usaid_indigenous_energy_production
sub_obj_3_1_imd_usaid_indigenous_energy_production <- backup

iea_regions <- c("AFRICA",
                 "ASIA",
                 "WORLD",
                 "WORLDAV",
                 "WORLDMAR",
                 "UNASIATOT",
                 "UNEUROPE",
                 "UNOCEANIA",
                 "UNAFRICA",
                 "UNAMERICAS", 
                 "OTHERAFRIC",
                 "OPEC",
                 "OTHERASIA",
                 "OTHERLATIN",
                 "OECDTOT",
                 "OECDEUR",
                 "OECDAM",
                 "OECDAO",
                 "NOECDTOT",
                 "MIDEAST",
                 "MYUGO",
                 "MG7",
                 "MG8",
                 "MG20",
                 "IEATOT",
                 "IEAFAMILY",
                 "FSUND",
                 "EU27_2020",
                 "EU28",
                 "MFSU15",
                 "MASEAN",
                 "LATAMER",
                 "EURASIA",
                 "YUGOND",
                 "GIBRALTAR")

sub_obj_3_1_imd_usaid_indigenous_energy_production <- sub_obj_3_1_imd_usaid_indigenous_energy_production %>%
  rename(location = V1,
         unsure1 = V2,
         year = V3,
         variable = V4,
         unit = V5,
         values = V6) %>%
  filter(!location %in% iea_regions) %>%
  filter(variable %in% c("TFC",
                        "TOTTRANS",
                        "OWNUSE",
                        "INDPROD")) %>%
  mutate(
    values = case_when(
      values == ".." ~ NA,
      TRUE ~ values)
  ) %>%
  filter(unit == "KTOE") %>%
  select(-c("unit", 
            "unsure1")) %>%
  pivot_wider(id_cols = c(location,
                          year),
              names_from = variable, 
              values_from = values)

sub_obj_3_1_imd_usaid_indigenous_energy_production$denominator <- as.numeric(sub_obj_3_1_imd_usaid_indigenous_energy_production$TFC) - as.numeric(sub_obj_3_1_imd_usaid_indigenous_energy_production$OWNUSE) + as.numeric(sub_obj_3_1_imd_usaid_indigenous_energy_production$TOTTRANS)

sub_obj_3_1_imd_usaid_indigenous_energy_production$values <- as.numeric(sub_obj_3_1_imd_usaid_indigenous_energy_production$INDPROD) / as.numeric(sub_obj_3_1_imd_usaid_indigenous_energy_production$denominator)

  
sub_obj_3_1_imd_usaid_indigenous_energy_production <- sub_obj_3_1_imd_usaid_indigenous_energy_production %>%
  select(c("location",
           "year",
           "values")) %>%
  mutate(values = values*100) %>%
  rename(country_name = location) %>%
  mutate(
    country_name = case_when(
      country_name == "ALBANIA" ~ "Albania",
      country_name == "ARMENIA" ~ "Armenia",
      country_name == "AUSTRIA" ~ "Austria",
      country_name == "AZERBAIJAN" ~ "Azerbaijan",
      country_name == "BELARUS" ~ "Belarus",
      country_name == "BELGIUM" ~ "Belgium",
      country_name == "BOSNIAHERZ" ~ "BiH",
      country_name == "BULGARIA" ~ "Bulgaria",
      country_name == "CROATIA" ~ "Croatia",
      country_name == "CZECH" ~ "Czechia",
      country_name == "DENMARK" ~ "Denmark",
      country_name == "ESTONIA" ~ "Estonia",
      country_name == "FINLAND" ~ "Finland",
      country_name == "FRANCE" ~ "France",
      country_name == "GEORGIA" ~ "Georgia",
      country_name == "GERMANY" ~ "Germany",
      country_name == "GREECE" ~ "Greece",
      country_name == "HUNGARY" ~ "Hungary",
      country_name == "IRELAND" ~ "Ireland",
      country_name == "ITALY" ~ "Italy",
      country_name == "KAZAKHSTAN" ~ "Kazakhstan",
      country_name == "KOSOVO" ~ "Kosovo",
      country_name == "KYRGYZSTAN" ~ "Kyrgyzstan",
      country_name == "LATVIA" ~ "Latvia",
      country_name == "LITHUANIA" ~ "Lithuania",
      country_name == "LUXEMBOU" ~ "Luxembourg",
      country_name == "MOLDOVA" ~ "Moldova",
      country_name == "MONTENEGRO" ~ "Montenegro",
      country_name == "NORTHMACED" ~ "N. Macedonia",
      country_name == "NETHLAND" ~ "Netherlands",
      country_name == "POLAND" ~ "Poland",
      country_name == "PORTUGAL" ~ "Portugal",
      country_name == "ROMANIA" ~ "Romania",
      country_name == "RUSSIA" ~ "Russia",
      country_name == "SERBIA" ~ "Serbia",
      country_name == "SLOVAKIA" ~ "Slovakia",
      country_name == "SLOVENIA" ~ "Slovenia",
      country_name == "SPAIN" ~ "Spain",
      country_name == "SWEDEN" ~ "Sweden",
      country_name == "TAJIKISTAN" ~ "Tajikistan",
      country_name == "TURKMENIST" ~ "Turkmenistan",
      country_name == "UK" ~ "U.K.",
      country_name == "UKRAINE" ~ "Ukraine",
      country_name == "USA" ~ "U.S.",
      country_name == "UZBEKISTAN" ~ "Uzbekistan",
      TRUE ~ "REMOVE"
    )
  ) %>%
  filter(country_name != "REMOVE") %>%
  pivot_wider(
    id_cols = country_name,
    names_from = year, 
    values_from = values
  ) %>%
  mutate(
    `2023` = NA
  ) %>%
  pivot_longer(cols = c(`2018`, `2019`, `2020`, `2021`, `2022`, `2023`), 
               names_to = "year", 
               values_to = "values") %>%
  mutate(indicator_name = "sub_obj_3_1_imd_usaid_indigenous_energy_production",
         high_value_is_good_outcome_flag = 1,
         year = as.numeric(year))

# inspect
sub_obj_3_1_imd_usaid_indigenous_energy_production
sub_obj_3_1_imd_usaid_indigenous_energy_production %>% glimpse()
sub_obj_3_1_imd_usaid_indigenous_energy_production %>% nrow() # 315
sub_obj_3_1_imd_usaid_indigenous_energy_production %>% ncol() # 5
sub_obj_3_1_imd_usaid_indigenous_energy_production %>% skim()
sub_obj_3_1_imd_usaid_indigenous_energy_production %>% group_by(year) %>% skim()

#missing Kosovo
sub_obj_3_1_imd_usaid_indigenous_energy_production %>% arrange(values)
sub_obj_3_1_imd_usaid_indigenous_energy_production %>% arrange(desc(values))
sub_obj_3_1_imd_usaid_indigenous_energy_production %>% filter(is.na(values)) %>% count(country_name) %>% arrange(desc(n))
sub_obj_3_1_imd_usaid_indigenous_energy_production %>%
  filter(country_name %in%
           (country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S.") %>%
              distinct(country) %>% pull(country))) %>%
  filter(is.na(values)) %>% count(country_name)

# inspect country names
sub_obj_3_1_imd_usaid_indigenous_energy_production %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>%
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_3_1_imd_usaid_indigenous_energy_production <- sub_obj_3_1_imd_usaid_indigenous_energy_production %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_3_1_imd_usaid_indigenous_energy_production",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_3_1_imd_usaid_indigenous_energy_production 
sub_obj_3_1_imd_usaid_indigenous_energy_production %>% glimpse()
sub_obj_3_1_imd_usaid_indigenous_energy_production %>% nrow() # 315
sub_obj_3_1_imd_usaid_indigenous_energy_production %>% ncol() # 40
sub_obj_3_1_imd_usaid_indigenous_energy_production %>% distinct(country) %>% nrow() # 45

sub_obj_3_1_imd_usaid_indigenous_energy_production %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_3_1_imd_usaid_indigenous_energy_production %>% filter(is.na(values), year >= 2018) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_3_1_imd_usaid_indigenous_energy_production %>% skim(values)
sub_obj_3_1_imd_usaid_indigenous_energy_production %>% group_by(year) %>% skim(values)
sub_obj_3_1_imd_usaid_indigenous_energy_production %>% filter(year >= 2018) %>% skim(values)
sub_obj_3_1_imd_usaid_indigenous_energy_production %>% filter(year >= 2018) %>% group_by(country) %>% skim(values)


# inspect interpolation
sub_obj_3_1_imd_usaid_indigenous_energy_production %>% filter(year >= 2018) %>%
  select(country, year, values) %>% print(n = 50)

sub_obj_3_1_imd_usaid_indigenous_energy_production %>% filter(is.na(values), year >= 2018) %>% 
  count(country, mcp_grouping) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
sub_obj_3_1_imd_usaid_indigenous_energy_production %>% filter(is.na(values),
                                             mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                             year >= 2018) %>%
  select(country, mcp_grouping, year, values) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
sub_obj_3_1_imd_usaid_indigenous_energy_production %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_3_1_imd_usaid_indigenous_energy_production %>% filter(year >= 2018) %>% group_by(mcp_grouping) %>% skim(values)


sub_obj_3_1_imd_usaid_indigenous_energy_production$year <- as.numeric(sub_obj_3_1_imd_usaid_indigenous_energy_production$year)

# plot
sub_obj_3_1_imd_usaid_indigenous_energy_production %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = jitter(values, 1), color = country)) + geom_line(size = 1)


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_3_1_imd_usaid_indigenous_energy_production %>% write_csv(file = "data/fmir/sub_obj_3_1_imd_usaid_indigenous_energy_production.csv")
sub_obj_3_1_imd_usaid_indigenous_energy_production <- read_csv(file = "data/fmir/sub_obj_3_1_imd_usaid_indigenous_energy_production.csv")

rm(backup)
rm(iea_regions)

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
# load sub_obj_3_2_itu_regulatory_authority  ####

sub_obj_3_2_itu_regulatory_authority <- read_excel(path = "data/itu/ict_regulatory_tracker/ICTRegulatoryTracker2007-2022_share_edited.xlsx", 
                                                         sheet = "Tracker 2007-2022") %>%
  filter(year %in% 2018:2023) %>%
  pivot_wider(id_cols = country_name, 
              names_from = year, 
              values_from = values) %>%
  mutate(
    `2021` = NA,
    `2023` = NA
  ) %>%
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
         indicator_name = "sub_obj_3_2_itu_regulatory_authority",
         year = as.numeric(year),
         country_name = case_when(
           country_name == "Bosnia and Herzegovina" ~ "BiH",
           country_name == "Czech Republic" ~ "Czechia",
           country_name == "North Macedonia" ~ "N. Macedonia",
           country_name == "Russian Federation" ~ "Russia",
           country_name == "United Kingdom" ~ "U.K.",
           country_name == "United States" ~ "U.S.",
           TRUE ~ country_name
         ))


# inspect
sub_obj_3_2_itu_regulatory_authority
sub_obj_3_2_itu_regulatory_authority %>% glimpse()
sub_obj_3_2_itu_regulatory_authority %>% nrow() # 1351
sub_obj_3_2_itu_regulatory_authority %>% ncol() # 5
sub_obj_3_2_itu_regulatory_authority %>% skim()
sub_obj_3_2_itu_regulatory_authority %>% group_by(year) %>% skim()

#missing Kosovo
sub_obj_3_2_itu_regulatory_authority %>% arrange(values)
sub_obj_3_2_itu_regulatory_authority %>% arrange(desc(values))
sub_obj_3_2_itu_regulatory_authority %>% filter(is.na(values)) %>% count(country_name) %>% arrange(desc(n))
sub_obj_3_2_itu_regulatory_authority %>%
  filter(country_name %in%
           (country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S.") %>%
              distinct(country) %>% pull(country))) %>%
  filter(is.na(values)) %>% count(country_name)

# inspect country names
sub_obj_3_2_itu_regulatory_authority %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>%
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))

#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_3_2_itu_regulatory_authority <- sub_obj_3_2_itu_regulatory_authority %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_3_2_itu_regulatory_authority",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_3_2_itu_regulatory_authority 
sub_obj_3_2_itu_regulatory_authority %>% glimpse()
sub_obj_3_2_itu_regulatory_authority %>% nrow() # 315
sub_obj_3_2_itu_regulatory_authority %>% ncol() # 40
sub_obj_3_2_itu_regulatory_authority %>% distinct(country) %>% nrow() # 45

sub_obj_3_2_itu_regulatory_authority %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_3_2_itu_regulatory_authority %>% filter(is.na(values), year >= 2018) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_3_2_itu_regulatory_authority %>% skim(values)
sub_obj_3_2_itu_regulatory_authority %>% group_by(year) %>% skim(values)
sub_obj_3_2_itu_regulatory_authority %>% filter(year >= 2018) %>% skim(values)
sub_obj_3_2_itu_regulatory_authority %>% filter(year >= 2018) %>% group_by(country) %>% skim(values)


# inspect interpolation
sub_obj_3_2_itu_regulatory_authority %>% filter(year >= 2018) %>%
  select(country, year, values) %>% print(n = 50)

sub_obj_3_2_itu_regulatory_authority %>% filter(is.na(values), year >= 2018) %>% 
  count(country, mcp_grouping) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
sub_obj_3_2_itu_regulatory_authority %>% filter(is.na(values),
                                                      mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                                      year >= 2018) %>%
  select(country, mcp_grouping, year, values) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
sub_obj_3_2_itu_regulatory_authority %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_3_2_itu_regulatory_authority %>% filter(year >= 2018) %>% group_by(mcp_grouping) %>% skim(values)


sub_obj_3_2_itu_regulatory_authority$year <- as.numeric(sub_obj_3_2_itu_regulatory_authority$year)

# plot
sub_obj_3_2_itu_regulatory_authority %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = jitter(values, 1), color = country)) + geom_line(size = 1)


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_3_2_itu_regulatory_authority %>% write_csv(file = "data/fmir/sub_obj_3_2_itu_regulatory_authority.csv")
sub_obj_3_2_itu_regulatory_authority <- read_csv(file = "data/fmir/sub_obj_3_2_itu_regulatory_authority.csv")

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


