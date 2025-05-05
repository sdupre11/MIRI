# load sub_obj_4_3_knomad_bilateral_fmi_remittances ####
sub_obj_4_3_knomad_bilateral_fmi_remittances <- read_excel(path = "data/knomad/brm/bilateral_remittance_matrix_2021.xlsx", 
                                                 sheet = "FINAL BRM 2021 ") %>%
  pivot_longer(cols = Afghanistan:Zimbabwe, 
               names_to = "country_name", 
               values_to = "values") %>%
  select(-c(Source)) %>%
  mutate(
    year = 2021
  ) %>% 
  pivot_wider(id_cols = country_name, 
              names_from = year, 
              values_from = values) %>%
  mutate(
    `2023` = NA,
    `2022` = NA,
    `2020` = NA,
    `2019` = NA,
    `2018` = NA
  ) %>%
  mutate(
    `2022` = case_when(
      country_name == "Ukraine" ~ NA,
      TRUE ~ `2022`
    ),
    `2023` = case_when(
      country_name == "Ukraine" ~ NA,
      TRUE ~ `2023`
    )) %>%
  pivot_longer(cols = c(`2018`,`2019`,`2020`,`2021`,`2022`,`2023`), 
               names_to = "year", 
               values_to = "values") %>%
  mutate(high_value_is_good_outcome_flag = 0,
         indicator_name = "sub_obj_4_3_knomad_bilateral_fmi_remittances",
         country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  country_name == "Republic of Moldova" ~ "Moldova",
                                  country_name == "United Kingdom of Great Britain and Northern Ireland" ~ "U.K.",
                                  country_name == "United Kingdom" ~ "U.K.",
                                  country_name == "United States of America" ~ "U.S.",
                                  country_name == "United States" ~ "U.S.",
                                  country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                  country_name == "Slovak Republic" ~ "Slovakia",
                                  TRUE ~ country_name),
         year = as.numeric(year))


# inspect
sub_obj_4_3_knomad_bilateral_fmi_remittances
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% glimpse()
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% nrow() # 1498
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% ncol() # 5
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% skim()
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% group_by(year) %>% skim()

#missing Kosovo
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% filter(country_name == "Kosovo") %>% arrange(year)
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% arrange(values)
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% arrange(desc(values))
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% filter(is.na(values)) %>% count(country_name) %>% arrange(desc(n))
sub_obj_4_3_knomad_bilateral_fmi_remittances %>%
  filter(country_name %in%
           (country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S.") %>%
              distinct(country) %>% pull(country))) %>%
  filter(is.na(values)) %>% count(country_name)

# inspect country names
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>%
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_4_3_knomad_bilateral_fmi_remittances <- sub_obj_4_3_knomad_bilateral_fmi_remittances %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_4_3_knomad_bilateral_fmi_remittances 
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% glimpse()
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% nrow() # 315
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% ncol() # 39
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% distinct(country) %>% nrow() # 45

sub_obj_4_3_knomad_bilateral_fmi_remittances %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% filter(is.na(values), year >= 2018) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% skim(values)
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% group_by(year) %>% skim(values)
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% filter(year >= 2018) %>% skim(values)
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% filter(year >= 2018) %>% group_by(country) %>% skim(values)


# inspect interpolation
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% filter(year >= 2018) %>%
  select(country, year, values) %>% print(n = 50)

# Missing values for all EU-15 and US
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% filter(is.na(values), year >= 2018) %>% 
  count(country, mcp_grouping) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% filter(is.na(values),
                                              mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                              year >= 2018) %>%
  select(country, mcp_grouping, year, values) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% filter(year >= 2018) %>% group_by(mcp_grouping) %>% skim(values)


sub_obj_4_3_knomad_bilateral_fmi_remittances$year <- as.numeric(sub_obj_4_3_knomad_bilateral_fmi_remittances$year)

# plot
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% 
  filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = jitter(values, 1), color = country)) + geom_line(size = 1)


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_4_3_knomad_bilateral_fmi_remittances %>% write_csv(file = "data/fmir/sub_obj_4_3_knomad_bilateral_fmi_remittances.csv")
sub_obj_4_3_knomad_bilateral_fmi_remittances <- read_csv(file = "data/fmir/sub_obj_4_3_knomad_bilateral_fmi_remittances.csv")


