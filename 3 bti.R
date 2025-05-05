# load sub_obj_2_3_bti_assembly_rights ####

# https://www.bti-project.org/en/index/political-transformation.html
# https://www.bti-project.org/en/methodology.html
# https://www.bti-project.org/content/en/downloads/codebooks/BTI_2020_Codebook.pdf


##Note from Steve
# note that BTI 2020 Codebook instructs reviewers to base their assessments "from 2017 to 2019" and 
# that "Your responses to the questions provided in the questionnaire should reflect the situation in your 
# country at the end of January 2019." 
# this seems to imply that the BTI 2020 edition data represents actual conditions for 2018, since it stopped measurement period
# as of january 2019;
# however, Eda told me, and we confirmed, that J2SR lags BTI edition year by 1 year to get actual year represented
# eg in "J2SR FY21 Control Figures Data" spreadsheet, go to "transformation & scaling" tab, filter to Albania, and
# metric = Environmental Policy (from BTI), and see that j2sr gives albania a 4 for 2005 and a 5 for 2007 and 
# in BTI data albania has a 4 for BTI 2006 edition, and a 5 for BTI 2008 edition
# so BTI 2020 edition data is determined and manually set to represent actual year 2019
# eda and i then both use the same method to interpolate the gaps, by taking midpoint between observations
# so that albania 2006 actual conditions is set equal to midpoint of 4 and 5, so 4.5


# load bti data by year tabs
bti_assembly_rights_2023 <- read_excel(path = "data/bs/bti/BTI_2006-2024_Scores.xlsx", sheet = "BTI 2024", na = c("", "-"))

colnames(bti_assembly_rights_2023)[1] <- "country_name"

bti_assembly_rights_2023 <- bti_assembly_rights_2023 %>%
  rename(assembly_rights = `Q2.3 | Association / assembly rights`) %>% 
  mutate(year = 2023) %>% select(country_name, year, assembly_rights)

bti_assembly_rights_2021 <- read_excel(path = "data/bs/bti/BTI_2006-2024_Scores.xlsx", sheet = "BTI 2022", na = c("", "-"))

colnames(bti_assembly_rights_2021)[1] <- "country_name"

bti_assembly_rights_2021 <- bti_assembly_rights_2021 %>%
  rename(assembly_rights = `Q2.3 | Association / assembly rights`) %>% 
  mutate(year = 2021) %>% select(country_name, year, assembly_rights)

bti_assembly_rights_2019 <- read_excel(path = "data/bs/bti/BTI_2006-2024_Scores.xlsx", sheet = "BTI 2020", na = c("", "-"))

colnames(bti_assembly_rights_2019)[1] <- "country_name"

bti_assembly_rights_2019 <- bti_assembly_rights_2019 %>%
  rename(assembly_rights = `Q2.3 | Association / assembly rights`) %>% 
  mutate(year = 2019) %>% select(country_name, year, assembly_rights)



#/////////////////////


# combine years
sub_obj_2_3_bti_assembly_rights <- #bti_assembly_rights_2017 %>%
  bti_assembly_rights_2019 %>%
  bind_rows(bti_assembly_rights_2021,
            bti_assembly_rights_2023) %>%  
  pivot_wider(id_cols = country_name, names_from = year, values_from = assembly_rights) %>%
  # mutate(`2018` = ((`2019` + `2017`)/2),
  #        `2020` = ((`2021` + `2019`)/2),
  #        `2022` = ((`2023` + `2021`)/2),
  #        `2024` = `2023`) %>%
  # dplyr::select(-c(`2017`)) %>%
  pivot_longer(cols = -country_name, names_to = "year", values_to = "values") %>%
  mutate(country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  TRUE ~ country_name),
         year = as.numeric(year),
         indicator_name = "sub_obj_2_3_bti_assembly_rights",
         high_value_is_good_outcome_flag = 1) %>%
  arrange(country_name,
          year)

rm(list = ls(pattern = "^bti"))
#//////////////////////////


# inspect
sub_obj_2_3_bti_assembly_rights
sub_obj_2_3_bti_assembly_rights %>% glimpse()
sub_obj_2_3_bti_assembly_rights %>% nrow() # 959
sub_obj_2_3_bti_assembly_rights %>% ncol() # 5
sub_obj_2_3_bti_assembly_rights %>% skim()
sub_obj_2_3_bti_assembly_rights %>% group_by(year) %>% skim()

##Steve's note below is no longer relevant as we're only doing 2018 onwards
# note kosovo is missing first two years in raw data, which means first 4 years in interpolated data, 
# all other ee_presence countries have no missing values
sub_obj_2_3_bti_assembly_rights %>% filter(country_name == "Kosovo") %>% arrange(year)
sub_obj_2_3_bti_assembly_rights %>% arrange(values)
sub_obj_2_3_bti_assembly_rights %>% arrange(desc(values))
sub_obj_2_3_bti_assembly_rights %>% filter(is.na(values)) %>% count(country_name) %>% arrange(desc(n))
sub_obj_2_3_bti_assembly_rights %>% 
  filter(country_name %in% 
           (country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
              distinct(country) %>% pull(country))) %>% 
  filter(is.na(values)) %>% count(country_name)

# inspect country names
sub_obj_2_3_bti_assembly_rights %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
sub_obj_2_3_bti_assembly_rights %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_2_3_bti_assembly_rights <- sub_obj_2_3_bti_assembly_rights %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_2_3_bti_assembly_rights",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_2_3_bti_assembly_rights 
sub_obj_2_3_bti_assembly_rights %>% glimpse()
sub_obj_2_3_bti_assembly_rights %>% nrow() # 315
sub_obj_2_3_bti_assembly_rights %>% ncol() # 39
sub_obj_2_3_bti_assembly_rights %>% distinct(country) %>% nrow() # 45

sub_obj_2_3_bti_assembly_rights %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_2_3_bti_assembly_rights %>% filter(is.na(values), year >= 2018) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_2_3_bti_assembly_rights %>% skim(values)
sub_obj_2_3_bti_assembly_rights %>% group_by(year) %>% skim(values)
sub_obj_2_3_bti_assembly_rights %>% filter(year >= 2018) %>% skim(values)
sub_obj_2_3_bti_assembly_rights %>% filter(year >= 2018) %>% group_by(country) %>% skim(values)


# inspect interpolation
sub_obj_2_3_bti_assembly_rights %>% filter(year >= 2018) %>%
  select(country, year, values) %>% print(n = 50)

# Missing values for all EU-15 and US
sub_obj_2_3_bti_assembly_rights %>% filter(is.na(values), year >= 2018) %>% 
  count(country, mcp_grouping) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
sub_obj_2_3_bti_assembly_rights %>% filter(is.na(values),
                                           mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                           year >= 2018) %>%
  select(country, mcp_grouping, year, values) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
sub_obj_2_3_bti_assembly_rights %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_2_3_bti_assembly_rights %>% filter(year >= 2018) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_2_3_bti_assembly_rights %>% 
  #filter(mcp_grouping == "E&E Balkans") %>%
  filter(mcp_grouping == "E&E Eurasia") %>%
  #filter(mcp_grouping == "E&E graduates") %>%
  #filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  # ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line() 
  ggplot(data = ., mapping = aes(x = year, y = jitter(values, 1), color = country)) + geom_line(size = 1)


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_2_3_bti_assembly_rights %>% write_csv(file = "data/fmir/sub_obj_2_3_bti_assembly_rights.csv")
sub_obj_2_3_bti_assembly_rights <- read_csv(file = "data/fmir/sub_obj_2_3_bti_assembly_rights.csv")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////

# load sub_obj_1_3_bti_free_and_fair_elections ####

# https://www.bti-project.org/en/index/political-transformation.html
# https://www.bti-project.org/en/methodology.html
# https://www.bti-project.org/content/en/downloads/codebooks/BTI_2020_Codebook.pdf


##Note from Steve
# note that BTI 2020 Codebook instructs reviewers to base their assessments "from 2017 to 2019" and 
# that "Your responses to the questions provided in the questionnaire should reflect the situation in your 
# country at the end of January 2019." 
# this seems to imply that the BTI 2020 edition data represents actual conditions for 2018, since it stopped measurement period
# as of january 2019;
# however, eda told me, and we confirmed, that J2SR lags BTI edition year by 1 year to get actual year represented
# eg in "J2SR FY21 Control Figures Data" spreadsheet, go to "transormation & scaling" tab, filter to Albania, and
# metric = Environmental Policy (from BTI), and see that j2sr gives albania a 4 for 2005 and a 5 for 2007 and 
# in BTI data albania has a 4 for BTI 2006 edition, and a 5 for BTI 2008 edition
# so BTI 2020 edition data is determined and manually set to represent actual year 2019
# eda and i then both use the same method to interpolate the gaps, by taking midpoint between observations
# so that albania 2006 actual conditions is set equal to midpoint of 4 and 5, so 4.5


# load bti data by year tabs
bti_free_and_fair_2023 <- read_excel(path = "data/bs/bti/BTI_2006-2024_Scores.xlsx", sheet = "BTI 2024", na = c("", "-"))

colnames(bti_free_and_fair_2023)[1] <- "country_name"

bti_free_and_fair_2023 <- bti_free_and_fair_2023 %>%
  rename(free_and_fair = `Q2.1 | Free and fair elections`) %>% 
  mutate(year = 2023) %>% select(country_name, year, free_and_fair)

bti_free_and_fair_2021 <- read_excel(path = "data/bs/bti/BTI_2006-2024_Scores.xlsx", sheet = "BTI 2022", na = c("", "-"))

colnames(bti_free_and_fair_2021)[1] <- "country_name"

bti_free_and_fair_2021 <- bti_free_and_fair_2021 %>%
  rename(free_and_fair = `Q2.1 | Free and fair elections`) %>% 
  mutate(year = 2021) %>% select(country_name, year, free_and_fair)

bti_free_and_fair_2019 <- read_excel(path = "data/bs/bti/BTI_2006-2024_Scores.xlsx", sheet = "BTI 2020", na = c("", "-"))

colnames(bti_free_and_fair_2019)[1] <- "country_name"

bti_free_and_fair_2019 <- bti_free_and_fair_2019 %>%
  rename(free_and_fair = `Q2.1 | Free and fair elections`) %>% 
  mutate(year = 2019) %>% select(country_name, year, free_and_fair)



#/////////////////////


# combine years
sub_obj_1_3_bti_free_and_fair_elections <- #bti_free_and_fair_2017 %>%
  bti_free_and_fair_2019 %>%
  bind_rows(bti_free_and_fair_2021,
            bti_free_and_fair_2023) %>%  
  pivot_wider(id_cols = country_name, names_from = year, values_from = free_and_fair) %>%
  # mutate(`2018` = ((`2019` + `2017`)/2),
  #        `2020` = ((`2021` + `2019`)/2),
  #        `2022` = ((`2023` + `2021`)/2),
  #        `2024` = `2023`) %>%
  # dplyr::select(-c(`2017`)) %>%
  pivot_longer(cols = -country_name, names_to = "year", values_to = "values") %>%
  mutate(country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  TRUE ~ country_name),
         year = as.numeric(year),
         indicator_name = "sub_obj_1_3_bti_free_and_fair_elections",
         high_value_is_good_outcome_flag = 1) %>%
  arrange(country_name,
          year)

rm(list = ls(pattern = "^bti"))
#//////////////////////////


# inspect
sub_obj_1_3_bti_free_and_fair_elections
sub_obj_1_3_bti_free_and_fair_elections %>% glimpse()
sub_obj_1_3_bti_free_and_fair_elections %>% nrow() # 959
sub_obj_1_3_bti_free_and_fair_elections %>% ncol() # 5
sub_obj_1_3_bti_free_and_fair_elections %>% skim()
sub_obj_1_3_bti_free_and_fair_elections %>% group_by(year) %>% skim()

##Steve's note below is no longer relevant as we're only doing 2018 onwards
# note kosovo is missing first two years in raw data, which means first 4 years in interpolated data, 
# all other ee_presence countries have no missing values
sub_obj_1_3_bti_free_and_fair_elections %>% filter(country_name == "Kosovo") %>% arrange(year)
sub_obj_1_3_bti_free_and_fair_elections %>% arrange(values)
sub_obj_1_3_bti_free_and_fair_elections %>% arrange(desc(values))
sub_obj_1_3_bti_free_and_fair_elections %>% filter(is.na(values)) %>% count(country_name) %>% arrange(desc(n))
sub_obj_1_3_bti_free_and_fair_elections %>% 
  filter(country_name %in% 
           (country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
              distinct(country) %>% pull(country))) %>% 
  filter(is.na(values)) %>% count(country_name)

# inspect country names
sub_obj_1_3_bti_free_and_fair_elections %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
sub_obj_1_3_bti_free_and_fair_elections %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_1_3_bti_free_and_fair_elections <- sub_obj_1_3_bti_free_and_fair_elections %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_1_3_bti_free_and_fair_elections",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")



#/////////////


# inspect
sub_obj_1_3_bti_free_and_fair_elections 
sub_obj_1_3_bti_free_and_fair_elections %>% glimpse()
sub_obj_1_3_bti_free_and_fair_elections %>% nrow() # 315
sub_obj_1_3_bti_free_and_fair_elections %>% ncol() # 36
sub_obj_1_3_bti_free_and_fair_elections %>% distinct(country) %>% nrow() # 45

sub_obj_1_3_bti_free_and_fair_elections %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_1_3_bti_free_and_fair_elections %>% filter(is.na(values), year >= 2018) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_1_3_bti_free_and_fair_elections %>% skim(values)
sub_obj_1_3_bti_free_and_fair_elections %>% group_by(year) %>% skim(values)
sub_obj_1_3_bti_free_and_fair_elections %>% filter(year >= 2018) %>% skim(values)
sub_obj_1_3_bti_free_and_fair_elections %>% filter(year >= 2018) %>% group_by(country) %>% skim(values)


# inspect interpolation
sub_obj_1_3_bti_free_and_fair_elections %>% filter(year >= 2018) %>%
  select(country, year, values) %>% print(n = 50)

# Missing values for all EU-15 and US
sub_obj_1_3_bti_free_and_fair_elections %>% filter(is.na(values), year >= 2018) %>% 
  count(country, mcp_grouping) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
sub_obj_1_3_bti_free_and_fair_elections %>% filter(is.na(values),
                                                             mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                                             year >= 2018) %>%
  select(country, mcp_grouping, year, values) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
sub_obj_1_3_bti_free_and_fair_elections %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_1_3_bti_free_and_fair_elections %>% filter(year >= 2018) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_1_3_bti_free_and_fair_elections %>% 
  # filter(mcp_grouping == "E&E Balkans") %>%
   filter(mcp_grouping == "E&E Eurasia") %>%
  # filter(mcp_grouping == "E&E graduates") %>%
  # filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  # ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line() 
  ggplot(data = ., mapping = aes(x = year, y = jitter(values, 1), color = country)) + geom_line(size = 1)


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_1_3_bti_free_and_fair_elections %>% write_csv(file = "data/fmir/sub_obj_1_3_bti_free_and_fair_elections.csv")
sub_obj_1_3_bti_free_and_fair_elections <- read_csv(file = "data/fmir/sub_obj_1_3_bti_free_and_fair_elections.csv")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////

# load sub_obj_1_3_bti_approval_of_democracy ####

# https://www.bti-project.org/en/index/political-transformation.html
# https://www.bti-project.org/en/methodology.html
# https://www.bti-project.org/content/en/downloads/codebooks/BTI_2020_Codebook.pdf


##Note from Steve
# note that BTI 2020 Codebook instructs reviewers to base their assessments "from 2017 to 2019" and 
# that "Your responses to the questions provided in the questionnaire should reflect the situation in your 
# country at the end of January 2019." 
# this seems to imply that the BTI 2020 edition data represents actual conditions for 2018, since it stopped measurement period
# as of january 2019;
# however, eda told me, and we confirmed, that J2SR lags BTI edition year by 1 year to get actual year represented
# eg in "J2SR FY21 Control Figures Data" spreadsheet, go to "transormation & scaling" tab, filter to Albania, and
# metric = Environmental Policy (from BTI), and see that j2sr gives albania a 4 for 2005 and a 5 for 2007 and 
# in BTI data albania has a 4 for BTI 2006 edition, and a 5 for BTI 2008 edition
# so BTI 2020 edition data is determined and manually set to represent actual year 2019
# eda and i then both use the same method to interpolate the gaps, by taking midpoint between observations
# so that albania 2006 actual conditions is set equal to midpoint of 4 and 5, so 4.5


# load bti data by year tabs
bti_approval_of_democracy_2023 <- read_excel(path = "data/bs/bti/BTI_2006-2024_Scores.xlsx", sheet = "BTI 2024", na = c("", "-"))

colnames(bti_approval_of_democracy_2023)[1] <- "country_name"

bti_approval_of_democracy_2023 <- bti_approval_of_democracy_2023 %>%
  rename(approval_democracy = `Q5.3 | Approval of democracy`) %>% 
  mutate(year = 2023) %>% select(country_name, year, approval_democracy)

bti_approval_of_democracy_2021 <- read_excel(path = "data/bs/bti/BTI_2006-2024_Scores.xlsx", sheet = "BTI 2022", na = c("", "-"))

colnames(bti_approval_of_democracy_2021)[1] <- "country_name"

bti_approval_of_democracy_2021 <- bti_approval_of_democracy_2021 %>%
  rename(approval_democracy = `Q5.3 | Approval of democracy`) %>% 
  mutate(year = 2021) %>% select(country_name, year, approval_democracy)

bti_approval_of_democracy_2019 <- read_excel(path = "data/bs/bti/BTI_2006-2024_Scores.xlsx", sheet = "BTI 2020", na = c("", "-"))

colnames(bti_approval_of_democracy_2019)[1] <- "country_name"

bti_approval_of_democracy_2019 <- bti_approval_of_democracy_2019 %>%
  rename(approval_democracy = `Q5.3 | Approval of democracy`) %>% 
  mutate(year = 2019) %>% select(country_name, year, approval_democracy)



#/////////////////////


# combine years
sub_obj_1_3_bti_approval_of_democracy <- #bti_approval_of_democracy_2017 %>%
  bti_approval_of_democracy_2019 %>%
  bind_rows(bti_approval_of_democracy_2021,
            bti_approval_of_democracy_2023) %>%  
  pivot_wider(id_cols = country_name, names_from = year, values_from = approval_democracy)

sub_obj_1_3_bti_approval_of_democracy[sub_obj_1_3_bti_approval_of_democracy == "n/a"] <- NA

#sub_obj_1_3_bti_approval_of_democracy$`2017` <- as.numeric(sub_obj_1_3_bti_approval_of_democracy$`2017`)
sub_obj_1_3_bti_approval_of_democracy$`2019` <- as.numeric(sub_obj_1_3_bti_approval_of_democracy$`2019`)
sub_obj_1_3_bti_approval_of_democracy$`2021` <- as.numeric(sub_obj_1_3_bti_approval_of_democracy$`2021`)
sub_obj_1_3_bti_approval_of_democracy$`2023` <- as.numeric(sub_obj_1_3_bti_approval_of_democracy$`2023`)

sub_obj_1_3_bti_approval_of_democracy <- sub_obj_1_3_bti_approval_of_democracy %>%
  # mutate(
  #   `2023` = case_when(
  #     is.na(`2023`) ~ `2021`,
  #     TRUE ~ `2023`
  #   )
  # ) %>%
  # mutate(
  #   `2017` = case_when(
  #     is.na(`2017`) ~ `2019`,
  #     TRUE ~ `2017`
  #   )
  # ) %>%
  # filter(!is.na(`2023`)) %>%
  # filter(!is.na(`2021`)) %>%
  # mutate(
  #   `2018` = ((as.numeric(`2019`) + as.numeric(`2017`))/2),
  #   `2020` = ((as.numeric(`2021`) + as.numeric(`2019`))/2),
  #   `2022` = ((as.numeric(`2023`) + as.numeric(`2021`))/2),
  #   `2024` = `2023`) %>%
  # select(-c(`2017`)) %>%
  pivot_longer(cols = -country_name, names_to = "year", values_to = "values") %>%
  mutate(country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  TRUE ~ country_name),
         year = as.numeric(year),
         indicator_name = "sub_obj_1_3_bti_approval_of_democracy",
         high_value_is_good_outcome_flag = 1) %>%
  arrange(country_name,
          year)

rm(list = ls(pattern = "^bti"))
#//////////////////////////


# inspect
sub_obj_1_3_bti_approval_of_democracy
sub_obj_1_3_bti_approval_of_democracy %>% glimpse()
sub_obj_1_3_bti_approval_of_democracy %>% nrow() # 536
sub_obj_1_3_bti_approval_of_democracy %>% ncol() # 5
sub_obj_1_3_bti_approval_of_democracy %>% skim()
sub_obj_1_3_bti_approval_of_democracy %>% group_by(year) %>% skim()

##Steve's note below is no longer relevant as we're only doing 2018 onwards
# note kosovo is missing first two years in raw data, which means first 4 years in interpolated data, 
# all other ee_presence countries have no missing values
sub_obj_1_3_bti_approval_of_democracy %>% filter(country_name == "Kosovo") %>% arrange(year)
sub_obj_1_3_bti_approval_of_democracy %>% arrange(values)
sub_obj_1_3_bti_approval_of_democracy %>% arrange(desc(values))
sub_obj_1_3_bti_approval_of_democracy %>% filter(is.na(values)) %>% count(country_name) %>% arrange(desc(n))
sub_obj_1_3_bti_approval_of_democracy %>% 
  filter(country_name %in% 
           (country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
              distinct(country) %>% pull(country))) %>% 
  filter(is.na(values)) %>% count(country_name)

# inspect country names
sub_obj_1_3_bti_approval_of_democracy %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
sub_obj_1_3_bti_approval_of_democracy %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_1_3_bti_approval_of_democracy <- sub_obj_1_3_bti_approval_of_democracy %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_1_3_bti_approval_of_democracy",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_1_3_bti_approval_of_democracy 
sub_obj_1_3_bti_approval_of_democracy %>% glimpse()
sub_obj_1_3_bti_approval_of_democracy %>% nrow() # 315
sub_obj_1_3_bti_approval_of_democracy %>% ncol() # 39
sub_obj_1_3_bti_approval_of_democracy %>% distinct(country) %>% nrow() # 45

sub_obj_1_3_bti_approval_of_democracy %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_1_3_bti_approval_of_democracy %>% filter(is.na(values), year >= 2018) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_1_3_bti_approval_of_democracy %>% skim(values)
sub_obj_1_3_bti_approval_of_democracy %>% group_by(year) %>% skim(values)
sub_obj_1_3_bti_approval_of_democracy %>% filter(year >= 2018) %>% skim(values)
sub_obj_1_3_bti_approval_of_democracy %>% filter(year >= 2018) %>% group_by(country) %>% skim(values)


# inspect interpolation
sub_obj_1_3_bti_approval_of_democracy %>% filter(year >= 2018) %>%
  select(country, year, values) %>% print(n = 50)

# Missing values for CARS (except Kyrgyzstan), Azerbaijan, Belarus, and all EU-15 and US
sub_obj_1_3_bti_approval_of_democracy %>% filter(is.na(values), year >= 2018) %>% 
  count(country, mcp_grouping) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
sub_obj_1_3_bti_approval_of_democracy %>% filter(is.na(values),
                                                 mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                                 year >= 2018) %>%
  select(country, mcp_grouping, year, values) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
sub_obj_1_3_bti_approval_of_democracy %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_1_3_bti_approval_of_democracy %>% filter(year >= 2018) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_1_3_bti_approval_of_democracy %>% 
  #filter(mcp_grouping == "E&E Balkans") %>%
  # filter(mcp_grouping == "E&E Eurasia") %>%
  #filter(mcp_grouping == "E&E graduates") %>%
  filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  # ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line() 
  ggplot(data = ., mapping = aes(x = year, y = jitter(values, 1), color = country)) + geom_line(size = 1)


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_1_3_bti_approval_of_democracy %>% write_csv(file = "data/fmir/sub_obj_1_3_bti_approval_of_democracy.csv")
sub_obj_1_3_bti_approval_of_democracy <- read_csv(file = "data/fmir/sub_obj_1_3_bti_approval_of_democracy.csv")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////

# load sub_obj_3_2_bti_competition_protections ####

# https://www.bti-project.org/en/index/political-transformation.html
# https://www.bti-project.org/en/methodology.html
# https://www.bti-project.org/content/en/downloads/codebooks/BTI_2020_Codebook.pdf


##Note from Steve
# note that BTI 2020 Codebook instructs reviewers to base their assessments "from 2017 to 2019" and 
# that "Your responses to the questions provided in the questionnaire should reflect the situation in your 
# country at the end of January 2019." 
# this seems to imply that the BTI 2020 edition data represents actual conditions for 2018, since it stopped measurement period
# as of january 2019;
# however, eda told me, and we confirmed, that J2SR lags BTI edition year by 1 year to get actual year represented
# eg in "J2SR FY21 Control Figures Data" spreadsheet, go to "transormation & scaling" tab, filter to Albania, and
# metric = Environmental Policy (from BTI), and see that j2sr gives albania a 4 for 2005 and a 5 for 2007 and 
# in BTI data albania has a 4 for BTI 2006 edition, and a 5 for BTI 2008 edition
# so BTI 2020 edition data is determined and manually set to represent actual year 2019
# eda and i then both use the same method to interpolate the gaps, by taking midpoint between observations
# so that albania 2006 actual conditions is set equal to midpoint of 4 and 5, so 4.5


# load bti data by year tabs
bti_competition_protections_2023 <- read_excel(path = "data/bs/bti/BTI_2006-2024_Scores.xlsx", sheet = "BTI 2024", na = c("", "-"))

colnames(bti_competition_protections_2023)[1] <- "country_name"

bti_competition_protections_2023 <- bti_competition_protections_2023 %>%
  rename(competition_protection = `Q7.2 | Competition policy`) %>% 
  mutate(year = 2023) %>% select(country_name, year, competition_protection)

bti_competition_protections_2021 <- read_excel(path = "data/bs/bti/BTI_2006-2024_Scores.xlsx", sheet = "BTI 2022", na = c("", "-"))

colnames(bti_competition_protections_2021)[1] <- "country_name"

bti_competition_protections_2021 <- bti_competition_protections_2021 %>%
  rename(competition_protection = `Q7.2 | Competition policy`) %>% 
  mutate(year = 2021) %>% select(country_name, year, competition_protection)

bti_competition_protections_2019 <- read_excel(path = "data/bs/bti/BTI_2006-2024_Scores.xlsx", sheet = "BTI 2020", na = c("", "-"))

colnames(bti_competition_protections_2019)[1] <- "country_name"

bti_competition_protections_2019 <- bti_competition_protections_2019 %>%
  rename(competition_protection = `Q7.2 | Competition policy`) %>% 
  mutate(year = 2019) %>% select(country_name, year, competition_protection)
 


#/////////////////////


# combine years
sub_obj_3_2_bti_competition_protections <- #bti_competition_protections_2017 %>%
  bti_competition_protections_2019 %>%
  bind_rows(bti_competition_protections_2021,
            bti_competition_protections_2023) %>%  
  pivot_wider(id_cols = country_name, names_from = year, values_from = competition_protection)

sub_obj_3_2_bti_competition_protections[sub_obj_3_2_bti_competition_protections == "n/a"] <- NA

#sub_obj_3_2_bti_competition_protections$`2017` <- as.numeric(sub_obj_3_2_bti_competition_protections$`2017`)
sub_obj_3_2_bti_competition_protections$`2019` <- as.numeric(sub_obj_3_2_bti_competition_protections$`2019`)
sub_obj_3_2_bti_competition_protections$`2021` <- as.numeric(sub_obj_3_2_bti_competition_protections$`2021`)
sub_obj_3_2_bti_competition_protections$`2023` <- as.numeric(sub_obj_3_2_bti_competition_protections$`2023`)

sub_obj_3_2_bti_competition_protections <- sub_obj_3_2_bti_competition_protections %>%
  # mutate(
  #   `2017` = case_when(
  #     is.na(`2017`) ~ `2019`,
  #     TRUE ~ `2017`
  #   )
  # ) %>%
  # mutate(
  #   `2018` = ((as.numeric(`2019`) + as.numeric(`2017`))/2),
  #   `2020` = ((as.numeric(`2021`) + as.numeric(`2019`))/2),
  #   `2022` = ((as.numeric(`2023`) + as.numeric(`2021`))/2),
  #   `2024` = `2023`) %>%
  # select(-c(`2017`)) %>%
  pivot_longer(cols = -country_name, names_to = "year", values_to = "values") %>%
  mutate(country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  TRUE ~ country_name),
         year = as.numeric(year),
         indicator_name = "sub_obj_3_2_bti_competition_protections",
         high_value_is_good_outcome_flag = 1) %>%
  arrange(country_name,
          year)

rm(list = ls(pattern = "^bti"))

#//////////////////////////


# inspect
sub_obj_3_2_bti_competition_protections
sub_obj_3_2_bti_competition_protections %>% glimpse()
sub_obj_3_2_bti_competition_protections %>% nrow() # 959
sub_obj_3_2_bti_competition_protections %>% ncol() # 5
sub_obj_3_2_bti_competition_protections %>% skim()
sub_obj_3_2_bti_competition_protections %>% group_by(year) %>% skim()

##Steve's note below is no longer relevant as we're only doing 2018 onwards
# note kosovo is missing first two years in raw data, which means first 4 years in interpolated data, 
# all other ee_presence countries have no missing values
sub_obj_3_2_bti_competition_protections %>% filter(country_name == "Kosovo") %>% arrange(year)
sub_obj_3_2_bti_competition_protections %>% arrange(values)
sub_obj_3_2_bti_competition_protections %>% arrange(desc(values))
sub_obj_3_2_bti_competition_protections %>% filter(is.na(values)) %>% count(country_name) %>% arrange(desc(n))
sub_obj_3_2_bti_competition_protections %>% 
  filter(country_name %in% 
           (country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
              distinct(country) %>% pull(country))) %>% 
  filter(is.na(values)) %>% count(country_name)

# inspect country names
sub_obj_3_2_bti_competition_protections %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
sub_obj_3_2_bti_competition_protections %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_3_2_bti_competition_protections <- sub_obj_3_2_bti_competition_protections %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_3_2_bti_competition_protections",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_3_2_bti_competition_protections 
sub_obj_3_2_bti_competition_protections %>% glimpse()
sub_obj_3_2_bti_competition_protections %>% nrow() # 315
sub_obj_3_2_bti_competition_protections %>% ncol() # 39
sub_obj_3_2_bti_competition_protections %>% distinct(country) %>% nrow() # 45

sub_obj_3_2_bti_competition_protections %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_3_2_bti_competition_protections %>% filter(is.na(values), year >= 2018) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_3_2_bti_competition_protections %>% skim(values)
sub_obj_3_2_bti_competition_protections %>% group_by(year) %>% skim(values)
sub_obj_3_2_bti_competition_protections %>% filter(year >= 2018) %>% skim(values)
sub_obj_3_2_bti_competition_protections %>% filter(year >= 2018) %>% group_by(country) %>% skim(values)


# inspect interpolation
sub_obj_3_2_bti_competition_protections %>% filter(year >= 2018) %>%
  select(country, year, values) %>% print(n = 50)

# Missing values for and all EU-15 and US
sub_obj_3_2_bti_competition_protections %>% filter(is.na(values), year >= 2018) %>% 
  count(country, mcp_grouping) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
sub_obj_3_2_bti_competition_protections %>% filter(is.na(values),
                                          mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                          year >= 2018) %>%
  select(country, mcp_grouping, year, values) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
sub_obj_3_2_bti_competition_protections %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_3_2_bti_competition_protections %>% filter(year >= 2018) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_3_2_bti_competition_protections %>% 
  # filter(mcp_grouping == "E&E Balkans") %>%
   filter(mcp_grouping == "E&E Eurasia") %>%
  # filter(mcp_grouping == "E&E graduates") %>%
  # filter(mcp_grouping == "CARs") %>%
  # filter(mcp_grouping == "EU-15") %>%
  # ggplot(data = ., mapping = aes(x = year, y = values, color = country)) + geom_line() 
  ggplot(data = ., mapping = aes(x = year, y = jitter(values, 1), color = country)) + geom_line(size = 1)


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_3_2_bti_competition_protections %>% write_csv(file = "data/fmir/sub_obj_3_2_bti_competition_protections.csv")
sub_obj_3_2_bti_competition_protections <- read_csv(file = "data/fmir/sub_obj_3_2_bti_competition_protections.csv")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////

# load sub_obj_4_2_bti_banking_system ####

# https://www.bti-project.org/en/index/political-transformation.html
# https://www.bti-project.org/en/methodology.html
# https://www.bti-project.org/content/en/downloads/codebooks/BTI_2020_Codebook.pdf


##Note from Steve
# note that BTI 2020 Codebook instructs reviewers to base their assessments "from 2017 to 2019" and 
# that "Your responses to the questions provided in the questionnaire should reflect the situation in your 
# country at the end of January 2019." 
# this seems to imply that the BTI 2020 edition data represents actual conditions for 2018, since it stopped measurement period
# as of january 2019;
# however, eda told me, and we confirmed, that J2SR lags BTI edition year by 1 year to get actual year represented
# eg in "J2SR FY21 Control Figures Data" spreadsheet, go to "transormation & scaling" tab, filter to Albania, and
# metric = Environmental Policy (from BTI), and see that j2sr gives albania a 4 for 2005 and a 5 for 2007 and 
# in BTI data albania has a 4 for BTI 2006 edition, and a 5 for BTI 2008 edition
# so BTI 2020 edition data is determined and manually set to represent actual year 2019
# eda and i then both use the same method to interpolate the gaps, by taking midpoint between observations
# so that albania 2006 actual conditions is set equal to midpoint of 4 and 5, so 4.5


# load bti data by year tabs
bti_banking_system_2023 <- read_excel(path = "data/bs/bti/BTI_2006-2024_Scores.xlsx", sheet = "BTI 2024", na = c("", "-"))

colnames(bti_banking_system_2023)[1] <- "country_name"

bti_banking_system_2023 <- bti_banking_system_2023 %>%
  rename(banking_system = `Q7.4 | Banking system`) %>% 
  mutate(year = 2023) %>% select(country_name, year, banking_system)

bti_banking_system_2021 <- read_excel(path = "data/bs/bti/BTI_2006-2024_Scores.xlsx", sheet = "BTI 2022", na = c("", "-"))

colnames(bti_banking_system_2021)[1] <- "country_name"

bti_banking_system_2021 <- bti_banking_system_2021 %>%
  rename(banking_system = `Q7.4 | Banking system`) %>% 
  mutate(year = 2021) %>% select(country_name, year, banking_system)

bti_banking_system_2019 <- read_excel(path = "data/bs/bti/BTI_2006-2024_Scores.xlsx", sheet = "BTI 2020", na = c("", "-"))

colnames(bti_banking_system_2019)[1] <- "country_name"

bti_banking_system_2019 <- bti_banking_system_2019 %>%
  rename(banking_system = `Q7.4 | Banking system`) %>% 
  mutate(year = 2019) %>% select(country_name, year, banking_system)


#/////////////////////

sub_obj_4_2_bti_banking_system <- #bti_banking_system_2017 %>%
  bti_banking_system_2019 %>%
  bind_rows(bti_banking_system_2021,
            bti_banking_system_2023) %>%  
  pivot_wider(id_cols = country_name, names_from = year, values_from = banking_system)

sub_obj_4_2_bti_banking_system[sub_obj_4_2_bti_banking_system == "n/a"] <- NA

#sub_obj_4_2_bti_banking_system$`2017` <- as.numeric(sub_obj_4_2_bti_banking_system$`2017`)
sub_obj_4_2_bti_banking_system$`2019` <- as.numeric(sub_obj_4_2_bti_banking_system$`2019`)
sub_obj_4_2_bti_banking_system$`2021` <- as.numeric(sub_obj_4_2_bti_banking_system$`2021`)
sub_obj_4_2_bti_banking_system$`2023` <- as.numeric(sub_obj_4_2_bti_banking_system$`2023`)

sub_obj_4_2_bti_banking_system <- sub_obj_4_2_bti_banking_system %>%
  pivot_longer(cols = -country_name, names_to = "year", values_to = "values") %>%
  mutate(country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  TRUE ~ country_name),
         year = as.numeric(year),
         indicator_name = "sub_obj_4_2_bti_banking_system",
         high_value_is_good_outcome_flag = 1) %>%
  arrange(country_name,
          year)

rm(list = ls(pattern = "^bti"))
#//////////////////////////


# inspect
sub_obj_4_2_bti_banking_system
sub_obj_4_2_bti_banking_system %>% glimpse()
sub_obj_4_2_bti_banking_system %>% nrow() # 959
sub_obj_4_2_bti_banking_system %>% ncol() # 5
sub_obj_4_2_bti_banking_system %>% skim()
sub_obj_4_2_bti_banking_system %>% group_by(year) %>% skim()

##Steve's note below is no longer relevant as we're only doing 2018 onwards
# note kosovo is missing first two years in raw data, which means first 4 years in interpolated data, 
# all other ee_presence countries have no missing values
sub_obj_4_2_bti_banking_system %>% filter(country_name == "Kosovo") %>% arrange(year)
sub_obj_4_2_bti_banking_system %>% arrange(values)
sub_obj_4_2_bti_banking_system %>% arrange(desc(values))
sub_obj_4_2_bti_banking_system %>% filter(is.na(values)) %>% count(country_name) %>% arrange(desc(n))
sub_obj_4_2_bti_banking_system %>% 
  filter(country_name %in% 
           (country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
              distinct(country) %>% pull(country))) %>% 
  filter(is.na(values)) %>% count(country_name)

# inspect country names
sub_obj_4_2_bti_banking_system %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
sub_obj_4_2_bti_banking_system %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_4_2_bti_banking_system <- sub_obj_4_2_bti_banking_system %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_4_2_bti_banking_system",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_4_2_bti_banking_system 
sub_obj_4_2_bti_banking_system %>% glimpse()
sub_obj_4_2_bti_banking_system %>% nrow() # 315
sub_obj_4_2_bti_banking_system %>% ncol() # 39
sub_obj_4_2_bti_banking_system %>% distinct(country) %>% nrow() # 45

sub_obj_4_2_bti_banking_system %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_4_2_bti_banking_system %>% filter(is.na(values), year >= 2018) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_4_2_bti_banking_system %>% skim(values)
sub_obj_4_2_bti_banking_system %>% group_by(year) %>% skim(values)
sub_obj_4_2_bti_banking_system %>% filter(year >= 2018) %>% skim(values)
sub_obj_4_2_bti_banking_system %>% filter(year >= 2018) %>% group_by(country) %>% skim(values)


# inspect interpolation
sub_obj_4_2_bti_banking_system %>% filter(year >= 2018) %>%
  select(country, year, values) %>% print(n = 50)

# Missing values for Kosovo (2006-2009), Montenegro (2006-2007), and all EU-15 and US
sub_obj_4_2_bti_banking_system %>% filter(is.na(values), year >= 2018) %>% 
  count(country, mcp_grouping) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
sub_obj_4_2_bti_banking_system %>% filter(is.na(values),
                                          mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                          year >= 2018) %>%
  select(country, mcp_grouping, year, values) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
sub_obj_4_2_bti_banking_system %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_4_2_bti_banking_system %>% filter(year >= 2018) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_4_2_bti_banking_system %>% 
   filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = jitter(values, 1), color = country)) + geom_line(size = 1)


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_4_2_bti_banking_system %>% write_csv(file = "data/fmir/sub_obj_4_2_bti_banking_system.csv")
sub_obj_4_2_bti_banking_system <- read_csv(file = "data/fmir/sub_obj_4_2_bti_banking_system.csv")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////

# load sub_obj_4_2_bti_monetary_stability ####

# https://www.bti-project.org/en/index/political-transformation.html
# https://www.bti-project.org/en/methodology.html
# https://www.bti-project.org/content/en/downloads/codebooks/BTI_2020_Codebook.pdf


##Note from Steve
# note that BTI 2020 Codebook instructs reviewers to base their assessments "from 2017 to 2019" and 
# that "Your responses to the questions provided in the questionnaire should reflect the situation in your 
# country at the end of January 2019." 
# this seems to imply that the BTI 2020 edition data represents actual conditions for 2018, since it stopped measurement period
# as of january 2019;
# however, eda told me, and we confirmed, that J2SR lags BTI edition year by 1 year to get actual year represented
# eg in "J2SR FY21 Control Figures Data" spreadsheet, go to "transormation & scaling" tab, filter to Albania, and
# metric = Environmental Policy (from BTI), and see that j2sr gives albania a 4 for 2005 and a 5 for 2007 and 
# in BTI data albania has a 4 for BTI 2006 edition, and a 5 for BTI 2008 edition
# so BTI 2020 edition data is determined and manually set to represent actual year 2019
# eda and i then both use the same method to interpolate the gaps, by taking midpoint between observations
# so that albania 2006 actual conditions is set equal to midpoint of 4 and 5, so 4.5


# load bti data by year tabs
bti_monetary_stability_2023 <- read_excel(path = "data/bs/bti/BTI_2006-2024_Scores.xlsx", sheet = "BTI 2024", na = c("", "-"))

colnames(bti_monetary_stability_2023)[1] <- "country_name"

bti_monetary_stability_2023 <- bti_monetary_stability_2023 %>%
  rename(monetary_stability = `Q8.1 | Monetary stability`) %>% 
  mutate(year = 2023) %>% select(country_name, year, monetary_stability)

bti_monetary_stability_2021 <- read_excel(path = "data/bs/bti/BTI_2006-2024_Scores.xlsx", sheet = "BTI 2022", na = c("", "-"))

colnames(bti_monetary_stability_2021)[1] <- "country_name"

bti_monetary_stability_2021 <- bti_monetary_stability_2021 %>%
  rename(monetary_stability = `Q8.1 | Monetary stability`) %>% 
  mutate(year = 2021) %>% select(country_name, year, monetary_stability)

bti_monetary_stability_2019 <- read_excel(path = "data/bs/bti/BTI_2006-2024_Scores.xlsx", sheet = "BTI 2020", na = c("", "-"))

colnames(bti_monetary_stability_2019)[1] <- "country_name"

bti_monetary_stability_2019 <- bti_monetary_stability_2019 %>%
  rename(monetary_stability = `Q8.1 | Monetary stability`) %>% 
  mutate(year = 2019) %>% select(country_name, year, monetary_stability)

#/////////////////////


# combine years
sub_obj_4_2_bti_monetary_stability <- #bti_monetary_stability_2017 %>%
  bti_monetary_stability_2019 %>%
  bind_rows(bti_monetary_stability_2021,
            bti_monetary_stability_2023) %>%  
  pivot_wider(id_cols = country_name, names_from = year, values_from = monetary_stability)

sub_obj_4_2_bti_monetary_stability[sub_obj_4_2_bti_monetary_stability == "n/a"] <- NA

#sub_obj_4_2_bti_monetary_stability$`2017` <- as.numeric(sub_obj_4_2_bti_monetary_stability$`2017`)
sub_obj_4_2_bti_monetary_stability$`2019` <- as.numeric(sub_obj_4_2_bti_monetary_stability$`2019`)
sub_obj_4_2_bti_monetary_stability$`2021` <- as.numeric(sub_obj_4_2_bti_monetary_stability$`2021`)
sub_obj_4_2_bti_monetary_stability$`2023` <- as.numeric(sub_obj_4_2_bti_monetary_stability$`2023`)

sub_obj_4_2_bti_monetary_stability <- sub_obj_4_2_bti_monetary_stability %>%
  pivot_longer(cols = -country_name, names_to = "year", values_to = "values") %>%
  mutate(country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  TRUE ~ country_name),
         year = as.numeric(year),
         indicator_name = "sub_obj_4_2_bti_monetary_stability",
         high_value_is_good_outcome_flag = 1) %>%
  arrange(country_name,
          year)

rm(list = ls(pattern = "^bti"))

#//////////////////////////


# inspect
sub_obj_4_2_bti_monetary_stability
sub_obj_4_2_bti_monetary_stability %>% glimpse()
sub_obj_4_2_bti_monetary_stability %>% nrow() # 959
sub_obj_4_2_bti_monetary_stability %>% ncol() # 5
sub_obj_4_2_bti_monetary_stability %>% skim()
sub_obj_4_2_bti_monetary_stability %>% group_by(year) %>% skim()

##Steve's note below is no longer relevant as we're only doing 2018 onwards
# note kosovo is missing first two years in raw data, which means first 4 years in interpolated data, 
# all other ee_presence countries have no missing values
sub_obj_4_2_bti_monetary_stability %>% filter(country_name == "Kosovo") %>% arrange(year)
sub_obj_4_2_bti_monetary_stability %>% arrange(values)
sub_obj_4_2_bti_monetary_stability %>% arrange(desc(values))
sub_obj_4_2_bti_monetary_stability %>% filter(is.na(values)) %>% count(country_name) %>% arrange(desc(n))
sub_obj_4_2_bti_monetary_stability %>% 
  filter(country_name %in% 
           (country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
              distinct(country) %>% pull(country))) %>% 
  filter(is.na(values)) %>% count(country_name)

# inspect country names
sub_obj_4_2_bti_monetary_stability %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
sub_obj_4_2_bti_monetary_stability %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_4_2_bti_monetary_stability <- sub_obj_4_2_bti_monetary_stability %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_4_2_bti_monetary_stability",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_4_2_bti_monetary_stability 
sub_obj_4_2_bti_monetary_stability %>% glimpse()
sub_obj_4_2_bti_monetary_stability %>% nrow() # 315
sub_obj_4_2_bti_monetary_stability %>% ncol() # 39
sub_obj_4_2_bti_monetary_stability %>% distinct(country) %>% nrow() # 45

sub_obj_4_2_bti_monetary_stability %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_4_2_bti_monetary_stability %>% filter(is.na(values), year >= 2018) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_4_2_bti_monetary_stability %>% skim(values)
sub_obj_4_2_bti_monetary_stability %>% group_by(year) %>% skim(values)
sub_obj_4_2_bti_monetary_stability %>% filter(year >= 2018) %>% skim(values)
sub_obj_4_2_bti_monetary_stability %>% filter(year >= 2018) %>% group_by(country) %>% skim(values)


# inspect interpolation
sub_obj_4_2_bti_monetary_stability %>% filter(year >= 2018) %>%
  select(country, year, values) %>% print(n = 50)

# Missing values for all EU-15 and US
sub_obj_4_2_bti_monetary_stability %>% filter(is.na(values), year >= 2018) %>% 
  count(country, mcp_grouping) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
sub_obj_4_2_bti_monetary_stability %>% filter(is.na(values),
                                          mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                          year >= 2018) %>%
  select(country, mcp_grouping, year, values) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
sub_obj_4_2_bti_monetary_stability %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_4_2_bti_monetary_stability %>% filter(year >= 2018) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_4_2_bti_monetary_stability %>% 
   filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = jitter(values, 1), color = country)) + geom_line(size = 1)


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_4_2_bti_monetary_stability %>% write_csv(file = "data/fmir/sub_obj_4_2_bti_monetary_stability.csv")
sub_obj_4_2_bti_monetary_stability <- read_csv(file = "data/fmir/sub_obj_4_2_bti_monetary_stability.csv")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////

# load sub_obj_4_2_bti_fiscal_stability ####

# https://www.bti-project.org/en/index/political-transformation.html
# https://www.bti-project.org/en/methodology.html
# https://www.bti-project.org/content/en/downloads/codebooks/BTI_2020_Codebook.pdf


##Note from Steve
# note that BTI 2020 Codebook instructs reviewers to base their assessments "from 2017 to 2019" and 
# that "Your responses to the questions provided in the questionnaire should reflect the situation in your 
# country at the end of January 2019." 
# this seems to imply that the BTI 2020 edition data represents actual conditions for 2018, since it stopped measurement period
# as of january 2019;
# however, eda told me, and we confirmed, that J2SR lags BTI edition year by 1 year to get actual year represented
# eg in "J2SR FY21 Control Figures Data" spreadsheet, go to "transormation & scaling" tab, filter to Albania, and
# metric = Environmental Policy (from BTI), and see that j2sr gives albania a 4 for 2005 and a 5 for 2007 and 
# in BTI data albania has a 4 for BTI 2006 edition, and a 5 for BTI 2008 edition
# so BTI 2020 edition data is determined and manually set to represent actual year 2019
# eda and i then both use the same method to interpolate the gaps, by taking midpoint between observations
# so that albania 2006 actual conditions is set equal to midpoint of 4 and 5, so 4.5


# load bti data by year tabs
bti_fiscal_stability_2023 <- read_excel(path = "data/bs/bti/BTI_2006-2024_Scores.xlsx", sheet = "BTI 2024", na = c("", "-"))

colnames(bti_fiscal_stability_2023)[1] <- "country_name"

bti_fiscal_stability_2023 <- bti_fiscal_stability_2023 %>%
  rename(fiscal_stability = `Q8.2 | Fiscal stability`) %>% 
  mutate(year = 2023) %>% select(country_name, year, fiscal_stability)

bti_fiscal_stability_2021 <- read_excel(path = "data/bs/bti/BTI_2006-2024_Scores.xlsx", sheet = "BTI 2022", na = c("", "-"))

colnames(bti_fiscal_stability_2021)[1] <- "country_name"

bti_fiscal_stability_2021 <- bti_fiscal_stability_2021 %>%
  rename(fiscal_stability = `Q8.2 | Fiscal stability`) %>% 
  mutate(year = 2021) %>% select(country_name, year, fiscal_stability)

bti_fiscal_stability_2019 <- read_excel(path = "data/bs/bti/BTI_2006-2024_Scores.xlsx", sheet = "BTI 2020", na = c("", "-"))

colnames(bti_fiscal_stability_2019)[1] <- "country_name"

bti_fiscal_stability_2019 <- bti_fiscal_stability_2019 %>%
  rename(fiscal_stability = `Q8.2 | Fiscal stability`) %>% 
  mutate(year = 2019) %>% select(country_name, year, fiscal_stability)


#/////////////////////


# combine years
sub_obj_4_2_bti_fiscal_stability <- #bti_fiscal_stability_2017 %>%
  bti_fiscal_stability_2019 %>%
  bind_rows(bti_fiscal_stability_2021,
            bti_fiscal_stability_2023) %>%  
  pivot_wider(id_cols = country_name, names_from = year, values_from = fiscal_stability)

sub_obj_4_2_bti_fiscal_stability[sub_obj_4_2_bti_fiscal_stability == "n/a"] <- NA

#sub_obj_4_2_bti_fiscal_stability$`2017` <- as.numeric(sub_obj_4_2_bti_fiscal_stability$`2017`)
sub_obj_4_2_bti_fiscal_stability$`2019` <- as.numeric(sub_obj_4_2_bti_fiscal_stability$`2019`)
sub_obj_4_2_bti_fiscal_stability$`2021` <- as.numeric(sub_obj_4_2_bti_fiscal_stability$`2021`)
sub_obj_4_2_bti_fiscal_stability$`2023` <- as.numeric(sub_obj_4_2_bti_fiscal_stability$`2023`)

sub_obj_4_2_bti_fiscal_stability <- sub_obj_4_2_bti_fiscal_stability %>%
  pivot_longer(cols = -country_name, names_to = "year", values_to = "values") %>%
  mutate(country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Czech Republic" ~ "Czechia",
                                  TRUE ~ country_name),
         year = as.numeric(year),
         indicator_name = "sub_obj_4_2_bti_fiscal_stability",
         high_value_is_good_outcome_flag = 1) %>%
  arrange(country_name,
          year)

rm(list = ls(pattern = "^bti"))

#//////////////////////////


# inspect
sub_obj_4_2_bti_fiscal_stability
sub_obj_4_2_bti_fiscal_stability %>% glimpse()
sub_obj_4_2_bti_fiscal_stability %>% nrow() # 959
sub_obj_4_2_bti_fiscal_stability %>% ncol() # 5
sub_obj_4_2_bti_fiscal_stability %>% skim()
sub_obj_4_2_bti_fiscal_stability %>% group_by(year) %>% skim()

##Steve's note below is no longer relevant as we're only doing 2018 onwards
# note kosovo is missing first two years in raw data, which means first 4 years in interpolated data, 
# all other ee_presence countries have no missing values
sub_obj_4_2_bti_fiscal_stability %>% filter(country_name == "Kosovo") %>% arrange(year)
sub_obj_4_2_bti_fiscal_stability %>% arrange(values)
sub_obj_4_2_bti_fiscal_stability %>% arrange(desc(values))
sub_obj_4_2_bti_fiscal_stability %>% filter(is.na(values)) %>% count(country_name) %>% arrange(desc(n))
sub_obj_4_2_bti_fiscal_stability %>% 
  filter(country_name %in% 
           (country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S.") %>% 
              distinct(country) %>% pull(country))) %>% 
  filter(is.na(values)) %>% count(country_name)

# inspect country names
sub_obj_4_2_bti_fiscal_stability %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>% 
  distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
sub_obj_4_2_bti_fiscal_stability %>% 
  filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
  distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join country_crosswalk and fmir_framework
# need to add indicator_name and high_value_is_good_outcome_flag for new years added after join that have NA 
sub_obj_4_2_bti_fiscal_stability <- sub_obj_4_2_bti_fiscal_stability %>% 
  left_join(country_crosswalk_expanded %>% filter(ee_region_flag == 1 | country == "U.S."), ., 
            by = c("country" = "country_name", "year" = "year")) %>%
  mutate(indicator_name = "sub_obj_4_2_bti_fiscal_stability",
         high_value_is_good_outcome_flag = 1) %>%
  left_join(., fmir_framework, by = "indicator_name")


#/////////////


# inspect
sub_obj_4_2_bti_fiscal_stability 
sub_obj_4_2_bti_fiscal_stability %>% glimpse()
sub_obj_4_2_bti_fiscal_stability %>% nrow() # 315
sub_obj_4_2_bti_fiscal_stability %>% ncol() # 39
sub_obj_4_2_bti_fiscal_stability %>% distinct(country) %>% nrow() # 45

sub_obj_4_2_bti_fiscal_stability %>% filter(is.na(values)) %>% count(country) %>% 
  arrange(desc(n)) %>% print(n = nrow(.))
sub_obj_4_2_bti_fiscal_stability %>% filter(is.na(values), year >= 2018) %>%
  count(country, year) %>% print(n = nrow(.))
sub_obj_4_2_bti_fiscal_stability %>% skim(values)
sub_obj_4_2_bti_fiscal_stability %>% group_by(year) %>% skim(values)
sub_obj_4_2_bti_fiscal_stability %>% filter(year >= 2018) %>% skim(values)
sub_obj_4_2_bti_fiscal_stability %>% filter(year >= 2018) %>% group_by(country) %>% skim(values)


# inspect interpolation
sub_obj_4_2_bti_fiscal_stability %>% filter(year >= 2018) %>%
  select(country, year, values) %>% print(n = 50)

# Missing values for Kosovo (2006-2009), Montenegro (2006-2007), and all EU-15 and US
sub_obj_4_2_bti_fiscal_stability %>% filter(is.na(values), year >= 2018) %>% 
  count(country, mcp_grouping) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
sub_obj_4_2_bti_fiscal_stability %>% filter(is.na(values),
                                          mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "CARs", "E&E graduates", "Russia"),
                                          year >= 2018) %>%
  select(country, mcp_grouping, year, values) %>% arrange(mcp_grouping) %>% print(n = nrow(.))
sub_obj_4_2_bti_fiscal_stability %>% group_by(mcp_grouping) %>% skim(values)
sub_obj_4_2_bti_fiscal_stability %>% filter(year >= 2018) %>% group_by(mcp_grouping) %>% skim(values)


# plot
sub_obj_4_2_bti_fiscal_stability %>% 
   filter(mcp_grouping == "E&E Eurasia") %>%
  ggplot(data = ., mapping = aes(x = year, y = jitter(values, 1), color = country)) + geom_line(size = 1)


#/////////////////////////////////////////////////////////////////////////////////////////////


# read/write
sub_obj_4_2_bti_fiscal_stability %>% write_csv(file = "data/fmir/sub_obj_4_2_bti_fiscal_stability.csv")
sub_obj_4_2_bti_fiscal_stability <- read_csv(file = "data/fmir/sub_obj_4_2_bti_fiscal_stability.csv")


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
