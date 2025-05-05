# # load run_loop()
# current_wd <- getwd()
# setwd("H:/R/assorted_helper_scripts")
# source("get_label_position.R")
# setwd(current_wd)

# create get_label_position() for use in facet charts ####

get_label_position <- function(chart_data, label_var = "country", shift = 0, gap = .04) {
        
        # handle label_var arg, converting input to strings
        
        # handle single bare variables passed as label_var
        if(deparse(substitute(label_var)) %in% names(chart_data)) {
                label_var <- deparse(substitute(label_var))
                
        } else if("quosure" %in% class(label_var) | "quosures" %in% class(label_var)) {
                
                # handle label_var if it's passed using quo(), quos(), or label_var(), including tidyselect helpers
                label_var <- chart_data %>% ungroup() %>% select(!!!(label_var)) %>% names()
                
        } else if(class(label_var) == "character") {
                
                # handle label_var as a string
                label_var <- label_var
        }
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # get last_year_in_data
        chart_data <- chart_data %>% 
                group_by(!!!syms(label_var)) %>% arrange(desc(year)) %>%
                mutate(non_missing_flag = case_when(!is.na(values) ~ 1, 
                                                    TRUE ~ 0),
                       new_to_old_cumsum_non_missing_flag = cumsum(non_missing_flag),
                       last_year_in_data = case_when(new_to_old_cumsum_non_missing_flag == 1 & non_missing_flag == 1 ~ year),
                       last_values_in_data = case_when(new_to_old_cumsum_non_missing_flag == 1 & non_missing_flag == 1 ~ values)) %>%
                fill(last_year_in_data, .direction = "downup") %>%
                fill(last_values_in_data, .direction = "downup") %>%
                ungroup()
        
        # get initial label_position
        chart_data <- chart_data %>% 
                mutate(label_values = case_when((is.na(values)) & (year == max(year)) ~ last_values_in_data,
                                                TRUE ~ values)) %>%
                arrange(year, label_values) %>% 
                group_by(year) %>%
                mutate(label_position = round(label_values, digits = 3),
                       dist_to_lower = round(label_position - lag(label_position, n = 1), digits = 3),
                       dist_to_higher = round(lead(label_position, n = 1) - label_position, digits = 3),
                       overlap_flag = case_when(dist_to_lower < gap ~ 1,
                                                TRUE ~ 0)) %>%
                ungroup()
        
        # set counter
        counter <- 0
        
        # run loop to update label_position
        while((chart_data %>% mutate(sum_overlap_flag = sum(overlap_flag)) %>% slice(1) %>% pull(sum_overlap_flag)) != 0) {
                
                counter <- counter + 1
                print(counter + 1)
                
                chart_data <- chart_data %>% arrange(year, label_values) %>% 
                        group_by(year) %>%
                        mutate(label_position = case_when(is.na(dist_to_lower) | 
                                                                  dist_to_lower >= gap ~ round(label_position, digits = 3),
                                                          TRUE ~ round(label_position + (gap - dist_to_lower), digits = 3)),
                               dist_to_higher = round(lead(label_position, n = 1) - label_position, digits = 3),
                               dist_to_lower = round(label_position - lag(label_position, n = 1), digits = 3),
                               overlap_flag = case_when(dist_to_lower < gap ~ 1,
                                                        TRUE ~ 0)) %>%
                        ungroup() %>%
                        arrange(desc(year), label_values)
        }
        
        # apply shift
        chart_data <- chart_data %>% mutate(label_position = label_position + shift)
        
        # return
        return(chart_data)
}


#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////


# example

# set.seed(123)
# tibble(country = rep(c("U.S.", "France", "U.K.", "Germany", "Australia", "Japan"), times = 2),
#        year = c(rep(2010, times = 6), rep(2020, times = 6)),
#        values = sample(seq(from = .40, to = .60, by = .01), size = 12)) %>%
#         arrange(country) %>%
#         get_label_position()
# 
# set.seed(123)
# tibble(country = rep(c("U.S.", "France", "U.K.", "Germany", "Australia", "Japan"), times = 2),
#        year = c(rep(2010, times = 6), rep(2020, times = 6)),
#        values = sample(seq(from = .40, to = .60, by = .01), size = 12)) %>%
#         arrange(country) %>%
#         get_label_position(label_var = country)
# 
# # specify shift arg
# set.seed(123)
# tibble(mcp_grouping = rep(c("U.S.", "E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs", "EU-15"), times = 2),
#        year = c(rep(2010, times = 6), rep(2020, times = 6)),
#        values = sample(seq(from = .40, to = .60, by = .01), size = 12)) %>%
#         arrange(mcp_grouping) %>%
#         get_label_position(label_var = mcp_grouping, shift = .10)
# 
# # specify shift arg
# set.seed(123)
# tibble(country = rep(c("U.S.", "France", "U.K.", "Germany", "Australia", "Japan"), times = 2),
#        year = c(rep(2010, times = 6), rep(2020, times = 6)),
#        values = sample(seq(from = .40, to = .60, by = .01), size = 12)) %>%
#         arrange(country) %>%
#         get_label_position(label_var = country, shift = -.10)
# 
# # specify gap arg
# set.seed(123)
# tibble(country = rep(c("U.S.", "France", "U.K.", "Germany", "Australia", "Japan"), times = 2),
#        year = c(rep(2010, times = 6), rep(2020, times = 6)),
#        values = sample(seq(from = .40, to = .60, by = .01), size = 12)) %>%
#         arrange(country) %>%
#         get_label_position(label_var = country, gap = .02)
# 
# # specify shift and gap arg
# set.seed(123)
# tibble(country = rep(c("U.S.", "France", "U.K.", "Germany", "Australia", "Japan"), times = 2),
#        year = c(rep(2010, times = 6), rep(2020, times = 6)),
#        values = sample(seq(from = .40, to = .60, by = .01), size = 12)) %>%
#         arrange(country) %>%
#         get_label_position(label_var = country, shift = .10, gap = .02)
