# # load get_group_index()
# current_wd <- getwd()
# setwd("H:/R/helper_scripts")
# source("add_group_index.R")
# setwd(current_wd)


library(tidyverse)

# create add_group_index function
# this avoids group_indices() issue of creating indices based on alphabetical ordering of group var, not on the order data appears
# also, avoids group_indices issue of not being able to handle grouped tbls without extra workarounds with group_map etc
add_group_index <- function(data, group_vars, group_name = NULL) {
  
  # handle group_vars arg, converting input to strings
  
  # handle single bare variables passed as group_vars
  if(deparse(substitute(group_vars)) %in% names(data)) {
    
    group_vars <- deparse(substitute(group_vars))
    
  } else if("quosure" %in% class(group_vars) | "quosures" %in% class(group_vars)) {
    
    # handle group_vars if it's passed using quo(), quos(), or group_vars(), including tidyselect helpers
    group_vars <- data %>% ungroup() %>% select(!!!(group_vars)) %>% names()
    
  } else if(class(group_vars) == "character") {
    
    # handle group_vars as a string
    group_vars <- group_vars
  }
  
  
  ################################################################################################################################
  
  
  # handle ungrouped tbl
  if(is.null(data %>% groups())) {
    
    if(is.null(group_name)) {
      return(data %>% distinct(!!!syms(group_vars)) %>% mutate(group_index = row_number()) %>% left_join(data, ., by = group_vars)) 
    }
    
    if(!(is.null(group_name))) {
      return(data %>% distinct(!!!syms(group_vars)) %>% mutate(group_index = row_number()) %>% left_join(data, ., by = group_vars) %>%
               rename(!!sym(group_name) := group_index)) 
    }
  }
  
  
  #######################
  
  
  # handle grouped tbl
  if(!(is.null(data %>% groups()))) {
    
    # get grouping_var from grouped tbl
    grouping_var <- data %>% groups() %>% map(.x = ., .f = ~ as_label(.x)) %>% unlist()
    
    # select just grouping_var before splitting out groups to reduce compute time,
    # then get group index based on order that group values appear in data, left_join group index with data,
    # and return as an ungrouped tbl
    
    if(is.null(group_name)) {
      return(data %>% distinct(!!!syms(group_vars)) %>% mutate(group_index = row_number()) %>% ungroup() %>% 
               left_join(data, ., by = c(grouping_var, group_vars))) 
    }
    
    if(!(is.null(group_name))) {
      return(data %>% distinct(!!!syms(group_vars)) %>% mutate(group_index = row_number()) %>% ungroup() %>% 
               left_join(data, ., by = c(grouping_var, group_vars)) %>% rename(!!sym(group_name) := group_index)) 
    }
    
  }
  
}


#####################


# # test add_group_index()
# starwars %>% add_group_index(group_vars = "species")
# starwars %>% add_group_index(group_vars = species)
# starwars %>% add_group_index(group_vars = vars(species, gender))
# 
# starwars %>% mutate(movie = sample(x = c("old", "new"), size = nrow(.), replace = TRUE)) %>%
#         group_by(movie) %>% add_group_index(group_vars = species, group_name = "my_group_index") %>%
#         select(movie, species, my_group_index) %>% print(n = 15)
# starwars %>% mutate(movie = sample(x = c("old", "new"), size = nrow(.), replace = TRUE),
#                     good_or_bad = sample(x = c("good", "bad"), size = nrow(.), replace = TRUE)) %>%
#         group_by(movie) %>% add_group_index(group_vars = vars(species, good_or_bad), group_name = "my_group_index") %>%
#         select(movie, species, good_or_bad, my_group_index) %>% print(n = 15)
# starwars %>% mutate(movie = sample(x = c("old", "new"), size = nrow(.), replace = TRUE),
#             good_or_bad = sample(x = c("good", "bad"), size = nrow(.), replace = TRUE)) %>%
# group_by(movie, gender) %>% add_group_index(group_vars = vars(species, good_or_bad), group_name = "my_group_index") %>%
# select(movie, gender, species, good_or_bad, my_group_index) %>% print(n = 15)


##############################################################################################################################
##############################################################################################################################
##############################################################################################################################


# # create add_group_index_2() using group_split()
# # result: this version is slower
# add_group_index_2 <- function(data, group) {
#         
#         # get var_names from var_inputs
#         
#         # handle single bare variables passed as group
#         # the first negated str_detect condition will return TRUE if group is not a character
#         # the second negated str_detect condition returns TRUE if group deparsed isn't wrapped in "vars()"
#         if((!(str_detect(string = deparse(substitute(group)), pattern = regex("^\".*\"$|^c\\(\".*\"\\)$")))) &
#            (!(str_detect(string = deparse(substitute(group)), pattern = regex("^vars\\(.*\\)$"))))) {
#                 
#                 group <- deparse(substitute(group))
#         } else
#                 
#                 # handle group if it's passed using quo(), quos(), or vars(), including tidyselect helpers
#                 if((!(str_detect(string = deparse(substitute(group)), pattern = regex("^\".*\"$|^c\\(\".*\"\\)$")))) &
#                    (str_detect(string = deparse(substitute(group)), pattern = regex("^vars\\(.*\\)$")))) {
#                         
#                         group <- group %>% map(.x = ., .f = as_label) %>% unlist()
#                 } else
#                         
#                         # handle group as a string
#                         if(class(group) == "character") {
#                                 
#                                 group <- group
#                         }
#         
#         
#         ################################################################################################################################
#         
#         
#         # handle ungrouped tbl
#         if(is.null(data %>% groups())) {
#                 
#                 # get group index based on order that group values appear in data, left_join group index with data, then return as a tbl
#                 return(data %>% distinct(!!!syms(group)) %>% mutate(group_index = row_number()) %>% left_join(data, ., by = group)) 
#         }
#         
#         
#         #######################
#         
#         
#         # handle grouped tbl
#         if(!(is.null(data %>% groups()))) {
#                 
#                 # get grouping_var from grouped tbl
#                 grouping_var <- data %>% groups() %>% map(.x = ., .f = ~ as_label(.x)) %>% unlist()
#                 
#                 # select just grouping_var before splitting out groups to reduce compute time,
#                 # then get group index based on order that group values appear in data, left_join group index with data,
#                 # and return as a tbl
#                 return(data %>% select(!!!syms(grouping_var), !!!syms(group)) %>% group_split() %>%
#                                map_dfr(.x = ., .f = ~ .x %>% distinct(!!!syms(grouping_var), !!!syms(group)) %>% 
#                                                mutate(group_index = row_number())) %>%
#                                left_join(data, ., by = c(grouping_var, group)))
#         }
#         
# }


##################################


# test add_group_index_2()
# starwars %>% add_group_index_2(group = "species")
# starwars %>% add_group_index_2(group = species)
# starwars %>% add_group_index_2(group = vars(species, gender))
# 
# starwars %>% mutate(movie = sample(x = c("old", "new"), size = nrow(.), replace = TRUE)) %>%
#         group_by(movie) %>% add_group_index_2(group = species) %>% print(n = 15)
# starwars %>% mutate(movie = sample(x = c("old", "new"), size = nrow(.), replace = TRUE),
#                     good_or_bad = sample(x = c("good", "bad"), size = nrow(.), replace = TRUE)) %>%
#         group_by(movie) %>% add_group_index_2(group = vars(species, good_or_bad)) %>% 
#         select(movie, species, good_or_bad, group_index) %>% print(n = 15)
# starwars %>% mutate(movie = sample(x = c("old", "new"), size = nrow(.), replace = TRUE),
#                     good_or_bad = sample(x = c("good", "bad"), size = nrow(.), replace = TRUE)) %>%
#         group_by(movie, gender) %>% add_group_index_2(group = vars(species, good_or_bad)) %>% 
#         select(movie, gender, species, good_or_bad, group_index) %>% print(n = 15)


###############################################################################################


# # note that using match/unique workaround for this problem is slower than add_group_index
# # see preserve_order_for_group_indices.R
# 
# # create preserve_order_for_group_indices function
# preserve_order_for_group_indices <- function(x) {
#         match(x, unique(x))
# }
# 
# # works on ungrouped tbl
# df %>% mutate(group_index = group_indices(., category) %>% preserve_order_for_group_indices())
# 
# # but group_indices() doesn't work on grouped tbl directly
# starwars %>% mutate(movie = sample(x = c("old", "new"), size = nrow(.), replace = TRUE)) %>%
#         group_by(movie) %>% mutate(group_index = group_indices(., species) %>% preserve_order_for_group_indices())
# 
# # need to use group_map with grouped_tbl,
# # and some part of the necessary reshaping (e.g. enframe, unnest, etc) with this method makes is slower than add_group_index


##################


# conduct speed test

# # create wrapper functions for speed test
# test_group_indices <- function(.x) {
#         set.seed(123)
#         starwars %>% mutate(movie = sample(x = c("old", "new"), size = nrow(.), replace = TRUE), row_number = row_number()) %>%
#                 group_by(movie) %>%
#                 group_map(.f = ~ mutate(., group_index = group_indices(., species) %>% preserve_order_for_group_indices()),
#                           keep = TRUE) %>%
#                 enframe() %>% select(value) %>% unnest(value) %>% arrange(row_number) %>% select(- row_number) %>% group_by(movie)
# }
# test_group_indices()
# 
# test_add_group_index <- function(.x) {
#         set.seed(123)
#         starwars %>% mutate(movie = sample(x = c("old", "new"), size = nrow(.), replace = TRUE)) %>%
#                 group_by(movie) %>%
#                 add_group_index(group = species)
# }
# test_add_group_index()
# 
# test_add_group_index_2 <- function(.x) {
#         set.seed(123)
#         starwars %>% mutate(movie = sample(x = c("old", "new"), size = nrow(.), replace = TRUE)) %>%
#                 group_by(movie) %>%
#                 add_group_index_2(group = species)
# }
# test_add_group_index_2()
# 
# # confirm identical output
# identical(test_group_indices(), test_add_group_index())
# identical(test_group_indices(), test_add_group_index_2())
# 
# # test
# # result: add_group_index is significantly faster
# library(tictoc)
# 
# tic()
# walk(.x = 1:1000, .f = test_group_indices)
# toc()
# 
# tic()
# walk(.x = 1:1000, .f = test_add_group_index)
# toc()
# 
# tic()
# walk(.x = 1:1000, .f = test_add_group_index_2)
# toc()