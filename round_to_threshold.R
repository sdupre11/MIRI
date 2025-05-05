# # load add_dummies()
# current_wd <- getwd()
# setwd("H:/R/assorted_helper_scripts")
# source("round_to_threshold.R")
# setwd(current_wd)



# https://stackoverflow.com/questions/43627679/round-any-equivalent-for-dplyr
# https://github.com/tidyverse/ggplot2/releases/tag/v2.0.0
# https://ggplot2.tidyverse.org/reference/cut_interval.html

# note that ggplot2::cut_width is supposed to have replaced dplyr::round_any but it unhelpfully returns a factor, instead of numeric

# round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}


round_to_threshold <- function(number, accuracy, fun = c(ceiling, floor)) {
        
        fun(x = number / accuracy) * accuracy
}


#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


# # test
# round_to_threshold(number = .325, accuracy = .01, fun = ceiling)
# round_to_threshold(number = .325, accuracy = .05, fun = ceiling)
# round_to_threshold(number = .355, accuracy = .05, fun = ceiling)
# round_to_threshold(number = .325, accuracy = .1, fun = ceiling)
# round_to_threshold(number = .325, accuracy = 0, fun = ceiling)
# round_to_threshold(number = .325, accuracy = 1, fun = ceiling)
# 
# round_to_threshold(number = 325.253, accuracy = .1, fun = ceiling)
# round_to_threshold(number = 325.253, accuracy = .5, fun = ceiling)
# round_to_threshold(number = 325.553, accuracy = .5, fun = ceiling)
# round_to_threshold(number = 325.253, accuracy = .1, fun = floor)
# round_to_threshold(number = 325.253, accuracy = .1, fun = round)
# round_to_threshold(number = 325.243, accuracy = .1, fun = round)
# round_to_threshold(number = 325.243, accuracy = .5, fun = round)
# round_to_threshold(number = 325.543, accuracy = .5, fun = round)
# 
# round_to_threshold(number = 325.253, accuracy = 1, fun = ceiling)
# round_to_threshold(number = 325.253, accuracy = 5, fun = ceiling)
# round_to_threshold(number = 324.253, accuracy = 5, fun = ceiling)
# round_to_threshold(number = 325.253, accuracy = 1, fun = floor)
# round_to_threshold(number = 325.253, accuracy = 1, fun = round)
# round_to_threshold(number = 325.243, accuracy = 1, fun = round)
# round_to_threshold(number = 325.243, accuracy = 5, fun = round)
# round_to_threshold(number = 322.243, accuracy = 5, fun = round)
# 
# round_to_threshold(number = 325.253, accuracy = 10, fun = ceiling)
# round_to_threshold(number = 325.253, accuracy = 50, fun = ceiling)
# round_to_threshold(number = 355.253, accuracy = 50, fun = ceiling)
# round_to_threshold(number = 325.253, accuracy = 10, fun = floor)
# round_to_threshold(number = 325.253, accuracy = 10, fun = round)
# round_to_threshold(number = 325.243, accuracy = 10, fun = round)
# round_to_threshold(number = 325.243, accuracy = 50, fun = round)
# round_to_threshold(number = 324.243, accuracy = 50, fun = round)



