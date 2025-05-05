extrafont::loadfonts(device="win") 
library(tidyverse)
library(lubridate)
library(readxl)
library(skimr)
library(rlang)
library(haven)
library(fs)
library(janitor)
library(ggridges)
library(officer)
library(devEMF)
library(ggrepel)
library(rvest)
library(scales)
library(testthat)
library(patchwork)
library(vdemdata)
library(naniar)
library(dichromat)
library(RColorBrewer)
library(fmsb)
library(ggcorrplot)
library(bazar)
library(cowplot)
library(ggplotify)
library(ggforce)
library(magick)
library(ggpubr)
library(grid)
library(rworldxtra)
library(sf)        
library(lwgeom)    
library(rworldmap)
library(pals)
library(openxlsx)


setwd("C:/Users/sdupre/Desktop/usaid/mcp/malign_influence")
options(scipen = 999)


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


# note that for coord_fixed_ratio the optimal ratio was 10/1 when y-axis spanned range of .6
# so the y-axis units were given pixels equal to (range = .6) * (coord_fixed_numerator = 10) = (pixel units = 6)
# so to get the same size chart with different y-axis range, 
# use formula: range * coord_fixed_numerator = pixel_units; 
# so: (range = 1) * (unknown_coord_fixed_numerator) = (pixel_units = 6)
# after rearranging: unknown_coord_fixed_numerator = (pixel_units = 6) / (range = 1)
# solving: unknown_coord_fixed_numerator = 6
# note the coord_fixed_denominator is 1, and doesn't need to be adjusted in this case because x-axis units are unchanged
# but if needed, could adjust coord_fixed_denominator by similar formula
# range * coord_fixed_denominator = pixel_units


#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////


# read country_crosswalk ####
current_wd <- getwd()
setwd("C:/Users/sdupre/Desktop/usaid/mcp/useful_info/country_crosswalk")
country_crosswalk <- read_csv("C:/Users/sdupre/Desktop/usaid/mcp/useful_info/country_crosswalk/country_crosswalk.csv", lazy = FALSE)
setwd(current_wd)


#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////


# read round_to_threshold ####
current_wd <- getwd()
setwd("C:/Users/sdupre/Desktop/usaid/mcp/code/assorted_helper_scripts")
source("round_to_threshold.R")
setwd(current_wd)


#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////


# read get_label_position ####
current_wd <- getwd()
setwd("C:/Users/sdupre/Desktop/usaid/mcp/code/assorted_helper_scripts")
source("get_label_position.R")
setwd(current_wd)


#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////


# read round_to_digits ####
current_wd <- getwd()
setwd("C:/Users/sdupre/Desktop/usaid/mcp/code/assorted_helper_scripts")
source("round_to_digits.R")
setwd(current_wd)


#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////


# read final fmir data ####
fmir <- read_excel(path = "data/fmir/fmir_20250123.xlsx")


# inspect
fmir
fmir %>% glimpse()
fmir %>% nrow() # 35604
fmir %>% ncol() # 67


fmir %>% count(obj)
fmir %>% count(obj_num)
fmir %>% count(obj_short_name)
fmir %>% count(sub_obj)
fmir %>% count(sub_obj_num)
fmir %>% count(sub_obj_short_name)
fmir %>% count(concept)
fmir %>% count(indicator_name) %>% print(n = nrow(.))


# #/////////////////////////////////////////////////////////////////////////////////////////////////
# #/////////////////////////////////////////////////////////////////////////////////////////////////
# #/////////////////////////////////////////////////////////////////////////////////////////////////
# country profiles ####
# load get_country_profile() 
# source("code/get_country_profile.R")

fmir %>% filter(mcp_grouping %in% c("E&E Balkans", 
                                    "E&E Eurasia",
                                    "E&E graduates",
                                    "CARs"
                                    )) %>%
        distinct(country) %>% pull(country) %>%
        walk(.x = ., .f = ~ get_country_profile(current_country = .x, current_fy = 2024))

