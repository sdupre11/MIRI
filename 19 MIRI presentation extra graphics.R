library(scales)

chart_data <- fmir %>% 
  filter(year == 2023, mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs")) %>% 
  select(country, mcp_grouping, obj_num, obj_avg, miri_avg) %>% distinct() %>%
  mutate(var = obj_num, 
         values = obj_avg / 5) %>%
  mutate(color_bin = case_when(mcp_grouping == "E&E Balkans" ~ 
                                 "E&E Balkans    ",
                               mcp_grouping == "E&E Eurasia" ~ 
                                 "E&E Eurasia    ",
                               mcp_grouping == "E&E graduates" ~ 
                                 "E&E Graduates    ",
                               mcp_grouping == "CARs" ~ 
                                 "CARs    "),
         color = case_when(mcp_grouping == "E&E Balkans" ~ "#4E75A2",
                           mcp_grouping == "E&E Eurasia" ~ "#DAB579",
                           mcp_grouping == "E&E graduates" ~ "#A2CDD9",
                           mcp_grouping == "CARs" ~ "#64C194"),
         color_bin_order = case_when(color_bin == "E&E Balkans    " ~ 3,
                                     color_bin == "E&E Eurasia    " ~ 4,
                                     color_bin == "E&E Graduates    " ~ 1,
                                     color_bin == "CARs    " ~ 5),
         color_bin = fct_reorder(.f = color_bin, .x = color_bin_order, .desc = TRUE))


# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color, color_bin_order) %>% 
  arrange(color_bin_order) %>%
  pull(color)

names(chart_data_color_list) <- chart_data %>% count(color_bin, color, color_bin_order) %>% 
  arrange(color_bin_order) %>%
  pull(color_bin)
chart_data_color_list


#/////////////////////

obj_charts <- function(objective) {
  
internal <- chart_data %>%
  filter(obj_num == objective)


# create chart
# for factsheet version with just E&E and graduates: ratio = 1.5 / 1 and font size = 9
# for chart deck version with E&E, graduates, and CARs: ratio = 3 / 1 and font size = 8
internal_plot <- internal %>% 
  ggplot(data = ., aes(x = fct_reorder(.f = country, .x = obj_avg, .desc = FALSE), 
                       y = obj_avg,
                       fill = color_bin)) + 
  geom_col(position = "stack", width = .75) + 
  scale_fill_manual(values = chart_data_color_list) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2),
                     limits = c(0, 1),
                     expand = c(0, 0),
                     labels = label_number(accuracy = .01)) +
  labs(x = NULL, y = "Score (higher is better)", 
       title = NULL,
       caption = NULL, fill = "") +
  coord_fixed(ratio = 8 / 1, clip = "off") +
  theme_bw() +
  theme(
    # plot.background = element_rect(fill = "blue"),
    plot.margin = unit(c(0, 8, 0, 2), "mm"),
    plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
    # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#DDDDDD"),
    # panel.grid.major.y = element_line(color = "#000000"),
    # panel.border = element_blank(),
    panel.border = element_rect(color = "#DDDDDD", size = .5),
    # panel.grid = element_blank(),
    # line = element_blank(),
    # rect = element_blank(),
    axis.ticks.y = element_blank(),
    # axis.ticks.x = element_blank(),
    # axis.ticks.length.y.left = unit(.2, "cm"),
    axis.ticks.length.x.bottom = unit(.2, "cm"),
    axis.text.x = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333",
                               margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1),
    # axis.text.x = element_blank(),
    axis.text.y = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                               margin = margin(t = 0, r = 5, b = 0, l = 0)),
    axis.line.x.bottom = element_line(color = "#333333"),
    axis.line.y.left = element_blank(),
    # axis.title.x = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
    #                             margin = margin(t = 13, r = 0, b = 5, l = 0)),
    axis.title.y = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                margin = margin(t = 0, r = 13, b = 0, l = 0)),
    plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                              margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
    legend.position = "bottom",
    # legend.key.size = unit(2, "mm"), 
    legend.title = element_text(size = 9, family = "Calibri", face = "plain", color = "#333333"),
    legend.text = element_text(size = 9, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                               hjust = .5, color = "#333333")
    # legend.spacing.y = unit(5.5, "cm"),
    # legend.spacing.x = unit(.3, "cm"),
    # legend.key = element_rect(size = 5),
    # legend.key.size = unit(2, 'lines')
    # legend.key.width = unit(2, "cm")
  ) + 
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, label.hjust = 0, color = "#333333"))

# inspect
internal_plot

return(internal_plot)
}

obj_1_by_country_bars <- obj_charts("obj_1")
obj_2_by_country_bars <- obj_charts("obj_2")
obj_3_by_country_bars <- obj_charts("obj_3")
obj_4_by_country_bars <- obj_charts("obj_4")
obj_co_by_country_bars <- obj_charts("obj_co")

ggsave(file = str_c("C:/Users/sdupre/Desktop/usaid/mcp/malign_influence/output/charts/results_presentation/obj_1_by_country_bars.png"), plot = obj_1_by_country_bars, 
       dpi = 400, 
       height = 5,
       width = 9)

ggsave(file = str_c("C:/Users/sdupre/Desktop/usaid/mcp/malign_influence/output/charts/results_presentation/obj_2_by_country_bars.png"), plot = obj_2_by_country_bars, 
       dpi = 400, 
       height = 5,
       width = 9)

ggsave(file = str_c("C:/Users/sdupre/Desktop/usaid/mcp/malign_influence/output/charts/results_presentation/obj_3_by_country_bars.png"), plot = obj_3_by_country_bars, 
       dpi = 400, 
       height = 5,
       width = 9)

ggsave(file = str_c("C:/Users/sdupre/Desktop/usaid/mcp/malign_influence/output/charts/results_presentation/obj_4_by_country_bars.png"), plot = obj_4_by_country_bars, 
       dpi = 400, 
       height = 5,
       width = 9)

ggsave(file = str_c("C:/Users/sdupre/Desktop/usaid/mcp/malign_influence/output/charts/results_presentation/obj_co_by_country_bars.png"), plot = obj_co_by_country_bars, 
       dpi = 400, 
       height = 5,
       width = 9)


#### Stacked dot charts for regional averages by objective ####

chart_data_temp <- fmir %>% 
  filter(year == 2023, mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs")) %>% 
  select(country, mcp_grouping, obj_num, obj_avg, miri_avg) %>% distinct() %>%
  group_by(mcp_grouping, obj_num) %>%
  summarize(obj_avg = mean(obj_avg),
            miri_avg = mean(miri_avg))

chart_data_temp_miri <- chart_data_temp %>%
  select(mcp_grouping, obj_num, miri_avg) %>%
  group_by(mcp_grouping) %>%
  summarize(obj_avg = mean(miri_avg)) %>%
  mutate(obj_num = "obj_miri")

chart_data <- chart_data_temp %>%
  select(mcp_grouping, obj_num, obj_avg) %>%
  group_by(mcp_grouping) %>%
  bind_rows(chart_data_temp_miri) %>%
 mutate(color_bin = case_when(mcp_grouping == "E&E Balkans" ~ 
                                 "E&E Balkans    ",
                               mcp_grouping == "E&E Eurasia" ~ 
                                 "E&E Eurasia    ",
                               mcp_grouping == "E&E graduates" ~ 
                                 "E&E Graduates    ",
                               mcp_grouping == "CARs" ~ 
                                 "CARs    "),
         color = case_when(mcp_grouping == "E&E Balkans" ~ "#4E75A2",
                           mcp_grouping == "E&E Eurasia" ~ "#DAB579",
                           mcp_grouping == "E&E graduates" ~ "#A2CDD9",
                           mcp_grouping == "CARs" ~ "#64C194"),
         color_bin_order = case_when(color_bin == "E&E Balkans    " ~ 4,
                                     color_bin == "E&E Eurasia    " ~ 3,
                                     color_bin == "E&E Graduates    " ~ 1,
                                     color_bin == "CARs    " ~ 5),
         color_bin = fct_reorder(.f = color_bin, .x = color_bin_order, .desc = TRUE),
        x_order = case_when(
          obj_num == "obj_1" ~ 1,
          obj_num == "obj_2" ~ 2,
          obj_num == "obj_3" ~ 3,
          obj_num == "obj_4" ~ 4,
          obj_num == "obj_co" ~ 5,
          obj_num == "obj_miri" ~ 0
        ))
  
  
  
 regions_stacked_dots <- ggplot(chart_data, aes(x = x_order, y = obj_avg, color = color_bin)) +
   geom_segment(
     data = chart_data %>%
       group_by(x_order) %>%
       summarise(y_min = min(obj_avg, na.rm = TRUE), y_max = max(obj_avg, na.rm = TRUE)),
     aes(x = x_order, xend = x_order, y = y_min, yend = y_max),
     inherit.aes = FALSE, # Prevents inheriting the global aesthetics
     color = "gray", size = 1
   ) + 
   geom_point(size = 6) +
    scale_x_continuous(breaks = NULL) + # Remove x-axis ticks
    # labs(
    #   y = "Objective Average",
    #   x = NULL,
    #   title = "Vertical String of Dots"
    # ) +
  scale_color_manual(values = chart_data_color_list) +
    # scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2),
                       limits = c(0, 1.1),
                       expand = c(0, 0),
                       labels = label_number(accuracy = .01)) +
    labs(x = NULL, y = "Score (higher is better)", 
         title = NULL,
         caption = NULL, fill = "") +
    coord_fixed(ratio = 8 / 1, clip = "off") +
    theme_bw() +
    theme(
      # plot.background = element_rect(fill = "blue"),
      plot.margin = unit(c(0, 8, 0, 2), "mm"),
      plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                  color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
      # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#DDDDDD"),
      # panel.grid.major.y = element_line(color = "#000000"),
      # panel.border = element_blank(),
      panel.border = element_rect(color = "#DDDDDD", size = .5),
      # panel.grid = element_blank(),
      # line = element_blank(),
      # rect = element_blank(),
      axis.ticks.y = element_blank(),
      # axis.ticks.x = element_blank(),
      # axis.ticks.length.y.left = unit(.2, "cm"),
      axis.ticks.length.x.bottom = unit(.2, "cm"),
      axis.text.x = element_blank(),
      # axis.text.x = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333",
      #                            margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1),
      # axis.text.x = element_blank(),
      axis.text.y = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                 margin = margin(t = 0, r = 5, b = 0, l = 0)),
      axis.line.x.bottom = element_line(color = "#333333"),
      axis.line.y.left = element_blank(),
      # axis.title.x = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
      #                             margin = margin(t = 13, r = 0, b = 5, l = 0)),
      axis.title.y = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                                  margin = margin(t = 0, r = 13, b = 0, l = 0)),
      plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
      legend.position = "bottom",
      # legend.key.size = unit(2, "mm"), 
      legend.title = element_text(size = 9, family = "Calibri", face = "plain", color = "#333333"),
      legend.text = element_text(size = 9, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                 hjust = .5, color = "#333333")
      # legend.spacing.y = unit(5.5, "cm"),
      # legend.spacing.x = unit(.3, "cm"),
      # legend.key = element_rect(size = 5),
      # legend.key.size = unit(2, 'lines')
      # legend.key.width = unit(2, "cm")
    ) + 
    guides(fill = guide_legend(nrow = 1, byrow = TRUE, label.hjust = 0, color = "#333333"))

 ggsave(file = str_c("C:/Users/sdupre/Desktop/usaid/mcp/malign_influence/output/charts/results_presentation/regions_stacked_dots_obj.png"), plot = regions_stacked_dots, 
        dpi = 400, 
        height = 5,
        width = 9)
  
 
 
 #### Armenia ####
chart_data <- fmir %>% 
   filter(country == "Armenia" & obj_num %in% c("obj_1", "obj_2", "obj_co")) %>% 
   select("indicator_name", "year", "indicator_standardized_values", "sub_obj_num") %>% 
   group_by(sub_obj_num, year) %>% 
   summarize(mean = mean(indicator_standardized_values)) #%>% 
   # pivot_wider(names_from = "year", values_from = "mean")
 
 
 ggplot(chart_data, aes(x = year, y = mean, group = sub_obj_num, color = as.factor(sub_obj_num))) +
   geom_line() +
   geom_point() +
   scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2),
                      limits = c(0, 1.1),
                      expand = c(0, 0),
                      labels = label_number(accuracy = .01)) +
   labs(x = NULL, y = "Score (higher is better)", 
        title = NULL,
        caption = NULL, fill = "") +
   scale_color_discrete(name = "Sub Objective Number") +
   # labs(title = "Mean Trends by Sub Objective Number",
   #      x = "Year",
   #      y = "Mean") +
   theme_minimal()
 
 
 chart_data <- fmir %>% 
   filter(country == "Armenia" & obj_num %in% c("obj_2")) %>% 
   select("indicator_name", "year", "indicator_standardized_values", "sub_obj_num") %>% 
   group_by(sub_obj_num, year) %>% 
   summarize(mean = mean(indicator_standardized_values)) #%>% 
 # pivot_wider(names_from = "year", values_from = "mean")
 
 
 ggplot(chart_data, aes(x = year, y = mean, group = sub_obj_num, color = as.factor(sub_obj_num))) +
   geom_line() +
   geom_point() +
   scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2),
                      limits = c(0, 1.1),
                      expand = c(0, 0),
                      labels = label_number(accuracy = .01)) +
   labs(x = NULL, y = "Score (higher is better)", 
        title = NULL,
        caption = NULL, fill = "") +
   scale_color_discrete(name = "Sub Objective Number") +
   # labs(title = "Mean Trends by Sub Objective Number",
   #      x = "Year",
   #      y = "Mean") +
   theme_minimal()
 
 
 #### Azerbaijan ####
 
 chart_data <- fmir %>% 
   filter(country == "Azerbaijan" & obj_num %in% c("obj_3")) %>% 
   select("indicator_name", "year", "indicator_standardized_values", "sub_obj_num") %>% 
 pivot_wider(names_from = "year", values_from = "indicator_standardized_values")
 
 chart_data <- read_excel(path = "data/iea/new/ImpData.xlsx") %>%
   filter(source == "Russia" & country_name == "Azerbaijan")
 
 ggplot(chart_data, aes(x = year, y = values, group = source, color = as.factor(source))) +
   geom_line(linewidth = 1.5) +
   geom_point() +
   scale_y_continuous(breaks = seq(from = 0, to = 40000, by = 10000),
                      limits = c(0, 40000),
                      expand = c(0, 0),
                      labels = label_number(accuracy = 1)) +
   labs(x = NULL, y = "Import Total (in TJ)", 
        title = NULL,
        caption = NULL, fill = "") +
   scale_color_discrete(name = "Sub Objective Number") +
   # labs(title = "Mean Trends by Sub Objective Number",
   #      x = "Year",
   #      y = "Mean") +
   theme_minimal()
 
 #### Georgia ####
 
 chart_data <- fmir %>% 
   filter(country == "Georgia" & obj_num %in% c("obj_2", "obj_co")) %>% 
   select("indicator_name", "year", "indicator_standardized_values", "sub_obj_num") %>% 
   group_by(sub_obj_num, year) %>% 
   summarize(mean = mean(indicator_standardized_values)) #%>% 
 # pivot_wider(names_from = "year", values_from = "mean")
 
 ggplot(chart_data, aes(x = year, y = mean, group = sub_obj_num, color = as.factor(sub_obj_num))) +
   geom_line() +
   geom_point() +
   scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2),
                      limits = c(0, 1.1),
                      expand = c(0, 0),
                      labels = label_number(accuracy = .01)) +
   labs(x = NULL, y = "Score (higher is better)", 
        title = NULL,
        caption = NULL, fill = "") +
   scale_color_discrete(name = "Sub Objective Number") +
   # labs(title = "Mean Trends by Sub Objective Number",
   #      x = "Year",
   #      y = "Mean") +
   theme_minimal()
 
 #### Albania ####
 
 chart_data <- fmir %>% 
   filter(country == "Albania" & obj_num %in% c("obj_2", "obj_co")) %>% 
   select("indicator_name", "year", "indicator_standardized_values", "sub_obj_num") %>% 
   group_by(sub_obj_num, year) %>% 
   summarize(mean = mean(indicator_standardized_values)) #%>% 
 #pivot_wider(names_from = "year", values_from = "mean")
 
 ggplot(chart_data, aes(x = year, y = mean, group = sub_obj_num, color = as.factor(sub_obj_num))) +
   geom_line() +
   geom_point() +
   scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2),
                      limits = c(0, 1.1),
                      expand = c(0, 0),
                      labels = label_number(accuracy = .01)) +
   labs(x = NULL, y = "Score (higher is better)", 
        title = NULL,
        caption = NULL, fill = "") +
   scale_color_discrete(name = "Sub Objective Number") +
   # labs(title = "Mean Trends by Sub Objective Number",
   #      x = "Year",
   #      y = "Mean") +
   theme_minimal()
 
 #### Slovakia ####
 
 chart_data <- fmir %>% 
   filter(country == "Slovakia" & obj_num %in% c("obj_2")) %>% 
   select("indicator_name", "year", "indicator_standardized_values", "sub_obj_num") #%>% 
   # group_by(sub_obj_num, year) %>% 
   # summarize(mean = mean(indicator_standardized_values)) %>% 
 # pivot_wider(names_from = "year", values_from = "indicator_standardized_values")
 
 ggplot(chart_data, aes(x = year, y = indicator_standardized_values, group = indicator_name, color = as.factor(indicator_name))) +
   geom_line() +
   geom_point() +
   scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2),
                      limits = c(0, 1.1),
                      expand = c(0, 0),
                      labels = label_number(accuracy = .01)) +
   labs(x = NULL, y = "Score (higher is better)", 
        title = NULL,
        caption = NULL, fill = "") +
   scale_color_discrete(name = "Sub Objective Number") +
   # labs(title = "Mean Trends by Sub Objective Number",
   #      x = "Year",
   #      y = "Mean") +
   theme_minimal()
 
 
 #### Lithuania ####
 
 chart_data <- fmir %>% 
   filter(country == "Lithuania" & obj_num %in% c("obj_3")) %>% 
   filter(indicator_name %in% c("sub_obj_3_1_iea_natural_gas_imports_from_FMI_as_share_of_natural_gas_imports",
                                "sub_obj_3_2_rise_energy_efficiency_regulation_rank",
"sub_obj_3_2_rise_renewable_energy_regulation_rank",
"sub_obj_3_2_itu_regulatory_authority",
"sub_obj_3_2_wef_market_dominance"))
   select("indicator_name", "year", "indicator_standardized_values", "sub_obj_num") #%>% 
   # group_by(sub_obj_num, year) %>% 
   # summarize(mean = mean(indicator_standardized_values)) %>% 
 # pivot_wider(names_from = "year", values_from = "indicator_standardized_values")
 
 ggplot(chart_data, aes(x = year, y = indicator_standardized_values, group = indicator_name, color = as.factor(indicator_name))) +
   geom_line() +
   geom_point() +
   scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2),
                      limits = c(0, 1.1),
                      expand = c(0, 0),
                      labels = label_number(accuracy = .01)) +
   labs(x = NULL, y = "Score (higher is better)", 
        title = NULL,
        caption = NULL, fill = "") +
   scale_color_discrete(name = "Sub Objective Number") +
   # labs(title = "Mean Trends by Sub Objective Number",
   #      x = "Year",
   #      y = "Mean") +
   theme_minimal()
 
 
 #### Uzbekistan ####
 
 chart_data <- fmir %>% 
   filter(country == "Uzbekistan" & sub_obj_num %in% c("sub_obj_2_2")) %>% 
 select("indicator_name", "year", "indicator_standardized_values", "sub_obj_num") %>% 
 group_by(sub_obj_num, year) %>%
 summarize(mean = mean(indicator_standardized_values)) #%>%
 # pivot_wider(names_from = "year", values_from = "indicator_standardized_values")
 
 ggplot(chart_data, aes(x = year, y = mean, group = sub_obj_num, color = as.factor(sub_obj_num))) +
   geom_line(linewidth=1.5) +
   # geom_point() +
   scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2),
                      limits = c(0, 1.1),
                      expand = c(0, 0),
                      labels = label_number(accuracy = .01)) +
   labs(x = NULL, y = "Score (higher is better)", 
        title = NULL,
        caption = NULL, fill = "") +
   scale_color_discrete(name = "Sub Objective Number") +
   # labs(title = "Mean Trends by Sub Objective Number",
   #      x = "Year",
   #      y = "Mean") +
   theme_minimal()
 
 
 #### Belarus v Serbia ####
 
 chart_data <- fmir %>% 
   filter(country == "Belarus" & obj_num %in% c("obj_4")) %>% 
   select("indicator_name", "year", "indicator_standardized_values", "sub_obj_num") %>% 
   group_by(sub_obj_num, year) %>%
   summarize(mean = mean(indicator_standardized_values)) %>%
 pivot_wider(names_from = "year", values_from = "mean")
 
 chart_data <- fmir %>% 
   filter(country == "Belarus" & sub_obj_num %in% c("sub_obj_4_3")) %>% 
   select("indicator_name", "year", "indicator_standardized_values", "sub_obj_num") #%>% 
   # group_by(sub_obj_num, year) %>%
   # summarize(mean = mean(indicator_standardized_values)) %>%
   # pivot_wider(names_from = "year", values_from = "indicator_standardized_values")
 
 
 ggplot(chart_data, aes(x = year, y = indicator_standardized_values, group = indicator_name, color = as.factor(indicator_name))) +
   geom_line(linewidth=1.5) +
   # geom_point() +
   scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2),
                      limits = c(0, 1.1),
                      expand = c(0, 0),
                      labels = label_number(accuracy = .01)) +
   labs(x = NULL, y = "Score (higher is better)", 
        title = NULL,
        caption = NULL, fill = "") +
   scale_color_discrete(name = "Sub Objective Number") +
   # labs(title = "Mean Trends by Sub Objective Number",
   #      x = "Year",
   #      y = "Mean") +
   theme_minimal()
 
 
 
 chart_data <- fmir %>% 
   filter(country == "Belarus" & obj_num %in% c("obj_2")) %>% 
   select("indicator_name", "year", "indicator_standardized_values", "sub_obj_num") %>% 
 group_by(sub_obj_num, year) %>%
 summarize(mean = mean(indicator_standardized_values)) #%>%
 # pivot_wider(names_from = "year", values_from = "indicator_standardized_values")
 
 
 ggplot(chart_data, aes(x = year, y = mean, group = sub_obj_num, color = as.factor(sub_obj_num))) +
   geom_line(linewidth=1.5) +
   # geom_point() +
   scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2),
                      limits = c(0, 1.1),
                      expand = c(0, 0),
                      labels = label_number(accuracy = .01)) +
   labs(x = NULL, y = "Score (higher is better)", 
        title = NULL,
        caption = NULL, fill = "") +
   scale_color_discrete(name = "Sub Objective Number") +
   # labs(title = "Mean Trends by Sub Objective Number",
   #      x = "Year",
   #      y = "Mean") +
   theme_minimal()
 
 
 chart_data <- fmir %>% 
   filter(country == "Serbia" & obj_num %in% c("obj_2")) %>% 
   select("indicator_name", "year", "indicator_standardized_values", "sub_obj_num") %>% 
   group_by(sub_obj_num, year) %>%
   summarize(mean = mean(indicator_standardized_values)) #%>%
 # pivot_wider(names_from = "year", values_from = "indicator_standardized_values")
 
 
 ggplot(chart_data, aes(x = year, y = mean, group = sub_obj_num, color = as.factor(sub_obj_num))) +
   geom_line(linewidth=1.5) +
   # geom_point() +
   scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2),
                      limits = c(0, 1.1),
                      expand = c(0, 0),
                      labels = label_number(accuracy = .01)) +
   labs(x = NULL, y = "Score (higher is better)", 
        title = NULL,
        caption = NULL, fill = "") +
   scale_color_discrete(name = "Sub Objective Number") +
   # labs(title = "Mean Trends by Sub Objective Number",
   #      x = "Year",
   #      y = "Mean") +
   theme_minimal()
 
 
 
 #### Media Freedom ####

 sub_obj_1_1_vdem_judicial_constraints_on_exec <- vdem %>% select(country_name, 
                                                                  year, 
                                                                  v2caautmob,
                                                                  v2x_diagacc,
                                                                  v2xme_altinf,
                                                                  v2x_freexp,
                                                                  v2x_freexp_altinf) %>%
   filter(year >= 2016) %>%
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