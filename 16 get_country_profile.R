# country_profiles ####

# note the best workflow is to create plot_layout with patchwork, but mainly just to establish output size that will cover entire
# pdf page with out need for any resizing in word before converting to pdf (just horizontal/vertical centering)
# note the pdf page has wider margins than word doc, so in word the plot will look to be over edge of margins, but
# once print to pdf it will be within pdf page boundaries (setting word margins to zero didn't have any effect)
# then all plots/images/text are saved as png, and added with cowplot::draw_image/label
# cowplot gives more control than patchwork::inset_element, since inset_element only works on ggplot objects, not png
# also the coordinates for inset_elements are highly finicky based on adding/subtracting additional elements, requiring lots of tuning
# but cowplot draw_image coordinates stay fixed even when adding/subtracting other images - a huge time saver for adjustments

# to get wider/narrower png shape on page, use combo of aspect.ratio for relative height, but then also 
# adjust draw_image width/height (cropped as needed)
# note that the draw_image x/y coordinates can specify the center, left, or right of image, depending on hjust/vjust arg

# note that manually adding an image or shape to word output caused some kind of issue preventing the banner across top
# from spanning the entire pdf. but if no manual image/shapes are added to word, the banner print correctly to pdf spanning page

# notes on ggsave dpi
# https://largeprinting.com/resources/image-resolution-and-dpi.html
# main takeaway is 300 dpi is the standard for high resolution print graphics
# to confirm 300 dpi image on printed page, divide pixel count by 300 dpi, to get pixels/dots per inch
# ggsave has arguments for dpi and height width - the output pixel dimensions will be width * dpi
# so dpi = 300 and width = 6 yields an 1800 x 1800 image file, which can be displayed at 300 dpi on printed page if 6 inch or less
# "So, if you want to print an image that is 1024 ? 768 (listed as Width=1024px, Height=768px on a PC), 
# you need to divide each value by 300 to see how many inches you can print at 300 dpi.
# 1024 / 300 = 3.4133 (width)
# 768 / 300 = 2.56 (height)
# So, you could print this 1024px ? 768px image at 300 DPI at a size of 3.4133 ? 2.56 - 
# any bigger than this, and you risk the image becoming pixellated"

# note that getting rounded rectangle can be problematic in extreme cases because geom_shape has an aspect ratio limit for wide/narrow, 
# and element_rect_round has a weird outcome where it automatically fills the entire background when saved as png
# but the geom_shape aspect.ratio limit is not binding for this use case
# this is just an issue with geom_shape though, geom_rect supports any aspect.ratio
# widening ggsave width just widens the printed white box, but the plot itself still retains its same aspect ratio


#////////////////////////////////////////////////////////////////////////////////////////////////////


# create get_country_profile()
get_country_profile <- function(current_country, current_fy) {
        
        print(current_country)
        
        # get current_country_full_name
        current_country_full_name <- case_when(current_country == "BiH" ~ "Bosnia and Herzegovina", #"Bosnia and\nHerzegovina",
                                               current_country == "N. Macedonia" ~ "North Macedonia", #"North\nMacedonia",
                                               current_country == "U.K." ~ "United Kingdom", #"United\nKingdom",  #Updated, but UK no longer applicable here
                                               TRUE ~ current_country)
        
        
        #////////////////////////////////////////////////////////////////////////////////////////////////////
        #////////////////////////////////////////////////////////////////////////////////////////////////////
        #////////////////////////////////////////////////////////////////////////////////////////////////////
        
        # create get_country_profile_radar_chart() ####
        #/////////////////////////
        
        
        get_country_profile_radar_chart <- function (df, country = NULL, axistype = 0, seg = 4, pty = 16, pcol = 1:8, plty = 1:6, 
                                                     plwd = 1, pdensity = NULL, pangle = 45, pfcol = NA, cglty = 3, 
                                                     cglwd = 1, cglcol = "navy", axislabcol = "blue", vlabcol = "black", vlab_fontface = 1, title = "", 
                                                     maxmin = TRUE, na.itp = TRUE, centerzero = FALSE, vlabels = NULL, 
                                                     vlcex = NULL, caxislabels = NULL, calcex = NULL, paxislabels = NULL, 
                                                     palcex = NULL, ...) {
                
                # set default parameters
                axistype = 1 
                
                # point symbol
                pty = 32
                
                # color for outside of plotted rings
                pcol = c("#083D7F", "#EF6712")
                
                # color for inside of plotted rings
                pfcol = c(NA, "#EF671270")
                
                # width of plotted rings
                # plwd = 2
                plwd = 12
                
                # line type for plotted rings
                plty = 1
                
                # number of axis segments between center and outer rim
                seg = 5 
                
                # line color for grid segments
                cglcol = "#999999"
                
                # line type for grid segments
                cglty = 1 
                
                # color of axis grid segment labels
                axislabcol = "#333333"
                
                # values for axis grid segment labels
                caxislabels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0")
                
                # line width for axis grid segments
                # cglwd = 0.8
                cglwd = 3
                
                # font color for outer labels
                vlabcol = "#333333"
                
                # font face for outer labels
                vlab_fontface = 1
                
                # font magnification for outer labels
                # vlcex = .75
                vlcex = 2
                
                # font magnification for center labels
                # calcex = .75
                calcex = 2
                
                # font magnification for peripheral labels
                # palcex = .75
                palcex = 2
                
                # set font as parameter; 1 is normal, 2 is bold, 3 is italic
                current_parameters <- par(family = "Calibri", font = 1)
                par(current_parameters)
                
                
                #///////////////////////////////////////////////////////////////////////////////////////////////////////
                
                
                if (!is.data.frame(df)) {
                        cat("The data must be given as dataframe.\n")
                        return()
                }
                if ((n <- length(df)) < 3) {
                        cat("The number of variables must be 3 or more.\n")
                        return()
                }
                if (maxmin == FALSE) {
                        dfmax <- apply(df, 2, max)
                        dfmin <- apply(df, 2, min)
                        df <- rbind(dfmax, dfmin, df)
                }
                plot(c(-1.4, 1.4), c(-1.4, 1.4), type = "n", frame.plot = FALSE, 
                     axes = FALSE, xlab = "", ylab = "", main = title, asp = 1, 
                     ...)
                theta <- seq(90, 450, length = n + 1) * pi/180
                theta <- theta[1:n]
                xx <- cos(theta)
                yy <- sin(theta)
                CGap <- ifelse(centerzero, 0, 1)
                for (i in 0:seg) {
                        polygon(xx * (i + CGap)/(seg + CGap), yy * (i + CGap)/(seg + 
                                                                                       CGap), lty = cglty, lwd = cglwd, border = cglcol)
                        if (axistype == 1 | axistype == 3) 
                                CAXISLABELS <- paste(i/seg * 100, "(%)")
                        if (axistype == 4 | axistype == 5) 
                                CAXISLABELS <- sprintf("%3.2f", i/seg)
                        if (!is.null(caxislabels) & (i < length(caxislabels))) 
                                CAXISLABELS <- caxislabels[i + 1]
                        if (axistype == 1 | axistype == 3 | axistype == 4 | 
                            axistype == 5) {
                                if (is.null(calcex)) 
                                        text(-0.05, (i + CGap)/(seg + CGap), CAXISLABELS, 
                                             col = axislabcol)
                                else text(-0.05, (i + CGap)/(seg + CGap), CAXISLABELS, 
                                          col = axislabcol, cex = calcex)
                        }
                }
                if (centerzero) {
                        arrows(0, 0, xx * 1, yy * 1, lwd = cglwd, lty = cglty, 
                               length = 0, col = cglcol)
                }
                else {
                        arrows(xx/(seg + CGap), yy/(seg + CGap), xx * 1, yy * 
                                       1, lwd = cglwd, lty = cglty, length = 0, col = cglcol)
                }
                PAXISLABELS <- df[1, 1:n]
                if (!is.null(paxislabels)) 
                        PAXISLABELS <- paxislabels
                if (axistype == 2 | axistype == 3 | axistype == 5) {
                        if (is.null(palcex)) 
                                text(xx[1:n], yy[1:n], PAXISLABELS, col = axislabcol)
                        else text(xx[1:n], yy[1:n], PAXISLABELS, col = axislabcol, 
                                  cex = palcex)
                }
                VLABELS <- colnames(df)
                if (!is.null(vlabels)) 
                        VLABELS <- vlabels
                if (is.null(vlcex)) 
                        text(xx * 1.2, yy * 1.2, VLABELS, col = vlabcol, font = vlab_fontface)
                else text(xx * 1.2, yy * 1.2, VLABELS, cex = vlcex, col = vlabcol, font = vlab_fontface)
                series <- length(df[[1]])
                SX <- series - 2
                if (length(pty) < SX) {
                        ptys <- rep(pty, SX)
                }
                else {
                        ptys <- pty
                }
                if (length(pcol) < SX) {
                        pcols <- rep(pcol, SX)
                }
                else {
                        pcols <- pcol
                }
                if (length(plty) < SX) {
                        pltys <- rep(plty, SX)
                }
                else {
                        pltys <- plty
                }
                if (length(plwd) < SX) {
                        plwds <- rep(plwd, SX)
                }
                else {
                        plwds <- plwd
                }
                if (length(pdensity) < SX) {
                        pdensities <- rep(pdensity, SX)
                }
                else {
                        pdensities <- pdensity
                }
                if (length(pangle) < SX) {
                        pangles <- rep(pangle, SX)
                }
                else {
                        pangles <- pangle
                }
                if (length(pfcol) < SX) {
                        pfcols <- rep(pfcol, SX)
                }
                else {
                        pfcols <- pfcol
                }
                for (i in 3:series) {
                        xxs <- xx
                        yys <- yy
                        scale <- CGap/(seg + CGap) + (df[i, ] - df[2, ])/(df[1, 
                        ] - df[2, ]) * seg/(seg + CGap)
                        if (sum(!is.na(df[i, ])) < 3) {
                                cat(sprintf("[DATA NOT ENOUGH] at %d\n%g\n", i, 
                                            df[i, ]))
                        }
                        else {
                                for (j in 1:n) {
                                        if (is.na(df[i, j])) {
                                                if (na.itp) {
                                                        left <- ifelse(j > 1, j - 1, n)
                                                        while (is.na(df[i, left])) {
                                                                left <- ifelse(left > 1, left - 1, n)
                                                        }
                                                        right <- ifelse(j < n, j + 1, 1)
                                                        while (is.na(df[i, right])) {
                                                                right <- ifelse(right < n, right + 1, 
                                                                                1)
                                                        }
                                                        xxleft <- xx[left] * CGap/(seg + CGap) + 
                                                                xx[left] * (df[i, left] - df[2, left])/(df[1, 
                                                                                                           left] - df[2, left]) * seg/(seg + CGap)
                                                        yyleft <- yy[left] * CGap/(seg + CGap) + 
                                                                yy[left] * (df[i, left] - df[2, left])/(df[1, 
                                                                                                           left] - df[2, left]) * seg/(seg + CGap)
                                                        xxright <- xx[right] * CGap/(seg + CGap) + 
                                                                xx[right] * (df[i, right] - df[2, right])/(df[1, 
                                                                                                              right] - df[2, right]) * seg/(seg + 
                                                                                                                                                    CGap)
                                                        yyright <- yy[right] * CGap/(seg + CGap) + 
                                                                yy[right] * (df[i, right] - df[2, right])/(df[1, 
                                                                                                              right] - df[2, right]) * seg/(seg + 
                                                                                                                                                    CGap)
                                                        if (xxleft > xxright) {
                                                                xxtmp <- xxleft
                                                                yytmp <- yyleft
                                                                xxleft <- xxright
                                                                yyleft <- yyright
                                                                xxright <- xxtmp
                                                                yyright <- yytmp
                                                        }
                                                        xxs[j] <- xx[j] * (yyleft * xxright - yyright * 
                                                                                   xxleft)/(yy[j] * (xxright - xxleft) - 
                                                                                                    xx[j] * (yyright - yyleft))
                                                        yys[j] <- (yy[j]/xx[j]) * xxs[j]
                                                }
                                                else {
                                                        xxs[j] <- 0
                                                        yys[j] <- 0
                                                }
                                        }
                                        else {
                                                xxs[j] <- xx[j] * CGap/(seg + CGap) + xx[j] * 
                                                        (df[i, j] - df[2, j])/(df[1, j] - df[2, 
                                                                                             j]) * seg/(seg + CGap)
                                                yys[j] <- yy[j] * CGap/(seg + CGap) + yy[j] * 
                                                        (df[i, j] - df[2, j])/(df[1, j] - df[2, 
                                                                                             j]) * seg/(seg + CGap)
                                        }
                                }
                                if (is.null(pdensities)) {
                                        polygon(xxs, yys, lty = pltys[i - 2], lwd = plwds[i - 
                                                                                                  2], border = pcols[i - 2], col = pfcols[i - 
                                                                                                                                                  2])
                                }
                                else {
                                        polygon(xxs, yys, lty = pltys[i - 2], lwd = plwds[i - 
                                                                                                  2], border = pcols[i - 2], density = pdensities[i - 
                                                                                                                                                          2], angle = pangles[i - 2], col = pfcols[i - 
                                                                                                                                                                                                           2])
                                }
                                points(xx * scale, yy * scale, pch = ptys[i - 2],
                                       col = pcols[i - 2])
                        }
                }
                
                
                #//////////////////////////////////////////////////////////////////////////////////////////////
                
                
                # set default legend
                legend(x = .6, y = 1.2, legend = c("EU-15", country), bty = "n",
                       pch = 20, col = c("#083D7F", "#EF6712") , text.col = "#333333", cex = 2, pt.cex = 6)
                
        }
        
        
        #////////////////////////////////////////////////////////////////////////////////////////////////////
        #////////////////////////////////////////////////////////////////////////////////////////////////////
        #////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # create obj_radar_charts ####
        
        print(str_c(current_country, " radar chart"))
        
        # get eu_obj_avg 
        # note that the only NA obj_avg for EU is luxembourg for obj_3 in 2018
        
        
        fmir_radar <- fmir %>%
          filter(!(obj_num %in% c("obj_fr")))
        
        eu_obj_avg <- fmir_radar %>% filter(mcp_grouping == "EU-15", year == 2023) %>%
          distinct(country, year, obj_num, obj_avg) %>%
          arrange(country, obj_num) %>%
          group_by(obj_num, year) %>%
          mutate(obj_avg_for_eu = mean(obj_avg, na.rm = TRUE)) %>%
          slice(1) %>%
          ungroup() %>% select(-c(country, obj_avg))
        
        

        #//////////////////////
        
        # test eu_obj_avg
        test_eu_obj_avg <- function() {
                
                # test obj_1
                expect_equal(object = eu_obj_avg %>% filter(obj_num == "obj_1", year == 2023) %>% pull(obj_avg_for_eu),
                             expected = fmir_radar %>% filter(obj_num == "obj_1", year == 2023, mcp_grouping == "EU-15") %>% 
                                     distinct(country, year, obj_num, obj_avg) %>% 
                                     summarize(obj_avg_for_eu = mean(obj_avg, na.rm = TRUE)) %>% pull(obj_avg_for_eu))  
                
                # test obj_2
                expect_equal(object = eu_obj_avg %>% filter(obj_num == "obj_2", year == 2023) %>% pull(obj_avg_for_eu),
                             expected = fmir_radar %>% filter(obj_num == "obj_2", year == 2023, mcp_grouping == "EU-15") %>% 
                                     distinct(country, year, obj_num, obj_avg) %>% 
                                     summarize(obj_avg_for_eu = mean(obj_avg, na.rm = TRUE)) %>% pull(obj_avg_for_eu))
                
                # test obj_3
                expect_equal(object = eu_obj_avg %>% filter(obj_num == "obj_3", year == 2023) %>% pull(obj_avg_for_eu),
                             expected = fmir_radar %>% filter(obj_num == "obj_3", year == 2023, mcp_grouping == "EU-15") %>% 
                                     distinct(country, year, obj_num, obj_avg) %>% 
                                     summarize(obj_avg_for_eu = mean(obj_avg, na.rm = TRUE)) %>% pull(obj_avg_for_eu))
                
                # test obj_4
                expect_equal(object = eu_obj_avg %>% filter(obj_num == "obj_4", year == 2023) %>% pull(obj_avg_for_eu),
                             expected = fmir_radar %>% filter(obj_num == "obj_4", year == 2023, mcp_grouping == "EU-15") %>% 
                                     distinct(country, year, obj_num, obj_avg) %>% 
                                     summarize(obj_avg_for_eu = mean(obj_avg, na.rm = TRUE)) %>% pull(obj_avg_for_eu))
 
        }
        test_eu_obj_avg()
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # country_profile_radar_chart ####
        current_country_obj <- fmir_radar %>% filter(country == current_country, year == 2023) %>% 
          distinct(country, year, obj_num, obj_avg) %>%
          arrange(obj_num) %>%
          select(obj_num, obj_avg) %>%
          pivot_wider(id_cols = NULL, names_from = obj_num, values_from = obj_avg)
        
        
        # get chart_data
        chart_data <- tibble(obj_1 = c(1, 0), 
                             obj_2 = c(1, 0),
                             obj_3 = c(1, 0),
                             obj_4 = c(1, 0)
                             ) %>%
                bind_rows(., eu_obj_avg %>% select(-year) %>% 
                                  pivot_wider(id_cols = NULL, names_from = obj_num, values_from = "obj_avg_for_eu")) %>%
                bind_rows(., current_country_obj) %>%
                rename("Democratic" = obj_1, 
                       "Information         \n" = obj_2, 
                       "Energy\n" = obj_3, 
                       "          Economic   \n" = obj_4
                       )
        
        # chart_data
        
        
        
        
        #//////////////////////////
        
        
        # save plot as png
        png(filename = str_c("output/charts/country_profile/country_profile_radar_chart_", str_to_lower(current_country), ".png"),
            width = 12, height = 8, units = "in", res = 300)
        get_country_profile_radar_chart(df = chart_data, country = current_country)
        dev.off()
        
        # load png, crop, and resave as png
        country_profile_radar_chart_png <- image_read(str_c("output/charts/country_profile/country_profile_radar_chart_", 
                                                            str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 2400, height = 1700, x_off = 700, y_off = 250)) 
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # country_profile_dot_plots ####
        
        print(str_c(current_country, " dot_plots"))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # country_profile_dot_plot_legend ####
        
        current_sub_obj_num <- "sub_obj_1_1"
        current_sub_obj_short_name <- fmir %>% filter(sub_obj_num == current_sub_obj_num) %>% distinct(sub_obj_short_name) %>%
                mutate(sub_obj_short_name = case_when(sub_obj_short_name == "Checks and balances, oversight, and rule of law" ~
                                                              "Checks and balances\noversight and\nrule of law",
                                                      TRUE ~ sub_obj_short_name)) %>% pull(sub_obj_short_name)
        
        # get chart_data
        chart_data <- fmir %>% 
                filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg, avg_sub_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, sub_obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(sub_obj_avg = avg_sub_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg) %>%
                                  select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2021 ~ 1, 
                                                year == 2023 ~ 2,
                                                year == 2022 ~ 3),
                       sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                               TRUE ~ NA_real_),
                       var = mcp_grouping, values = sub_obj_avg,
                       dot_label = case_when(country == current_country ~ str_c(country, "  "),
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", 
                                                            #"EU-15",
                                                            "CARs") ~ str_c(mcp_grouping, "  "),
                                             TRUE ~ "Individual countries  "),
                       dot_label_plot_order = case_when(dot_label == "Individual countries  " ~ 1,
                                                        # dot_label == "EU-15  " ~ 2,
                                                        dot_label == "CARs  " ~ 3,
                                                        dot_label == "E&E graduates  " ~ 4,
                                                        dot_label == "E&E Eurasia  " ~ 5,
                                                        dot_label == "E&E Balkans  " ~ 6,
                                                        dot_label == str_c(current_country, "    ") ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == str_c(current_country, "  ") ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans  " ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia  " ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates  " ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs  " ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "EU-15  " ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Individual countries  " ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(
                               TRUE ~ 3),
                       point_shape = case_when(color_bin == "Individual countries  " ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Individual countries  " ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == str_c(current_country, "  ") ~ 1,
                                         color_bin == "E&E Balkans  " ~ 2,
                                         color_bin == "E&E Eurasia  " ~ 3,
                                         color_bin == "E&E graduates  " ~ 4,
                                         color_bin == "CARs  " ~ 5,
                                         # color_bin == "EU-15  " ~ 6,
                                         color_bin == "Individual countries  " ~ 7)) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == str_c(current_country, "  ") ~ 1,
                                         color_bin == "E&E Balkans  " ~ 2,
                                         color_bin == "E&E Eurasia  " ~ 3,
                                         color_bin == "E&E graduates  " ~ 4,
                                         color_bin == "CARs  " ~ 5,
                                         # color_bin == "EU-15  " ~ 6,
                                         color_bin == "Individual countries  " ~ 7)) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                mutate(level = case_when(color_bin == str_c(current_country, "  ") ~ 1,
                                         color_bin == "E&E Balkans  " ~ 2,
                                         color_bin == "E&E Eurasia  " ~ 3,
                                         color_bin == "E&E graduates  " ~ 4,
                                         color_bin == "CARs  " ~ 5,
                                         # color_bin == "EU-15  " ~ 6,
                                         color_bin == "Individual countries  " ~ 7)) %>%
                arrange(level) %>%
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                mutate(level = case_when(color_bin == str_c(current_country, "  ") ~ 1,
                                         color_bin == "E&E Balkans  " ~ 2,
                                         color_bin == "E&E Eurasia  " ~ 3,
                                         color_bin == "E&E graduates  " ~ 4,
                                         color_bin == "CARs  " ~ 5,
                                         # color_bin == "EU-15  " ~ 6,
                                         color_bin == "Individual countries  " ~ 7)) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        country_profile_dot_plot_legend <- chart_data %>% 
          filter(mcp_grouping != "EU-15") %>% #
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, 
                                   name = "") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = label_number(accuracy = 1)) +
                # scale_x_continuous(breaks = seq(from = 2018, to = 2023, by = 1)) +
                labs(x = NULL, y = current_sub_obj_short_name, 
                     caption = NULL, color = "test", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1,
                                           y = 1, yend = 1), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke)) +
                scale_shape_manual(values = chart_data_shape_list, 
                                   name = "") +
                # scale_size_continuous(range = c(1, 3), guide = "none") +
                scale_size_continuous(range = c(4, 6), guide = "none") +
                coord_fixed(ratio = 1 / 15, clip = "off") +
                theme_bw() +
                theme(
                        # plot.background = element_rect(fill = "blue"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_line(size = .5, color = "#DDDDDD"),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333", 
                                                   margin = margin(t = 2, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 0)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 7, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 8, family = "Calibri", face = "bold", color = "#083D7F"),
                        # legend.title = element_blank(),
                        # legend.text = element_text(size = 8.5, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                        #                            hjust = .5, color = "#333333")
                        legend.text = element_text(size = 9, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) +
                guides(color = guide_legend(nrow = 2, byrow = TRUE, 
                                            label.hjust = 0,
                                            color = "#333333", 
                                            keywidth = .5,
                                            override.aes = list(size = 5)
                ))
        
        #/////////////////////
        
        
        # extract legend
        country_profile_dot_plot_legend <- as_ggplot(get_legend(country_profile_dot_plot_legend))
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/country_profile_dot_plot_legend.png"), 
               plot = country_profile_dot_plot_legend, 
               dpi = 600, height = 6, width = 6)
        
        # crop png
        country_profile_dot_plot_legend_png <- image_read("output/charts/country_profile/country_profile_dot_plot_legend.png") %>%
                image_crop(image = ., geometry = geometry_area(width = 2750, height = 400, x_off = 390, y_off = 1600))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # miri_country_profile_dot_plot ####
        name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>%
          filter(mcp_grouping != "EU-15") %>% #X
                filter(year %in% c(2023, 2022, 2021), !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, miri_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2023, 2022, 2021),
                                             !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_miri_avg = mean(miri_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, 
                                           miri_avg, avg_miri_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, miri_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(miri_avg = avg_miri_avg) %>%
                                  distinct(country, mcp_grouping, year,  
                                           miri_avg) %>%
                                  select(country, mcp_grouping, year, miri_avg)) %>%
                mutate(y_axis_order = case_when(year == 2021 ~ 1, 
                                                year == 2023 ~ 2,
                                                year == 2022 ~ 3),
                       miri_avg = case_when(y_axis_order == 2 ~ miri_avg,
                                            TRUE ~ NA_real_),
                       var = mcp_grouping, values = miri_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", #X "EU-15", 
                                                            "CARs") ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        #X dot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         #X color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5 #X,
                                         #X color_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5 #X,
                                         #X color_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        miri_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = label_number(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 2.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        # axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_blank(),
                        # axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333",
                        # margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/miri_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = miri_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        miri_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/miri_country_profile_dot_plot_", 
                                                              str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, 
                                                               height = 153, x_off = 0, y_off = 825))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # obj_1_country_profile_dot_plot ####
        current_obj_num <- "obj_1"
        obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>%
          filter(mcp_grouping != "EU-15") %>% #X
                filter(year %in% c(2023, 2022, 2021), obj_num == current_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, obj_short_name, obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2023, 2022, 2021), obj_num == current_obj_num, 
                                             !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_obj_avg = mean(obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, obj_short_name, 
                                           obj_avg, avg_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(obj_avg = avg_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, obj_short_name, 
                                           obj_avg) %>%
                                  select(country, mcp_grouping, year, obj_short_name, obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2021 ~ 1, 
                                                year == 2023 ~ 2,
                                                year == 2022 ~ 3),
                       obj_avg = case_when(y_axis_order == 2 ~ obj_avg,
                                           TRUE ~ NA_real_),
                       var = mcp_grouping, values = obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs" #X, "EU-15"
                                                            ) ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        #X dot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         #X color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5 #X,
                                         #X color_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5 #X,
                                         #color_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        obj_1_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = label_number(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 2.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        # axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_blank(),
                        # axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333",
                        # margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/obj_1_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = obj_1_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        obj_1_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/obj_1_country_profile_dot_plot_", 
                                                               str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, 
                                                               height = 153, x_off = 0, y_off = 825))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # obj_2_country_profile_dot_plot ####
        current_obj_num <- "obj_2"
        obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
          filter(mcp_grouping != "EU-15") %>% #X
                filter(year %in% c(2023, 2022, 2021), obj_num == current_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, obj_short_name, obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2023, 2022, 2021), obj_num == current_obj_num, 
                                             !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_obj_avg = mean(obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, obj_short_name, 
                                           obj_avg, avg_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(obj_avg = avg_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, obj_short_name, 
                                           obj_avg) %>%
                                  select(country, mcp_grouping, year, obj_short_name, obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2021 ~ 1, 
                                                year == 2023 ~ 2,
                                                year == 2022 ~ 3),
                       obj_avg = case_when(y_axis_order == 2 ~ obj_avg,
                                           TRUE ~ NA_real_),
                       var = mcp_grouping, values = obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs" #X, "EU-15"
                                                            ) ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        #X dot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         #X color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5 #X,
                                         #X color_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5 #X,
                                         #X color_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        obj_2_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = label_number(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 2.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        # axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_blank(),
                        # axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 9, color = "#333333", 
                        #                            margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 

        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/obj_2_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = obj_2_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        obj_2_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/obj_2_country_profile_dot_plot_", 
                                                               str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, 
                                                               height = 153, x_off = 0, y_off = 825))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # obj_3_country_profile_dot_plot ####
        current_obj_num <- "obj_3"
        obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
          filter(mcp_grouping != "EU-15") %>% #X
                filter(year %in% c(2023, 2022, 2021), obj_num == current_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, obj_short_name, obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2023, 2022, 2021), obj_num == current_obj_num, 
                                             !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_obj_avg = mean(obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, obj_short_name, 
                                           obj_avg, avg_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(obj_avg = avg_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, obj_short_name, 
                                           obj_avg) %>%
                                  select(country, mcp_grouping, year, obj_short_name, obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2021 ~ 1, 
                                                year == 2023 ~ 2,
                                                year == 2022 ~ 3),
                       obj_avg = case_when(y_axis_order == 2 ~ obj_avg,
                                           TRUE ~ NA_real_),
                       var = mcp_grouping, values = obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs"#X, "EU-15"
                                                            ) ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        #X dot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         #X color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5,
                                         color_bin == "EU-15" ~ 6)) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        obj_3_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = label_number(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 2.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        # axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_blank(),
                        # axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 9, color = "#333333", 
                        #                            margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/obj_3_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = obj_3_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        obj_3_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/obj_3_country_profile_dot_plot_", 
                                                               str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, 
                                                                 height = 153, x_off = 0, y_off = 825))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # obj_4_country_profile_dot_plot ####
        current_obj_num <- "obj_4"
        obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
          filter(mcp_grouping != "EU-15") %>% #X
                filter(year %in% c(2023, 2022, 2021), obj_num == current_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, obj_short_name, obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2023, 2022, 2021), obj_num == current_obj_num, 
                                             !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_obj_avg = mean(obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, obj_short_name, 
                                           obj_avg, avg_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(obj_avg = avg_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, obj_short_name, 
                                           obj_avg) %>%
                                  select(country, mcp_grouping, year, obj_short_name, obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2021 ~ 1, 
                                                year == 2023 ~ 2,
                                                year == 2022 ~ 3),
                       obj_avg = case_when(y_axis_order == 2 ~ obj_avg,
                                           TRUE ~ NA_real_),
                       var = mcp_grouping, values = obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs"#X, "EU-15"
                                                            ) ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        #Xdot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         #Xcolor_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5#X,
                                         #Xcolor_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5#X,
                                         #Xcolor_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        obj_4_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = label_number(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 2.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        # axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_blank(),
                        # axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 9, color = "#333333", 
                        #                            margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 

        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/obj_4_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = obj_4_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        obj_4_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/obj_4_country_profile_dot_plot_", 
                                                               str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, 
                                                               height = 153, x_off = 0, y_off = 825))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # sub_obj_1_1_country_profile_dot_plot ####
        current_sub_obj_num <- "sub_obj_1_1"
        sub_obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
          filter(mcp_grouping != "EU-15") %>% #X
                filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg, avg_sub_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, sub_obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(sub_obj_avg = avg_sub_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg) %>%
                                  select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2021 ~ 1, 
                                                year == 2023 ~ 2,
                                                year == 2022 ~ 3),
                       sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                               TRUE ~ NA_real_),
                       var = mcp_grouping, values = sub_obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs"#X, "EU-15"
                                                            ) ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        #Xdot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         #Xcolor_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5#X,
                                         #Xcolor_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5#X,
                                         #Xcolor_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        sub_obj_1_1_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = label_number(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = sub_obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 1.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        # axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_blank(),
                        # axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333",
                        # margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/sub_obj_1_1_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = sub_obj_1_1_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        sub_obj_1_1_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_1_1_country_profile_dot_plot_", 
                                                                     str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 153, x_off = 0, y_off = 825))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # sub_obj_1_2_country_profile_dot_plot ####
        current_sub_obj_num <- "sub_obj_1_2"
        sub_obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
          filter(mcp_grouping != "EU-15") %>% #X
                filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg, avg_sub_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, sub_obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(sub_obj_avg = avg_sub_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg) %>%
                                  select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2021 ~ 1, 
                                                year == 2023 ~ 2,
                                                year == 2022 ~ 3),
                       sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                               TRUE ~ NA_real_),
                       var = mcp_grouping, values = sub_obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs"#X, "EU-15"
                                                            ) ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        #Xdot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         #Xcolor_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5#X,
                                         #Xcolor_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5#X,
                                         #Xcolor_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        sub_obj_1_2_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = label_number(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = sub_obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 1.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        # axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_blank(),
                        # axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 9, color = "#333333", 
                        #                            margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/sub_obj_1_2_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = sub_obj_1_2_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        sub_obj_1_2_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_1_2_country_profile_dot_plot_", 
                                                                     str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 153, x_off = 0, y_off = 825))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # sub_obj_1_3_country_profile_dot_plot ####
        current_sub_obj_num <- "sub_obj_1_3"
        sub_obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
          filter(mcp_grouping != "EU-15") %>% #X
                filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg, avg_sub_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, sub_obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(sub_obj_avg = avg_sub_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg) %>%
                                  select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2021 ~ 1, 
                                                year == 2023 ~ 2,
                                                year == 2022 ~ 3),
                       sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                               TRUE ~ NA_real_),
                       var = mcp_grouping, values = sub_obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs"#X, "EU-15"
                                                            ) ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        #Xdot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         #X color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5 #X,
                                         #X color_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5#X ,
                                         #X color_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        check_1_3 <- chart_data
        
        sub_obj_1_3_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = label_number(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = sub_obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 1.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"), #plot.margin = unit(c(2, 0, 2, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        #axis.ticks.x = element_blank(),
                        axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        #axis.text.x = element_blank(),
                        axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333",
                                                    margin = margin(t = 2, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        axis.line.x.bottom = element_line(color = "#333333"),
                        # axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        
        #///////////////////////
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/sub_obj_1_3_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = sub_obj_1_3_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        sub_obj_1_3_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_1_3_country_profile_dot_plot_", 
                                                                     str_to_lower(current_country), ".png")) %>%
          image_crop(image = ., geometry = geometry_area(width = 1800, height = 200,
                                                         x_off = 0, y_off = 825))
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # sub_obj_1_4_country_profile_dot_plot ####
        current_sub_obj_num <- "sub_obj_1_4"
        sub_obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
          filter(mcp_grouping != "EU-15") %>% #X
          filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
          select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
          bind_rows(., fmir %>% filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                      group_by(mcp_grouping, year) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                      ungroup() %>%
                      distinct(country, mcp_grouping, year, sub_obj_short_name, 
                               sub_obj_avg, avg_sub_obj_avg) %>%
                      arrange(mcp_grouping) %>%
                      select(-c(country, sub_obj_avg)) %>% 
                      mutate(country = mcp_grouping) %>%
                      rename(sub_obj_avg = avg_sub_obj_avg) %>%
                      distinct(country, mcp_grouping, year, sub_obj_short_name, 
                               sub_obj_avg) %>%
                      select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
          mutate(y_axis_order = case_when(year == 2021 ~ 1, 
                                          year == 2023 ~ 2,
                                          year == 2022 ~ 3),
                 sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                         TRUE ~ NA_real_),
                 var = mcp_grouping, values = sub_obj_avg,
                 dot_label = case_when(country == current_country ~ country,
                                       country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs"#X, "EU-15"
                                                      ) ~ mcp_grouping,
                                       TRUE ~ "Other"),
                 dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                  #X dot_label == "EU-15" ~ 2,
                                                  dot_label == "CARs" ~ 3,
                                                  dot_label == "E&E graduates" ~ 4,
                                                  dot_label == "E&E Eurasia" ~ 5,
                                                  dot_label == "E&E Balkans" ~ 6,
                                                  dot_label == current_country ~ 7)) %>%
          arrange(dot_label_plot_order) %>%
          mutate(color_bin = dot_label,
                 color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                   color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                   color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                   color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                   color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                   # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                   #X color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                   # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                   color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                 point_size = case_when(color_bin == "Other" ~ 2,
                                        TRUE ~ 3),
                 point_shape = case_when(color_bin == "Other" ~ 1,
                                         TRUE ~ 19),
                 point_stroke = case_when(color_bin == "Other" ~ 1,
                                          TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
          mutate(level = case_when(color_bin == current_country ~ 1,
                                   color_bin == "E&E Balkans" ~ 2,
                                   color_bin == "E&E Eurasia" ~ 3,
                                   color_bin == "E&E graduates" ~ 4,
                                   color_bin == "CARs" ~ 5#X ,
                                   #X color_bin == "EU-15" ~ 6
                                   )) %>%
          arrange(level) %>%
          pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
          mutate(level = case_when(color_bin == current_country ~ 1,
                                   color_bin == "E&E Balkans" ~ 2,
                                   color_bin == "E&E Eurasia" ~ 3,
                                   color_bin == "E&E graduates" ~ 4,
                                   color_bin == "CARs" ~ 5#X ,
                                   #X color_bin == "EU-15" ~ 6
                                   )) %>%
          arrange(level) %>%
          pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
          pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
          pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        sub_obj_1_4_country_profile_dot_plot <- chart_data %>% 
          ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                         color = color_bin, shape = color_bin)) + 
          scale_color_manual(values = chart_data_color_list, guide = "none") +
          # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
          scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                             labels = label_number(accuracy = 1)) +
          scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
          labs(x = NULL, y = sub_obj_short_name_placeholder, 
               caption = NULL, color = "", linetype = "") +
          # geom_segment(data = chart_data,
          #              mapping = aes(x = -.02, xend = -.02,
          #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
          geom_segment(data = chart_data,
                       mapping = aes(x = -.02, xend = 1.02,
                                     y = 1, yend = 1), color = "#333333", size = 1.5) +
          # geom_segment(data = chart_data,
          #              mapping = aes(x = -.02, xend = 1,
          #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
          geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
          scale_shape_manual(values = chart_data_shape_list) +
          scale_size_continuous(range = c(4, 6)) +
          # coord_fixed(ratio = 1 / 20, clip = "off") +
          theme_bw() +
          theme(
            aspect.ratio = 1 / 10,
            # plot.background = element_rect(fill = "#E0ECF7"),
            # plot.background = element_rect(colour = "#DDDDDD", size = .5),
            # plot.background = element_rect(fill = "transparent", color = NA),
            # plot.background = element_blank(),
            # plot.margin = unit(c(0, -8, 0, 0), "mm"),
            plot.margin = unit(c(2, 0, 2, 0), "mm"),
            plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                        color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
            # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            # panel.grid.major.y = element_line(color = "#DDDDDD"),
            # panel.background = element_rect(fill = "transparent"), 
            panel.background = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.border = element_blank(),
            # panel.border = element_rect(color = "#DDDDDD", size = .5),
            # panel.grid = element_blank(),
            # line = element_blank(),
            # rect = element_blank(),
            axis.ticks.y = element_blank(),
            # axis.ticks.x = element_blank(),
            axis.ticks.x = element_line(size = .5),
            # axis.ticks.length.y.left = unit(.2, "cm"),
            axis.ticks.length.x.bottom = unit(.1, "cm"),
            # axis.text.x = element_blank(),
            axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333",
                                       margin = margin(t = 2, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
            axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                       margin = margin(t = 0, r = -20, b = 0, l = 0)),
            # axis.line.x.bottom = element_line(color = "#333333"),
            axis.line.x.bottom = element_blank(),
            axis.line.y.left = element_blank(),
            axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                        margin = margin(t = 13, r = 0, b = 5, l = 0)),
            axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                        vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
            # axis.title.y = element_blank(),
            # axis.text.y = element_blank(),
            plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                      margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
            legend.position = "bottom",
            # legend.key.size = unit(2, "mm"), 
            legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
            legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                       hjust = .5, color = "#333333")
            # legend.spacing.y = unit(5.5, "cm"),
            # legend.key = element_rect(size = 5),
            # legend.key.size = unit(2, 'lines')
          ) 
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/sub_obj_1_4_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = sub_obj_1_4_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        sub_obj_1_4_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_1_4_country_profile_dot_plot_", 
                                                                     str_to_lower(current_country), ".png")) %>%
          image_crop(image = ., geometry = geometry_area(width = 1800, height = 200, x_off = 0, y_off = 810))
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # sub_obj_2_1_country_profile_dot_plot ####
        current_sub_obj_num <- "sub_obj_2_1"
        sub_obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
          filter(mcp_grouping != "EU-15") %>% #X
                filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg, avg_sub_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, sub_obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(sub_obj_avg = avg_sub_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg) %>%
                                  select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2021 ~ 1, 
                                                year == 2023 ~ 2,
                                                year == 2022 ~ 3),
                       sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                               TRUE ~ NA_real_),
                       var = mcp_grouping, values = sub_obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs"#X, "EU-15"
                                                            ) ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        #X dot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         #X color_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5#X ,
                                         #X color_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5#X ,
                                         #X color_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        sub_obj_2_1_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = label_number(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = sub_obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 1.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        # axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_blank(),
                        # axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333",
                        # margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/sub_obj_2_1_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = sub_obj_2_1_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        sub_obj_2_1_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_2_1_country_profile_dot_plot_", 
                                                                     str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 153, x_off = 0, y_off = 825))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # sub_obj_2_2_country_profile_dot_plot ####
        current_sub_obj_num <- "sub_obj_2_2"
        sub_obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
          filter(mcp_grouping != "EU-15") %>% #X
                filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg, avg_sub_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, sub_obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(sub_obj_avg = avg_sub_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg) %>%
                                  select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
                mutate(
                  sub_obj_avg = case_when(
                    country == "E&E Eurasia" & year == 2023 ~ 0.37, #Setting each distinct so that one isn't fully occluded
                    country == "E&E Balkans" & year == 2023 ~ 0.38, #Setting each distinct so that one isn't fully occluded
                    TRUE ~ sub_obj_avg),
                  y_axis_order = case_when(
                    year == 2021 ~ 1,
                    year == 2023 ~ 2,
                    year == 2022 ~ 3),
                  sub_obj_avg = case_when(
                    y_axis_order == 2 ~ sub_obj_avg,
                    TRUE ~ NA_real_),
                  var = mcp_grouping, values = sub_obj_avg,
                  dot_label = case_when(
                    country == current_country ~ country,
                    country %in% c("E&E Balkans", 
                                   "E&E Eurasia", 
                                   "E&E graduates", 
                                   "CARs"#X, 
                                   #"EU-15"
                                   ) ~ mcp_grouping,
                    TRUE ~ "Other"),
                  dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        #Xdot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         #Xcolor_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5#X,
                                         #Xcolor_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5#X,
                                         #Xcolor_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        sub_obj_2_2_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = label_number(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = sub_obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 1.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        # axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_blank(),
                        # axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 9, color = "#333333", 
                        #                            margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/sub_obj_2_2_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = sub_obj_2_2_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        sub_obj_2_2_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_2_2_country_profile_dot_plot_", 
                                                                     str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 153, x_off = 0, y_off = 825))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # sub_obj_2_3_country_profile_dot_plot ####
        current_sub_obj_num <- "sub_obj_2_3"
        sub_obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
          filter(mcp_grouping != "EU-15") %>% #X
                filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg, avg_sub_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, sub_obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(sub_obj_avg = avg_sub_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg) %>%
                                  select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2021 ~ 1, 
                                                year == 2023 ~ 2,
                                                year == 2022 ~ 3),
                       sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                               TRUE ~ NA_real_),
                       var = mcp_grouping, values = sub_obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs"#X, "EU-15"
                                                            ) ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        #Xdot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         #Xcolor_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5#X,
                                         #Xcolor_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5#X,
                                         #Xcolor_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        sub_obj_2_3_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = label_number(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = sub_obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 1.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        # axis.ticks.x = element_blank(),
                        axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        # axis.text.x = element_blank(),
                        axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333",
                                                   margin = margin(t = 2, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        axis.line.x.bottom = element_line(color = "#333333"),
                        # axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/sub_obj_2_3_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = sub_obj_2_3_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        sub_obj_2_3_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_2_3_country_profile_dot_plot_", 
                                                                     str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 200,#153, 
                                                               x_off = 0, y_off = 825))
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        # sub_obj_3_1_country_profile_dot_plot ####
        current_sub_obj_num <- "sub_obj_3_1"
        sub_obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        # get chart_data
        chart_data <- fmir %>% 
          filter(mcp_grouping != "EU-15") %>% #X
          mutate(sub_obj_avg = case_when(
            country == "Albania" ~ sub_obj_avg - 0.01,
            TRUE ~ sub_obj_avg
          )) %>% #X To prevent full overlap of Albania dot and E&E graduates dot
                filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg, avg_sub_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, sub_obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(sub_obj_avg = avg_sub_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg) %>%
                                  select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2021 ~ 1, 
                                                year == 2023 ~ 2,
                                                year == 2022 ~ 3),
                       sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                               TRUE ~ NA_real_),
                       var = mcp_grouping, values = sub_obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs"#X, "EU-15"
                                                            ) ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        #Xdot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         #Xcolor_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5#X,
                                         #Xcolor_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5#X,
                                         #Xcolor_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        sub_obj_3_1_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = label_number(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = sub_obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 1.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        # axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_blank(),
                        # axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333",
                        # margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/sub_obj_3_1_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = sub_obj_3_1_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        sub_obj_3_1_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_3_1_country_profile_dot_plot_", 
                                                                     str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 153, x_off = 0, y_off = 825))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # sub_obj_3_2_country_profile_dot_plot ####
        current_sub_obj_num <- "sub_obj_3_2"
        sub_obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
          filter(mcp_grouping != "EU-15") %>% #X
                filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg, avg_sub_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, sub_obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(sub_obj_avg = avg_sub_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg) %>%
                                  select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2021 ~ 1, 
                                                year == 2023 ~ 2,
                                                year == 2022 ~ 3),
                       sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                               TRUE ~ NA_real_),
                       var = mcp_grouping, values = sub_obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs"#X, "EU-15"
                                                            ) ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        #Xdot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         #Xcolor_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5#X,
                                         #Xcolor_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5#X,
                                         #Xcolor_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        sub_obj_3_2_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = label_number(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = sub_obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 1.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        # axis.ticks.x = element_blank(),
                        axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        # axis.text.x = element_blank(),
                        axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 9, color = "#333333", 
                                                    margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        axis.line.x.bottom = element_line(color = "#333333"),
                        # axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/sub_obj_3_2_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = sub_obj_3_2_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        sub_obj_3_2_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_3_2_country_profile_dot_plot_", 
                                                                     str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 200,#153, 
                                                               x_off = 0, y_off = 810))
        
        
  
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # sub_obj_4_1_country_profile_dot_plot ####
        current_sub_obj_num <- "sub_obj_4_1"
        sub_obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>%
          filter(mcp_grouping != "EU-15") %>% #X
                filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg, avg_sub_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, sub_obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(sub_obj_avg = avg_sub_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg) %>%
                                  select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2021 ~ 1, 
                                                year == 2023 ~ 2,
                                                year == 2022 ~ 3),
                       sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                               TRUE ~ NA_real_),
                       var = mcp_grouping, values = sub_obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs"#X, "EU-15"
                                                            ) ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        #Xdot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         #Xcolor_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5#X,
                                         #Xcolor_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5#X,
                                         #Xcolor_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        sub_obj_4_1_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = label_number(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = sub_obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 1.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        # axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_blank(),
                        # axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 9, color = "#333333", 
                        #                            margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/sub_obj_4_1_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = sub_obj_4_1_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        sub_obj_4_1_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_4_1_country_profile_dot_plot_", 
                                                                     str_to_lower(current_country), ".png")) %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 153, x_off = 0, y_off = 825))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # sub_obj_4_2_country_profile_dot_plot ####
        current_sub_obj_num <- "sub_obj_4_2"
        sub_obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
          filter(mcp_grouping != "EU-15") %>% #X
                filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
                select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
                bind_rows(., fmir %>% filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                                  group_by(mcp_grouping, year) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg, avg_sub_obj_avg) %>%
                                  arrange(mcp_grouping) %>%
                                  select(-c(country, sub_obj_avg)) %>% 
                                  mutate(country = mcp_grouping) %>%
                                  rename(sub_obj_avg = avg_sub_obj_avg) %>%
                                  distinct(country, mcp_grouping, year, sub_obj_short_name, 
                                           sub_obj_avg) %>%
                                  select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
                mutate(y_axis_order = case_when(year == 2021 ~ 1, 
                                                year == 2023 ~ 2,
                                                year == 2022 ~ 3),
                       sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                               TRUE ~ NA_real_),
                       var = mcp_grouping, values = sub_obj_avg,
                       dot_label = case_when(country == current_country ~ country,
                                             country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs"#X, "EU-15"
                                                            ) ~ mcp_grouping,
                                             TRUE ~ "Other"),
                       dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                        #Xdot_label == "EU-15" ~ 2,
                                                        dot_label == "CARs" ~ 3,
                                                        dot_label == "E&E graduates" ~ 4,
                                                        dot_label == "E&E Eurasia" ~ 5,
                                                        dot_label == "E&E Balkans" ~ 6,
                                                        dot_label == current_country ~ 7)) %>%
                arrange(dot_label_plot_order) %>%
                mutate(color_bin = dot_label,
                       color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                         color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                         # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                         #Xcolor_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                         # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                         color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                       point_size = case_when(color_bin == "Other" ~ 2,
                                              TRUE ~ 3),
                       point_shape = case_when(color_bin == "Other" ~ 1,
                                               TRUE ~ 19),
                       point_stroke = case_when(color_bin == "Other" ~ 1,
                                                TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5#X,
                                         #Xcolor_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
                mutate(level = case_when(color_bin == current_country ~ 1,
                                         color_bin == "E&E Balkans" ~ 2,
                                         color_bin == "E&E Eurasia" ~ 3,
                                         color_bin == "E&E graduates" ~ 4,
                                         color_bin == "CARs" ~ 5#X,
                                         #Xcolor_bin == "EU-15" ~ 6
                                         )) %>%
                arrange(level) %>%
                pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
                pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        sub_obj_4_2_country_profile_dot_plot <- chart_data %>% 
                ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                               color = color_bin, shape = color_bin)) + 
                scale_color_manual(values = chart_data_color_list, guide = "none") +
                # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
                scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                                   labels = label_number(accuracy = 1)) +
                scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
                labs(x = NULL, y = sub_obj_short_name_placeholder, 
                     caption = NULL, color = "", linetype = "") +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = -.02,
                #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
                geom_segment(data = chart_data,
                             mapping = aes(x = -.02, xend = 1.02,
                                           y = 1, yend = 1), color = "#DDDDDD", size = 1.5) +
                # geom_segment(data = chart_data,
                #              mapping = aes(x = -.02, xend = 1,
                #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
                geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
                scale_shape_manual(values = chart_data_shape_list) +
                scale_size_continuous(range = c(4, 6)) +
                # coord_fixed(ratio = 1 / 20, clip = "off") +
                theme_bw() +
                theme(
                        aspect.ratio = 1 / 10,
                        # plot.background = element_rect(fill = "#E0ECF7"),
                        # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                        # plot.background = element_rect(fill = "transparent", color = NA),
                        # plot.background = element_blank(),
                        # plot.margin = unit(c(0, -8, 0, 0), "mm"),
                        plot.margin = unit(c(0, 0, 0, 0), "mm"),
                        plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                                    color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                        # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        # panel.grid.major.y = element_line(color = "#DDDDDD"),
                        # panel.background = element_rect(fill = "transparent"), 
                        panel.background = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.border = element_blank(),
                        # panel.border = element_rect(color = "#DDDDDD", size = .5),
                        # panel.grid = element_blank(),
                        # line = element_blank(),
                        # rect = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        # axis.ticks.x = element_line(size = .5),
                        # axis.ticks.length.y.left = unit(.2, "cm"),
                        axis.ticks.length.x.bottom = unit(.1, "cm"),
                        axis.text.x = element_blank(),
                        # axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333",
                         #                          margin = margin(t = 2, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                        axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                   margin = margin(t = 0, r = -20, b = 0, l = 0)),
                        # axis.line.x.bottom = element_line(color = "#333333"),
                        axis.line.x.bottom = element_blank(),
                        axis.line.y.left = element_blank(),
                        axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                                    margin = margin(t = 13, r = 0, b = 5, l = 0)),
                        axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                                    vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
                        # axis.title.y = element_blank(),
                        # axis.text.y = element_blank(),
                        plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                                  margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                        legend.position = "bottom",
                        # legend.key.size = unit(2, "mm"), 
                        legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
                        legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                   hjust = .5, color = "#333333")
                        # legend.spacing.y = unit(5.5, "cm"),
                        # legend.key = element_rect(size = 5),
                        # legend.key.size = unit(2, 'lines')
                ) 
        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/sub_obj_4_2_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = sub_obj_4_2_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        sub_obj_4_2_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_4_2_country_profile_dot_plot_", 
                                                                     str_to_lower(current_country), ".png")) %>%
          image_crop(image = ., geometry = geometry_area(width = 1800, height = 153, x_off = 0, y_off = 825))
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # sub_obj_4_3_country_profile_dot_plot ####
        current_sub_obj_num <- "sub_obj_4_3"
        sub_obj_short_name_placeholder <- str_c(rep(x = " ", times = 32), collapse = "")
        
        
        # get chart_data
        chart_data <- fmir %>% 
          filter(mcp_grouping != "EU-15") %>% #X
          filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>% 
          select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg) %>% distinct() %>%
          bind_rows(., fmir %>% filter(year %in% c(2023, 2022, 2021), sub_obj_num == current_sub_obj_num, !(country %in% c("Russia", "U.S."))) %>%
                      group_by(mcp_grouping, year) %>% mutate(avg_sub_obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
                      ungroup() %>%
                      distinct(country, mcp_grouping, year, sub_obj_short_name, 
                               sub_obj_avg, avg_sub_obj_avg) %>%
                      arrange(mcp_grouping) %>%
                      select(-c(country, sub_obj_avg)) %>% 
                      mutate(country = mcp_grouping) %>%
                      rename(sub_obj_avg = avg_sub_obj_avg) %>%
                      distinct(country, mcp_grouping, year, sub_obj_short_name, 
                               sub_obj_avg) %>%
                      select(country, mcp_grouping, year, sub_obj_short_name, sub_obj_avg)) %>%
          mutate(y_axis_order = case_when(year == 2021 ~ 1, 
                                          year == 2023 ~ 2,
                                          year == 2022 ~ 3),
                 sub_obj_avg = case_when(y_axis_order == 2 ~ sub_obj_avg,
                                         TRUE ~ NA_real_),
                 var = mcp_grouping, values = sub_obj_avg,
                 dot_label = case_when(country == current_country ~ country,
                                       country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs"#X, "EU-15"
                                                      ) ~ mcp_grouping,
                                       TRUE ~ "Other"),
                 dot_label_plot_order = case_when(dot_label == "Other" ~ 1,
                                                  #Xdot_label == "EU-15" ~ 2,
                                                  dot_label == "CARs" ~ 3,
                                                  dot_label == "E&E graduates" ~ 4,
                                                  dot_label == "E&E Eurasia" ~ 5,
                                                  dot_label == "E&E Balkans" ~ 6,
                                                  dot_label == current_country ~ 7)) %>%
          arrange(dot_label_plot_order) %>%
          mutate(color_bin = dot_label,
                 color = case_when(color_bin == current_country ~ color_palette %>% slice(11) %>% pull(hex),
                                   color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                   color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                   color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                   color_bin == "CARs" ~ color_palette %>% slice(5) %>% pull(hex),
                                   # color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                   #Xcolor_bin == "EU-15" ~ color_palette %>% slice(7) %>% pull(hex),
                                   # color_bin == "U.S." ~ color_palette %>% slice(8) %>% pull(hex)),
                                   color_bin == "Other" ~ color_palette %>% slice(4) %>% pull(hex)),
                 point_size = case_when(color_bin == "Other" ~ 2,
                                        TRUE ~ 3),
                 point_shape = case_when(color_bin == "Other" ~ 1,
                                         TRUE ~ 19),
                 point_stroke = case_when(color_bin == "Other" ~ 1,
                                          TRUE ~ 1))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% 
          mutate(level = case_when(color_bin == current_country ~ 1,
                                   color_bin == "E&E Balkans" ~ 2,
                                   color_bin == "E&E Eurasia" ~ 3,
                                   color_bin == "E&E graduates" ~ 4,
                                   color_bin == "CARs" ~ 5#X,
                                   #Xcolor_bin == "EU-15" ~ 6
                                   )) %>%
          arrange(level) %>%
          pull(color)
        
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% 
          mutate(level = case_when(color_bin == current_country ~ 1,
                                   color_bin == "E&E Balkans" ~ 2,
                                   color_bin == "E&E Eurasia" ~ 3,
                                   color_bin == "E&E graduates" ~ 4,
                                   color_bin == "CARs" ~ 5#X,
                                   #Xcolor_bin == "EU-15" ~ 6
                                   )) %>%
          arrange(level) %>%
          pull(color_bin)
        
        # inspect
        # chart_data_color_list
        
        
        # create chart_data_shape_list for to pass to scale_shape_manual
        chart_data_shape_list <- chart_data %>% count(color_bin, point_shape) %>% 
          pull(point_shape)
        
        names(chart_data_shape_list) <- chart_data %>% count(color_bin, point_shape) %>% 
          pull(color_bin)
        
        # inspect
        # chart_data_shape_list
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        sub_obj_4_3_country_profile_dot_plot <- chart_data %>% 
          ggplot(data = ., mapping = aes(x = values, y = y_axis_order, 
                                         color = color_bin, shape = color_bin)) + 
          scale_color_manual(values = chart_data_color_list, guide = "none") +
          # scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +
          scale_y_continuous(breaks = NULL, limits = c(1, 3), expand = c(0, 0),
                             labels = label_number(accuracy = 1)) +
          scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.02, 1.02)) +
          labs(x = NULL, y = sub_obj_short_name_placeholder, 
               caption = NULL, color = "", linetype = "") +
          # geom_segment(data = chart_data,
          #              mapping = aes(x = -.02, xend = -.02,
          #                            y = 1, yend = 3), color = "#DDDDDD", size = .25) +
          geom_segment(data = chart_data,
                       mapping = aes(x = -.02, xend = 1.02,
                                     y = 1, yend = 1), color = "#DDDDDD", size = 1.5) +
          # geom_segment(data = chart_data,
          #              mapping = aes(x = -.02, xend = 1,
          #                            y = 3., yend = 3), color = "#DDDDDD", size = .25) +
          geom_point(mapping = aes(size = point_size, stroke = point_stroke), show.legend = FALSE) +
          scale_shape_manual(values = chart_data_shape_list) +
          scale_size_continuous(range = c(4, 6)) +
          # coord_fixed(ratio = 1 / 20, clip = "off") +
          theme_bw() +
          theme(
            aspect.ratio = 1 / 10,
            # plot.background = element_rect(fill = "#E0ECF7"),
            # plot.background = element_rect(colour = "#DDDDDD", size = .5),
            # plot.background = element_rect(fill = "transparent", color = NA),
            # plot.background = element_blank(),
            # plot.margin = unit(c(0, -8, 0, 0), "mm"),
            plot.margin = unit(c(0, 0, 0, 0), "mm"),
            plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Gill Sans MT", 
                                        color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
            # text = element_text(family = "Gill Sans MT", size = 46, face = "plain", color = "#000000"),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            # panel.grid.major.y = element_line(color = "#DDDDDD"),
            # panel.background = element_rect(fill = "transparent"), 
            panel.background = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.border = element_blank(),
            # panel.border = element_rect(color = "#DDDDDD", size = .5),
            # panel.grid = element_blank(),
            # line = element_blank(),
            # rect = element_blank(),
            axis.ticks.y = element_blank(),
            # axis.ticks.x = element_blank(),
            # axis.ticks.x = element_line(size = .5),
            axis.ticks.length.y.left = unit(.2, "cm"),
            axis.ticks.length.x.bottom = unit(.1, "cm"),
            # axis.text.x = element_blank(),
            axis.text.x = element_text(family = "Gill Sans MT", face = "plain", size = 9, color = "#333333", 
                                        margin = margin(t = 10, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
            axis.text.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                       margin = margin(t = 0, r = -20, b = 0, l = 0)),
            axis.line.x.bottom = element_line(color = "#333333"),
            # axis.line.x.bottom = element_blank(),
            axis.line.y.left = element_blank(),
            axis.title.x = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", 
                                        margin = margin(t = 13, r = 0, b = 5, l = 0)),
            axis.title.y = element_text(family = "Gill Sans MT", face = "plain", size = 10, color = "#333333", angle = 0,
                                        vjust = .5, margin = margin(t = 0, r = 0, b = 0, l = 5)),
            # axis.title.y = element_blank(),
            # axis.text.y = element_blank(),
            plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Gill Sans", color = "#333333", 
                                      margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
            legend.position = "bottom",
            # legend.key.size = unit(2, "mm"), 
            legend.title = element_text(size = 10, family = "Gill Sans MT", face = "plain", color = "#333333"),
            legend.text = element_text(size = 8, family = "Gill Sans MT", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                       hjust = .5, color = "#333333")
            # legend.spacing.y = unit(5.5, "cm"),
            # legend.key = element_rect(size = 5),
            # legend.key.size = unit(2, 'lines')
          ) 

        
        
        #///////////////////////
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/sub_obj_4_3_country_profile_dot_plot_", 
                            str_to_lower(current_country), ".png"), plot = sub_obj_4_3_country_profile_dot_plot, 
               dpi = 300, height = 6, width = 6)
        
        # crop png
        sub_obj_4_3_country_profile_dot_plot_png <- image_read(str_c("output/charts/country_profile/sub_obj_4_3_country_profile_dot_plot_", 
                                                                     str_to_lower(current_country), ".png")) %>%
          image_crop(image = ., geometry = geometry_area(width = 1800, height = 200,#153, 
                                                         x_off = 0, y_off = 810))
    
        #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # create rect_background ####
        
        print(str_c(current_country, " blue_rect_background"))
        
        #//////////////////////////////
        
        
        # top/bottom margin are height of magnifying glass
        one_row_blue_rect_background <- tibble(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)) %>%
                ggplot(data = ., mapping = aes(x = x, y = y)) +
                geom_shape(radius = unit(.5, 'cm'), fill = "#ffffff") + #"#E0ECF7") +
                scale_x_continuous(limits = c(0, 2)) +
                scale_y_continuous(limits = c(0, 2)) +
                theme_nothing() +
                theme(aspect.ratio = 1 / 5.3) 
        
        # one_row_blue_rect_background
        ggsave(filename = "output/charts/country_profile/one_row_blue_rect_background.png", 
               plot = one_row_blue_rect_background, dpi = 300, width = 6, height = 6)
        one_row_blue_rect_background_png <- image_read(path = "output/charts/country_profile/one_row_blue_rect_background.png") %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 346, x_off = 0, y_off = 727))
        
        
        #//////////////////////////////
        
        
        # top/bottom margin are height of magnifying glass
        two_row_blue_rect_background <- tibble(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)) %>%
                ggplot(data = ., mapping = aes(x = x, y = y)) +
                geom_shape(radius = unit(.5, 'cm'), fill = "#ffffff") +
                scale_x_continuous(limits = c(0, 2)) +
                scale_y_continuous(limits = c(0, 2)) +
                theme_nothing() +
                theme(aspect.ratio = 1 / 3.8)
        
        # two_row_blue_rect_background
        ggsave(filename = "output/charts/country_profile/two_row_blue_rect_background.png", 
               plot = two_row_blue_rect_background, dpi = 300, width = 6, height = 6)
        two_row_blue_rect_background_png <- image_read(path = "output/charts/country_profile/two_row_blue_rect_background.png") %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 466, x_off = 0, y_off = 667))
        
        
        #/////////////////////
        
        
        # top/bottom margin are height of magnifying glass
        three_row_blue_rect_background <- tibble(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)) %>%
                ggplot(data = ., mapping = aes(x = x, y = y)) +
                geom_shape(radius = unit(.5, 'cm'), fill = "#ffffff") + #"#E0ECF7") +
                scale_x_continuous(limits = c(0, 2)) +
                scale_y_continuous(limits = c(0, 2)) +
                theme_nothing() +
                theme(aspect.ratio = 1 / 2.95)
        
        # three_row_blue_rect_background
        ggsave(filename = "output/charts/country_profile/three_row_blue_rect_background.png", 
               plot = three_row_blue_rect_background, dpi = 300, width = 6, height = 6)
        three_row_blue_rect_background_png <- image_read(path = "output/charts/country_profile/three_row_blue_rect_background.png") %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 590, x_off = 0, y_off = 605)) 
        
        
        #/////////////////////
        
        
        # top/bottom margin are height of magnifying glass
        four_row_blue_rect_background <- tibble(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)) %>%
          ggplot(data = ., mapping = aes(x = x, y = y)) +
          geom_shape(radius = unit(.5, 'cm'), fill = "#ffffff") + #"#E0ECF7") +
          scale_x_continuous(limits = c(0, 2)) +
          scale_y_continuous(limits = c(0, 2)) +
          theme_nothing() +
          theme(aspect.ratio = 1 / 2.45)
        
        # four_row_blue_rect_background
        ggsave(filename = "output/charts/country_profile/four_row_blue_rect_background.png", 
               plot = four_row_blue_rect_background, dpi = 300, width = 6, height = 6)
        four_row_blue_rect_background_png <- image_read(path = "output/charts/country_profile/four_row_blue_rect_background.png") %>%
          image_crop(image = ., geometry = geometry_area(width = 1800, height = 520, x_off = 0, y_off = 545)) 
        
        
        #/////////////////////  
        
        # top/bottom margin are height of magnifying glass
        six_row_blue_rect_background <- tibble(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)) %>%
                ggplot(data = ., mapping = aes(x = x, y = y)) +
                geom_shape(radius = unit(.5, 'cm'), fill = "#1B69AF") +
                scale_x_continuous(limits = c(0, 2)) +
                scale_y_continuous(limits = c(0, 2)) +
                theme_nothing() +
                theme(aspect.ratio = 1 / 1.4)
        
        # six_row_blue_rect_background
        ggsave(filename = "output/charts/country_profile/six_row_blue_rect_background.png", 
               plot = six_row_blue_rect_background, dpi = 300, width = 6, height = 6)
        six_row_blue_rect_background_png <- image_read(path = "output/charts/country_profile/six_row_blue_rect_background.png") %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 1300, #1200, #1500,
                                                               x_off = 0, y_off = 0))
        
        



        
        #//////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # country_profile_line_chart ####
        
        print(str_c(current_country, " line_chart"))
        
        #//////////////////
        
        
        # get chart_data
        chart_data <- fmir %>% 
          # filter(country == current_country & year != 2024) %>% 
          filter(country == current_country) %>%
                mutate(values = obj_avg) %>%
                distinct(country, mcp_grouping, year, obj_num, values) %>%
                bind_rows(., fmir %>% 
                                  filter(country == current_country) %>%
                                  distinct(country, mcp_grouping, year, miri_avg) %>%
                                  mutate(obj_num = "miri",
                                         values = miri_avg) %>%
                                  select(obj_num, country, year, mcp_grouping, values)) %>%
                arrange(desc(obj_num)) %>%
                mutate(color_bin = case_when(obj_num == "obj_1" ~ "     Democratic",
                                             obj_num == "obj_2" ~ "     Information",
                                             obj_num == "obj_3" ~ "     Energy",
                                             obj_num == "obj_4" ~ "     Economic",
                                             obj_num == "miri" ~ "    MIRI Overall"),
                       color = case_when(color_bin == "     Democratic" ~ color_palette %>% slice(1) %>% pull(hex),
                                         color_bin == "     Information" ~ color_palette %>% slice(2) %>% pull(hex),
                                         color_bin == "     Energy" ~ color_palette %>% slice(3) %>% pull(hex),
                                         color_bin == "     Economic" ~ "#EE6352",
                                         color_bin == "    MIRI Overall" ~ color_palette %>% slice(7) %>% pull(hex)),
                       linetype_bin = color_bin,
                       linetype = case_when(TRUE ~ "solid"))
        
        # create color_list for to pass to scale_color_manual
        chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
        names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
        chart_data_color_list
        
        # create linetype_list for to pass to scale_linetype_manual
        chart_data_linetype_list <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype)
        names(chart_data_linetype_list) <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype_bin)
        chart_data_linetype_list
        
        
        #/////////////////////
        
        
        # get label_position 
        
        # note that for coord_fixed_ratio the optimal ratio was 6/1 when y-axis spanned range of 1
        # so the y-axis units were given pixels equal to (range = 1) * (coord_fixed_numerator = 6) = (pixel units = 6)
        # so to get the same size chart with different y-axis range, 
        # use formula: range * coord_fixed_numerator = pixel_units; 
        # after rearranging: unknown_coord_fixed_numerator = pixel_units / range
        # note the coord_fixed_denominator is 1, and doesn't need to be adjusted in this case because x-axis units are unchanged
        # but if needed, could adjust coord_fixed_denominator by similar formula
        # range * coord_fixed_denominator = pixel_units
        chart_data <- chart_data %>%
          mutate(year = case_when(  #In here to stretch the x-axis without adjusting existing coord_fixed_ratio details
            year == 2019 ~ 2021,
            year == 2020 ~ 2024,
            year == 2021 ~ 2027,
            year == 2022 ~ 2030,
            year == 2023 ~ 2033,
            TRUE ~ year
          )) %>%
                mutate(max_value = max(values),
                       min_value = min(values),
                       max_value_rounded_10 = round_to_threshold(number = max_value, accuracy = .1, fun = ceiling),
                       min_value_rounded_10 = round_to_threshold(number = min_value, accuracy = .1, fun = floor),
                       max_value_rounded_20 = round_to_threshold(number = max_value, accuracy = .2, fun = ceiling),
                       min_value_rounded_20 = round_to_threshold(number = min_value, accuracy = .2, fun = floor),
                       values_range_10 = max_value_rounded_10 - min_value_rounded_10,
                       values_range_20 = max_value_rounded_20 - min_value_rounded_20,
                       coord_fixed_ratio = case_when(values_range_10 <= .6 ~ 6 / values_range_10,
                                                     values_range_10 > .6 ~ 6 / values_range_20),
                       coord_fixed_ratio = 15) #Hacky fix to make this work for now
        
        # get label_position when y-axis has .1 intervals
        if((chart_data %>% distinct(values_range_10) %>% pull(values_range_10)) <= .6) {
                if((chart_data %>% distinct(values_range_10) %>% pull(values_range_10)) >= .8) {
                        
                        chart_data <- chart_data %>% get_label_position(gap = .04) 
                }
                
                if(((chart_data %>% distinct(values_range_10) %>% pull(values_range_10)) >= .5) & 
                   ((chart_data %>% distinct(values_range_10) %>% pull(values_range_10)) < .8)) {
                        
                        chart_data <- chart_data %>% get_label_position(gap = .03)
                }
                
                if((chart_data %>% distinct(values_range_10) %>% pull(values_range_10)) < .5) {
                        
                        chart_data <- chart_data %>% get_label_position(gap = .02)
                }
        }
        
        # get label_position when y-axis has .2 intervals
        if((chart_data %>% distinct(values_range_10) %>% pull(values_range_10)) > .6) {
                if((chart_data %>% distinct(values_range_20) %>% pull(values_range_20)) >= .8) {
                        
                        chart_data <- chart_data %>% get_label_position(gap = .04)
                }
                
                if(((chart_data %>% distinct(values_range_20) %>% pull(values_range_20)) >= .5) & 
                   ((chart_data %>% distinct(values_range_20) %>% pull(values_range_20)) < .8)) {
                        
                        chart_data <- chart_data %>% get_label_position(gap = .03)
                }
                
                if((chart_data %>% distinct(values_range_20) %>% pull(values_range_20)) < .5) {
                        
                        chart_data <- chart_data %>% get_label_position(gap = .02)
                }
        }
     
        
        # inspect
        # chart_data
        
        
        
        #/////////////////////
        
        
        # create chart with y-axis gridlines with a .1 interval if values_range_10 <= .6
        if((chart_data %>% distinct(values_range_10) %>% pull(values_range_10)) <= .6) {
                
                # create chart
                country_profile_line_chart <- chart_data %>%
                        ggplot(data = ., aes(x = year, 
                                             y = values, 
                                             color = color_bin,
                                             linetype = color_bin)) + 
                        # geom_line(size = 1) +
                        # geom_point(size = 2) +
                        geom_text(data = chart_data %>% filter(year == max(year), color_bin == "     Democratic"),
                                  mapping = aes(x = year, y = label_position, label = color_bin),
                                  fontface = "plain", hjust = 0, size = 3.5, family = "Calibri") +
                        geom_text(data = chart_data %>% filter(year == max(year), color_bin == "     Information"),
                                  mapping = aes(x = year, y = label_position, label = color_bin),
                                  fontface = "plain", hjust = 0, size = 3.5, family = "Calibri") +
                        geom_text(data = chart_data %>% filter(year == max(year), color_bin == "     Energy"),
                                  mapping = aes(x = year, y = label_position, label = color_bin),
                                  fontface = "plain", hjust = 0, size = 3.5, family = "Calibri") +
                        geom_text(data = chart_data %>% filter(year == max(year), color_bin == "     Economic"),
                                  mapping = aes(x = year, y = label_position, label = color_bin),
                                  fontface = "plain", hjust = 0, size = 3.5, family = "Calibri") +
                        geom_text(data = chart_data %>% filter(year == max(year), color_bin == "    MIRI Overall"),
                                  mapping = aes(x = year, y = label_position, label = color_bin),
                                  fontface = "bold", hjust = 0, 
                                  size = 3.75,
                                  family = "Calibri") +
                        scale_color_manual(values = chart_data_color_list, guide = "none") +
                        scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +

                  scale_y_continuous(breaks = seq(0, 1,
                                                   by = .1),
                                     limits = c(0, 1),
                                     expand = c(.02, .02),
                                     labels = label_number(accuracy = .01)) +

                  scale_x_continuous(
                    breaks = c(2018, 2021, 2024, 2027, 2030, 2033),  # The actual values on the x-axis
                    labels = c("2018", "2019", "2020", "2021", "2022", "2023"),
                    expand = c(.02, .02) # Custom display labels
                  ) +
                        labs(x = NULL, y = "Score (higher is better)", 
                             caption = NULL, color = "", linetype = "") +
                        coord_fixed(ratio = (chart_data %>% distinct(coord_fixed_ratio) %>% pull(coord_fixed_ratio)), clip = "off") +
                        geom_line(size = 1.5) + 
                        # geom_point(size = 2) +
                        # geom_segment(data = segment_tbl, mapping = aes(x = x, xend = xend, y = y, yend = yend)) +
                        theme_bw() +
                        theme(
                                # plot.background = element_rect(fill = "blue"),
                                # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                                plot.margin = unit(c(0, 20, 0, 0), "mm"),
                                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                                            color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                                panel.grid.minor = element_blank(),
                                panel.grid.major.x = element_blank(),
                                panel.grid.major.y = element_line(color = "#DDDDDD"),
                                # panel.grid.major.y = element_blank(),
                                # panel.border = element_blank(),
                                panel.border = element_rect(color = "#DDDDDD", size = .5),
                                # panel.grid = element_blank(),
                                # line = element_blank(),
                                # rect = element_blank(),
                                axis.ticks.y = element_blank(),
                                # axis.ticks.y = element_line(size = .5, color = "#DDDDDD"),
                                axis.ticks.x = element_line(size = .5),
                                # axis.ticks.length.y.left = unit(.2, "cm"),
                                axis.ticks.length.x.bottom = unit(.2, "cm"),
                                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                                           margin = margin(t = 0, r = 2, b = 0, l = 0)),
                                axis.line.x.bottom = element_line(colour = "#333333", size = .5),
                                # axis.line.x.bottom = element_blank(),
                                axis.line.y.left = element_blank(),
                                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                                            margin = margin(t = 0, r = 10, b = 0, l = 5)),
                                # axis.title.y = element_blank(),
                                # axis.text.y = element_blank(),
                                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                                legend.position = "bottom",
                                # legend.key.size = unit(2, "mm"), 
                                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                           hjust = .5, color = "#333333")
                                # legend.spacing.y = unit(5.5, "cm"),
                                # legend.key = element_rect(size = 5),
                                # legend.key.size = unit(2, 'lines')
                        ) 

                
        }
        
        
        #///////////////////////
        
        
        # create chart with y-axis gridlines with a .2 interval if values_range > .6
        if((chart_data %>% distinct(values_range_10) %>% pull(values_range_10)) > .6) {
                
                # create chart
                country_profile_line_chart <- chart_data %>%
                        ggplot(data = ., aes(x = year, 
                                             y = values, 
                                             color = color_bin,
                                             linetype = color_bin)) + 
                        # geom_line(size = 1) +
                        # geom_point(size = 2) +
                        geom_text(data = chart_data %>% filter(year == max(year), color_bin == "     Democratic"),
                                  mapping = aes(x = year, y = label_position, label = color_bin),
                                  fontface = "plain", hjust = 0, size = 3.5, family = "Calibri") +
                        geom_text(data = chart_data %>% filter(year == max(year), color_bin == "     Information"),
                                  mapping = aes(x = year, y = label_position, label = color_bin),
                                  fontface = "plain", hjust = 0, size = 3.5, family = "Calibri") +
                        geom_text(data = chart_data %>% filter(year == max(year), color_bin == "     Energy"),
                                  mapping = aes(x = year, y = label_position, label = color_bin),
                                  fontface = "plain", hjust = 0, size = 3.5, family = "Calibri") +
                        geom_text(data = chart_data %>% filter(year == max(year), color_bin == "     Economic"),
                                  mapping = aes(x = year, y = label_position, label = color_bin),
                                  fontface = "plain", hjust = 0, size = 3.5, family = "Calibri") +
                        geom_text(data = chart_data %>% filter(year == max(year), color_bin == "    MIRI Overall"),
                                  mapping = aes(x = year, y = label_position, label = color_bin),
                                  fontface = "bold", hjust = 0, 
                                  size = 3.75,
                                  family = "Calibri") +
                        scale_color_manual(values = chart_data_color_list, guide = "none") +
                        scale_linetype_manual(values = chart_data_linetype_list, guide = "none") +

                  scale_y_continuous(breaks = seq(0, 1,
                                                  by = .1),
                                     limits = c(0, 1),
                                     expand = c(.02, .02),
                                     labels = label_number(accuracy = .01)) +
                  scale_x_continuous(
                    breaks = c(2018, 2021, 2024, 2027, 2030, 2033),  # The actual values on the x-axis
                    labels = c("2018", "2019", "2020", "2021", "2022", "2023"),
                    expand = c(.02, .02) # Custom display labels
                  ) +
                        labs(x = NULL, y = "Score (higher is better)", 
                             caption = NULL, color = "", linetype = "") +
                        coord_fixed(ratio = (chart_data %>% distinct(coord_fixed_ratio) %>% pull(coord_fixed_ratio)), clip = "off") +
                        geom_line(size = 1.5) + 
                        # geom_point(size = 2) +
                        # geom_segment(data = segment_tbl, mapping = aes(x = x, xend = xend, y = y, yend = yend)) +
                        theme_bw() +
                        theme(
                                # plot.background = element_rect(fill = "blue"),
                                # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                                plot.margin = unit(c(0, 20, 0, 0), "mm"),
                                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                                            color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                                panel.grid.minor = element_blank(),
                                panel.grid.major.x = element_blank(),
                                panel.grid.major.y = element_line(color = "#DDDDDD"),
                                # panel.grid.major.y = element_blank(),
                                # panel.border = element_blank(),
                                panel.border = element_rect(color = "#DDDDDD", size = .5),
                                # panel.grid = element_blank(),
                                # line = element_blank(),
                                # rect = element_blank(),
                                axis.ticks.y = element_blank(),
                                # axis.ticks.y = element_line(size = .5, color = "#DDDDDD"),
                                axis.ticks.x = element_line(size = .5),
                                # axis.ticks.length.y.left = unit(.2, "cm"),
                                axis.ticks.length.x.bottom = unit(.2, "cm"),
                                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                                           margin = margin(t = 0, r = 2, b = 0, l = 0)),
                                axis.line.x.bottom = element_line(colour = "#333333", size = .5),
                                # axis.line.x.bottom = element_blank(),
                                axis.line.y.left = element_blank(),
                                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                                            margin = margin(t = 0, r = 10, b = 0, l = 5)),
                                # axis.title.y = element_blank(),
                                # axis.text.y = element_blank(),
                                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                                legend.position = "bottom",
                                # legend.key.size = unit(2, "mm"), 
                                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                                           hjust = .5, color = "#333333")

                        ) 

        }
        
        
        #///////////////////////        
                
                
        
        
        # save plot as png
        ggsave(file = str_c("output/charts/country_profile/country_profile_line_chart_", 
                            str_to_lower(current_country), ".png"), plot = country_profile_line_chart, 
               dpi = 400, 
               height = 5,#6,
               width = 9) #7) #6)

        country_profile_line_chart_png <- image_read(str_c("output/charts/country_profile/country_profile_line_chart_", 
                                                           str_to_lower(current_country), ".png"))
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # country_profile_map ####
        
        print(str_c(current_country, " map"))
        
        # https://wilkelab.org/practicalgg/articles/Winkel_tripel.html
        # note that getMap uses natural earth for country boundaries, but fixes several issues, so better to pull from rworldmap
        # (see rworldmap docs: https://cran.r-project.org/web/packages/rworldmap/rworldmap.pdf)
        # ?countriesCoarse
        # ?countriesCoarseLessIslands
        
        # another option is rnaturalearth package: https://docs.ropensci.org/rnaturalearth/
        
        
        # get world map as sf object
        world_sf <- st_as_sf(getMap(resolution = "high"))
      
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # plot
        country_profile_map <- world_sf %>% 
                mutate(SOVEREIGNT = as.character(SOVEREIGNT),
                       SOVEREIGNT = case_when(SOVEREIGNT == "Czech Republic" ~ "Czechia",
                                              SOVEREIGNT == "Macedonia" ~ "N. Macedonia",
                                              SOVEREIGNT == "Republic of Serbia" ~ "Serbia",
                                              SOVEREIGNT == "United Kingdom" ~ "U.K.",
                                              SOVEREIGNT == "Bosnia and Herzegovina" ~ "BiH",
                                              TRUE ~ SOVEREIGNT),
                       fill_color_bin = "selected_color") %>%
                filter(SOVEREIGNT != "Antarctica") %>%
                filter(SOVEREIGNT == current_country) %>%
                ggplot(data = ., mapping = aes(fill = factor(fill_color_bin))) + 
                geom_sf(color = "#002F6C", #"#E0ECF7",
                       size = 0.1) +
                scale_fill_manual(values = list("selected_color" = "white"), #"#E0ECF7"), 
                                  guide = "none") +
                theme_bw() +
                theme(panel.grid.major = element_line(color = "transparent"),
                      plot.background = element_rect(fill = "#002F6C", color = NA), 
                      panel.grid.minor = element_blank(), panel.border = element_blank(),
                      axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
                      axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
                      # plot.title = element_text(size = 6, face = "bold", hjust = 0, family = "Trebuchet MS"), 
                      plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                                  color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                      legend.position = "right",
                      legend.key.size = unit(2, "mm"),
                      legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")), 
                      legend.title = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
                      panel.grid = element_blank(),
                      line = element_blank(),
                      rect = element_blank(),
                      text = element_blank())
        
        # inspect
        # country_profile_map
        
        ggsave(filename = str_c("output/charts/country_profile/map_", str_to_lower(current_country), ".png"), 
               plot = country_profile_map, 
               dpi = 300, width = 6, height = 6)
        
        
        if(current_country == "Albania") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 800, height = 1700, x_off = 500, y_off = 50))
                
        } else if(current_country == "Azerbaijan") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1300, x_off = 50, y_off = 250))
                
        } else if(current_country == "Belarus") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1500, x_off = 50, y_off = 150))
                
        } else if(current_country == "Bulgaria") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1200, x_off = 50, y_off = 300))
                
        } else if(current_country == "Czechia") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1000, x_off = 50, y_off = 400))
                
        } else if(current_country == "Estonia") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1200, x_off = 50, y_off = 300))
                
        } else if(current_country == "Georgia") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 900, x_off = 50, y_off = 450))
                
        } else if(current_country == "Hungary") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1100, x_off = 50, y_off = 350))
                
        } else if(current_country == "Kazakhstan") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1000, x_off = 50, y_off = 400))
                
        } else if(current_country == "Kyrgyzstan") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 800, x_off = 50, y_off = 500))
                
        } else if(current_country == "Latvia") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1100, x_off = 50, y_off = 350))
                
        } else if(current_country == "Lithuania") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1300, x_off = 50, y_off = 250))
                
        } else if(current_country == "N. Macedonia") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1300, x_off = 50, y_off = 250))
                
        } else if(current_country == "Moldova") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1300, height = 1700, x_off = 250, y_off = 50))
                
        } else if(current_country == "Montenegro") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1500, height = 1700, x_off = 150, y_off = 50))
                
        } else if(current_country == "Poland") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1600, x_off = 50, y_off = 75))
                
        } else if(current_country == "Romania") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1200, x_off = 50, y_off = 300))
                
        } else if(current_country == "Serbia") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1300, height = 1700, x_off = 250, y_off = 50))
                
        } else if(current_country == "Slovakia") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 900, x_off = 50, y_off = 450))
                
        } else if(current_country == "Slovenia") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1100, x_off = 50, y_off = 350))
                
        } else if(current_country == "Tajikistan") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1200, x_off = 50, y_off = 300))
                
        } else if(current_country == "Turkmenistan") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1200, x_off = 50, y_off = 300))
                
        } else if(current_country == "Ukraine") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1200, x_off = 50, y_off = 300))
                
        } else if(current_country == "Uzbekistan") {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country), 
                                                            ".png")) %>%
                        image_crop(image = ., geometry = geometry_area(width = 1700, height = 1200, x_off = 50, y_off = 300))
                
        } else {
                
                country_profile_map_png <- image_read(str_c("output/charts/country_profile/map_", str_to_lower(current_country),
                                                            ".png"))
                
        }
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # country_profile_usaid_logo ####
        
        print(str_c(current_country, " logo"))
        
        # https://www.usaid.gov/branding/resources # best source w white logo
        # https://www.usaid.gov/sites/default/files/documents/1869/USAID_GSM-02_04_2020.pdf
        # usaid blue: #002F6C
        
        # extra un-used sources
        # https://cloudconvert.com/ai-to-svg
        # https://seekvectorlogo.net/download-vector-logo-3901/#
        # https://stephanieevergreen.com/events/uganda/1200px-usaid-logo-svg/
        
        # read usaid white logo as png, convert to resized ggplot w blue background, resave as png
        # note that there was no discernable difference in 300 vs 600 dpi image; pdf file size increased by 200 kb, will go w 300 dpi
        usaid_png <- image_read(path = "output/charts/usaid_logo/Horizontal_RGB_294_White.png")
        usaid_logo <- ggplot() + background_image(usaid_png) + 
                coord_fixed(1 / 2.5) +
                theme_nothing() +
                theme(
                        panel.background = element_rect(fill = "#002F6C"),
                        plot.background = element_rect(fill = "#002F6C"),
                        rect = element_rect(fill = "#002F6C")
                )
        ggsave(filename = "output/charts/usaid_logo/usaid_logo_white_resized.png", plot = usaid_logo, dpi = 300, width = 6, height = 6)
        
        # crop
        country_profile_usaid_logo_png <- image_read("output/charts/usaid_logo/usaid_logo_white_resized.png") %>%
                image_crop(image = ., geometry = geometry_area(width = 1600, height = 600, x_off = 100, y_off = 600)) # crop

        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # create country_profile_blue_banner ####
        
        print(str_c(current_country, " blue_banner"))
        
        country_profile_blue_banner <- tibble(x = c(0, 0, 2, 2), y = c(0, 2, 2, 0)) %>%
                ggplot(data = ., mapping = aes(x = x, y = y)) +
                geom_rect(xmin = 0, xmax = 2, ymin = 0, ymax = 2, color = "#002F6C", fill = "#002F6C") +
                theme_nothing() +
                theme(
                        aspect.ratio = 1 / 100
                )
        # country_profile_blue_banner 
        ggsave(filename = "output/charts/usaid_logo/country_profile_blue_banner.png", 
               plot = country_profile_blue_banner, dpi = 300, width = 6, height = 6)
        
        # crop
        country_profile_blue_banner_png <- image_read("output/charts/usaid_logo/country_profile_blue_banner.png") %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 16, x_off = 0, y_off = 892)) 
        
        country_profile_blue_bottom_banner_png <- image_read("output/charts/usaid_logo/country_profile_blue_banner.png") %>%
          image_crop(image = ., geometry = geometry_area(width = 1800, height = 39,
                                                         x_off = 0, y_off = 892)) 
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # country_profile_divider ####
        
        print(str_c(current_country, " divider"))
        
        country_profile_divider <- tibble(x = c(0, 1), y = c(1, 3), color = "dot_color") %>%
                ggplot(data = ., mapping = aes(x = x, y = y, color = color)) +
                geom_point() +
                scale_color_manual(values = "#ffffff") +
                geom_segment(x = 0, y = 2, xend = 1, yend = 2, colour = "#CBCBCB", lineend = "round", size = 1) +
                theme_nothing() +
                theme(
                        aspect.ratio = 1 / 50
                )
        ggsave(filename = "output/charts/country_profile/country_profile_divider.png", 
               plot = country_profile_divider, dpi = 300, width = 6, height = 6)
        
        # read resized usaid logo png and crop
        country_profile_divider_png <- image_read("output/charts/country_profile/country_profile_divider.png") %>%
                image_crop(image = ., geometry = geometry_area(width = 1800, height = 50, x_off = 0, y_off = 875)) 
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # create country_profile layout ####
        print(str_c(current_country, " layout"))
        
        page_height <- 100
        page_width <- 100
        side_margin <- 11 # 11 give appropriate to edge in pdf
        center_margin <- 2
        below_margin <- 2
        dot_plot_section_height <- 60
        top_section_height <- 100 - dot_plot_section_height
        dot_plot_width <- (page_width - (2 * side_margin) - center_margin) / 2
        dot_plot_ri_height <- round((dot_plot_section_height - (3 * below_margin)) * (4/11))
        dot_plot_obj_1_height <- round((dot_plot_section_height - (3 * below_margin)) * (2/11)) 
        dot_plot_obj_2_height <- round((dot_plot_section_height - (3 * below_margin)) * (2/11))
        dot_plot_obj_3_height <- round((dot_plot_section_height - (3 * below_margin)) * (2/11))
        dot_plot_obj_4_height <- round((dot_plot_section_height - (3 * below_margin)) * (1.5/11))

        
        layout <- c(
                area(t = 1, l = 1, b = top_section_height, r = page_width), # spacer 1 to 1
                
                # dot_plot_ri
                area(t = (top_section_height) + 1, 
                     l = (side_margin) + 1, 
                     b = (top_section_height) + dot_plot_ri_height, 
                     r = (side_margin) + dot_plot_width), 
                
                # dot_plot_obj_1
                area(t = (top_section_height) + 1, 
                     l = (side_margin + dot_plot_width + center_margin) + 1, 
                     b = (top_section_height) + dot_plot_obj_1_height, 
                     r = (side_margin + dot_plot_width + center_margin) + dot_plot_width), 
                
                # dot_plot_obj_2
                area(t = (top_section_height + dot_plot_ri_height + below_margin) + 1, 
                     l = (side_margin) + 1, 
                     b = (top_section_height + dot_plot_ri_height + below_margin) + dot_plot_obj_2_height, 
                     r = (side_margin) + dot_plot_width), 
                
                # dot_plot_obj_3
                area(t = (top_section_height + dot_plot_obj_1_height + below_margin) + 1, 
                     l = (side_margin + dot_plot_width + center_margin) + 1, 
                     b = (top_section_height + dot_plot_obj_1_height + below_margin) + dot_plot_obj_3_height, 
                     r = (side_margin + dot_plot_width + center_margin) + dot_plot_width), 
                
                # dot_plot_obj_4
                area(t = (top_section_height + dot_plot_ri_height + below_margin + dot_plot_obj_2_height + below_margin) + 1, 
                     l = (side_margin) + 1, 
                     b = (top_section_height + dot_plot_ri_height + below_margin + dot_plot_obj_2_height) + dot_plot_obj_4_height, 
                     r = (side_margin) + dot_plot_width)

        )
        # plot(layout)
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # assemble country_profile patchwork ####
        
        print(str_c(current_country, " assembly"))
        
        # note this patchwork layout is no longer used, except to create canvas fitted to pdf page dimensions
        # instead cowplot is used to draw png images, which gave more efficiency / control because
        # patchwork inset_element had strange reference/alignment behavior requiring continual position adjustments
        # when new objects were added, whereas cowplot::draw_image sets permanent coordinates that are robust to additions
        # note however, that cowplot still has some weird behavior - in particular, it seems like coordinates are relative to
        # image size, which is weird. eg plot two country maps of same height but different width, and set vjust = 1
        # which should ensure that the top of each image is at the same height on the page, but instead one will be higher???
        # (eg azerbaijan and albania maps)
        
        # create country_profile_layout
        country_profile_layout <- wrap_plots(plot_spacer(),
                                             # six_row_blue_rect_background,
                                             # three_row_blue_rect_background,
                                             # three_row_blue_rect_background,
                                             # three_row_blue_rect_background,
                                             # three_row_blue_rect_background,
                                             # three_row_blue_rect_background
                                             plot_spacer(),
                                             plot_spacer(),
                                             plot_spacer(),
                                             plot_spacer(),
                                             plot_spacer(),
                                             plot_spacer()
        ) +
                plot_layout(design = layout)
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # set reference points
        
        new_top_dot_plot <- .65 #.7 #.67 #.74 #.77 #.84 #.74 #.73 #.725 #.72 #.750 #.999 #0.679
        
        top_dot_plot <- .4 #.43#.41 #.453 #.456 #.461 #.463 #.465 #.45 #.43 #.48 #.46 #.535 #.435
        
        # dot_plot_legend
        dot_plot_legend <- new_top_dot_plot - 1.94 #1.935 #1.95 #1.9 #2.25 #1.5 #.0127  #.4223
        
        # ri_dot_plot
        ri_dot_plot_ri <- new_top_dot_plot #.435, was just top_dot_plot
        ri_dot_plot_obj_1 <- new_top_dot_plot - .028#.025 #.410
        ri_dot_plot_obj_2 <- ri_dot_plot_obj_1 - .025 #.385
        ri_dot_plot_obj_3 <- ri_dot_plot_obj_2 - .025 #.360
        ri_dot_plot_obj_4 <- ri_dot_plot_obj_3 - .025 #.335
        
        # obj_1_dot_plot
        obj_1_dot_plot_sub_obj_1_1 <- top_dot_plot #+ .01 #- .0127 - .083 #dot_plot_legend - .083
        obj_1_dot_plot_sub_obj_1_2 <- obj_1_dot_plot_sub_obj_1_1 - .025#.025
        obj_1_dot_plot_sub_obj_1_3 <- obj_1_dot_plot_sub_obj_1_2 - .03#.025#.03#.025#0.55#.03
        # obj_1_dot_plot_sub_obj_1_4 <- obj_1_dot_plot_sub_obj_1_3 - .03#0.55#.03
        
        # obj_2_dot_plot
        obj_2_dot_plot_sub_obj_2_1 <- obj_1_dot_plot_sub_obj_1_3 - .05 #.06 #.07
        obj_2_dot_plot_sub_obj_2_2 <- obj_2_dot_plot_sub_obj_2_1 - .025#.025
        obj_2_dot_plot_sub_obj_2_3 <- obj_2_dot_plot_sub_obj_2_2 - .03#.025 #0.55#.03
        # obj_2_dot_plot_sub_obj_2_4 <- obj_2_dot_plot_sub_obj_2_3 - .03 #0.55#.03
        
        # obj_3_dot_plot
        obj_3_dot_plot_sub_obj_3_1 <- top_dot_plot #+ .01 
        obj_3_dot_plot_sub_obj_3_2 <- obj_3_dot_plot_sub_obj_3_1 - .03#.025
        # obj_3_dot_plot_sub_obj_3_3 <- obj_3_dot_plot_sub_obj_3_2 - .025#0.55#.03
        # obj_3_dot_plot_sub_obj_3_4 <- obj_3_dot_plot_sub_obj_3_3 - .03#0.55#.03
        
        # obj_4_dot_plot
        obj_4_dot_plot_sub_obj_4_1 <- obj_1_dot_plot_sub_obj_1_3 - .05 #.06 #.07
        #obj_4_dot_plot_sub_obj_4_1 <- obj_3_dot_plot_sub_obj_3_2 - .06#.055#.05 #.06#.076#.06 #.035#.076
        obj_4_dot_plot_sub_obj_4_2 <- obj_4_dot_plot_sub_obj_4_1 - .025#.02 #.03 #0.55#.03
        obj_4_dot_plot_sub_obj_4_3 <- obj_4_dot_plot_sub_obj_4_2 - .03#.04 #.025#0.55#.03
        # obj_4_dot_plot_sub_obj_4_4 <- obj_4_dot_plot_sub_obj_4_3 - .03#0.55#.03

        
        #///////////////////
        
        
        # set map scaling to account for cowplot::draw_image's odd behavior with coordinates being relative to image area
        # eg albania and azerbaijan maps set with vjust = 1 to same y coordinate will NOT be printed at same height on page???
        if(current_country == "Albania") {
                map_height_width_scale = .17
        }
        if(current_country == "Armenia") {
                map_height_width_scale = .18
        }
        if(current_country == "Azerbaijan") {
                map_height_width_scale = .21
        }
        if(current_country == "Belarus") {
                map_height_width_scale = .19
        }
        if(current_country == "BiH") {
                map_height_width_scale = .18
        }
        if(current_country == "Bulgaria") {
                map_height_width_scale = .25
        }
        if(current_country == "Croatia") {
                map_height_width_scale = .18
        }
        if(current_country == "Czechia") {
                map_height_width_scale = .28
        }
        if(current_country == "Estonia") {
                map_height_width_scale = .25
        }
        if(current_country == "Georgia") {
                map_height_width_scale = .32
        }
        if(current_country == "Hungary") {
                map_height_width_scale = .26
        }
        if(current_country == "Kazakhstan") {
                map_height_width_scale = .30
        }
        if(current_country == "Kosovo") {
                map_height_width_scale = .18
        }
        if(current_country == "Kyrgyzstan") {
                map_height_width_scale = .33
        }
        if(current_country == "Latvia") {
                map_height_width_scale = .27
        }
        if(current_country == "Lithuania") {
                map_height_width_scale = .22
        }
        if(current_country == "N. Macedonia") {
                map_height_width_scale = .22
        }
        if(current_country == "Moldova") {
                map_height_width_scale = .17
        }
        if(current_country == "Montenegro") {
                map_height_width_scale = .17
        }
        if(current_country == "Poland") {
                map_height_width_scale = .18
        }
        if(current_country == "Romania") {
                map_height_width_scale = .23
        }
        if(current_country == "Serbia") {
                map_height_width_scale = .17
        }
        if(current_country == "Slovakia") {
                map_height_width_scale = .34
        }
        if(current_country == "Slovenia") {
                map_height_width_scale = .25
        }
        if(current_country == "Tajikistan") {
                map_height_width_scale = .24
        }
        if(current_country == "Turkmenistan") {
                map_height_width_scale = .25
        }
        if(current_country == "Ukraine") {
                map_height_width_scale = .25
        }
        if(current_country == "Uzbekistan") {
                map_height_width_scale = .25
        }
        
        
        #///////////////////////////
        
        eu_members <- c("Bulgaria",
                        "Croatia",
                        "Czechia",
                        "Estonia",
                        "Hungary",
                        "Latvia",
                        "Lithuania",
                        "Poland",
                        "Romania",
                        "Slovakia",
                        "Slovenia")
        eu_partial <- c("Albania",
                        "BiH",
                        "Georgia",
                        "Kosovo",
                        "N. Macedonia",
                        "Moldova",
                        "Montenegro",
                        "Serbia",
                        "Ukraine")
        nato_members <- c("Albania",
                          "N. Macedonia",
                          "Montenegro",
                          "Bulgaria",
                          "Croatia",
                          "Czechia",
                          "Estonia",
                          "Hungary",
                          "Latvia",
                          "Lithuania",
                          "Poland",
                          "Romania",
                          "Slovakia",
                          "Slovenia")
        nato_pfp <- c("Armenia", 
                      "Azerbaijan",
                      "Belarus",
                      "BiH",
                      "Georgia",
                      "Kazakhstan",
                      "Kyrgyzstan",
                      "Moldova",
                      "Serbia",
                      "Tajikistan",
                      "Turkmenistan",
                      "Uzbekistan")
        csto_members <- c("Belarus",
                          "Kazakhstan",
                          "Kyrgyzstan",
                          "Russia",
                          "Tajikistan")
        eaeu_members <- c("Armenia",
                          "Belarus",
                          "Kazakhstan",
                          "Kyrgyzstan",
                          "Russia")
        eaeu_observers <- c("Moldova",
                            "Uzbekistan")

        
        if(current_country %in% eu_members) {
          eu_status = "Member"
        }
        if(current_country %in% eu_partial) {
          eu_status = "Accession In Progress"
        } 
        if(!(current_country %in% eu_members | current_country %in% eu_partial)) {
          eu_status = "Non-Member"
        }
        if(current_country %in% nato_members) {
          nato_status = "Member"
        }
        if(current_country %in% nato_pfp) {
          nato_status = "Partnership for Peace member only"
        }
        if(!(current_country %in% nato_members | current_country %in% nato_pfp)) {
          nato_status = "Non-member"
        }
        if(current_country %in% csto_members) {
          csto_status = "Member"
        }
        if(current_country == "Serbia") {
          csto_status = "Observer"
        }
        if(current_country == "Armenia") {
          csto_status = "Frozen"
        }
        if(!(current_country %in% csto_members | current_country %in% c("Armenia", "Serbia"))) {
          csto_status = "Non-Member"
        }
        if(current_country %in% eaeu_members) {
          eaeu_status = "Member"
        }
        if(current_country %in% eaeu_observers) {
          eaeu_status = "Observer"
        }
        if(!(current_country %in% eaeu_members | current_country %in% eaeu_observers)) {
          eaeu_status = "Non-Member"
        }
        
       
        #///////////////////////////
        
        
        # draw images
        country_profile <- ggdraw(country_profile_layout) +
                
                # draw banner and logo
                draw_image(country_profile_blue_banner_png, x = .5, y = .97, hjust = .5, vjust = .5, width = 7, height = 7) +
                draw_image(country_profile_usaid_logo_png, x = .15, y = .969, hjust = 0, vjust = .5, width = 0.14, height = 0.14) +
          
                
                
                # draw box text
                draw_label(label = "How to interpret these values", color = "#333333", size = 6, angle = 0,
                           fontface = "bold", fontfamily = "Calibri",
                           x = .19, #.15, #.23, #.35, #.231, #.535, #.54, #.545, #.55, #.53, #.58, #.7 #.583, 
                           y = .2, #.15, #.24, #.25, #.23, #.2, #.3,
                           hjust = 0, vjust = .5) +
          
                draw_label(label = "FY2024 data presented describes end-of-2023 conditions.\n\nUnderlying indicators are standardized so that a 0.5 is the average across all included countries, 0 is far below average (bad outcome), and 1 is far above\naverage (good outcome). A 1 does not imply perfection. Component indicators for each sub-objective are then aggregated to create the overall\nsub-objective scores.\n\nFor more information, including a set of country reports and methodology guide, reach out to the Monitoring Country Progress team: ee_mcp@usaid.gov", 
                           color = "#333333", size = 5, angle = 0,
                           fontface = "plain", fontfamily = "Calibri",
                           x = .19, #.15, #.23, #.35, #.231, #.535, #.54, #.545, #.55, #.53, #.58, #.7, #.583, 
                           y = .144, #.094,#.174,#.175, #.17, #.16, #.14,#.19, #.2, #.19, #.16, #.2, #.28, 
                           hjust = 0, vjust = .5) +

                # draw bottom banner
                draw_image(country_profile_blue_bottom_banner_png, x = .5, y = .0157,#.0155, #.0165, #.0175,#.019, #.02, #.021, #.022,#.062, 
                          hjust = .5, vjust = .5, width = 7, height = 14) + #15) + #14) +      
          
                # draw bottom banner content
                draw_label(label = paste("EU:", eu_status, "\nNATO:", nato_status, "\nCSTO:", csto_status, "\nEurasian Economic Union:", eaeu_status), 
                           color = "white", size = 6, #5, 
                           angle = 0,
                    fontface = "plain", fontfamily = "Calibri",
                     x = .16, y = .062, #.064,#.063,#.061, #.062, #.064,#.065, #.067, #.063, #.059, #.057, #.058, #.057, #.058,#.06, #.05, #.1, 
                    hjust = 0, vjust = .5) +

          
                #//////////////////////////////////////////////////////////////////////////////////////////////////////////
                

          draw_image(country_profile_map_png, 
                     x = 0.35, #0.3, #0.25, #.735, #.73, #.75, #.81, #.84, #.95, #.5, 
                     y = .969, #.96, #.975, #.985, #.992, #.995, #0.99, #.98, #.969, #.845, 
                     hjust = .5, #1, 
                     vjust = .5, #1, #.5, 
                     width = (map_height_width_scale/4), (map_height_width_scale/4)) +

                
                #//////////////////////////////////////////////////////////////////////////////////////////////////////////
          
          # draw line chart
          draw_image(country_profile_line_chart_png, 
                     x = .08, #.09, #.07, #.11, #.13,
                     y = .6, #.605, #.615, #.625,#.63, 
                     hjust = 0,#.5, 
                     vjust = .5, 
                     width = .48,#.46, #.44, #.42, #.4, #0.36, 
                     height = .48,#.46, #.44, #.42, #.4, #0.36, 
                     scale = 1) +
        
        #//////////////////////////////////////////////////////////////////////////////////////////////////////////
          # ri_dot_plot background and label
          # note that for some reason the blue_rect y coordinate does not seem to have same reference as label and dot_plots
          draw_image(six_row_blue_rect_background_png, x = .67,#.68, #.51, 
                     y = ri_dot_plot_ri + .15, #.12,#.138, #.140, #.141, #.1405, #.140,#.139,#.138, #.137, #.135, #.134,#.135,#.15,#.1,#.097,#.098,#.092, #.087,#.085,#.09,#.096,#.099,#.01,#.105,#.111,#.121,
                     hjust =.5, #1, 
                     vjust = 1, 
                     width = .37, height = .37) +
          
                #//////////////////////////////////////////////////////////////////////////////////////////////////////////
          
          # ri_dot_plot_ri
          draw_image(miri_country_profile_dot_plot_png, x = .67,#.68, #.165, 
                     y = ri_dot_plot_ri + .014, #.006,#, 
                     hjust = .5, #0, 
                     vjust = .5, 
                     width = 0.32, height = 0.32) +
          draw_label(label = "MIRI Overall", color = "#333333", size = 6, angle = 0,
                     fontface = "bold", fontfamily = "Calibri",
                     x = .58, #.565, #.756, #.241,#.35, #.205, 
                     y = ri_dot_plot_ri + .017,#.009,#.003, 
                     hjust = 1, 
                     vjust = .5) +
          
          # ri_dot_plot_obj_1
          draw_image(obj_1_country_profile_dot_plot_png, x = .67,#.68, #.165, 
                     y = ri_dot_plot_obj_1 + .014, #.006,#,
                     hjust = .5, #1, 
                     vjust = .5, width = 0.32, height = 0.32) +
          draw_label(label = "Democratic", color = "#333333", size = 5, angle = 0,
                     fontface = "plain", fontfamily = "Calibri",
                     x = .58, #.565, #.756, #.241,#.35, #.205, 
                     y = ri_dot_plot_obj_1 + .018, #.01, #.004, 
                     hjust = 1, 
                     vjust = .5) +
          
          # ri_dot_plot_obj_2
          draw_image(obj_2_country_profile_dot_plot_png, x = .67,#.68, #.165, 
                     y = ri_dot_plot_obj_2 + .014, #.006,#,
                     hjust = .5, #0, 
                     vjust = .5, 
                     width = 0.32, height = 0.32) +
          draw_label(label = "Information", color = "#333333", size = 5, angle = 0,
                     fontface = "plain", fontfamily = "Calibri",
                     x = .58, #.565, #.756, #.241,#.35, #.205, 
                     y = ri_dot_plot_obj_2 + .018, #.01, #.004, 
                     hjust = 1, #.5, #1, 
                     vjust = .5) +
          
          # ri_dot_plot_obj_3
          draw_image(obj_3_country_profile_dot_plot_png, x = .67,#.68, #.165, 
                     y = ri_dot_plot_obj_3 + .014, #.006,#,
                     hjust = .5, #0, 
                     vjust = .5, width = 0.32, height = 0.32) +
          draw_label(label = "Energy", color = "#333333", size = 5, angle = 0,
                     fontface = "plain", fontfamily = "Calibri",
                     x = .58, #.565, #.756, #.241,#.35, #.205, 
                     y = ri_dot_plot_obj_3 + .018, #.01, #.004, 
                     hjust = 1, #.5, #1, 
                     vjust = .5) +
          
          # ri_dot_plot_obj_4
          draw_image(obj_4_country_profile_dot_plot_png, x = .67,#.68, #.165, 
                     y = ri_dot_plot_obj_4 + .014, #.006,#,
                     hjust = .5, #0, 
                     vjust = .5, 
                     width = 0.32, height = 0.32) +
          draw_label(label = "Economic", color = "#333333", size = 5, angle = 0,
                     fontface = "plain", fontfamily = "Calibri",
                     x = .58, #.565, #.575, #.756, #.241,#.205, 
                     y = ri_dot_plot_obj_4 + .018, #.01, #.004, 
                     hjust = 1, #.5, #1, 
                     vjust = .5) +
          
                #//////////////////////////////////////////////////////////////////////////////////////////////////////////
          
          
                # draw radar chart
                draw_image(country_profile_radar_chart_png, 
                           x = .74, #.745, #.73, #.72, #.71, #.73, #.78, #.8, .#.81, #.84, 
                           y = .89, #.88, #.9, #.957, #.955,
                           hjust = 1, vjust = 1, width = 0.22, height = 0.22,
                           scale = 1 #0.9 #X
                ) +
               draw_label(label = "EU-15 countries\nare included\nhere as a radar\nbenchmark and\nare excluded\nfrom all dot\nplots below", 
                          color = "#333333", 
                          size = 5, 
                          angle = 0,
                          fontface = "plain", 
                          fontfamily = "Calibri",
                          x = .74, #.7, #.76, #.78, #.85, #.82,
                          y = .843, #.9, #.96,
                          hjust = 0,
                          vjust = 1) +

          
                #//////////////////////////////////////////////////////////////////////////////////////////////////////////
          
                
                # draw divider
                draw_image(country_profile_divider_png, x = .5, y = top_dot_plot + .038, #.035, #.02, #.05,#.03, #.06,#+ .012, #+ 0,#+ .052, 
                           hjust = .5, vjust = .5, width = .75, height = .75) +
                
                
                #//////////////////////////////////////////////////////////////////////////////////////////////////////////
                #//////////////////////////////////////////////////////////////////////////////////////////////////////////
                #//////////////////////////////////////////////////////////////////////////////////////////////////////////
                
              
                
        # obj_1_dot_plot background and label
        # note that for some reason the blue_rect y coordinate does not seem to have same reference as label and dot_plots
          draw_label(label = "Democratic sub-objectives", color = "#002F6C", size = 6, angle = 0,
                     fontface = "bold", fontfamily = "Calibri",
                     x = .165, y = obj_1_dot_plot_sub_obj_1_1 + .023, hjust = 0, vjust = .5) +
          
          # obj_1_dot_plot_sub_obj_1_1
          draw_image(sub_obj_1_1_country_profile_dot_plot_png, x = .165, y = obj_1_dot_plot_sub_obj_1_1,
                     hjust = 0, vjust = .5, width = 0.32, height = 0.25) +
          draw_label(label = "Checks, balances,\n& rule of law", color = "#333333", size = 5, angle = 0,
                     fontface = "plain", fontfamily = "Calibri",
                     x = .231, #.236, #.245,#.205, 
                     y = obj_1_dot_plot_sub_obj_1_1 + .003, hjust = 1, vjust = .5) +
          
          # obj_1_dot_plot_sub_obj_1_2
          draw_image(sub_obj_1_2_country_profile_dot_plot_png, x = .165, y = obj_1_dot_plot_sub_obj_1_2,
                     hjust = 0, vjust = .5, width = 0.32, height = 0.25) +
          draw_label(label = "Civil society", color = "#333333", size = 4.5, angle = 0,
                     fontface = "plain", fontfamily = "Calibri",
                     x = .231, #.236, #.245,#.205, 
                     y = obj_1_dot_plot_sub_obj_1_2 + .003, #.004, 
                     hjust = 1, vjust = .5) +
          
          # obj_1_dot_plot_sub_obj_1_3
          draw_image(sub_obj_1_3_country_profile_dot_plot_png, x = .165, y = obj_1_dot_plot_sub_obj_1_3,
                     hjust = 0, vjust = .5, width = 0.32, height = 0.25) +
          draw_label(label = "Citizen\nengagement", color = "#333333", size = 4.5, angle = 0,
                     fontface = "plain", fontfamily = "Calibri",
                     .231, #x = .236, #.245,#.205, 
                     y = obj_1_dot_plot_sub_obj_1_3 + .007,#.008, 
                     hjust = 1, vjust = .5) +
  
          
                #///////////////////////////////////////////////////////////////////////////////////////////////////////
                
                
                # obj_2_dot_plot background and label
                # note that for some reason the blue_rect y coordinate does not seem to have same reference as label and dot_plots
                draw_label(label = "Information sub-objectives", color = "#002F6C", size = 6, angle = 0,
                           fontface = "bold", fontfamily = "Calibri",
                           x = .165, 
                           y = obj_2_dot_plot_sub_obj_2_1 + .023, hjust = 0, vjust = .5) +
                
                # obj_2_dot_plot_sub_obj_2_1
                draw_image(sub_obj_2_1_country_profile_dot_plot_png, x = .165, y = obj_2_dot_plot_sub_obj_2_1,
                           hjust = 0, vjust = .5, width = 0.32, height = 0.32) +
                draw_label(label = "Media quality\n& capacity", color = "#333333", size = 5, angle = 0,
                           fontface = "plain", fontfamily = "Calibri",
                           x = .231, 
                           y = obj_2_dot_plot_sub_obj_2_1 + .003, hjust = 1, vjust = .5) +

                # obj_2_dot_plot_sub_obj_2_2
                draw_image(sub_obj_2_2_country_profile_dot_plot_png, x = .165, y = obj_2_dot_plot_sub_obj_2_2,
                           hjust = 0, vjust = .5, width = 0.32, height = 0.32) +
                draw_label(label = "Media literacy\n& engagement", color = "#333333", size = 5, angle = 0,
                           fontface = "plain", fontfamily = "Calibri",
                           x = .231,
                           y = obj_2_dot_plot_sub_obj_2_2 + .003, #.006,#.004, 
                           hjust = 1, vjust = .5) +

                # obj_2_dot_plot_sub_obj_2_3
                draw_image(sub_obj_2_3_country_profile_dot_plot_png, x = .165, y = obj_2_dot_plot_sub_obj_2_3,
                           hjust = 0, vjust = .5, width = 0.32, height = 0.32) +
                draw_label(label = "Whole of society\ninfo resilience", color = "#333333", size = 5, angle = 0,
                           fontface = "plain", fontfamily = "Calibri",
                           x = .231,
                           y = obj_2_dot_plot_sub_obj_2_3 + .007,#.008, #.005,#.006,#.008, 
                           hjust = 1, vjust = .5) +
          
                #///////////////////////////////////////////////////////////////////////////////////////////////////////
                
                
                # obj_3_dot_plot background and label
                # note that for some reason the blue_rect y coordinate does not seem to have same reference as label and dot_plots
                draw_label(label = "Energy sub-objectives", color = "#002F6C", size = 6, angle = 0,
                           fontface = "bold", fontfamily = "Calibri",
                           x = .518, 
                           y = obj_3_dot_plot_sub_obj_3_1 + .023, hjust = 0, vjust = .5) +
                
                # obj_3_dot_plot_sub_obj_3_1
                draw_image(sub_obj_3_1_country_profile_dot_plot_png, x = .518, y = obj_3_dot_plot_sub_obj_3_1,
                           hjust = 0, vjust = .5, width = 0.32, height = 0.32) +
                draw_label(label = "Energy & tech\nindependence", color = "#333333", size = 5, angle = 0,
                           fontface = "plain", fontfamily = "Calibri",
                           x = .583,
                           y = obj_3_dot_plot_sub_obj_3_1 + .003, hjust = 1, vjust = .5) +

                # obj_3_dot_plot_sub_obj_3_2
                draw_image(sub_obj_3_2_country_profile_dot_plot_png, x = .518, y = obj_3_dot_plot_sub_obj_3_2,
                           hjust = 0, vjust = .5, width = 0.32, height = 0.32) +
                draw_label(label = "Sector oversight\n& governance", color = "#333333", size = 5, angle = 0,
                           fontface = "plain", fontfamily = "Calibri",
                           x = .583,
                           y = obj_3_dot_plot_sub_obj_3_2 + .007,#.008, 
                           hjust = 1, vjust = .5) +
          
                #///////////////////////////////////////////////////////////////////////////////////////////////////////
                
        # obj_4_dot_plot background and label
        # note that for some reason the blue_rect y coordinate does not seem to have same reference as label and dot_plots
          draw_label(label = "Economic sub-objectives", color = "#002F6C", size = 6, angle = 0,
                     fontface = "bold", fontfamily = "Calibri",
                     x = .518, 
                     y = obj_4_dot_plot_sub_obj_4_1 + .023, hjust = 0, vjust = .5) +
          
          # obj_4_dot_plot_sub_obj_4_1
          draw_image(sub_obj_4_1_country_profile_dot_plot_png, x = .518, y = obj_4_dot_plot_sub_obj_4_1,
                     hjust = 0, vjust = .5, width = 0.32, height = 0.32) +
          draw_label(label = "Exports &\ncompetitiveness", color = "#333333", size = 5, angle = 0,
                     fontface = "plain", fontfamily = "Calibri",
                     x = .583,#.593, #.599,#.559, 
                     y = obj_4_dot_plot_sub_obj_4_1 + .003, hjust = 1, vjust = .5) +
          
          # obj_4_dot_plot_sub_obj_4_2
          draw_image(sub_obj_4_2_country_profile_dot_plot_png, x = .518, y = obj_4_dot_plot_sub_obj_4_2,
                     hjust = 0, vjust = .5, width = 0.32, height = 0.32) +
          draw_label(label = "Transparent\nfinance access", color = "#333333", size = 5, angle = 0,
                     fontface = "plain", fontfamily = "Calibri",
                     x = .583,#.593, #.599,#.559, 
                     y = obj_4_dot_plot_sub_obj_4_2 + .003, #008, 
                     hjust = 1, vjust = .5) +
          
          # obj_4_dot_plot_sub_obj_4_3
          draw_image(sub_obj_4_3_country_profile_dot_plot_png, x = .518, y = obj_4_dot_plot_sub_obj_4_3,
                     hjust = 0, vjust = .5, width = 0.32, height = 0.32) +
          draw_label(label = "Economic\nopportunities", color = "#333333", size = 5, angle = 0,
                     fontface = "plain", fontfamily = "Calibri",
                     x = .583,#.593, #.599,#.559, 
                     y = obj_4_dot_plot_sub_obj_4_3 + .007,#.008, 
                     hjust = 1, vjust = .5) +
         
          
          #///////////////////////////////////////////////////////////////////////////////////////////////////////
          
          
          # dot_plot_legend background and label
                # note that for some reason the blue_rect y coordinate does not seem to have same reference as label and dot_plots
                # dot_plot_legend
                draw_image(country_profile_dot_plot_legend_png, x = .67,#.68, #.165, #.518, 
                           y = .515, #.51, #.537, #.577, #.576, #.575, #.57,#.565,#.58,#.589,#.585,#.579,#.617,#.667,#.527,#.525,#.52,#.51,#.53,#.6, #.6325,#.6855,#dot_plot_legend + .003,
                           hjust = .5, #0, 
                           vjust = .5, width = 0.32, height = 0.32)
        
        #///////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        index_title <- "Malign Influence Resilience Index"
                country_profile_title <- str_c("FY ", as.character(current_fy), " Country Profile")

                country_profile <- ggdraw(country_profile) +

                        # draw flag
                        ##draw_image(country_profile_flag_png, x = .4455, y = .822, hjust = 0, vjust = .5, width = 0.07, height = 0.07) +


                        # draw title
                        draw_label(label = current_country_full_name, color = "#002F6C", size = 20, angle = 0,
                                   fontface = "bold", fontfamily = "Calibri",
                                   x = .16, y = .91, hjust = 0, vjust = 1) +
                        draw_label(label = index_title, color = "#002F6C", size = 10, angle = 0,
                                   fontface = "plain", fontfamily = "Calibri",
                                   x = .16, y = .862, hjust = 0, vjust = .5) +
                        draw_label(label = country_profile_title, color = "#002F6C", size = 10, angle = 0,
                                   fontface = "plain", fontfamily = "Calibri",
                                   x = .16, y = .832, hjust = 0, vjust = .5)
        
        #///////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # add classification label
        country_profile <- ggdraw(country_profile)
  
        #//////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # inspect
        # country_profile
        
        # save chart as emf
        filename <- tempfile(fileext = ".emf")
        emf(file = filename)
        print(country_profile)
        dev.off()
        
        # add emf to word doc
        output_name <- str_c("output/charts/country_profile_", str_to_lower(current_country), ".docx")
        read_docx() %>%
                body_add_img(src = filename, width = 11.68, height = 11.68) %>%
                print(target = output_name)
        
}

#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////



