extrafont::loadfonts(device="win") 
library(tidyverse)
library(readxl)
library(vdemdata)
library(skimr)
library(ggrepel)
library(cowplot)   # for theme_map()
library(sf)        # for manipulation of simple features objects
library(lwgeom)    # for st_transform_proj()
library(rworldmap) # for getMap()
library(janitor)
library(RColorBrewer)
library(scales)
library(officer)
library(devEMF)
# library(ggsn)
library(ggspatial) # ggspatial has the best map scale and north arrow
library(ggmap)
library(viridis)
library(pals)
library(ggforce)
library(biscale)
library(fmsb)
library(classInt)
library(testthat)
library(rlang)

# https://github.com/vdeminstitute/pandem
# https://www.v-dem.net/en/analysis/PanDem/
# https://www.v-dem.net/en/our-work/research-projects/pandemic-backsliding/
# https://github.com/vdeminstitute/vdemdata
# https://www.v-dem.net/en/data/data/v-dem-dataset/


#//////////////////////////////////////////////////////////////////////////////////////////////////////


# create blue_grey custom palette ####
color_palette <- tibble(hex = c("#08306B", "#08519C", "#4292C6", "#9ECAE1", "#BDBDBD", "#737373", "#484848"))
color_palette
show_col(color_palette %>% pull(hex))

# blue_grey palette supports 7 colors, plus possible extensions via fill/line type
show_col(color_palette %>% slice(1, 3) %>% pull(hex)) # 2 colors
show_col(color_palette %>% slice(1, 2, 3) %>% pull(hex)) # 3 colors
show_col(color_palette %>% slice(1, 2, 3, 4) %>% pull(hex)) # 4 colors
show_col(color_palette %>% slice(1, 2, 3, 6, 7) %>% pull(hex)) # 5 colors
show_col(color_palette %>% slice(1, 2, 3, 5, 6, 7) %>% pull(hex)) # 6 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6, 7) %>% pull(hex)) # 7 colors


#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////


# setwd
setwd("C:/Users/Stephen/Desktop/usaid/mcp/tso_portfolio_reviews/democracy_and_governance")

# load ee_country_crosswalk ####
current_directory <- getwd()
setwd("C:/Users/Stephen/Desktop/usaid/mcp/useful_info")
ee_country_crosswalk <- read_excel(path = "ee_country_crosswalk.xlsx", sheet = "ee_country_crosswalk", na = "NA")
setwd(current_directory)

ee_country_crosswalk %>% print(n = nrow(.))
ee_country_crosswalk %>% skim()


#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////


# load pandem data ####
full_pandem <- read_csv("https://github.com/vdeminstitute/pandem/raw/master/csv_files/PanDem_V4.csv")
full_pandem
full_pandem %>% glimpse()
# full_pandem %>% write_csv(path = "data/full_pandem.csv")


#//////////////////


# inspect
full_pandem %>% nrow() # 432
full_pandem %>% distinct(country_name) %>% nrow() # 144


#////////////////////////////////////////////////////////////////////////////////////////////////////////


# join ee_country_crosswalk ####

# check countries
# results: missing 3 e&e graduates (montenegro, latvia, estonia), 2 presence countries (greenland, kosovo), 1 EU-15 (luxembourg)
# change czech republic to czechia, change united states of america to u.s.
ee_country_crosswalk %>% filter(!is.na(mcp_grouping)) %>% 
        anti_join(., full_pandem %>% distinct(country_name), by = c("country" = "country_name"))

full_pandem %>% filter(str_detect(string = country_name, pattern = regex("estonia", ignore_case = TRUE)))
full_pandem %>% filter(str_detect(string = country_name, pattern = regex("montenegro", ignore_case = TRUE)))
full_pandem %>% filter(str_detect(string = country_name, pattern = regex("united states", ignore_case = TRUE)))
full_pandem %>% filter(str_detect(string = country_name, pattern = regex("czech", ignore_case = TRUE)))
full_pandem %>% filter(str_detect(string = country_name, pattern = regex("latvia", ignore_case = TRUE)))
full_pandem %>% filter(str_detect(string = country_name, pattern = regex("estonia", ignore_case = TRUE)))
full_pandem %>% filter(str_detect(string = country_name, pattern = regex("greenland", ignore_case = TRUE)))
full_pandem %>% filter(str_detect(string = country_name, pattern = regex("kosovo", ignore_case = TRUE))) %>% select(country_name)
full_pandem %>% filter(str_detect(string = country_name, pattern = regex("kyrg", ignore_case = TRUE))) %>% select(country_name)

full_pandem %>% mutate(country_name = case_when(country_name == "Czech Republic" ~ "Czechia", 
                                                country_name == "United States of America" ~ "United States",
                                                TRUE ~ country_name)) %>%
        distinct(country_name) %>% 
        anti_join(ee_country_crosswalk %>% filter(!is.na(mcp_grouping)), ., by = c("country" = "country_name"))


#///////////////////////


# join ee_country_crosswalk
full_pandem <- full_pandem %>% 
        mutate(country_name = case_when(country_name == "Czech Republic" ~ "Czechia", 
                                        country_name == "United States of America" ~ "United States",
                                        TRUE ~ country_name)) %>%
        left_join(., ee_country_crosswalk, by = c("country_name" = "country"))


#///////////////////////


# inspect
full_pandem %>% nrow() # 432
full_pandem %>% distinct(country_name) %>% nrow() # 144
full_pandem %>% glimpse()


#////////////////////////////////////////////////////////////////////////////////////////////////////////


# clean and inspect ####

# note that pandem/panback categories are taken from v-dem website map legend categorizations
# https://www.v-dem.net/en/analysis/PanDem/
data <- full_pandem %>% 
        filter(!is.na(mcp_grouping)) %>%
        mutate(pandem_category = case_when(pandem == 0 ~ "No violations",
                                        pandem > 0 & pandem < .17 ~ "Minor violations",
                                        pandem >= .17 & pandem < .3 ~ "Some violations",
                                        pandem >= .3 ~ "Major violations",
                                        TRUE ~ NA_character_),
               panback_category = case_when(panback < .1 ~ "Low risk",
                                            panback >= .1 & panback < .3 ~ "Moderate risk",
                                            panback >= .3 ~ "High risk",
                                            TRUE ~ NA_character_),
               country_name = case_when(country_name == "Bosnia and Herzegovina" ~ "BiH", 
                                        country_name == "North Macedonia" ~ "N. Macedonia",
                                        TRUE ~ country_name),
               mcp_grouping = case_when(mcp_grouping == "Central Asian Republics" ~ "CARs", TRUE ~ mcp_grouping)) %>%
        rename(type_1_discriminatory_measures = Type1,
                       type_2_derogation_of_nonderog_rights = Type2,
                       type_3_abusive_enforcement = Type3,
                       type_4_no_time_limit = Type4,
                       type_5_limits_on_legislature = Type5,
                       type_6_official_disinfo_campaign = Type6,
                       type_7_restriction_of_media_freedom = Type7,
                       time_period = timeperiod,
                       em_law = emlaw,
                       em_name = emname,
                       em_start_date = emstart,
                       type_4_sub_em_end_date = emend,
                       type_4_sub_em_limit = emlimit,
                       em_limit_date = emlimitdate,
                       em_req_legislature_approval = emlawapp,
                       em_get_legislature_approval = emlegapp,
                       type_5_sub_limitations_on_legislature = leglimit,
                       legislature_suspend_date = legdisdate,
                       legislature_resume_date = legresdate,
                       em_subnational_variation = subvar,
                       type_1_sub_discrimination = discrim,
                       type_2_sub_non_derog_rights_viol = ndrights,
                       type_7_sub_dejure_media_limitations = melim,
                       dejure_media_limitations_start_date = melimstart,
                       dejure_media_limitations_end_date = melimend,
                       type_7_sub_defacto_media_limitations_reporting_on_covid = merepfact,
                       type_7_sub_defacto_media_limitations_reporting_on_govt = merepgov,
                       type_7_sub_defacto_media_limitations_noncovid = merepoth,
                       type_7_sub_limitations_on_media_access_to_info = meinf,
                       type_7_sub_verbal_harassment_of_journalists = mevhar,
                       type_7_sub_physical_harassment_of_journalists = mephar,
                       type_6_sub_govt_disinfo = govdis,
                       type_3_sub_excess_police_military_physical_violence = pomviol,
                       military_involved_w_confinement = milinv,
                       miscellaneous_libdem_limitations = misclimit)


#///////////////////////


# inspect
# note each of 40 mcp countries has three records, one for each time period 
data %>% glimpse()
data %>% nrow() # 120
data %>% distinct(country_name) %>% nrow() # 40
data %>% count(mcp_grouping, country_name) %>% arrange(desc(n)) %>% print(n = nrow(.))
data %>% count(time_period)


#////////////////////////


# keep only march-september time_period
data <- data %>% filter(time_period == "Mar-Sep")


#///////////////////////


# inspect
data %>% nrow() # 40
data %>% distinct(country_name) %>% nrow() # 40
data %>% select(ee_status, mcp_grouping, country_name) %>% distinct() %>% 
        arrange(ee_status, mcp_grouping) %>% print(n = nrow(.))

data %>% skim()
data %>% select(contains("date")) %>% skim()
data %>% select(pandem, panback) %>% skim()
data %>% select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)"))) %>% skim()
data %>% select(matches("type_[0-9]_sub")) %>% skim()
data %>% filter(country_name == "Albania") %>% select(country_name, time_period, pandem, panback, v2x_libdem)
data %>% ggplot(data = ., aes(x = pandem)) + geom_histogram()
data %>% ggplot(data = ., aes(x = panback)) + geom_histogram()
# note that all countries in filtered data have zero for type_2_derogation_of_nonderog_rights
data %>% select(country_name, pandem, starts_with("type_2")) %>% print(n = nrow(.))
data %>% distinct(type_2_derogation_of_nonderog_rights)


#//////////////////////////


# create eu_15_record with average values
# note it will have NA for measurement vars not being used in analysis

# get measurement_vars
measurement_vars <- data %>% select(c(pandem, panback, v2x_libdem, 
                                                str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)")))) %>%
        names()
measurement_vars

# get non_measurement_vars
non_measurement_vars <- data %>% select(-c(!!!syms(measurement_vars))) %>% names()

# get eu_15_record
eu_15_record <- data %>% filter(mcp_grouping == "EU-15") %>% 
        summarize_at(.vars = vars(!!!syms(measurement_vars)), .funs = mean) %>%
        bind_cols(., map(.x = non_measurement_vars, .f = ~ tibble(!!.x := NA)) %>% bind_cols()) %>%
        select(c(!!!syms(data %>% names()))) %>% 
        mutate(country_name = "EU-15", mcp_grouping = "EU-15", iso_3_alpha = "EU-15",
               pandem_category = case_when(pandem == 0 ~ "No violations",
                                           pandem > 0 & pandem < .17 ~ "Minor violations",
                                           pandem >= .17 & pandem < .3 ~ "Some violations",
                                           pandem >= .3 ~ "Major violations",
                                           TRUE ~ NA_character_),
               panback_category = case_when(panback < .1 ~ "Low risk",
                                            panback >= .1 & panback < .3 ~ "Moderate risk",
                                            panback >= .3 ~ "High risk",
                                            TRUE ~ NA_character_))


#/////////////////////


# inspect
eu_15_record
eu_15_record %>% glimpse()
eu_15_record %>% nrow() # 1
eu_15_record %>% ncol() # 50
data %>% nrow() # 40
data %>% ncol() # 50
data %>% filter(mcp_grouping == "EU-15") %>% nrow() # 14
data %>% filter(mcp_grouping != "EU-15") %>% nrow() # 26
26 + 1 == 27
eu_15_record %>% select(!!!syms(measurement_vars))
data %>% filter(mcp_grouping == "EU-15") %>% summarize(pandem_mean = mean(pandem, na.rm = TRUE),
                                                       panback_mean = mean(panback, na.rm = TRUE),
                                                       v2x_libdem_mean = mean(v2x_libdem, na.rm = TRUE))


#/////////////////////


# replace indivual eu-15 country records with the avg eu_15_record
data <- data %>% filter(mcp_grouping != "EU-15") %>% bind_rows(., eu_15_record)


#/////////////////////


# inspect
data 
data %>% glimpse()
data %>% nrow() # 27
data %>% distinct(country_name) %>% nrow() # 27
data %>% count(mcp_grouping, country_name, iso_3_alpha) %>% print(n = nrow(.))


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# note the vdemdata package has most v-dem data, but not full_pandem data
# https://github.com/vdeminstitute/vdemdata
# https://www.v-dem.net/en/data/data/v-dem-dataset/
# https://www.v-dem.net/media/filer_public/e0/7f/e07f672b-b91e-4e98-b9a3-78f8cd4de696/v-dem_codebook_v8.pdf
vdem %>% glimpse()
vdem %>% select(country_name, year, v2x_libdem)
var_info("v2x_libdem")
var_info("pandem")
var_info("panback")

# note that full_pandem data includes latest v2x_libdem score for countries, to use in weighting calcuation of panback index
vdem %>% select(country_name, year, v2x_libdem) %>% filter(country_name == "Albania", year > 2017)
vdem %>% skim(v2x_libdem)
full_pandem %>% filter(country_name == "Albania")

# inspect pandem index
# pandem index is calculated separately for each index
# pandem index is the additive sum of type1-7 scores, divided by max additive value across countries
# note that type 4 has an actual max value of only 2, despite a theoretical max of 3
# this means that the pandem index is the sum of type scores divided by 20 ((6 types x 3 values) + 2); not the theoretical max 21
full_pandem %>% glimpse()
full_pandem %>% select(starts_with("type")) %>% skim()
full_pandem %>% filter(country_name == "Serbia")
10/20
7/20

# inspect panback index, which is derived from pandem index and weighted v2x_libdem index
full_pandem %>% filter(country_name == "Serbia")
4 * (.5 * (.254 * (1-.254)))
4 * (.35 * (.254 * (1-.254)))
mean(c(0.378968, 0.2652776))

full_pandem %>% filter(country_name == "Albania")
4 * (.15 * (0.431 * (1-0.431)))
4 * (0 * (0.431 * (1-0.431)))
mean(c(0.1471434, 0))

full_pandem %>% filter(country_name == "United States")
4 * (.3 * (.7 * (1-.7)))
4 * (.25 * (.7 * (1-.7)))
mean(c(.252, .21))


#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


# create pandem_bar_chart ####

# add color_bin and color
chart_data <- data %>% mutate(color_bin = mcp_grouping,
                                    color = case_when(color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                              color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                              color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                              color_bin == "CARs" ~ color_palette %>% slice(4) %>% pull(hex),
                              color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                              color_bin == "EU-15" ~ color_palette %>% slice(6) %>% pull(hex),
                              color_bin == "U.S." ~ color_palette %>% slice(7) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#/////////////////////


# create footnotes
# footnotes <- expression(paste("Source: Varieties of Democracy, ", italic("Pandemic Backsliding Project"), " (2020)"))

# create chart
pandem_bar_chart <- chart_data %>% 
        ggplot(data = ., aes(x = fct_reorder(.f = factor(country_name), .x = pandem), 
                             y = pandem, 
                             fill = factor(color_bin, levels = c("E&E Balkans", "E&E Eurasia", "E&E graduates", 
                                                                 "CARs", "Russia", "EU-15", "U.S.")))) + 
        geom_col(width = .8) + 
        scale_fill_manual(values = chart_data_color_list) +
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_continuous(breaks = seq(from = 0, to = .6, by = .1), limits = c(0, .65), expand = c(0, 0)) +
        labs(x = NULL, y = "Pandemic Democratic Violations Index\n(higher = more violations)", 
             title = NULL,
             caption = NULL, fill = "") +
        coord_fixed(ratio = 1/.05, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 0, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 0, r = 0, b = 0, l = 0), angle = 45, hjust = 1),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 0, r = 13, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) + 
        guides(fill = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333"))
        
# inspect
pandem_bar_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(pandem_bar_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/pandem_bar_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


# create panback_bar_chart ####

# add color_bin and color
chart_data <- data %>% mutate(color_bin = mcp_grouping,
                              color = case_when(color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                                color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                                color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                                color_bin == "CARs" ~ color_palette %>% slice(4) %>% pull(hex),
                                                color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                                color_bin == "EU-15" ~ color_palette %>% slice(6) %>% pull(hex),
                                                color_bin == "U.S." ~ color_palette %>% slice(7) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#/////////////////////


# create footnotes
# footnotes <- expression(paste("Source: Varieties of Democracy, ", italic("Pandemic Backsliding Project"), " (2020)"))

# create chart
panback_bar_chart <- chart_data %>% 
        ggplot(data = ., aes(x = fct_reorder(.f = factor(country_name), .x = panback), 
                             y = panback, 
                             fill = factor(color_bin, levels = c("E&E Balkans", "E&E Eurasia", "E&E graduates", 
                                                                 "CARs", "Russia", "EU-15", "U.S.")))) + 
        geom_col(width = .8) + 
        scale_fill_manual(values = chart_data_color_list) +
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_continuous(breaks = seq(from = 0, to = .6, by = .1), limits = c(0, .65), expand = c(0, 0)) +
        labs(x = NULL, y = "Pandemic Democratic Violations Index\n(higher = more violations)", 
             title = NULL,
             caption = NULL, fill = "") +
        coord_fixed(ratio = 1/.05, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 0, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 0, r = 0, b = 0, l = 0), angle = 45, hjust = 1),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 0, r = 13, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) + 
        guides(fill = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333"))


# inspect
panback_bar_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(panback_bar_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/panback_bar_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


# create pandem_panback_scatterplot ####

# add color_bin and color
chart_data <- data %>% mutate(color_bin = mcp_grouping,
                              color = case_when(color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                                color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                                color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                                color_bin == "CARs" ~ color_palette %>% slice(4) %>% pull(hex),
                                                color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                                color_bin == "EU-15" ~ color_palette %>% slice(6) %>% pull(hex),
                                                color_bin == "U.S." ~ color_palette %>% slice(7) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#/////////////////////


# create footnotes
# footnotes <- expression(paste("Source: Varieties of Democracy, ", italic("Pandemic Backsliding Project"), " (2020)"))

# create chart
pandem_panback_scatterplot <- chart_data %>% 
        ggplot(data = ., aes(x = pandem, y = panback, label = iso_3_alpha,
                             color = factor(color_bin, levels = c("E&E Balkans", "E&E Eurasia", "E&E graduates", 
                                                                 "CARs", "Russia", "EU-15", "U.S.")))) + 
        geom_point(size = 6) + 
        geom_text_repel(fontface = "bold", point.padding = .3, size = 3.25) +
        scale_color_manual(values = chart_data_color_list) +
        # scale_x_discrete(expand = c(0, 0)) +
        scale_y_continuous(breaks = seq(from = 0, to = .4, by = .1), limits = c(-.05, .45), expand = c(-.05, 0)) +
        scale_x_continuous(breaks = seq(from = 0, to = .6, by = .1), limits = c(-.05, .65), expand = c(-.05, 0)) +
        labs(x = "Pandemic Democratic Violations Index\n(higher = more violations)", 
             y = "Pandemic Backsliding Index\n(higher = more backsliding risk)", 
             title = NULL,
             caption = NULL, color = "") +
        coord_fixed(ratio = 1/1.3, clip = "off") +
        # coord_flip() + 
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 0, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_blank(),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_line(color = "#333333"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = -5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 0, r = 13, b = 0, l = 0)),
                plot.title = element_text(size = 18, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) + 
        guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333"))


# inspect
pandem_panback_scatterplot


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(pandem_panback_scatterplot)
dev.off()

# add emf to word doc - will manually crop map in word doc 
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/pandem_panback_scatterplot.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


# create pandem_libdem_scatterplot ####

# add color_bin and color
chart_data <- data %>% mutate(color_bin = mcp_grouping,
                              color = case_when(color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                                color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                                color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                                color_bin == "CARs" ~ color_palette %>% slice(4) %>% pull(hex),
                                                color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                                color_bin == "EU-15" ~ color_palette %>% slice(6) %>% pull(hex),
                                                color_bin == "U.S." ~ color_palette %>% slice(7) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#/////////////////////


# create footnotes
# footnotes <- expression(paste("Source: Varieties of Democracy, ", italic("Pandemic Backsliding Project"), " (2020)"))

# create chart
pandem_libdem_scatterplot <- chart_data %>% 
        ggplot(data = ., aes(x = v2x_libdem, y = pandem, label = iso_3_alpha,
                             color = factor(color_bin, levels = c("E&E Balkans", "E&E Eurasia", "E&E graduates", 
                                                                  "CARs", "Russia", "EU-15", "U.S.")))) + 
        geom_point(size = 6) + 
        geom_text_repel(fontface = "bold", point.padding = .3, size = 3.25) +
        scale_color_manual(values = chart_data_color_list) +
        # scale_x_discrete(expand = c(0, 0)) +
        scale_x_continuous(breaks = seq(from = 0, to = .9, by = .1), limits = c(-.05, 1), expand = c(-.05, 0)) +
        scale_y_continuous(breaks = seq(from = 0, to = .6, by = .1), limits = c(-.1, .65), expand = c(-.05, 0)) +
        labs(x = "Liberal Democracy Index\n(higher = more democratic)", 
             y = "Pandemic Democratic Violations Index\n(higher = more violations)", 
             title = NULL,
             caption = NULL, color = "") +
        coord_fixed(ratio = 1/1.3, clip = "off") +
        # coord_flip() + 
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 0, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_blank(),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.y.left = element_line(color = "#333333"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = -5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 0, r = 13, b = 0, l = 0)),
                plot.title = element_text(size = 18, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) + 
        guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333"))


# inspect
pandem_libdem_scatterplot


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(pandem_libdem_scatterplot)
dev.off()

# add emf to word doc - will manually crop map in word doc 
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/pandem_libdem_scatterplot.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


# get custom radarchart2, which takes vlabcol and vlab_fontface arg ####
# https://stackoverflow.com/questions/54185029/change-labels-colors-in-r-radarchart
radarchart2 <- function (df, axistype = 0, seg = 4, pty = 16, pcol = 1:8, plty = 1:6, 
                         plwd = 1, pdensity = NULL, pangle = 45, pfcol = NA, cglty = 3, 
                         cglwd = 1, cglcol = "navy", axislabcol = "blue", vlabcol = "black", vlab_fontface = 1, title = "", 
                         maxmin = TRUE, na.itp = TRUE, centerzero = FALSE, vlabels = NULL, 
                         vlcex = NULL, caxislabels = NULL, calcex = NULL, paxislabels = NULL, 
                         palcex = NULL, ...) 
{
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
        plot(c(-1.2, 1.2), c(-1.2, 1.2), type = "n", frame.plot = FALSE, 
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
}


#//////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////


# create pandem_type_albania_radar_chart ####

# inspect
data %>% select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)"))) %>% skim()

# get ee_type_means
ee_type_means <- data %>% filter(ee_status == "ee_presence") %>% 
        summarize(type_1_discriminatory_measures = mean(type_1_discriminatory_measures),
                  type_2_derogation_of_nonderog_rights = mean(type_2_derogation_of_nonderog_rights),
                  type_3_abusive_enforcement = mean(type_3_abusive_enforcement),
                  type_4_no_time_limit = mean(type_4_no_time_limit),
                  type_5_limits_on_legislature = mean(type_5_limits_on_legislature),
                  type_6_official_disinfo_campaign = mean(type_6_official_disinfo_campaign),
                  type_7_restriction_of_media_freedom = mean(type_7_restriction_of_media_freedom))
ee_type_means

# get chart_data
current_country_name <- "Albania"
data %>% filter(country_name == current_country_name) %>% 
        select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)")))
chart_data <- tibble(type_1_discriminatory_measures = c(3, 0),
                     type_2_derogation_of_nonderog_rights = c(3, 0),
                     type_3_abusive_enforcement = c(3, 0),
                     type_4_no_time_limit = c(3, 0),
                     type_5_limits_on_legislature = c(3, 0),
                     type_6_official_disinfo_campaign = c(3, 0),
                     type_7_restriction_of_media_freedom = c(3, 0)) %>%
        bind_rows(., ee_type_means) %>%
        bind_rows(., data %>% filter(country_name == current_country_name) %>% 
                          select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)")))) %>%
        rename("Discriminatory\nmeasures" = type_1_discriminatory_measures,
               "Derogation of\nnon-derogable rights" = type_2_derogation_of_nonderog_rights,
               "Abusive\nenforcement" = type_3_abusive_enforcement,
               "No time limit\non emergency measures" = type_4_no_time_limit,
               "Limits on\nlegislature" = type_5_limits_on_legislature,
               "Official\ndisinformation campaign" = type_6_official_disinfo_campaign,
               "Restrictions\nof media freedom" = type_7_restriction_of_media_freedom)
chart_data       
 

#//////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)

# set font as parameter; 1 is normal, 2 is bold, 3 is italic
op <- par(family = "Calibri", font = 1)

radarchart2(df = chart_data, axistype = 1, 
            # point symbol
            pty = 32, 
            # color for outside of plotted rings
            pcol = c("#ff4d4d", "#2171B5"), 
            # color for inside of plotted rings
            pfcol = c(NA, "#2171B570"), 
            # width of plotted rings
            plwd = 4, 
            # line type for plotted rings
            plty = 1,
            # number of axis segments between center and outer rim
            seg = 3, 
            # line color for grid segments
            cglcol = "#999999", 
            # line type for grid segments
            cglty = 1, 
            # color of axis grid segment labels
            # axislabcol = "#333333", 
            axislabcol = "#333333",
            # values for axis grid segment labels
            caxislabels = c("None", "Minor", "Some", "Major"), 
            # line width for axis grid segments
            cglwd = 0.8, 
            # font magnification for outer labels
            vlcex = 1,
            # font color for outer labels
            vlabcol = "#333333",
            # font face for outer labels
            vlab_fontface = 1,
            # font magnification for center labels
            calcex = 1)
# Add a legend
legend(x = .6, y = 1.2, legend = c("E&E average", current_country_name), bty = "n", 
       pch = 20 , col = c("#ff4d4d", "#2171B5") , text.col = "#333333", cex = 1, pt.cex = 3)

# write emf
dev.off()

# add emf to word doc - will manually crop map in word doc 
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/pandem_type_albania_radar_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


# create pandem_type_armenia_radar_chart ####

# inspect
data %>% select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)"))) %>% skim()

# get ee_type_means
ee_type_means <- data %>% filter(ee_status == "ee_presence") %>% 
        summarize(type_1_discriminatory_measures = mean(type_1_discriminatory_measures),
                  type_2_derogation_of_nonderog_rights = mean(type_2_derogation_of_nonderog_rights),
                  type_3_abusive_enforcement = mean(type_3_abusive_enforcement),
                  type_4_no_time_limit = mean(type_4_no_time_limit),
                  type_5_limits_on_legislature = mean(type_5_limits_on_legislature),
                  type_6_official_disinfo_campaign = mean(type_6_official_disinfo_campaign),
                  type_7_restriction_of_media_freedom = mean(type_7_restriction_of_media_freedom))
ee_type_means

# get chart_data
current_country_name <- "Armenia"
data %>% filter(country_name == current_country_name) %>% 
        select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)")))
chart_data <- tibble(type_1_discriminatory_measures = c(3, 0),
                     type_2_derogation_of_nonderog_rights = c(3, 0),
                     type_3_abusive_enforcement = c(3, 0),
                     type_4_no_time_limit = c(3, 0),
                     type_5_limits_on_legislature = c(3, 0),
                     type_6_official_disinfo_campaign = c(3, 0),
                     type_7_restriction_of_media_freedom = c(3, 0)) %>%
        bind_rows(., ee_type_means) %>%
        bind_rows(., data %>% filter(country_name == current_country_name) %>% 
                          select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)")))) %>%
        rename("Discriminatory\nmeasures" = type_1_discriminatory_measures,
               "Derogation of\nnon-derogable rights" = type_2_derogation_of_nonderog_rights,
               "Abusive\nenforcement" = type_3_abusive_enforcement,
               "No time limit\non emergency measures" = type_4_no_time_limit,
               "Limits on\nlegislature" = type_5_limits_on_legislature,
               "Official\ndisinformation campaign" = type_6_official_disinfo_campaign,
               "Restrictions\nof media freedom" = type_7_restriction_of_media_freedom)
chart_data       


#//////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)

# set font as parameter; 1 is normal, 2 is bold, 3 is italic
op <- par(family = "Calibri", font = 1)

# create radar_chart
radarchart(df = chart_data, axistype = 1, 
           # point symbol
           pty = 32, 
           # color for outside of plotted rings
           pcol = c("#ff4d4d", "#2171B5"), 
           # color for inside of plotted rings
           pfcol = c(NA, "#2171B570"), 
           # width of plotted rings
           plwd = 4, 
           # line type for plotted rings
           plty = 1,
           # number of axis segments between center and outer rim
           seg = 3, 
           # line color for grid segments
           cglcol = "#999999", 
           # line type for grid segments
           cglty = 1, 
           # color of axis grid segment labels
           axislabcol = "#333333", 
           # values for axis grid segment labels
           caxislabels = c("None", "Minor", "Some", "Major"), 
           # line width for axis grid segments
           cglwd = 0.8, 
           # font magnification for outer labels
           vlcex = 1,
           # font magnification for center labels
           calcex = 1)
# Add a legend
legend(x = .6, y = 1.2, legend = c("E&E average", current_country_name), bty = "n", 
       pch = 20 , col = c("#ff4d4d", "#2171B5") , text.col = "#000000", cex = 1, pt.cex = 3)

# write emf
dev.off()

# add emf to word doc - will manually crop map in word doc 
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/pandem_type_armenia_radar_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


# create pandem_type_azerbaijan_radar_chart ####

# inspect
data %>% select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)"))) %>% skim()

# get ee_type_means
ee_type_means <- data %>% filter(ee_status == "ee_presence") %>% 
        summarize(type_1_discriminatory_measures = mean(type_1_discriminatory_measures),
                  type_2_derogation_of_nonderog_rights = mean(type_2_derogation_of_nonderog_rights),
                  type_3_abusive_enforcement = mean(type_3_abusive_enforcement),
                  type_4_no_time_limit = mean(type_4_no_time_limit),
                  type_5_limits_on_legislature = mean(type_5_limits_on_legislature),
                  type_6_official_disinfo_campaign = mean(type_6_official_disinfo_campaign),
                  type_7_restriction_of_media_freedom = mean(type_7_restriction_of_media_freedom))
ee_type_means

# get chart_data
current_country_name <- "Azerbaijan"
data %>% filter(country_name == current_country_name) %>% 
        select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)")))
chart_data <- tibble(type_1_discriminatory_measures = c(3, 0),
                     type_2_derogation_of_nonderog_rights = c(3, 0),
                     type_3_abusive_enforcement = c(3, 0),
                     type_4_no_time_limit = c(3, 0),
                     type_5_limits_on_legislature = c(3, 0),
                     type_6_official_disinfo_campaign = c(3, 0),
                     type_7_restriction_of_media_freedom = c(3, 0)) %>%
        bind_rows(., ee_type_means) %>%
        bind_rows(., data %>% filter(country_name == current_country_name) %>% 
                          select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)")))) %>%
        rename("Discriminatory\nmeasures" = type_1_discriminatory_measures,
               "Derogation of\nnon-derogable rights" = type_2_derogation_of_nonderog_rights,
               "Abusive\nenforcement" = type_3_abusive_enforcement,
               "No time limit\non emergency measures" = type_4_no_time_limit,
               "Limits on\nlegislature" = type_5_limits_on_legislature,
               "Official\ndisinformation campaign" = type_6_official_disinfo_campaign,
               "Restrictions\nof media freedom" = type_7_restriction_of_media_freedom)
chart_data       


#//////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)

# set font as parameter; 1 is normal, 2 is bold, 3 is italic
op <- par(family = "Calibri", font = 1)

# create radar_chart
radarchart(df = chart_data, axistype = 1, 
           # point symbol
           pty = 32, 
           # color for outside of plotted rings
           pcol = c("#ff4d4d", "#2171B5"), 
           # color for inside of plotted rings
           pfcol = c(NA, "#2171B570"), 
           # width of plotted rings
           plwd = 4, 
           # line type for plotted rings
           plty = 1,
           # number of axis segments between center and outer rim
           seg = 3, 
           # line color for grid segments
           cglcol = "#999999", 
           # line type for grid segments
           cglty = 1, 
           # color of axis grid segment labels
           axislabcol = "#333333", 
           # values for axis grid segment labels
           caxislabels = c("None", "Minor", "Some", "Major"), 
           # line width for axis grid segments
           cglwd = 0.8, 
           # font magnification for outer labels
           vlcex = 1,
           # font magnification for center labels
           calcex = 1)
# Add a legend
legend(x = .6, y = 1.2, legend = c("E&E average", current_country_name), bty = "n", 
       pch = 20 , col = c("#ff4d4d", "#2171B5") , text.col = "#000000", cex = 1, pt.cex = 3)

# write emf
dev.off()

# add emf to word doc - will manually crop map in word doc 
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/pandem_type_azerbaijan_radar_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


# create pandem_type_belarus_radar_chart ####

# inspect
data %>% select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)"))) %>% skim()

# get ee_type_means
ee_type_means <- data %>% filter(ee_status == "ee_presence") %>% 
        summarize(type_1_discriminatory_measures = mean(type_1_discriminatory_measures),
                  type_2_derogation_of_nonderog_rights = mean(type_2_derogation_of_nonderog_rights),
                  type_3_abusive_enforcement = mean(type_3_abusive_enforcement),
                  type_4_no_time_limit = mean(type_4_no_time_limit),
                  type_5_limits_on_legislature = mean(type_5_limits_on_legislature),
                  type_6_official_disinfo_campaign = mean(type_6_official_disinfo_campaign),
                  type_7_restriction_of_media_freedom = mean(type_7_restriction_of_media_freedom))
ee_type_means

# get chart_data
current_country_name <- "Belarus"
data %>% filter(country_name == current_country_name) %>% 
        select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)")))
chart_data <- tibble(type_1_discriminatory_measures = c(3, 0),
                     type_2_derogation_of_nonderog_rights = c(3, 0),
                     type_3_abusive_enforcement = c(3, 0),
                     type_4_no_time_limit = c(3, 0),
                     type_5_limits_on_legislature = c(3, 0),
                     type_6_official_disinfo_campaign = c(3, 0),
                     type_7_restriction_of_media_freedom = c(3, 0)) %>%
        bind_rows(., ee_type_means) %>%
        bind_rows(., data %>% filter(country_name == current_country_name) %>% 
                          select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)")))) %>%
        rename("Discriminatory\nmeasures" = type_1_discriminatory_measures,
               "Derogation of\nnon-derogable rights" = type_2_derogation_of_nonderog_rights,
               "Abusive\nenforcement" = type_3_abusive_enforcement,
               "No time limit\non emergency measures" = type_4_no_time_limit,
               "Limits on\nlegislature" = type_5_limits_on_legislature,
               "Official\ndisinformation campaign" = type_6_official_disinfo_campaign,
               "Restrictions\nof media freedom" = type_7_restriction_of_media_freedom)
chart_data       


#//////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)

# set font as parameter; 1 is normal, 2 is bold, 3 is italic
op <- par(family = "Calibri", font = 1)

# create radar_chart
radarchart(df = chart_data, axistype = 1, 
           # point symbol
           pty = 32, 
           # color for outside of plotted rings
           pcol = c("#ff4d4d", "#2171B5"), 
           # color for inside of plotted rings
           pfcol = c(NA, "#2171B570"), 
           # width of plotted rings
           plwd = 4, 
           # line type for plotted rings
           plty = 1,
           # number of axis segments between center and outer rim
           seg = 3, 
           # line color for grid segments
           cglcol = "#999999", 
           # line type for grid segments
           cglty = 1, 
           # color of axis grid segment labels
           axislabcol = "#333333", 
           # values for axis grid segment labels
           caxislabels = c("None", "Minor", "Some", "Major"), 
           # line width for axis grid segments
           cglwd = 0.8, 
           # font magnification for outer labels
           vlcex = 1,
           # font magnification for center labels
           calcex = 1)
# Add a legend
legend(x = .6, y = 1.2, legend = c("E&E average", current_country_name), bty = "n", 
       pch = 20 , col = c("#ff4d4d", "#2171B5") , text.col = "#000000", cex = 1, pt.cex = 3)

# write emf
dev.off()

# add emf to word doc - will manually crop map in word doc 
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/pandem_type_belarus_radar_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


# create pandem_type_bih_radar_chart ####

# inspect
data %>% select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)"))) %>% skim()

# get ee_type_means
ee_type_means <- data %>% filter(ee_status == "ee_presence") %>% 
        summarize(type_1_discriminatory_measures = mean(type_1_discriminatory_measures),
                  type_2_derogation_of_nonderog_rights = mean(type_2_derogation_of_nonderog_rights),
                  type_3_abusive_enforcement = mean(type_3_abusive_enforcement),
                  type_4_no_time_limit = mean(type_4_no_time_limit),
                  type_5_limits_on_legislature = mean(type_5_limits_on_legislature),
                  type_6_official_disinfo_campaign = mean(type_6_official_disinfo_campaign),
                  type_7_restriction_of_media_freedom = mean(type_7_restriction_of_media_freedom))
ee_type_means

# get chart_data
current_country_name <- "BiH"
data %>% filter(country_name == current_country_name) %>% 
        select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)")))
chart_data <- tibble(type_1_discriminatory_measures = c(3, 0),
                     type_2_derogation_of_nonderog_rights = c(3, 0),
                     type_3_abusive_enforcement = c(3, 0),
                     type_4_no_time_limit = c(3, 0),
                     type_5_limits_on_legislature = c(3, 0),
                     type_6_official_disinfo_campaign = c(3, 0),
                     type_7_restriction_of_media_freedom = c(3, 0)) %>%
        bind_rows(., ee_type_means) %>%
        bind_rows(., data %>% filter(country_name == current_country_name) %>% 
                          select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)")))) %>%
        rename("Discriminatory\nmeasures" = type_1_discriminatory_measures,
               "Derogation of\nnon-derogable rights" = type_2_derogation_of_nonderog_rights,
               "Abusive\nenforcement" = type_3_abusive_enforcement,
               "No time limit\non emergency measures" = type_4_no_time_limit,
               "Limits on\nlegislature" = type_5_limits_on_legislature,
               "Official\ndisinformation campaign" = type_6_official_disinfo_campaign,
               "Restrictions\nof media freedom" = type_7_restriction_of_media_freedom)
chart_data       


#//////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)

# set font as parameter; 1 is normal, 2 is bold, 3 is italic
op <- par(family = "Calibri", font = 1)

# create radar_chart
radarchart(df = chart_data, axistype = 1, 
           # point symbol
           pty = 32, 
           # color for outside of plotted rings
           pcol = c("#ff4d4d", "#2171B5"), 
           # color for inside of plotted rings
           pfcol = c(NA, "#2171B570"), 
           # width of plotted rings
           plwd = 4, 
           # line type for plotted rings
           plty = 1,
           # number of axis segments between center and outer rim
           seg = 3, 
           # line color for grid segments
           cglcol = "#999999", 
           # line type for grid segments
           cglty = 1, 
           # color of axis grid segment labels
           axislabcol = "#333333", 
           # values for axis grid segment labels
           caxislabels = c("None", "Minor", "Some", "Major"), 
           # line width for axis grid segments
           cglwd = 0.8, 
           # font magnification for outer labels
           vlcex = 1,
           # font magnification for center labels
           calcex = 1)
# Add a legend
legend(x = .6, y = 1.2, legend = c("E&E average", current_country_name), bty = "n", 
       pch = 20 , col = c("#ff4d4d", "#2171B5") , text.col = "#000000", cex = 1, pt.cex = 3)

# write emf
dev.off()

# add emf to word doc - will manually crop map in word doc 
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/pandem_type_bih_radar_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


# create pandem_type_georgia_radar_chart ####

# inspect
data %>% select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)"))) %>% skim()

# get ee_type_means
ee_type_means <- data %>% filter(ee_status == "ee_presence") %>% 
        summarize(type_1_discriminatory_measures = mean(type_1_discriminatory_measures),
                  type_2_derogation_of_nonderog_rights = mean(type_2_derogation_of_nonderog_rights),
                  type_3_abusive_enforcement = mean(type_3_abusive_enforcement),
                  type_4_no_time_limit = mean(type_4_no_time_limit),
                  type_5_limits_on_legislature = mean(type_5_limits_on_legislature),
                  type_6_official_disinfo_campaign = mean(type_6_official_disinfo_campaign),
                  type_7_restriction_of_media_freedom = mean(type_7_restriction_of_media_freedom))
ee_type_means

# get chart_data
current_country_name <- "Georgia"
data %>% filter(country_name == current_country_name) %>% 
        select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)")))
chart_data <- tibble(type_1_discriminatory_measures = c(3, 0),
                     type_2_derogation_of_nonderog_rights = c(3, 0),
                     type_3_abusive_enforcement = c(3, 0),
                     type_4_no_time_limit = c(3, 0),
                     type_5_limits_on_legislature = c(3, 0),
                     type_6_official_disinfo_campaign = c(3, 0),
                     type_7_restriction_of_media_freedom = c(3, 0)) %>%
        bind_rows(., ee_type_means) %>%
        bind_rows(., data %>% filter(country_name == current_country_name) %>% 
                          select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)")))) %>%
        rename("Discriminatory\nmeasures" = type_1_discriminatory_measures,
               "Derogation of\nnon-derogable rights" = type_2_derogation_of_nonderog_rights,
               "Abusive\nenforcement" = type_3_abusive_enforcement,
               "No time limit\non emergency measures" = type_4_no_time_limit,
               "Limits on\nlegislature" = type_5_limits_on_legislature,
               "Official\ndisinformation campaign" = type_6_official_disinfo_campaign,
               "Restrictions\nof media freedom" = type_7_restriction_of_media_freedom)
chart_data       


#//////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)

# set font as parameter; 1 is normal, 2 is bold, 3 is italic
op <- par(family = "Calibri", font = 1)

# create radar_chart
radarchart(df = chart_data, axistype = 1, 
           # point symbol
           pty = 32, 
           # color for outside of plotted rings
           pcol = c("#ff4d4d", "#2171B5"), 
           # color for inside of plotted rings
           pfcol = c(NA, "#2171B570"), 
           # width of plotted rings
           plwd = 4, 
           # line type for plotted rings
           plty = 1,
           # number of axis segments between center and outer rim
           seg = 3, 
           # line color for grid segments
           cglcol = "#999999", 
           # line type for grid segments
           cglty = 1, 
           # color of axis grid segment labels
           axislabcol = "#333333", 
           # values for axis grid segment labels
           caxislabels = c("None", "Minor", "Some", "Major"), 
           # line width for axis grid segments
           cglwd = 0.8, 
           # font magnification for outer labels
           vlcex = 1,
           # font magnification for center labels
           calcex = 1)
# Add a legend
legend(x = .6, y = 1.2, legend = c("E&E average", current_country_name), bty = "n", 
       pch = 20 , col = c("#ff4d4d", "#2171B5") , text.col = "#000000", cex = 1, pt.cex = 3)

# write emf
dev.off()

# add emf to word doc - will manually crop map in word doc 
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/pandem_type_georgia_radar_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


# create pandem_type_moldova_radar_chart ####

# inspect
data %>% select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)"))) %>% skim()

# get ee_type_means
ee_type_means <- data %>% filter(ee_status == "ee_presence") %>% 
        summarize(type_1_discriminatory_measures = mean(type_1_discriminatory_measures),
                  type_2_derogation_of_nonderog_rights = mean(type_2_derogation_of_nonderog_rights),
                  type_3_abusive_enforcement = mean(type_3_abusive_enforcement),
                  type_4_no_time_limit = mean(type_4_no_time_limit),
                  type_5_limits_on_legislature = mean(type_5_limits_on_legislature),
                  type_6_official_disinfo_campaign = mean(type_6_official_disinfo_campaign),
                  type_7_restriction_of_media_freedom = mean(type_7_restriction_of_media_freedom))
ee_type_means

# get chart_data
current_country_name <- "Moldova"
data %>% filter(country_name == current_country_name) %>% 
        select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)")))
chart_data <- tibble(type_1_discriminatory_measures = c(3, 0),
                     type_2_derogation_of_nonderog_rights = c(3, 0),
                     type_3_abusive_enforcement = c(3, 0),
                     type_4_no_time_limit = c(3, 0),
                     type_5_limits_on_legislature = c(3, 0),
                     type_6_official_disinfo_campaign = c(3, 0),
                     type_7_restriction_of_media_freedom = c(3, 0)) %>%
        bind_rows(., ee_type_means) %>%
        bind_rows(., data %>% filter(country_name == current_country_name) %>% 
                          select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)")))) %>%
        rename("Discriminatory\nmeasures" = type_1_discriminatory_measures,
               "Derogation of\nnon-derogable rights" = type_2_derogation_of_nonderog_rights,
               "Abusive\nenforcement" = type_3_abusive_enforcement,
               "No time limit\non emergency measures" = type_4_no_time_limit,
               "Limits on\nlegislature" = type_5_limits_on_legislature,
               "Official\ndisinformation campaign" = type_6_official_disinfo_campaign,
               "Restrictions\nof media freedom" = type_7_restriction_of_media_freedom)
chart_data       


#//////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)

# set font as parameter; 1 is normal, 2 is bold, 3 is italic
op <- par(family = "Calibri", font = 1)

# create radar_chart
radarchart(df = chart_data, axistype = 1, 
           # point symbol
           pty = 32, 
           # color for outside of plotted rings
           pcol = c("#ff4d4d", "#2171B5"), 
           # color for inside of plotted rings
           pfcol = c(NA, "#2171B570"), 
           # width of plotted rings
           plwd = 4, 
           # line type for plotted rings
           plty = 1,
           # number of axis segments between center and outer rim
           seg = 3, 
           # line color for grid segments
           cglcol = "#999999", 
           # line type for grid segments
           cglty = 1, 
           # color of axis grid segment labels
           axislabcol = "#333333", 
           # values for axis grid segment labels
           caxislabels = c("None", "Minor", "Some", "Major"), 
           # line width for axis grid segments
           cglwd = 0.8, 
           # font magnification for outer labels
           vlcex = 1,
           # font magnification for center labels
           calcex = 1)
# Add a legend
legend(x = .6, y = 1.2, legend = c("E&E average", current_country_name), bty = "n", 
       pch = 20 , col = c("#ff4d4d", "#2171B5") , text.col = "#000000", cex = 1, pt.cex = 3)

# write emf
dev.off()

# add emf to word doc - will manually crop map in word doc 
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/pandem_type_moldova_radar_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


# create pandem_type_north_macedonia_radar_chart ####

# inspect
data %>% select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)"))) %>% skim()

# get ee_type_means
ee_type_means <- data %>% filter(ee_status == "ee_presence") %>% 
        summarize(type_1_discriminatory_measures = mean(type_1_discriminatory_measures),
                  type_2_derogation_of_nonderog_rights = mean(type_2_derogation_of_nonderog_rights),
                  type_3_abusive_enforcement = mean(type_3_abusive_enforcement),
                  type_4_no_time_limit = mean(type_4_no_time_limit),
                  type_5_limits_on_legislature = mean(type_5_limits_on_legislature),
                  type_6_official_disinfo_campaign = mean(type_6_official_disinfo_campaign),
                  type_7_restriction_of_media_freedom = mean(type_7_restriction_of_media_freedom))
ee_type_means

# get chart_data
current_country_name <- "N. Macedonia"
data %>% filter(country_name == current_country_name) %>% 
        select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)")))
chart_data <- tibble(type_1_discriminatory_measures = c(3, 0),
                     type_2_derogation_of_nonderog_rights = c(3, 0),
                     type_3_abusive_enforcement = c(3, 0),
                     type_4_no_time_limit = c(3, 0),
                     type_5_limits_on_legislature = c(3, 0),
                     type_6_official_disinfo_campaign = c(3, 0),
                     type_7_restriction_of_media_freedom = c(3, 0)) %>%
        bind_rows(., ee_type_means) %>%
        bind_rows(., data %>% filter(country_name == current_country_name) %>% 
                          select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)")))) %>%
        rename("Discriminatory\nmeasures" = type_1_discriminatory_measures,
               "Derogation of\nnon-derogable rights" = type_2_derogation_of_nonderog_rights,
               "Abusive\nenforcement" = type_3_abusive_enforcement,
               "No time limit\non emergency measures" = type_4_no_time_limit,
               "Limits on\nlegislature" = type_5_limits_on_legislature,
               "Official\ndisinformation campaign" = type_6_official_disinfo_campaign,
               "Restrictions\nof media freedom" = type_7_restriction_of_media_freedom)
chart_data       


#//////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)

# set font as parameter; 1 is normal, 2 is bold, 3 is italic
op <- par(family = "Calibri", font = 1)

# create radar_chart
radarchart(df = chart_data, axistype = 1, 
           # point symbol
           pty = 32, 
           # color for outside of plotted rings
           pcol = c("#ff4d4d", "#2171B5"), 
           # color for inside of plotted rings
           pfcol = c(NA, "#2171B570"), 
           # width of plotted rings
           plwd = 4, 
           # line type for plotted rings
           plty = 1,
           # number of axis segments between center and outer rim
           seg = 3, 
           # line color for grid segments
           cglcol = "#999999", 
           # line type for grid segments
           cglty = 1, 
           # color of axis grid segment labels
           axislabcol = "#333333", 
           # values for axis grid segment labels
           caxislabels = c("None", "Minor", "Some", "Major"), 
           # line width for axis grid segments
           cglwd = 0.8, 
           # font magnification for outer labels
           vlcex = 1,
           # font magnification for center labels
           calcex = 1)
# Add a legend
legend(x = .6, y = 1.2, legend = c("E&E average", current_country_name), bty = "n", 
       pch = 20 , col = c("#ff4d4d", "#2171B5") , text.col = "#000000", cex = 1, pt.cex = 3)

# write emf
dev.off()

# add emf to word doc - will manually crop map in word doc 
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/pandem_type_north_macedonia_radar_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


# create pandem_type_serbia_radar_chart ####

# inspect
data %>% select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)"))) %>% skim()

# get ee_type_means
ee_type_means <- data %>% filter(ee_status == "ee_presence") %>% 
        summarize(type_1_discriminatory_measures = mean(type_1_discriminatory_measures),
                  type_2_derogation_of_nonderog_rights = mean(type_2_derogation_of_nonderog_rights),
                  type_3_abusive_enforcement = mean(type_3_abusive_enforcement),
                  type_4_no_time_limit = mean(type_4_no_time_limit),
                  type_5_limits_on_legislature = mean(type_5_limits_on_legislature),
                  type_6_official_disinfo_campaign = mean(type_6_official_disinfo_campaign),
                  type_7_restriction_of_media_freedom = mean(type_7_restriction_of_media_freedom))
ee_type_means

# get chart_data
current_country_name <- "Serbia"
data %>% filter(country_name == current_country_name) %>% 
        select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)")))
chart_data <- tibble(type_1_discriminatory_measures = c(3, 0),
                     type_2_derogation_of_nonderog_rights = c(3, 0),
                     type_3_abusive_enforcement = c(3, 0),
                     type_4_no_time_limit = c(3, 0),
                     type_5_limits_on_legislature = c(3, 0),
                     type_6_official_disinfo_campaign = c(3, 0),
                     type_7_restriction_of_media_freedom = c(3, 0)) %>%
        bind_rows(., ee_type_means) %>%
        bind_rows(., data %>% filter(country_name == current_country_name) %>% 
                          select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)")))) %>%
        rename("Discriminatory\nmeasures" = type_1_discriminatory_measures,
               "Derogation of\nnon-derogable rights" = type_2_derogation_of_nonderog_rights,
               "Abusive\nenforcement" = type_3_abusive_enforcement,
               "No time limit\non emergency measures" = type_4_no_time_limit,
               "Limits on\nlegislature" = type_5_limits_on_legislature,
               "Official\ndisinformation campaign" = type_6_official_disinfo_campaign,
               "Restrictions\nof media freedom" = type_7_restriction_of_media_freedom)
chart_data       


#//////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)

# set font as parameter; 1 is normal, 2 is bold, 3 is italic
op <- par(family = "Calibri", font = 1)

# create radar_chart
radarchart(df = chart_data, axistype = 1, 
           # point symbol
           pty = 32, 
           # color for outside of plotted rings
           pcol = c("#ff4d4d", "#2171B5"), 
           # color for inside of plotted rings
           pfcol = c(NA, "#2171B570"), 
           # width of plotted rings
           plwd = 4, 
           # line type for plotted rings
           plty = 1,
           # number of axis segments between center and outer rim
           seg = 3, 
           # line color for grid segments
           cglcol = "#999999", 
           # line type for grid segments
           cglty = 1, 
           # color of axis grid segment labels
           axislabcol = "#333333", 
           # values for axis grid segment labels
           caxislabels = c("None", "Minor", "Some", "Major"), 
           # line width for axis grid segments
           cglwd = 0.8, 
           # font magnification for outer labels
           vlcex = 1,
           # font magnification for center labels
           calcex = 1)
# Add a legend
legend(x = .6, y = 1.2, legend = c("E&E average", current_country_name), bty = "n", 
       pch = 20 , col = c("#ff4d4d", "#2171B5") , text.col = "#000000", cex = 1, pt.cex = 3)

# write emf
dev.off()

# add emf to word doc - will manually crop map in word doc 
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/pandem_type_serbia_radar_chart.docx")



#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


# create pandem_type_ukraine_radar_chart ####

# inspect
data %>% select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)"))) %>% skim()

# get ee_type_means
ee_type_means <- data %>% filter(ee_status == "ee_presence") %>% 
        summarize(type_1_discriminatory_measures = mean(type_1_discriminatory_measures),
                  type_2_derogation_of_nonderog_rights = mean(type_2_derogation_of_nonderog_rights),
                  type_3_abusive_enforcement = mean(type_3_abusive_enforcement),
                  type_4_no_time_limit = mean(type_4_no_time_limit),
                  type_5_limits_on_legislature = mean(type_5_limits_on_legislature),
                  type_6_official_disinfo_campaign = mean(type_6_official_disinfo_campaign),
                  type_7_restriction_of_media_freedom = mean(type_7_restriction_of_media_freedom))
ee_type_means

# get chart_data
current_country_name <- "Ukraine"
data %>% filter(country_name == current_country_name) %>% 
        select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)")))
chart_data <- tibble(type_1_discriminatory_measures = c(3, 0),
                     type_2_derogation_of_nonderog_rights = c(3, 0),
                     type_3_abusive_enforcement = c(3, 0),
                     type_4_no_time_limit = c(3, 0),
                     type_5_limits_on_legislature = c(3, 0),
                     type_6_official_disinfo_campaign = c(3, 0),
                     type_7_restriction_of_media_freedom = c(3, 0)) %>%
        bind_rows(., ee_type_means) %>%
        bind_rows(., data %>% filter(country_name == current_country_name) %>% 
                          select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)")))) %>%
        rename("Discriminatory\nmeasures" = type_1_discriminatory_measures,
               "Derogation of\nnon-derogable rights" = type_2_derogation_of_nonderog_rights,
               "Abusive\nenforcement" = type_3_abusive_enforcement,
               "No time limit\non emergency measures" = type_4_no_time_limit,
               "Limits on\nlegislature" = type_5_limits_on_legislature,
               "Official\ndisinformation campaign" = type_6_official_disinfo_campaign,
               "Restrictions\nof media freedom" = type_7_restriction_of_media_freedom)
chart_data       


#//////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)

# set font as parameter; 1 is normal, 2 is bold, 3 is italic
op <- par(family = "Calibri", font = 1)

# create radar_chart
# use radarchart2 with custom vlabcol arg: https://stackoverflow.com/questions/54185029/change-labels-colors-in-r-radarchart
radarchart2(df = chart_data, axistype = 1, 
           # point symbol
           pty = 32, 
           # color for outside of plotted rings
           pcol = c("#ff4d4d", "#2171B5"), 
           # color for inside of plotted rings
           pfcol = c(NA, "#2171B570"), 
           # width of plotted rings
           plwd = 4, 
           # line type for plotted rings
           plty = 1,
           # number of axis segments between center and outer rim
           seg = 3, 
           # line color for grid segments
           cglcol = "#999999", 
           # line type for grid segments
           cglty = 1, 
           # color of axis grid segment labels
           # axislabcol = "#333333", 
           axislabcol = "#333333",
           # values for axis grid segment labels
           caxislabels = c("None", "Minor", "Some", "Major"), 
           # line width for axis grid segments
           cglwd = 0.8, 
           # font magnification for outer labels
           vlcex = 1,
           # font color for outer labels
           vlabcol = "#333333",
           # font face for outer labels
           vlab_fontface = 1,
           # font magnification for center labels
           calcex = 1)
# Add a legend
legend(x = .6, y = 1.2, legend = c("E&E average", current_country_name), bty = "n", 
       pch = 20 , col = c("#ff4d4d", "#2171B5") , text.col = "#333333", cex = 1, pt.cex = 3)

# write emf
dev.off()

# add emf to word doc - will manually crop map in word doc 
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/pandem_type_ukraine_radar_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


# create pandem_type_jitter_plot ####

# reshape to get chart_data
# note that no country has a value for type_2_derogation_of_nonderog_rights, 
# but can leave it in because geom_jitter can plot vars that lack variation, unlike geom_sina
data %>% select(str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)"))) %>% skim()
chart_data <- data %>% 
        select(country_name, iso_3_alpha, mcp_grouping, 
               str_subset(string = peek_vars(), pattern = regex("type_[0-9]_(?!sub)"))) %>%
        rename("Discriminatory\nmeasures" = type_1_discriminatory_measures,
               "Derogation of\nnon-derogable\nrights" = type_2_derogation_of_nonderog_rights,
               "Abusive\nenforcement" = type_3_abusive_enforcement,
               "No time limit\non emerg.\nmeasures" = type_4_no_time_limit,
               "Limits\non\nlegislature" = type_5_limits_on_legislature,
               "Official\ndisinfo.\ncampaign" = type_6_official_disinfo_campaign,
               "Restrictions\nof media\nfreedom" = type_7_restriction_of_media_freedom) %>%
        pivot_longer(cols = -c(country_name, iso_3_alpha, mcp_grouping), names_to = "var", values_to = "value") %>%
        mutate(value = case_when(value == 0 ~ "None",
                                 value == 1 ~ "Minor",
                                 value == 2 ~ "Some",
                                 value == 3 ~ "Major"))


#////////////////////////////


# inspect
# note that eu-15 might be better to drop, since the chart will show discrete sub_type violation categories
# and the EU only has a continuous average
chart_data
chart_data %>% nrow() # 189
chart_data %>% distinct(country_name) %>% nrow() # 27
chart_data %>% count(value)
chart_data %>% filter(is.na(value))
chart_data %>% filter(value > 0) %>% count(var, country_name) %>% print(n = nrow(.))


#///////////////////////////


# drop eu-15
chart_data <- chart_data %>% filter(country_name != "EU-15")
chart_data %>% nrow() # 182
chart_data %>% distinct(country_name) %>% nrow() # 26


#///////////////////////////


# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = mcp_grouping,
                              color = case_when(color_bin == "E&E Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                                color_bin == "E&E Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                                color_bin == "E&E graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                                color_bin == "CARs" ~ color_palette %>% slice(4) %>% pull(hex),
                                                color_bin == "Russia" ~ color_palette %>% slice(5) %>% pull(hex),
                                                color_bin == "EU-15" ~ color_palette %>% slice(6) %>% pull(hex),
                                                color_bin == "U.S." ~ color_palette %>% slice(7) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#/////////////////////


# create jitter_plot
# note: decided to omit labels because it looked too crowded
jitter_positions <- position_jitter(width = 0.2, height = .2, seed = 2)
pandem_type_jitter_plot <- chart_data %>% 
        mutate(iso_3_alpha = case_when(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia") &
                                               value %in% c("Some", "Major") ~ iso_3_alpha,
                                       TRUE ~ NA_character_)) %>%
        ggplot(data = ., aes(x = var, y = factor(value, levels = c("None", "Minor", "Some", "Major")), 
                             label = iso_3_alpha, size = 1,
                               color = factor(color_bin, levels = c("E&E Balkans", "E&E Eurasia", "E&E graduates", 
                                                                    "CARs", "Russia", "EU-15", "U.S.")))) +   
        # geom_jitter(size = 4, width = .2, height = .2) +
        geom_jitter(size = 4, position = jitter_positions) +
        geom_text_repel(position = jitter_positions, point.padding = .1, size = 3, fontface = "bold") +
        scale_size(guide = "none") +
        scale_color_manual(values = chart_data_color_list) +
        scale_x_discrete(expand = c(.1, 0)) +
        scale_y_discrete(expand = c(.1, 0)) +
        # scale_x_continuous(breaks = seq(from = 0, to = .8, by = .1), limits = c(-.05, .85), expand = c(-.05, 0)) +
        # scale_y_continuous(breaks = seq(from = 0, to = .6, by = .1), limits = c(-.05, .65), expand = c(-.05, 0)) +
        labs(x = "Pandemic Democratic Violations Index sub-indicators", y = "Type of violation",
             title = NULL,
             caption = NULL, color = "") +
        coord_fixed(ratio = 1/1.1, clip = "off") +
        # coord_flip() + 
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 0, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.y.left = element_line(color = "#595959"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = -5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#333333", 
                                            margin = margin(t = 0, r = 13, b = 0, l = 0)),
                plot.title = element_text(size = 18, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) + 
        guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333"))
        # coord_flip()

# inspect
pandem_type_jitter_plot


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(pandem_type_jitter_plot)
dev.off()

# add emf to word doc - will manually crop map in word doc 
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/pandem_type_jitter_plot.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////


# create pandem_panback_map ####

# setwd
setwd("C:/Users/Stephen/Desktop/R/sf")

# get world map as sf object
world_sf <- st_as_sf(getMap(resolution = "low"))
world_sf
world_sf %>% glimpse()
world_sf %>% class()
st_crs(world_sf)

# setwd
setwd("C:/Users/Stephen/Desktop/usaid/mcp/tso_portfolio_reviews/democracy_and_governance")


#///////////////////////////


# inspect country names
# convert to czech republic, macedonia, republic of serbia
data %>% anti_join(., world_sf, by = c("country_name" = "SOVEREIGNT"))
world_sf %>% filter(SOVEREIGNT == "Republic of Serbia")
data %>% 
        mutate(country_name = case_when(country_name == "Czechia" ~ "Czech Republic",
                                        country_name == "N. Macedonia" ~ "Macedonia",
                                        country_name == "BiH" ~ "Bosnia and Herzegovina",
                                        country_name == "Serbia" ~ "Republic of Serbia",
                                        TRUE ~ country_name)) %>%
        anti_join(., world_sf, by = c("country_name" = "SOVEREIGNT"))


#//////////////////////////


# join data of interest with world_sf
world_sf <- data %>% 
        mutate(country_name = case_when(country_name == "Czechia" ~ "Czech Republic",
                                        country_name == "N. Macedonia" ~ "Macedonia",
                                        country_name == "BiH" ~ "Bosnia and Herzegovina",
                                        country_name == "Serbia" ~ "Republic of Serbia",
                                        TRUE ~ country_name)) %>%
        left_join(world_sf, ., by = c("SOVEREIGNT" = "country_name"))

# inspect
world_sf %>% glimpse()


#////////////////////////


# create biscale_ee_sf which has NA pandem/panback values for non-ee presence/grad countries
biscale_ee_sf <- world_sf %>%
        mutate(pandem = case_when(!(ee_status %in% c("ee_presence", "ee_graduate")) ~ NA_real_,
                                  TRUE ~ pandem),
               panback = case_when(!(ee_status %in% c("ee_presence", "ee_graduate")) ~ NA_real_,
                                  TRUE ~ panback))


#///////////////////////


# inspect
inspect_biscale_ee_sf <- biscale_ee_sf
st_geometry(inspect_biscale_ee_sf) <- NULL
inspect_biscale_ee_sf %>% as_tibble() %>% filter(!(ee_status %in% c("ee_presence", "ee_graduate"))) %>% 
                                                         distinct(pandem, panback)
inspect_biscale_ee_sf %>% as_tibble() %>% filter(ee_status %in% c("ee_presence", "ee_graduate")) %>% 
        distinct(SOVEREIGNT, pandem, panback)


#///////////////////////


# add bi_class variable, which is just a string listing the x/y color palette pairing for each value
# note that the first number of bi_class is the class of the x variable, the second number is the class of the y variable
biscale_ee_sf <- biscale_ee_sf %>% bi_class(.data = ., x = pandem, y = panback, style = "jenks", dim = 2) %>%
        mutate(bi_class = case_when(bi_class == "NA-NA" ~ NA_character_, TRUE ~ bi_class))

# inspect
biscale_ee_sf
biscale_ee_sf %>% glimpse()
biscale_ee_sf %>% pull(bi_class) %>% tibble(bi_class = .) %>% distinct(bi_class)
inspect_biscale_ee_sf <- biscale_ee_sf
st_geometry(inspect_biscale_ee_sf) <- NULL
inspect_biscale_ee_sf %>% as_tibble() %>% count(pandem, panback, bi_class) %>% arrange(desc(n))
inspect_biscale_ee_sf %>% as_tibble() %>% count(bi_class)
?classIntervals
# https://en.wikipedia.org/wiki/Jenks_natural_breaks_optimization
# https://www.ehdp.com/methods/jenks-natural-breaks-explain.htm (note this seems to omit mention of last step from wikipedia)
classIntervals(var = inspect_biscale_ee_sf %>% as_tibble() %>% pull(pandem), n = 2, style = "jenks")
classIntervals(var = inspect_biscale_ee_sf %>% as_tibble() %>% pull(panback), n = 2, style = "jenks")


#/////////////////////////////////////


# get ee_bbox for test_ee_map
ee_bbox <- unname(st_bbox(c(xmin = 11, xmax = 55, ymax = 57, ymin = 36), crs = st_crs(4326)))
ee_bbox

test_map_ee <- get_map(location = ee_bbox, source = "stamen", maptype = "terrain", zoom = 7, language = "en-EN")
ggmap(test_map_ee)


# create ggmap_bbox() to convert the bbox to be in EPSG:3857
ggmap_bbox <- function(map) {
        if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
        # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
        # and set the names to what sf::st_bbox expects:
        map_bbox <- setNames(unlist(attr(map, "bb")), 
                             c("ymin", "xmin", "ymax", "xmax"))
        
        # Coonvert the bbox to an sf polygon, transform it to 3857, 
        # and convert back to a bbox (convoluted, but it works)
        bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
        
        # Overwrite the bbox of the ggmap object with the transformed coordinates 
        attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
        attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
        attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
        attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
        map
}

# update ggmap bbox
test_map_ee <- ggmap_bbox(test_map_ee)


#/////////////////////////////////////


# add fill_color_bin and fill_color
biscale_ee_sf <- biscale_ee_sf %>% 
        mutate(fill_color_bin = case_when(is.na(n) ~ "0_to_1",
                                          n > 0 & n < 50 ~ "1_to_50",
                                          n >= 50 & n < 150 ~ "50_to_150",
                                          n >= 150 & n < 1500 ~ "150_to_1500",
                                          n >= 1500 & n < 5000 ~ "1500_to_5000",
                                          n >= 5000 ~ "5000_to_130000"),
               fill_color = case_when(fill_color_bin == "0_to_1" ~ "#ffffff",
                                      fill_color_bin == "1_to_50" ~ color_palette %>% slice(5) %>% pull(hex),
                                      fill_color_bin == "50_to_150" ~ color_palette %>% slice(4) %>% pull(hex),
                                      fill_color_bin == "150_to_1500" ~ color_palette %>% slice(3) %>% pull(hex),
                                      fill_color_bin == "1500_to_5000" ~ color_palette %>% slice(2) %>% pull(hex),
                                      fill_color_bin == "5000_to_130000" ~ color_palette %>% slice(1) %>% pull(hex)))

# inspect
biscale_ee_sf
biscale_ee_sf %>% distinct(SOVEREIGNT, fill_color_bin, fill_color) %>% count(fill_color_bin, fill_color)
biscale_ee_sf %>% distinct(SOVEREIGNT, n) %>% filter(is.na(n))        



#/////////////////////////////////////


# convert crs to 3857
biscale_ee_sf_3857 <- st_transform(biscale_ee_sf, 3857)
st_crs(biscale_ee_sf_3857)

# create map
biscale_output_map <- ggmap(test_map_ee) + 
        coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
        geom_sf(data = biscale_ee_sf_3857, inherit.aes = FALSE, mapping = aes(fill = bi_class), 
                color = "#ffffff", size = .5) +
        bi_scale_fill(pal = "DkBlue", dim = 2, na.translate = FALSE) +
        # scale_alpha_discrete(range = c(.5, 1.5)) +
        theme_bw() +
        theme(
                # plot.margin = unit(c(0, 10, 0, 0), "mm"),
                panel.grid.major = element_line(color = "transparent"),
                plot.background = element_blank(), 
                panel.grid.minor = element_blank(), panel.border = element_blank(),
                axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
                axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
                # plot.title = element_text(size = 6, face = "bold", hjust = 0, family = "Trebuchet MS"), 
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                legend.position = "none",
                legend.key.size = unit(2, "mm"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")), 
                legend.title = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
                legend.justification = "center",
                panel.grid = element_blank(),
                line = element_blank(),
                rect = element_blank(),
                text = element_blank()) +
        guides(fill = guide_legend(reverse = FALSE, 
                                   # title = "Fake data\nvalues", 
                                   title.hjust = .5,
                                   keywidth = 2,
                                   nrow = 2, byrow = TRUE))

# inspect
biscale_output_map

# create legend
legend <- bi_legend(pal = "DkBlue", dim = 2, 
                    xlab = "Higher\nviolations ", ylab = "Higher\nbacksliding risk ", size = 8)

# add legend
final_biscale_output_map <- ggdraw() +
        draw_plot(biscale_output_map, x = 0, y = 0, width = .8, height = .8) +
        draw_plot(legend, x = 0.8, y = .3, width = 0.2, height = 0.2)

# inspect
final_biscale_output_map


#///////////////////


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(final_biscale_output_map)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
# note when saving from word to pdf, you'll need to print -> as pdf, 
# instead of save_as -> pdf, since save_as has issues rendering emfs
read_docx() %>% 
        body_add_img(src = filename, width = 7, height = 7) %>% 
        print(target = "output/map/ee_biscale_output_map.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

























