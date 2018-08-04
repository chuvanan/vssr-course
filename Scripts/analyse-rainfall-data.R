
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)
library(forcats)

read_csv <- readr::read_csv
str_to_title <- stringr::str_to_title

dta <- read_csv("Day 1/Dataset references/MonthlyRainfall_mm_ByProvince.csv")
names(dta) <- c("year", "province", "may", "jun", "jul", "aug", "sep", "oct", "nov")

dta <- dta %>%
    mutate(region = case_when(
        province %in% c("Ha Noi", "Vinh Phuc", "Bac Ninh", "Quang Ninh", "Bai Chay",
                        "Hai Duong", "Hai Phong", "Hung Yen", "Thai Binh",
                        "Ha Nam", "Nam Dinh", "Ninh Binh") ~ "Red River Delta",
        province %in% c("Ha Giang", "Cao Bang", "Bac Kan", "Tuyen Quang", "Lao Cai",
                        "Yen Bai", "Thai Nguyen", "Lang Son", "Bac Giang", "Phu Tho",
                        "Dien Bien", "Lai Chau", "Son La", "Hoa Binh") ~ "Northern midlands and mountain areas",
        province %in% c("Thanh Hoa", "Nghe An", "Ha Tinh", "Quang Binh", "Vinh",
                        "Quang Tri", "Thua Thien-Hue", "Da Nang", "Quang  Nam", "Hue",
                        "Quang  Ngai", "Binh Dinh", "Phu Yen", "Khanh  Hoa", "Qui Nhon", "Nha Trang",
                        "Ninh  Thuan", "Binh Thuan") ~ "Northern Central area and Central coastal area",
        province %in% c("Kon Tum", "Gia Lai", "Dak Lak", "Playku",
                        "Dak Nong", "Lam Dong", "Da Lat") ~ "Central Highlands",
        province %in% c("Binh Phuoc", "Tay Ninh", "Binh Duong", "Dong Nai",
                        "Ba Ria - Vung Tau", "Ho Chi Minh city", "Vung Tau") ~ "South East",
        province %in% c("Long An", "Tien Giang", "Ben Tre", "Tra Vinh", "Vinh Long",
                        "Dong Thap", "An Giang", "Kien  Giang", "Can Tho", "Hau Giang",
                        "Soc Trang", "Bac Lieu", "Ca Mau") ~ "Mekong River Delta"
    ))

region_list <- c("Northern midlands and mountain areas", "Red River Delta",
                 "Northern Central area and Central coastal area", "Central Highlands",
                 "South East", "Mekong River Delta")

tidy_dta <- dta %>%
    gather(key = "month", value = "rainfall", -year, -province, -region) %>%
    mutate(month = str_to_title(month)) %>%
    mutate(month_fct = factor(month, levels = month.abb),
           month_num = as.numeric(month_fct)) %>%
    mutate(region = factor(region, levels = region_list))


tidy_dta <- tidy_dta %>%
    group_by(region, month) %>%
    mutate(max_rainfall = max(rainfall, na.rm = TRUE),
           min_rainfall = min(rainfall, na.rm = TRUE),
           mean_rainfall = mean(rainfall), na.rm = TRUE) %>%
    ungroup()

province_list <- c("Lai Chau", "Son La", "Tuyen Quang",
                   "Bai Chay", "Ha Noi", "Nam Dinh",
                   "Vinh", "Hue", "Da Nang", "Qui Nhon", "Nha Trang",
                   "Playku", "Da Lat", "Vung Tau", "Ca Mau")

tidy_dta <- tidy_dta %>%
    mutate(province = factor(province, levels = province_list))

# ggplot(tidy_dta) +
#     geom_ribbon(aes(month_num,
#                     ymin = min_rainfall, ymax = max_rainfall),
#                 fill = "steelblue", alpha = 0.3) +
#     geom_line(aes(month_num, max_rainfall), color = "steelblue") +
#     geom_line(aes(month_num, min_rainfall), color = "steelblue") +
#     geom_line(aes(month_num, mean_rainfall), color = "red4", size = 0.9) +
#     facet_wrap(~ region) +
#     scale_x_continuous(breaks = 5:11,
#                        labels = c("May", "Jun", "Jul", "Aug",
#                                   "Sep", "Oct", "Nov")) +
#     labs(x = NULL, y = "Rainfall (mm)") +
#     theme_minimal(base_family = "Roboto Slab", base_size = 12) +
#     theme(strip.text = element_text(face = "bold", size = 10),
#           panel.grid.minor.y = element_blank(),
#           panel.grid.minor.x = element_blank())


my_theme <- function() {
    theme(panel.grid.major.y = element_blank(),
          axis.ticks = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", hjust = 0, family = "Carlito", size = 14),
          axis.text = element_text(size = 12, color = "gray30"),
          axis.title.x = element_text(hjust = 0.5, color = "gray20"),
          plot.caption = element_text(color = "gray30"),
          plot.title = element_text(size = 24, vjust = 8),
          plot.subtitle = element_text(size = 14, color = "gray30", vjust = 6),
          plot.background = element_rect(fill = "gray97", color = "gray97"),
          panel.spacing.y = unit(1, "lines"),
          panel.spacing.x = unit(0, "lines"),
          plot.margin = unit(c(1.5, 0.5, 0.5, 0.5), "cm"),
          legend.position = "top",
          legend.title = element_blank(),
          legend.direction = "horizontal")
}

ggplot(tidy_dta, aes(rainfall, fct_rev(month_fct))) +
    geom_density_ridges(aes(fill = region), color = "gray80") +
    scale_fill_cyclical(values = c("#afa83a", "#7f63b8", "#56ae6c",
                                   "#b84c7d", "#ac873f", "#ba4e3d")) +
    labs(x = "Rainfall (mm)", y = NULL,
         caption = "Data Source: GSO",
         title = "Comparision of Rainfall Across Regions in Vietnam",
         subtitle = "The data show the density distribution of rainfall in selected provinces from 2006 to 2016") +
    theme_ridges(font_family = "Carlito") +
    facet_wrap(~ province, nrow = 3) +
    my_theme() +
    guides(fill = guide_legend(nrow = 1))

