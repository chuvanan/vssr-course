
library(tidyr)
library(dplyr)
library(ggplot2)

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


ggplot(tidy_dta) +
    geom_ribbon(aes(month_num,
                    ymin = min_rainfall, ymax = max_rainfall),
                fill = "steelblue", alpha = 0.3) +
    geom_line(aes(month_num, max_rainfall), color = "steelblue") +
    geom_line(aes(month_num, min_rainfall), color = "steelblue") +
    geom_line(aes(month_num, mean_rainfall),
              color = "red4", size = 0.9) +
    facet_wrap(~ region) +
    scale_x_continuous(breaks = 5:11,
                       labels = c("May", "Jun", "Jul", "Aug",
                                  "Sep", "Oct", "Nov")) +
    labs(x = NULL, y = "Rainfall (mm)") +
    theme_minimal(base_family = "Roboto") +
    theme(strip.text = element_text(face = "bold", size = 10),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank())






