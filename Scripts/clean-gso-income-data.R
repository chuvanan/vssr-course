
library(tidyr)
library(dplyr)
read_csv2 <- readr::read_csv2
write_csv <- readr::write_csv

dta <- read_csv2("Day 1/Dataset references/HighLowIncomeQuintile2010-2016.csv",
                 skip = 2)

names(dta) <- c("province",
                "lowest_income_quintile_2010", "highest_income_quintile_2010", "income_ratio_2010",
                "lowest_income_quintile_2012", "highest_income_quintile_2012", "income_ratio_2012",
                "lowest_income_quintile_2014", "highest_income_quintile_2014", "income_ratio_2014",
                "lowest_income_quintile_2016", "highest_income_quintile_2016", "income_ratio_2016")

region_list <- c("Red River Delta", "Northern midlands and mountain areas",
                 "Northern Central area and Central coastal area", "Central Highlands",
                 "South East", "Mekong River Delta")

regions <- filter(dta, province %in% region_list) %>%
    rename(region = province)

provinces <- filter(dta, !province %in% region_list)

# tidy up  ----------------------------------------------------------------

provinces <- provinces %>%
    mutate(region = case_when(
        province %in% c("Ha Noi", "Vinh Phuc", "Bac Ninh", "Quang Ninh",
                        "Hai Duong", "Hai Phong", "Hung Yen", "Thai Binh",
                        "Ha Nam", "Nam Dinh", "Ninh Binh") ~ "Red River Delta",
        province %in% c("Ha Giang", "Cao Bang", "Bac Kan", "Tuyen Quang", "Lao Cai",
                        "Yen Bai", "Thai Nguyen", "Lang Son", "Bac Giang", "Phu Tho",
                        "Dien Bien", "Lai Chau", "Son La", "Hoa Binh") ~ "Northern midlands and mountain areas",
        province %in% c("Thanh Hoa", "Nghe An", "Ha Tinh", "Quang Binh",
                        "Quang Tri", "Thua Thien-Hue", "Da Nang", "Quang  Nam",
                        "Quang  Ngai", "Binh Dinh", "Phu Yen", "Khanh  Hoa",
                        "Ninh  Thuan", "Binh Thuan") ~ "Northern Central area and Central coastal area",
        province %in% c("Kon Tum", "Gia Lai", "Dak Lak",
                        "Dak Nong", "Lam Dong") ~ "Central Highlands",
        province %in% c("Binh Phuoc", "Tay Ninh", "Binh Duong", "Dong Nai",
                        "Ba Ria - Vung Tau", "Ho Chi Minh city") ~ "South East",
        province %in% c("Long An", "Tien Giang", "Ben Tre", "Tra Vinh", "Vinh Long",
                        "Dong Thap", "An Giang", "Kien  Giang", "Can Tho", "Hau Giang",
                        "Soc Trang", "Bac Lieu", "Ca Mau") ~ "Mekong River Delta"
    )) %>%
    select(region, province, everything())

write_csv(provinces, "Day 1/province_quintiles.csv")
write_csv(regions, "Day 1/region_quintiles.csv")