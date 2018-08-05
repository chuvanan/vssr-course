
library(tidyr)
library(dplyr)
read_csv2 <- readr::read_csv2
write_csv <- readr::write_csv
str_extract_all <- stringr::str_extract_all

dta <- read_csv2("Day 1/Dataset references/HighLowIncomeQuintile2010-2016.csv",
                 skip = 2)

names(dta) <- c("province",
                "lowest_income_quintile_2010", "highest_income_quintile_2010", "income_ratio_2010",
                "lowest_income_quintile_2012", "highest_income_quintile_2012", "income_ratio_2012",
                "lowest_income_quintile_2014", "highest_income_quintile_2014", "income_ratio_2014",
                "lowest_income_quintile_2016", "highest_income_quintile_2016", "income_ratio_2016")

# for unknown reason, read_csv2() failed to parse period-separator in the data
dta$income_ratio_2010 <- dta$income_ratio_2010 / 10
dta$income_ratio_2012 <- dta$income_ratio_2012 /10
dta$income_ratio_2014 <- dta$income_ratio_2014 / 10
dta$income_ratio_2016 <- dta$income_ratio_2016 / 10

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


# tidy up -----------------------------------------------------------------

province_dta <- provinces
region_dta <- regions

province_income_ratio <- province_dta %>%
    select(region, province, contains("ratio")) %>%
    gather(key = "year", value = "ratio", -region, -province) %>%
    mutate(year = as.numeric(gsub("income_ratio_", "", year)))

region_income_ratio <- region_dta %>%
    select(region, contains("ratio")) %>%
    gather(key = "year", value = "ratio", -region) %>%
    mutate(year = as.numeric(gsub("income_ratio_", "", year)))

province_income_quintile <- province_dta %>%
    select(region, province, contains("quintile")) %>%
    gather(key = "year", value = "income", -region, -province) %>%
    mutate(quintile = unlist(str_extract_all(year, ".*quintile"))) %>%
    mutate(quintile = case_when(
               grepl("lowest", quintile) ~ "lowest",
               grepl("highest", quintile) ~ "highest"
           )) %>%
    mutate(year = as.numeric(gsub(".*_income_quintile_", "", year))) %>%
    select(region, province, quintile, year, income)

region_income_quintile <- region_dta %>%
    select(region, contains("quintile")) %>%
    gather(key = "year", value = "income", -region) %>%
    mutate(quintile = unlist(str_extract_all(year, ".*quintile"))) %>%
    mutate(quintile = case_when(
               grepl("lowest", quintile) ~ "lowest",
               grepl("highest", quintile) ~ "highest"
           )) %>%
    mutate(year = as.numeric(gsub(".*_income_quintile_", "", year))) %>%
    select(region, quintile, year, income)

write_csv(province_income_ratio, "Day 1/province_income_ratio.csv")
write_csv(region_income_ratio, "Day 1/region_income_ratio.csv")
write_csv(province_income_quintile, "Day 1/province_income_quintile.csv")
write_csv(region_income_quintile, "Day 1/region_income_quintile.csv")
