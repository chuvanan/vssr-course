
library(tidyr)
library(dplyr)
library(ggplot2)
library(gghighlight)
read_csv <- readr::read_csv
write_csv <- readr::write_csv
str_extract_all <- stringr::str_extract_all

province_dta <- read_csv("Day 1/province_quintiles.csv")
region_dta <- read_csv("Day 1/region_quintiles.csv")

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

## write_csv(province_income_ratio, "Day 1/province_income_ratio.csv")
## write_csv(region_income_ratio, "Day 1/region_income_ratio.csv")
## write_csv(province_income_quintile, "Day 1/province_income_quintile.csv")
## write_csv(region_income_quintile, "Day 1/region_income_quintile.csv")


## -----------------------------------------------------------------------------
## EDA

ggplot(province_income_ratio, aes(ratio)) +
    geom_histogram(color = "white", bins = 10)
## => 50-100 times difference

ggplot(province_income_ratio, aes(ratio)) +
    geom_histogram(color = "white", bins = 10) +
    facet_wrap( ~ region)
## => not any clear pattern

ggplot(province_income_ratio,
       aes(year, ratio, group = province)) +
    geom_line() +
    facet_wrap( ~ region)
## => not all province increased inequality

ggplot(region_income_ratio,
       aes(year, ratio, group = region, color = region)) +
    geom_line()
## => exception: South East? Why?


## Did all provinces have increasing income?

ggplot(region_income_quintile,
       aes(year, income, color = region)) +
    geom_line() +
    geom_point() +
    facet_wrap( ~ quintile)
