
library(tidyr)
library(ggplot2)

read_csv <- readr::read_csv

dta <- read_csv("Day 1/CleaningExercise.csv")

names(dta) <- c("Year", "Province", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")

dta$May <- gsub("\"", "", dta$May)
dta$May <- as.numeric(dta$May)

dta$Province[dta$Province == "DaLat"] <- "Da Lat"
dta$Province[dta$Province == "Huee"] <- "Hue"

dta$Nov[dta$Nov == "low"] <- NA
dta$Nov <- as.numeric(dta$Nov)
dta$Nov <- abs(dta$Nov)

for (i in 1:nrow(dta)) {
    if (is.na(dta$Year[i])) {
        dta$Year[i] <- dta$Year[i - 1]
    }
}

tidy_dta <- dta %>%
    gather(key = "Month", value = "Rainfall", -Year, -Province)

head(tidy_dta)

tidy_dta %>%
    spread(Year, Rainfall) %>%
    ggplot(aes(`2005`, `2006`)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ Province, scales = "free")

ggplot(tidy_dta, aes(Month, Rainfall, fill = factor(Year))) +
    geom_col(position = "stack") +
    facet_wrap(~ Province)


