

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

