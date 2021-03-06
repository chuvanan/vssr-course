
library(tidyr)
library(dplyr)
library(ggplot2) # ggplot2 v3.0.0 is required

# install.packages("devtools")
library(patchwork) # for arrange multiple plots

read_csv <- readr::read_csv


# Import data -------------------------------------------------------------

df_base <- read_csv("Day 1/Dataset references/MonthlyRainfall_mm_ByProvince.csv")

head(df_base)
dim(df_base)

df_clean <- na.omit(df_base) # remove rows with missing values

describe <- function(df, col) {
    summary(df[[col]])
}

describe(df_clean, "May")
describe(df_clean, "Aug.")

summary(df_clean) # summarise all columns at once


# -------------------------------------------------------------------------

hist_rain <- function(df, col1, col2, num_bins = 30) {

    # unquote variable
    col1_q <- enquo(col1)
    col2_q <- enquo(col2)
    col1_c <- deparse(substitute(col1))
    col2_c <- deparse(substitute(col2))

    p1 <- ggplot(df, aes(!!col1_q)) +
        geom_histogram(color = "white",
                       fill = "orange",
                       bins = num_bins,
                       alpha = 0.7) +
        geom_vline(xintercept = mean(df[[col1_c]], na.rm = TRUE),
                   linetype = "dashed") +
        theme_minimal()

    p2 <- ggplot(df, aes(!!col2_q)) +
        geom_histogram(color = "white",
                       fill = "orange",
                       bins = num_bins,
                       alpha = 0.7) +
        geom_vline(xintercept = mean(df[[col2_c]], na.rm = TRUE),
                   linetype = "dashed") +
        theme_minimal()

    p1 + p2 + plot_layout(ncol = 1)
}

hist_rain(df_clean, Aug., May)
hist_rain(df_clean, Aug., July, num_bins = 40)
hist_rain(df_clean, Aug., July, num_bins = 50)

# -------------------------------------------------------------------------

bar_histogram <- function(df, col, title, num_bins = 8) {
    col_q <- enquo(col)
    ggplot(df, aes(!!col_q)) +
        geom_histogram(color = "white", fill = "orange",
                       alpha = 0.7, bins = num_bins) +
        labs(title = title, y = "Province Count") +
        theme_minimal()
}

years <- filter(df_clean, Year >= 2012)
bar_histogram(years, Aug.,
              title = "Rainfall in August across all provinces since 2012",
              num_bins = 8)

# -------------------------------------------------------------------------

count_heavy_rain_days <- function(df, from = 2013) {

    out <- df %>%
        filter(Year >= from) %>%
        gather(key = "Month", value = "Rainfall", -Year, -Province) %>%
        filter(Rainfall > 200) %>%
        group_by(Month) %>%
        summarise(N_Province = n())

    month_levels <- c("May", "June", "July", "Aug.", "Sep.", "Oct.", "Nov.")
    out$Month <- factor(out$Month, levels = month_levels)

    out
}

count_heavy_rain_days(df_clean, 2013)

plot_heavy_rain_days <- function(df, from) {

    dta <- count_heavy_rain_days(df, from = from)

    ggplot(dta, aes(Month, N_Province)) +
        geom_col(fill = "steelblue") +
        labs(x = NULL, y = "Number of provinces",
             title = paste0("Count Provinces with Monthly Rainfall > 200mm by Since ", from)) +
        theme_minimal()
}

plot_heavy_rain_days(df_clean, 2008)

# -------------------------------------------------------------------------

box_plot <- function(df, month, from) {
    month_q <- enquo(month)
    from_q <- enquo(from)
    dta <- filter(df, Year >= !!from)

    ggplot(dta, aes(factor(Year), !!month_q)) +
        geom_boxplot() +
        labs(x = NULL, y = "Rainfall (mm)") +
        ggtitle("Comparision of Rainfall across Year") +
        theme_minimal()
}

box_plot(df_clean, Aug., from = 2010)

# -------------------------------------------------------------------------

scatter_plot <- function(df, col1, col2, title) {
    col1_q <- enquo(col1)
    col2_q <- enquo(col2)
    ggplot(df, aes(!!col1_q, !!col2_q)) +
        geom_point(color = "white", shape = 21,
                   fill = "steelblue", size = 3) +
        theme_minimal() +
        labs(title = title)
}

scatter_plot(df_clean, May, June, "Comparison of Rainfall in May and June")
scatter_plot(df_clean, May, July, "Comparison of Rainfall in May and July")
scatter_plot(df_clean, May, Aug., "Comparison of Rainfall in May and August")
scatter_plot(df_clean, May, Sep., "Comparison of Rainfall in May and September")














