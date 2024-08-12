library(dplyr)
library(highcharter)
library(tidyr)
library(htmlwidgets)
library(lubridate)
library(reticulate)
library(pandoc)


pd <- import("pandas")
x <- pd$read_feather("../data/output/filtered_weekends_vacation_covid_delays.feather")

# read in enrollment nums

delays_monyr <- x %>%
  mutate(
    day = day(occurred_on),
    month = month(occurred_on),
    year = year(occurred_on)
  ) %>%
  filter(occurred_on >= as.Date("2017-09-01")) %>%  # Filter dates before 2017 - 2018 SY
  group_by(year, month) %>%
  summarize(
    count = n(),
    average_month = mean(delay_time),
  ) %>%
  mutate(
    month_char = month.abb[month],
    my = factor(month_char, levels = unique(month_char)),
    monyr = ym(paste(year, month, sep = " ")),
    School_Year = ifelse(month >= 9, paste(year, year + 1, sep = "-"), paste(year - 1, year, sep = "-"))
  )

# Filter relevant columns and pivot data to wide format using pivot_wider
plotdata <- delays_monyr %>%
  select(month_char, School_Year, count) %>%
  subset(select = -c(year) ) %>%
  pivot_wider(
    names_from = School_Year,
    values_from = count,
    values_fill = list(count = NULL)  # Fill missing values with 0
  )

# Ensure that 'month_char' is treated as a factor with levels in the specified order
plotdata$month_char <- factor(plotdata$month_char, levels = c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun"))

# Filter out any rows where 'month_char' is not one of the expected values
plotdata <- plotdata %>%
  filter(!is.na(month_char))

# Generate the Highcharter plot
h <- highchart() %>%
  hc_xAxis(categories = plotdata$month_char)

# Add series for each school year
for (sy in colnames(plotdata)[-1]) {
  h <- h %>% hc_add_series(name = sy, data = as.list(plotdata[[sy]]))
}

saveWidget(h, '../visuals/num_monthly_delays1.html', selfcontained = TRUE)