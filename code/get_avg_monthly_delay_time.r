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
    average_month = round(mean(delay_time), 0),
  ) %>%
  mutate(
    month_char = month.abb[month],
    my = factor(month_char, levels = unique(month_char)),
    monyr = ym(paste(year, month, sep = " ")),
    School_Year = ifelse(month >= 9, paste(year, year + 1, sep = "-"), paste(year - 1, year, sep = "-"))
  )

# Filter relevant columns and pivot data to wide format using pivot_wider
plotdata <- delays_monyr %>%
  select(month_char, School_Year, average_month) %>%
  subset(select = -c(year) ) %>%
  pivot_wider(
    names_from = School_Year,
    values_from = average_month,
    values_fill = list(average_month = NULL)  # Fill missing values with 0
  )

# Ensure that 'month_char' is treated as a factor with levels in the specified order
plotdata$month_char <- factor(plotdata$month_char, levels = c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun"))

# Filter out any rows where 'month_char' is not one of the expected values
plotdata <- plotdata %>%
  filter(!is.na(month_char))

# Generate the Highcharter plot
h <- highchart() %>%
  hc_xAxis(categories = plotdata$month_char, title = list(text = "School Year Calendar Months")) %>%
  hc_yAxis(title = list(text = "Average Delay Time (minutes)")) 

current_date <- Sys.Date()
current_time <- format(Sys.time(), "%H:%M:%S")

# Add series for each school year
for (sy in colnames(plotdata)[-1]) {
  h <- h %>% hc_add_series(name = sy, data = as.list(plotdata[[sy]]))
}

h <- h %>% hc_title(text = "Average Monthly Delay Times") %>%
  hc_caption(text = paste("Updated", current_date, "at", current_time))

h <- h %>%
  hc_exporting(
    enabled = TRUE,
    buttons = list(
      contextButton = list(
        menuItems = list(
          list(
            text = 'Download PNG',
            onclick = JS("function () { this.exportChart(); }")  # Default function to download PNG
          ),
          list(
            text = 'Download CSV',
            onclick = JS("function () { this.downloadCSV(); }")
          )
        )
      )
    )
  )

saveWidget(h, '../visuals/avg_monthly_delay_times1.html', selfcontained = TRUE)