# read in filtered, cleaned school bus delay data -----

library(tidyverse)
library(ggplot2)
library(ggiraph)
library(councilverse)

setwd("~/Desktop/schoolBusDelays2022-main/code")
x <-  read_csv("../data/output/filtered_weekends_vacation_covid_delays.csv")
# read in enrollment nums
enroll <- read_csv("../data/input/raw/enrollment_nums.csv") %>% 
  mutate(year = as.numeric(year))

# 01 Total Delays per Month ----

# data prep 
delays_monyr <- x %>% 
  mutate(day=day(occurred_on), 
         month=month(occurred_on), 
         year=year(occurred_on)) %>% 
  group_by(year, month) %>% 
  summarize(count=n(), 
            average_month=mean(delay_time)) %>% 
  mutate(month_char=month.abb[month], 
         my=factor(month_char, levels= unique(month_char)),
         monyr=ym(paste(year,month, sep=" ")),
         School_Year = case_when(monyr >='2017-09-01' & 
                                   monyr<='2018-06-01' ~ '2017-2018',
                                 monyr >='2018-09-01' & 
                                   monyr<='2019-06-01' ~ '2018-2019',
                                 monyr >='2019-09-01' & 
                                   monyr<='2020-06-01' ~ '2019-2020',
                                 monyr >='2020-09-01' & 
                                   monyr<='2021-06-01' ~ '2020-2021',
                                 monyr >='2021-09-01' & 
                                   monyr<='2022-06-01' ~ '2021-2022',
                                 monyr >='2022-09-01' & 
                                   monyr<='2023-06-01' ~ '2022-2023',
                                 monyr >='2023-09-01' & 
                                   monyr<='2024-06-01' ~ '2023-2024'
                                ))
# filtering out N/A values
delays_monyr = delays_monyr %>%
  filter(!is.na(School_Year))

# plot

plot <-   
  ggplot(data = delays_monyr, 
         aes(x = my,  y = count)) +
  geom_line(aes(color=School_Year, group=School_Year), alpha=1) +
  geom_point_interactive(size=1.5, alpha=0.85,
                         aes(color=School_Year, group=School_Year),
                         tooltip = paste(delays_monyr$month_char,
                                         delays_monyr$year, " :", 
                                scales::comma(delays_monyr$count),
                                         " delays")) + 
  scale_color_nycc(palette = "main", reverse = T) +
  # geom_smooth(aes(group=1), se=F, size=0.75,
  #                         alpha =.5, color="#23417D") +
  scale_y_continuous(breaks = seq(0,
                                  max(delays_monyr$count),
                                  1000),
                     labels = scales::comma(seq(0,
                                  max(delays_monyr$count),
                                  1000))) +
  labs(title="Number of Delays Over Time", 
       x="School Year Calendar Months",  
       y="Number of Delays", color="SY") +
  theme_nycc() 

tooltip_css <- "background-color:#CACACA;"

plot_interactive <- girafe(ggobj = plot,   
                           width_svg = 9,
                           height_svg = 5, 
                           options = list(
                             opts_tooltip(css = tooltip_css)
                           )
)
            


htmltools::save_html(plot_interactive, "../visuals/num_monthly_delays.html")


# 02 Avg Delay Times per Month ----

# same dataset is used

# plot

plot <-   
 delays_monyr %>% 
  ggplot( aes(x = my,  y = average_month)) +
  geom_line(aes(color=School_Year, group=School_Year), alpha=1) +
  geom_point_interactive(size=1.5, alpha=0.85,
                         aes(color=School_Year, group=School_Year),
                         tooltip = paste(delays_monyr$month_char,
                                         delays_monyr$year, " :",
                        scales::number(delays_monyr$average_month,
                                       accuracy = 1),
                                         " minutes") )+ 
  scale_color_nycc(palette = "main", reverse = T) +
  # geom_smooth(aes(group=1), se=F, size=0.75,
  #                         alpha =.5, color="#23417D") +
  scale_y_continuous(breaks = seq(0, max(delays_monyr$average_month),
                                  2),
                     labels = seq(0,max(delays_monyr$average_month),
                                                2)) +
  labs(title="Average Daily Delay Times", 
       x="School Year Calendar Months",  
       y="Minutes", color="SY") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 9),
        legend.position = "top",
        axis.text.y = element_text(size = 9))

tooltip_css <- "background-color:#CACACA;"

plot_interactive <- girafe(ggobj = plot,   
                           width_svg = 9,
                           height_svg = 5, 
                           options = list(
                             opts_tooltip(css = tooltip_css)
                           )
)



htmltools::save_html(plot_interactive, "visuals/avg_monthly_delay_times.html")


# 03 Longest delays by reason -----

# data prep
reasons <- x %>% 
  filter(Reason!="")  %>% 
  mutate(day=day(Occurred_On), 
         month=month(Occurred_On), 
         year=year(Occurred_On)) %>% 
  filter(year>=2021) %>% 
  group_by(Reason) %>% 
  summarize(count=n(), 
            average_time=mean(delay_time)) %>% 
  top_n(5, wt = average_time)

# plot
plot <- 
  reasons %>% 
  ggplot(aes(x = reorder(Reason,average_time), 
             y=average_time, fill = Reason)) +
  geom_col_interactive(width = 0.6,
                      tooltip = 
                         paste("Reason:", reasons$Reason, 
                               "<br>Average:", 
                               round(reasons$average_time,2))) +
  scale_fill_nycc(palette = "main") +
  coord_flip() +
  geom_text(show.legend = F, size = 3,
            label= paste0(round(reasons$average_time, 0), " min."), 
            nudge_x = 0, nudge_y = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  ylab("Average Minutes Delayed") + xlab("") +
  labs(title="",
       subtitle = "Longest Delays", 
       x="",  y="Average Minutes Delayed") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 9),
        legend.position = "none",
        axis.text.y = element_text(size = 9))

tooltip_css <- "background-color:#CACACA;"

plot_interactive <- girafe(ggobj = plot,   
                           width_svg = 6,
                           height_svg = 5, 
                           options = list(
                             opts_tooltip(css = tooltip_css)
                           )
)


htmltools::save_html(plot_interactive, "visuals/longest_delays.html")
  



# 04 Most delays by reason -----

# data prep
reasons <- x %>% 
  filter(Reason!="")  %>% 
  mutate(day=day(Occurred_On), 
         month=month(Occurred_On), 
         year=year(Occurred_On)) %>% 
  filter(year>=2021) %>% 
  group_by(Reason) %>% 
  summarize(count=n()) %>% 
  mutate(percent=round(count/sum(count)*100,2)) %>% 
  top_n(5, wt = percent)


# plot
plot <- 
  reasons %>% 
  ggplot(aes(x = reorder(Reason, percent), 
             y=count, fill = Reason)) +
  geom_col_interactive(width = 0.6,
                       tooltip = 
            paste0("Reason: ", reasons$Reason, 
                               "<br>Count: ", 
                      scales::comma(round(reasons$count,2)), 
                   "<br>Percent: ", 
                     scales::percent(reasons$percent, scale = 1))
                ) +
  scale_fill_nycc(palette = "main") +
  coord_flip() +
  geom_text(show.legend = F, size = 3,
            label= scales::comma(round(reasons$count,2)), 
            nudge_x = 0, hjust = -0.15) + #nudge_y not working for some reason
  scale_y_continuous(breaks = seq(0,max(reasons$count), 10000),
                     labels = scales::comma(
                       round(seq(0,max(reasons$count),10000),2)),
                     expand = expansion(mult = c(0, .1))) +
  labs(title="",
       subtitle = "Most Delays", 
       x="",  y="Number of Delays", color="SY") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 9),
        legend.position = "none",
        axis.text.y = element_text(size = 9))

tooltip_css <- "background-color:#CACACA;"

plot_interactive <- girafe(ggobj = plot,   
                           width_svg = 6,
                           height_svg = 5, 
                           options = list(
                             opts_tooltip(css = tooltip_css)
                           )
)


htmltools::save_html(plot_interactive, "visuals/most_delays.html")




# 05 Reasons for delay over time ----

# data prep
top_reasons <- x %>% 
  mutate(day=day(Occurred_On), 
         month=month(Occurred_On), 
         year=year(Occurred_On)) %>% 
  filter(year>=2021) %>% 
  filter(Reason!="") %>% 
  group_by(Reason) %>% 
  summarize(count=n()) %>% 
  mutate(percent=round(count/sum(count)*100,3)) %>% 
  arrange(desc(count)) %>% top_n(5, wt=percent) %>% 
  select(Reason) %>% unlist()

t <- x %>% 
  mutate(day=day(Occurred_On), 
         month=month(Occurred_On), 
         year=year(Occurred_On)) %>% 
  filter(Reason %in% top_reasons) %>% 
  group_by(year, month, Reason) %>% 
  summarize(count=n(), 
            average_month=mean(delay_time)) %>% 
  
  mutate(Reason = factor(Reason, levels = top_reasons),
    month_char=month.abb[month], 
         my=factor(month_char, levels= unique(delays_monyr$month_char)),
    month=factor(month, levels= unique(delays_monyr$month)),
         monyr=ym(paste(year,month, sep=" ")),
         School_Year = case_when(monyr >='2017-09-01' & 
                                   monyr<='2018-06-01' ~ '2017-2018',
                                 monyr >='2018-09-01' & 
                                   monyr<='2019-06-01' ~ '2018-2019',
                                 monyr >='2019-09-01' & 
                                   monyr<='2020-06-01' ~ '2019-2020',
                                 monyr >='2020-09-01' & 
                                   monyr<='2021-06-01' ~ '2020-2021',
                                 monyr >='2021-09-01' & 
                                   monyr<='2022-06-01' ~ '2021-2022',
                                 monyr >='2022-09-01' & 
                                   monyr<='2023-06-01' ~ '2022-2023')) %>% 
  
  left_join(enroll, by="year") %>% #normalize to 2022 pop levels
  mutate(norm_count=round(count*1039828/enrollment,2)) 


plot <- t %>% 
  ggplot(aes(x = my,  y = norm_count)) +
  geom_line(aes(color=School_Year, group=School_Year), 
            alpha=1) +
  facet_wrap(~Reason, ncol=5) +
  geom_point_interactive(size=1.5, alpha=0.85,
                         aes(color=School_Year, 
                             group=School_Year),
                         tooltip = paste(t$month_char,
                                         t$year, " :",
                                         scales::comma(t$norm_count,
                                                        accuracy = 1),
                                         " delays") )+ 
  scale_color_nycc(palette = "main", reverse = T) +
  scale_y_continuous(breaks = seq(0, max(t$norm_count),
                                  1000),
                     labels = scales::comma(seq(0,max(t$norm_count),
                                  1000))) +
  labs(title="", 
       x="School Year Calendar Months",  
       y="Number of Delays",
       color = "SY") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
         legend.position = "top",
         axis.text.y = element_text(size = 9))

tooltip_css <- "background-color:#CACACA;"

plot_interactive <- girafe(ggobj = plot,   
                           width_svg = 10,
                           height_svg = 5, 
                           options = list(
                             opts_tooltip(css = tooltip_css)
                           )
)



htmltools::save_html(plot_interactive, "visuals/reasons_num_delays.html")

# 06  SWD delays by delay type ----

#read in new data that was previously missing
library(readr)
myData <- read_csv("data/output/school_bus_delays_2022_updated.csv.zip")

# data prep
swd <- myData %>% 
  mutate(day=day(Occurred_On), 
         month=month(Occurred_On), 
         year=year(Occurred_On)) %>% 
  mutate(type=ifelse(grepl("General", Run_Type)==TRUE, 
                     "General Ed",
                     ifelse(grepl("Special", Run_Type)==TRUE,
                            "Special Ed","Other"))) %>% 
  group_by(year, month, type) %>% 
  summarize(count=n(), 
            average_month=mean(delay_time)) %>% 
  mutate(month_char=month.abb[month],
         my=factor(month_char, 
                   levels= unique(month_char))) %>%
  filter(type=="General Ed"| type=="Special Ed") %>% 
  mutate(type = factor(type, levels = c("Special Ed", "General Ed")),
         monyr=ym(paste(year,month, sep=" ")),
         School_Year = case_when(monyr >='2017-09-01' & 
                          monyr<='2018-06-01' ~ '2017-2018',
                        monyr >='2018-09-01' & 
                          monyr<='2019-06-01' ~ '2018-2019',
                        monyr >='2019-09-01' & 
                          monyr<='2020-06-01' ~ '2019-2020',
                        monyr >='2020-09-01' & 
                          monyr<='2021-06-01' ~ '2020-2021',
                        monyr >='2021-09-01' & 
                          monyr<='2022-06-01' ~ '2021-2022',
                        monyr >='2022-09-01' & 
                          monyr<='2023-06-01' ~ '2022-2023'))
# plot
plot <- swd %>% 
  ggplot(aes(x = my,  y = average_month)) +
  geom_line(aes(color=School_Year, group=School_Year), 
            alpha=1) +
  facet_wrap(~type, ncol=2) +
  geom_point_interactive(size=1.5, alpha=0.85,
                         aes(color=School_Year, 
                             group=School_Year),
             tooltip = paste(swd$month_char,
                             swd$year, " :",
                scales::number(swd$average_month,
                               accuracy = 1),
                 " minutes") )+ 
  scale_color_nycc(palette = "main", reverse = T) +
  scale_y_continuous(breaks = seq(0, max(swd$average_month),
                                  4),
                     labels = seq(0,max(swd$average_month),
                                  4)) +
  labs(title="", 
       x="School Year Calendar Months",  
       y="Average Daily Minutes",
       color="SY") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 9),
        legend.position = "top",
        axis.text.y = element_text(size = 9))
  
  tooltip_css <- "background-color:#CACACA;"
  
  plot_interactive <- girafe(ggobj = plot,   
                             width_svg = 10,
                             height_svg = 5, 
                             options = list(
                               opts_tooltip(css = tooltip_css)
                             )
  )
  
  
  
  htmltools::save_html(plot_interactive, "visuals/swd_delaytimes.html")
  
  
 ######## unused code ----
library(readxl)

d1 <- read_xlsx("./data/input/raw/calendar_days_hs.xlsx") %>% 
   mutate(hs = ymd(hs))
d2 <- read_xlsx("./data/input/raw/calendar_days_elementary.xlsx") %>% 
  # slice(1:(n() - 1))  %>% 
  mutate(elementary = mdy(elementary))

names(d2) <- names(d1)

dates <- rbind(d1,d2)

dates <- dates %>%  distinct()
