# read in filtered, cleaned school bus delay data -----
x <-  read_csv("data/output/filtered_weekends_vacation_covid_delays.csv")

# Total Delays per Month ----

delays_monyr <- x %>% 
  mutate(day=day(Occurred_On), 
         month=month(Occurred_On), 
         year=year(Occurred_On)) %>% 
  group_by(year, month) %>% 
  summarize(count=n(), 
            average_month=mean(delay_time)) %>% 
  mutate(month_char=month.abb[month], 
         my=factor(month_char, levels= unique(delays_monyr$month_char)),
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
  labs(title="School Bus Delays per Month", 
       x="School Year Calendar Months",  
       y="Number of Delays") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        legend.position = "top")

tooltip_css <- "background-color:#CACACA;"

plot_interactive <- girafe(ggobj = plot,   
                           width_svg = 11,
                           height_svg = 8, 
                           options = list(
                             opts_tooltip(css = tooltip_css)
                           )
)
            


htmltools::save_html(plot_interactive, "visuals/num_monthly_delays.html")


# Avg DelayTimes per Month ----

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
  labs(title="School Bus Average Delay Times per Month", 
       x="School Year Calendar Months",  
       y="Average Delay Times") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        legend.position = "top")

tooltip_css <- "background-color:#CACACA;"

plot_interactive <- girafe(ggobj = plot,   
                           width_svg = 11,
                           height_svg = 8, 
                           options = list(
                             opts_tooltip(css = tooltip_css)
                           )
)



htmltools::save_html(plot_interactive, "visuals/avg_monthly_delay_times.html")


## delays by delay type overall ----
## SWD delays by delay type ----


plot <- 
  ggplot(data = m, 
         aes(x=rank(facre_pc), y=COVID_DEATH_RATE, color=MedInc)) +
  geom_point_interactive(
    tooltip = paste0(
      "Neighborhood (modzcta): ", m$NEIGHBORHOOD_NAME, " (", m$MODZCTA, ")", 
      "\n", 
      "Borough: ", m$BOROUGH_GROUP, 
      "\n", 
      "Park Access (Functional Acres Per 100,000 Residents): ", round(m$facre_pc * 100000, 1), 
      "\n", 
      "COVID-19 Death Rate (Per 100,000): ", round(m$COVID_DEATH_RATE, 0), 
      "\n", 
      "Median Income ($): ", scales::comma(round(m$MedInc, 0))
    )
  ) + 
  geom_vline(xintercept = median(rank(modzcta_facre$facre_pc), na.rm=TRUE),
             color ="#666666",linetype = "dashed") +
  geom_hline(yintercept = as.numeric(median(modzcta_facre$COVID_DEATH_RATE, na.rm=TRUE)),
             color ="#666666",linetype = "dashed") +
  geom_hline(aes(yintercept = as.numeric(median(modzcta_facre$COVID_DEATH_RATE, na.rm=TRUE)), linetype = "Median"), 
             colour = "#666666") + 
  scale_linetype_manual(name = NULL, values = 4) +
  scale_y_continuous(label = scales::comma_format()) +
  ggtitle("Park Equity in NYC", "Comparing Park Acreage, Covid Death Rates, and Median Income for Every Zipcode") +
  labs(
    x = "Park Acreage\n (Functional Acreage Per Capita) \n Ranked from Least to Greatest",
    y = "Covid Death Rate (Per 100,000)",
    color = "Median Income",
    caption = expression(paste(italic("Source: Census ACS; NYC DOHMH; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  scale_color_gradientn(
    labels = scales::dollar_format(),
    colours = c('#800000','#DD6C54','#E6E6E6','#AFB3D1','#7683BC','#2F56A6'),
    values = scales::rescale(c(50000,70000,100000, 150000, 200000, 250000) ) )+
  
  annotate("text", x = 25, y = 1350, label = "Low Park Access\nHigh Covid") +
  annotate("text", x = 125, y = 1350, label = "High Park Access\nHigh Covid") +
  annotate("text", x = 25, y = 50, label = "Low Park Access\nLow Covid") +
  annotate("text", x = 125, y = 50, label = "High Park Access\nLow Covid") +
  
  theme(legend.position="right", legend.text = element_text(size=8),
        legend.title = element_text(size=10, family = 'Georgia'),
        
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(family = "Georgia",size = 14),
        axis.title.y = element_text(size = 11, 
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size = 11, 
                                   margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.x = element_text(size = 11, 
                                   margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 11, 
                                    margin = margin(t = 10, r = 0, b = 0, l = 0))) 

tooltip_css <- "background-color:#CACACA;"

plot_interactive <- girafe(ggobj = plot,   
                           width_svg = 9,
                           height_svg = 5, 
                           options = list(
                             opts_tooltip(css = tooltip_css)
                           )
)

htmltools::save_html(plot_interactive, "figures/mainplot_interactive.html")






library(readxl)

d1 <- read_xlsx("./data/input/raw/calendar_days_hs.xlsx") %>% 
   mutate(hs = ymd(hs))
d2 <- read_xlsx("./data/input/raw/calendar_days_elementary.xlsx") %>% 
  # slice(1:(n() - 1))  %>% 
  mutate(elementary = mdy(elementary))

names(d2) <- names(d1)

dates <- rbind(d1,d2)

dates <- dates %>%  distinct()
