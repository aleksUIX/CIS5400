# http://margintale.blogspot.in/2012/04/ggplot2-time-series-heatmaps.html
library(tidyverse)
library(dplyr)
library(plyr)
library(scales)
library(zoo)
library(lubridate)
library(waffle)
library(tidyr)

df <- read.csv('db.csv')
df$date <- as.Date(df$Date.of.Launch, format = "%d/%m/%Y")
df <- df %>% select(Country.of.Operator.Owner, Launch.Site, Detailed.Purpose, Country.of.Contractor, date, Apogee..km., Class.of.Orbit, Type.of.Orbit, Users,  Dry.Mass..kg.., Power..watts., Eccentricity, Expected.Lifetime..yrs.., Period..minutes.)


dfcluster <- df %>% mutate(xcoord = as.numeric(Expected.Lifetime..yrs..),  ycoord = as.numeric(Apogee..km.))
dfcluster <- dfcluster %>% filter(xcoord != '', ycoord != "", Detailed.Purpose != "")
dfcluster <- dfcluster %>% drop_na(xcoord, ycoord)


ggplot(dfcluster, aes(x = xcoord, y = ycoord, color = Detailed.Purpose)) +
  geom_point() +
  labs(title = "Expected Lifetime vs Apogee",
       x = "Expected Lifetime",
       y = "Apogee",
       color = "Detailed Purpose") +
  theme_minimal()




orbit_data <- df %>% filter(df$Type.of.Orbit != "", df$Detailed.Purpose != "")

ggplot(orbit_data, aes(x = orbit_data$Type.of.Orbit, y = orbit_data$Detailed.Purpose)) + 
  geom_point() + labs(x = "Orbit Type", y = "Mission", title = "Satellite Mission vs Orbit Type") 

unique(df$Users)
unique(df$Country.of.Operator.Owner)
unique(df$Class.of.Orbit)
unique(df$Type.of.Orbit)

chart_data <- ""
chart_data <- df %>% 
  group_by(date) %>% 
  dplyr::summarize(df = n()) %>%
  arrange(date) %>% 
  mutate(cumulative_launches = cumsum(df))


ggplot(chart_data, aes(x = date, y = cumulative_launches)) +
  geom_line() +
  labs(x = "Date", y = "Launches", title = "Cumulative Rocket Launches") + 
  theme(plot.margin = margin(1, 1, 1, 1, "cm"))

ggplot(chart_data, aes(x = date, y = cumulative_launches)) +
  geom_line() + scale_y_log10() +
  labs(x = "Date", y = "Launches", title = "Cumulative Rocket Launches in Log Scale") + 
  theme(plot.margin = margin(1, 1, 1, 1, "cm"))



###########################################################################################
#               cumulative launches for top 10 countries
###########################################################################################

df_cumulative <- df %>% select(Country.of.Operator.Owner, date) %>% na.omit()

cumulative_data <- df_cumulative %>%
  group_by(Country.of.Operator.Owner) %>%
  arrange(date) %>%
  mutate(cumulative_launches = cumsum(1)) %>%
  mutate(year = year(date))
  ungroup()


df_sum <- cumulative_data %>%
  group_by(Country.of.Operator.Owner, year) %>%
  summarize(cumulative_launches = sum(cumulative_launches))


df_cum <- cumulative_data %>%
  group_by(Country.of.Operator.Owner, year) %>%
  dplyr::reframe(annual_launches = n()) %>% 
  group_by(Country.of.Operator.Owner) %>%
  dplyr::reframe(cumulative_launches = cumsum(annual_launches), date = year) %>%
  mutate(total_launches = cumsum(cumulative_launches)) %>% ungroup() 


# problem here is total launches are summed up for all countries
top10_countries <- df_cum %>% group_by(Country.of.Operator.Owner) %>% filter(!Country.of.Operator.Owner %in% c("Multinational", "ESA")) %>%
  dplyr::summarize(total_launches = n()) %>%
  arrange(desc(total_launches)) %>%
  top_n(10) %>% select(Country.of.Operator.Owner)

# filter data for top 10 countries
df_top10 <- df_cum %>%
  filter(Country.of.Operator.Owner %in% top10_countries$Country.of.Operator.Owner)
df_top10 <- df_top10 %>% mutate(year = date)

df_top10 <- subset(df_top10, date >= "2003")


cbPalette <- c("#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd","#8c564b","#e377c2","#7f7f7f","#bcbd22","#17becf")



ggplot(df_top10, aes(x = date, y = cumulative_launches, fill = Country.of.Operator.Owner)) +
  geom_area() +
  scale_x_continuous(breaks = seq(min(df_top10$date), max(df_top10$date), by = 1)) +
  labs(title = "Cumulative Satellite Operator Country by Year (Top 10 Countries)",
       x = "Year",
       y = "Satellites",
       fill = "Country") +scale_fill_manual(values=cbPalette) +
  theme(plot.margin = margin(1, 1, 1, 1, "cm")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




###########################################################################################
#               launch composition per country of operator
###########################################################################################

satellite_cumulative <- df_top10 %>% 
  distinct(Country.of.Operator.Owner, cumulative_launches, year)

latest <- df_top10 %>% filter(year==2022)
latest <- latest %>% mutate(prop_launches = round((cumulative_launches / sum(latest$cumulative_launches)) * 100))

latest$percentage <- round(100 * latest$prop_launches / sum(latest$prop_launches), 1)
latest <- latest %>% arrange(percentage)

# pie
ggplot(latest, aes(x = "", y = percentage, fill = Country.of.Operator.Owner)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(x=1.7,label=paste0(percentage, "%")), position = position_stack(vjust = 0.6)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "New Launches in 2022 per country" ) +theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank())






# waffle
vec <- latest$prop_launches
names(vec) <- latest$Country.of.Operator.Owner

sorted_data <- sort(vec, decreasing = TRUE)


waffle(sorted_data,
       rows = 10,     # Define the number of rows in the chart
       colors = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628", "#f781bf", "#999999", "#000000"),  # Define the colors for each group
       legend_pos = "bottom",
       title = "Distribution of 2022 Launches")



###########################################################################################
#               Cumulative Rocket launches over the entire database
###########################################################################################

drawTrendChart <- function (data, scale, chartTitle) {
  if (scale == 'log') {
    ggplot(data, aes(x = date, y = cumulative_launches, color = data$`df$Country.of.Operator.Owner`)) +
      geom_line() + 
      scale_y_log10() +
      labs(x = "Date", y = "Cumulative Launches", title = chartTitle ) + 
      theme(plot.margin = margin(1, 1, 1, 1, "cm"))
  } else {
    ggplot(data, aes(x = date, y = cumulative_launches, color = data$`df$Country.of.Operator.Owner`)) +
      geom_line() + 
      labs(x = "Date", y = "Cumulative Launches", title = chartTitle) + 
      theme(plot.margin = margin(1, 1, 1, 1, "cm"))
  }
}

# Filter data to only include last 20 years
data_last_20_years <- subset(df, date >= as.Date("2003-04-07") & date <= Sys.Date())

drawTrendChart(data, 'normal', "Cumulative Rocket Launches")
drawTrendChart(data_last_20_years, "Cumulative Rocket Launches - Last 20 years")

drawTrendChart(data, 'log', "Cumulative Rocket Launches (Log Scale)")
drawTrendChart(data_last_20_years,'log', "Cumulative Rocket Launches (Log Scale) - Last 20 years")





# errorbar
df$Apogee..km. <- as.numeric(df$Apogee..km.)
apogee_range = range(df[!(is.na(df$Apogee..km.) | df$Apogee..km.==""), ]$Apogee..km)

ggplot(df, aes(x = Apogee..km., y = Purpose) ) +
    geom_point(position = position_dodge(width = .75) ) +
    theme_bw() + 
    labs(y = "Difference in Growth (cm)",
         x = "Planting Date")

