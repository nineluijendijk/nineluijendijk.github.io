library(tidyverse)
library(plotly)
library(scales)
library(here)

rmarkdown::render("06_parameterizedcovid.Rmd", params = list(country = c("Germany", "France", "Netherlands"),
                                                        year = 2022,
                                                        month = 1:3))

rmarkdown::render("06_parameterizedcovid.Rmd", params = list(country = c("Estonia", "Latvia", "Lithuania"),
                                                             year = 2020:2021,
                                                             month = 1:12))


if(length(params$year) > 1){
  datebreaks <- "1 month"
}else{
  datebreaks <- "2 weeks"
}

data <- read.csv(here("data_raw/data.csv"))

data_filtered <- data %>% filter(countriesAndTerritories %in% c("Estonia", "Latvia", "Lithuania"), year %in% 2020, month %in% 1:6)

data_filtered <- mutate(data_filtered, "date" = paste(day, month, year, sep="/"))

data_filtered$date <- as.Date(data_filtered$date, format="%d/%m/%Y")

plot_cases <- ggplot(data_filtered,
                     aes(x = date, y = cases, group = countriesAndTerritories,
                         color = countriesAndTerritories))+
  geom_line()+
  geom_point(size = 1)+
  labs(title = "Number of newly reported COVID-19 cases over time by country",
       y = "Number of COVID-19 cases",
       x = "Date",
       color = "Country")+
  scale_x_date(date_breaks = datebreaks, date_labels = "%d-%m-%y")+
  scale_y_continuous(labels = label_comma())+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplotly(plot_cases)


plot_deaths <- ggplot(data_filtered,
                     aes(x = date, y = deaths, group = countriesAndTerritories,
                         color = countriesAndTerritories))+
  geom_line()+
  geom_point(size = 1)+
  labs(title = "Number of newly reported COVID-19 deaths over time by country",
       y = "Number of COVID-19 deaths",
       x = "Date",
       color = "Country")+
  scale_x_date(date_breaks = datebreaks, date_labels = "%d-%m-%y")+
  scale_y_continuous(labels = label_comma())+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplotly(plot_deaths)
