library(here)
library(dslabs)
library(tidyverse)
library(DBI)
library(RPostgres)
library(car)
library(plotly)

dengue_data <- read_csv(here("data_raw/dengue_data.csv"), skip = 11) #first 11 rows contain metadata
flu_data <- read_csv(here("data_raw/flu_data.csv"), skip = 11) #first 11 rows contain metadata

dengue_tidy <- pivot_longer(data = dengue_data, 
                            cols = c(2:ncol(dengue_data)), 
                            names_to = "country", 
                            values_to = "cases") #each observation must have its own row

flu_tidy <- pivot_longer(data = flu_data, 
                         cols = c(2:ncol(flu_data)), 
                         names_to = "country", 
                         values_to = "cases") #each observation must have its own row

dengue_tidy <- dengue_tidy %>% mutate("year" = substr(Date, 1, 4)) #add "year" column
dengue_tidy$year <- as.integer(dengue_tidy$year) #change class of column year from chr to int

flu_tidy <- flu_tidy %>% mutate("year" = substr(Date, 1, 4)) #add "year" column
flu_tidy$year <- as.integer(flu_tidy$year) #change class of column year from chr to int

dengue_tidy$country <- as.factor(dengue_tidy$country) #the column country in gapminder is of the class factor
flu_tidy$country <- as.factor(flu_tidy$country) #the column country in gapminder is of the class factor

saveRDS(gapminder, here("data/gapminder.rds")) #store the three tables as .rds and .csv files
saveRDS(dengue_tidy, here("data/dengue.rds"))
saveRDS(flu_tidy, here("data/flu.rds"))
write_csv(gapminder, here("data/gapminder.csv"))
write_csv(dengue_tidy, here("data/dengue.csv"))
write_csv(flu_tidy, here("data/flu.csv"))

con <- dbConnect(Postgres(), #connect to the database
                 dbname = "workflowsdb", 
                 host="localhost", 
                 port="5432", 
                 user="postgres", 
                 password="password")

dbWriteTable(con, "gapminder", gapminder) #insert the tables into the database
dbWriteTable(con, "dengue", dengue_tidy)
dbWriteTable(con, "flu", flu_tidy)

SELECT *
  FROM gapminder
WHERE year IS NULL; #make sure no important values are missing

SELECT *
  FROM gapminder
WHERE country IS NULL;

SELECT *
  FROM dengue
WHERE year IS NULL;

SELECT *
  FROM dengue
WHERE country IS NULL;

SELECT *
  FROM flu
WHERE year IS NULL;

SELECT *
  FROM flu
WHERE country IS NULL;

SELECT COUNT(cases) #count the number of observations (ignoring NULL)
FROM dengue; 

SELECT COUNT(cases)
FROM flu;

SELECT SUM(cases) #calculate the total number of cases
FROM dengue;

SELECT SUM(cases)
FROM flu;

SELECT year, country, cases*1000 AS cases1000 #look at the highest observed numbers of cases
FROM dengue
WHERE cases IS NOT NULL
ORDER BY cases1000 DESC;

SELECT year, country, cases
FROM flu
WHERE cases IS NOT NULL
ORDER BY cases DESC;

dengue_mutated <- dengue_tidy %>% mutate("cases1000" = cases * 1000) #add a column with the multiplied number of cases

maxcasesdengue <- subset(dengue_mutated, cases1000 == max(na.omit(dengue_mutated$cases1000)))  
mincasesdengue <- subset(dengue_mutated, cases1000 == min(na.omit(dengue_mutated$cases1000)))
unique(maxcasesdengue$country) #look at which countries have the highest observed number of dengue cases
unique(mincasesdengue$country) #look at which countries have the lowest observed number of dengue cases

maxcasesflu <- subset(flu_tidy, cases == max(na.omit(flu_tidy$cases)))  
mincasesflu <- subset(flu_tidy, cases == min(na.omit(flu_tidy$cases)))  
unique(maxcasesflu$country) #look at which countries have the highest observed number of flu cases
unique(mincasesflu$country) #look at which countries have the lowest observed number of flu cases

dengue_country_means <- dengue_mutated %>% group_by(country) %>% 
  summarize(total = sum(cases1000, na.rm = TRUE), mean = mean(cases1000, na.rm = TRUE)) %>% 
  arrange(desc(mean)) #calculate the total number of cases and mean number of cases per country

flu_country_means <- flu_tidy %>% group_by(country) %>%
  summarize(total = sum(cases, na.rm = TRUE), mean = mean(cases, na.rm = TRUE)) %>% 
  arrange(desc(mean))

tibble(dengue_country_means) #print the dataframes
print.data.frame(flu_country_means)

#calculate the number of cases per year and the average number of cases per week per country
dengue_combine <- na.omit(dengue_mutated) %>% group_by(country, year) %>%
  summarize(total_cases_dengue = sum(cases1000), mean_weekly_dengue = mean(cases1000))

flu_combine <- na.omit(flu_tidy) %>% group_by(country, year) %>% 
  summarize(total_cases_flu = sum(cases), mean_weekly_flu = mean(cases))

dbWriteTable(con, "dengue_combine", dengue_combine) #insert the new tables into the database
dbWriteTable(con, "flu_combine", flu_combine)

CREATE TABLE dengue_gapminder AS #combine dengue_data and gapminder
SELECT
gapminder.country,
gapminder.year,
gapminder.population,
gapminder.continent,
gapminder.region,
dengue_combine.total_cases_dengue,
dengue_combine.mean_weekly_dengue
FROM gapminder
LEFT JOIN dengue_combine
ON gapminder.country = dengue_combine.country
AND gapminder.year = dengue_combine.year;

CREATE TABLE flu_gapminder AS #combine flu_data and gapminder
SELECT
gapminder.country,
gapminder.year,
gapminder.population,
gapminder.continent,
gapminder.region,
flu_combine.total_cases_flu,
flu_combine.mean_weekly_flu
FROM gapminder
LEFT JOIN flu_combine
ON gapminder.country = flu_combine.country
AND gapminder.year = flu_combine.year;

CREATE TABLE flu_gapminder_dengue AS	#combine all 3 tables
SELECT
flu_gapminder.country,
flu_gapminder.year,
flu_gapminder.population,
flu_gapminder.continent,
flu_gapminder.region,
flu_gapminder.total_cases_flu,
flu_gapminder.mean_weekly_flu,
dengue_combine.total_cases_dengue,
dengue_combine.mean_weekly_dengue
FROM flu_gapminder
INNER JOIN dengue_combine
ON flu_gapminder.country = dengue_combine.country
AND flu_gapminder.year = dengue_combine.year;

dengue_gapminder <- dbReadTable(con, "dengue_gapminder") #load the combined tables into R
flu_gapminder <- dbReadTable(con, "flu_gapminder")
flu_gapminder_dengue <- dbReadTable(con, "flu_gapminder_dengue")

dengue_gapminder_nona <- na.omit(dengue_gapminder) #remove NA values
flu_gapminder_nona <- na.omit(flu_gapminder)
flu_gapminder_dengue_nona <- na.omit(flu_gapminder_dengue)

#normalize the data by calculating the average number of cases per 100,000 population
dengue_gapminder_normalized <- dengue_gapminder_nona %>% mutate("normalized_dengue" = (total_cases_dengue/population)*100000,
                                                                "normalized_dengue_mean" = (mean_weekly_dengue/population)*100000)

flu_gapminder_normalized <- flu_gapminder_nona %>% mutate("normalized_flu" = (total_cases_flu/population)*100000,
                                                          "normalized_flu_mean" = (mean_weekly_flu/population)*100000)

flu_gapminder_dengue_normalized <- flu_gapminder_dengue_nona %>% mutate("normalized_dengue" = (total_cases_dengue/population)*100000,
                                                                        "normalized_flu" = (total_cases_flu/population)*100000,
                                                                        "normalized_dengue_mean" = (mean_weekly_dengue/population)*100000,
                                                                        "normalized_flu_mean" = (mean_weekly_flu/population)*100000)

#calculate means, standard deviations and Shapiro-Wilk p-values per region
dengue_gap_summary <- dengue_gapminder_normalized %>% group_by(region) %>% 
  summarize(mean = mean(total_cases_dengue),
            sd = sd(total_cases_dengue),
            pvalue_sw = shapiro.test(total_cases_dengue)$p.value)

flu_gap_summary <-flu_gapminder_normalized %>% group_by(region) %>% 
  summarize(mean = mean(total_cases_flu),
            sd = sd(total_cases_flu),
            pvalue_sw = shapiro.test(total_cases_flu)$p.value)

#calculate means, standard deviations and Shapiro-Wilk p-values per country
dengue_gap_summary_country <- dengue_gapminder_normalized %>% group_by(country) %>% 
  summarize(mean = mean(total_cases_dengue),
            sd = sd(total_cases_dengue),
            pvalue_sw = shapiro.test(total_cases_dengue)$p.value)

flu_gap_summary_country <-flu_gapminder_normalized %>% group_by(country) %>% 
  summarize(mean = mean(total_cases_flu),
            sd = sd(total_cases_flu),
            pvalue_sw = shapiro.test(total_cases_flu)$p.value)

#find the countries where the data is a normal distribution
countries_normal_dengue <- subset(dengue_gap_summary_country, pvalue_sw > 0.05)[,1] %>% pull()
countries_normal_flu <- subset(flu_gap_summary_country, pvalue_sw > 0.05)[,1] %>% pull()

#filter for the rows with observations from those countries
dengue_normal <- dengue_gapminder_normalized[dengue_gapminder_normalized$country %in% countries_normal_dengue,]
flu_normal <- flu_gapminder_normalized[flu_gapminder_normalized$country %in% countries_normal_flu,]

#perform levene's test
leveneTest(total_cases_dengue ~ country, data = dengue_normal, center = mean)[1,3]
leveneTest(total_cases_flu ~ country, data = flu_normal, center = mean)[1,3]

#perform a pearson correlation analysis 
dengue_flu_cor <- round(cor.test(flu_gapminder_dengue_normalized$total_cases_dengue, 
                                 flu_gapminder_dengue_normalized$total_cases_flu, 
                                 method = "pearson")$estimate, 3)

cor_pvalue <- cor.test(flu_gapminder_dengue_normalized$total_cases_dengue, 
         flu_gapminder_dengue_normalized$total_cases_flu, 
         method = "pearson")$p.value

#visualize the correlation in a scatter plot
ggplot(data = flu_gapminder_dengue_normalized, aes(y = total_cases_dengue, x = total_cases_flu)) +
  geom_point()+
  labs(title = "Relation between dengue and flu cases",
       subtitle = paste("pearson's r = ", dengue_flu_cor),
       y = "Number of dengue cases",
       x = "Number of flu cases")+
  theme_minimal()

#visualize the number of dengue cases per region in a bar plot
ggplot(dengue_gap_summary,
       aes(x = region, y = mean,
           fill = region))+ 
  geom_col()+ 
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd, width = 0.2))+
  labs(title = "Number of dengue cases per region", 
       subtitle = "Error bars depict 1 standard deviation",
       x="Region", 
       y="Average number of dengue cases")+
  theme_minimal()+
  theme(legend.position = "none")
  

#create a scatter plot of the dengue data
dengue_plot <- ggplot(dengue_gapminder_normalized,
       aes(x = year, y = mean_weekly_dengue, group = country,
           color = country))+ 
  geom_line()+
  geom_point()+
  labs(title = "Weekly dengue cases per country",
       y = "Average number of dengue cases per week per 100.000 population",
       x = "Year",
       color = "Country")+
  scale_x_continuous(breaks = seq(from = min(dengue_gapminder_normalized$year),
                              to = max(dengue_gapminder_normalized$year),
                              by = 1))+
  theme_minimal()

ggplotly(dengue_plot) #visualize the number of dengue cases per country in an interactive scatter plot


#visualize the number of flu cases per region in a bar plot
ggplot(flu_gap_summary,
       aes(x = region, y = mean,
           fill = region))+ 
  geom_col()+ 
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd, width = 0.2))+
  labs(title = "Number of flu cases per region", 
       subtitle = "Error bars depict 1 standard deviation",
       x="Region", 
       y="Average number of flu cases")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

SELECT DISTINCT country, region
FROM flu_gapminder
WHERE region = 'Southern Africa'
AND total_cases_flu IS NOT NULL

SELECT year, country, cases
FROM flu
WHERE country = 'South Africa'
AND cases IS NOT NULL
ORDER BY RANDOM();

SELECT year, country, cases
FROM flu
WHERE cases IS NOT NULL
ORDER BY RANDOM();

#create a scatter plot of the flu data
flu_plot <- ggplot(flu_gapminder_normalized,
       aes(x = year, y = mean_weekly_flu, group = country,
           color = country)) + 
  geom_line() +
  geom_point() +
  labs(title = "Weekly flu cases per country",
       y = "Average number of flu cases per week per 100.000 population",
       x = "Year",
       color = "Country")+
  scale_x_continuous(breaks = seq(from = min(flu_gapminder_normalized$year),
                                  to = max(flu_gapminder_normalized$year),
                                  by = 1))+
  theme_minimal()

ggplotly(flu_plot) #visualize the number of flu cases per country in an interactive scatter plot
