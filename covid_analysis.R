# covid_analysis.R

# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)

# Read the dataset (download from: https://covid.ourworldindata.org/data/owid-covid-data.csv)
covid <- read_csv("owid-covid-data.csv")

# Filter for selected countries
countries <- c("India", "United States", "Brazil")
covid_filtered <- covid %>%
  filter(location %in% countries & !is.na(total_cases))

# Time series plot of total cases
ggplot(covid_filtered, aes(x = date, y = total_cases, color = location)) +
  geom_line() +
  labs(title = "Total COVID-19 Cases Over Time", x = "Date", y = "Total Cases")

# Bar plot of total deaths by country
latest <- covid %>%
  group_by(location) %>%
  filter(date == max(date)) %>%
  select(location, total_deaths) %>%
  drop_na() %>%
  arrange(desc(total_deaths)) %>%
  head(10)

ggplot(latest, aes(x = reorder(location, -total_deaths), y = total_deaths)) +
  geom_col(fill = "red") +
  labs(title = "Top 10 Countries by Total Deaths", x = "Country", y = "Deaths") +
  coord_flip()

# Vaccination progress comparison
vaccine_data <- covid %>%
  filter(location %in% countries & !is.na(people_vaccinated_per_hundred))

ggplot(vaccine_data, aes(x = date, y = people_vaccinated_per_hundred, color = location)) +
  geom_line() +
  labs(title = "Vaccination Progress", x = "Date", y = "People Vaccinated per 100")