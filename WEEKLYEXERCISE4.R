library(usethis)
git_default_branch()
# the name of this branch is main
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)

Olympics <- read_csv("Olympics.csv")

newOlympics <- Olympics %>%
  pivot_wider(
    id_cols = country,
    names_from = year,
    values_from = c(gold, silver, bronze),
    values_fill = 0
  )

# total medals
newOlympics <- newOlympics %>%
  mutate(total.medals=rowSums(across(starts_with(c("gold","silver","bronze"))),na.rm = TRUE)
  )
newOlympics[,c("country", "total.medals")]

# gold medals
newOlympics <- newOlympics %>%
  mutate(gold.medals=rowSums(across(starts_with(c("gold"))),na.rm = TRUE)
  )
newOlympics[,c("country", "gold.medals")]

# medal total by year
total_medals_by_year <- Olympics %>%
  group_by(year) %>%
  summarise(
    TotalGold = sum(gold, na.rm = TRUE),
    TotalSilver = sum(silver, na.rm = TRUE),
    TotalBronze = sum(bronze, na.rm = TRUE),
    TotalByYear = sum(gold + silver + bronze, na.rm = TRUE)
  )
print(total_medals_by_year)

athletes1992 <- Olympics %>%
  filter(year == 1992) %>%
  select(country, athletes) %>% 
  arrange(desc(athletes))

athletes1992

#7b

countries <- c("United States", "France", "Germany", "Russia", "China")


goldmedalcountries <- Olympics %>%
  filter(country %in% countries) %>%
  ggplot(aes(x = year, y = gold, color = country, group = country)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Gold Medals Earned Over Time By Country",
    x = "Year",
    y = "Number of Gold Medals",
    color = "Country"
  ) +
  theme_minimal()

print(goldmedalcountries)
