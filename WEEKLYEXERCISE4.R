library(usethis)
git_default_branch()
# The name of the branch is "main".
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)

Olympics <- read_csv("Olympics.csv")

athletes1992 <- Olympics %>%
  filter(year == 1992) %>%
  select(country, athletes) %>% 
  arrange(desc(athletes))

athletes1992
