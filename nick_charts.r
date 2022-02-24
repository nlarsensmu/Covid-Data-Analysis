library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
library("gridExtra")
library("knitr")

cases <- read_csv(".\\data\\COVID-19-20220207.csv")

# I would like to drill down into this graph to see what all is really in the big bin
ggplot(cases_TX, mapping = aes(confirmed_cases)) + geom_histogram(bins = 20)
ggsave(".\\charts\\1HistCountiesByCase20bins.pdf")

ggplot(filter(cases_TX, confirmed_cases < 100000), mapping=aes(confirmed_cases)) + geom_histogram(bins=20)
ggsave(".\\charts\\1HistCountiesByCase20bins-zoomed.pdf")

ggplot(filter(cases_TX, confirmed_cases > 100000), mapping=aes(confirmed_cases)) + geom_histogram(bins=40)
ggsave(".\\charts\\1HistCountiesByCase20bins-zoomed2.pdf")


# Lets look at missing data for the following columns

deaths_cases_per_county <- read.csv(".\\data\\deaths_cases_per_county_2021.csv")


deaths_cases_per_county <- read.csv(".\\data\\query1_02_23_22.csv")
missing.values <- deaths_cases_per_county %>% 
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>% 
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing)) 
missing.values %>% kable()
