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

deaths_cases_per_county <- read.csv(".\\data\\query1_02_23_22.csv")
ggplot(deaths_cases_per_county, mapping = aes(deaths)) + geom_histogram(bins = 20)
deaths_cases_per_county["deaths_by_pop"] = deaths_cases_per_county$deaths / deaths_cases_per_county$total_pop
ggplot(deaths_cases_per_county, mapping = aes(deaths_by_pop)) + geom_histogram(bins = 20)


# Plot hist
# Add in deaths per pop
ggplot(deaths_cases_per_county, mapping = aes(cases)) + geom_histogram(bins = 500)
deaths_cases_per_county %>% filter(cases >200000000)


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
