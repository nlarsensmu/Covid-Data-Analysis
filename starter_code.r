library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")

cases <- read_csv(".\\data\\COVID-19_cases_plus_census.csv")

cases

cases <- cases %>% mutate_if(is.character, factor)
dim(cases)

cases_TX <- cases %>% filter(state == "TX")
dim(cases_TX)

summary(cases_TX[,1:10])


# I would like to drill down into this graph to see what all is really in the big bin
ggplot(cases_TX, mapping = aes(confirmed_cases)) + geom_histogram(bins = 20)

ggplot(cases_TX, mapping = aes(x = confirmed_cases, y = deaths, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_TX, deaths >= 1000)) 

cases_TX_select <- cases_TX %>% filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, confirmed_cases, deaths, total_pop, median_income)
cases_TX_select <- cases_TX_select %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000, 
  deaths_per_1000 = deaths/total_pop*1000, 
  death_per_case = deaths/confirmed_cases)

head(cases_TX_select)

# Usable table
datatable(cases_TX_select) %>% formatRound(6:7, 4) %>% formatPercentage(8, 2)

ggplot(cases_TX_select, mapping = aes(x = cases_per_1000, y = deaths_per_1000, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_TX_select, deaths_per_1000 > quantile(deaths_per_1000, .95)))


# Does death per case depend on population?
ggplot(cases_TX_select, mapping = aes(x= total_pop, y = deaths_per_1000, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_TX_select, deaths_per_1000 > quantile(deaths_per_1000, .95)))
