library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
library("gridExtra")
library("knitr")
library("ggpubr")



covid_data_2021_by_month <- read_csv("./data/2020_2021_deaths_county_month.csv")

# Normalize population
data_final$origianl_total_pop = data_final$total_pop
data_final <- data_final %>% mutate_at(c("total_pop"), ~(scale(.) %>% as.vector))

# get the new cases/deaths feature
data_offset <- filter(covid_data_2021_by_month, year == 2020 | (year == 2021 & month <= 11))
data_final <- filter(covid_data_2021_by_month, (year == 2020 & month >= 2) | year == 2021)

data_final$new_cases <- data_final$confirmed_cases - data_offset$confirmed_cases
data_final$new_deaths <- data_final$deaths - data_offset$deaths

# Get the rent burden feature
# Break up into Severe burden, rent burden and no burden
data_final$severe_burden = data_final$rent_over_50_percent / 
  data_final$origianl_total_pop
data_final$rent_burden = (data_final$rent_40_to_50_percent +
                            data_final$rent_35_to_40_percent +
                            data_final$rent_30_to_35_percent) / data_final$origianl_total_pop
data_final$no_rent_burden = (data_final$rent_25_to_30_percent +
                               data_final$rent_20_to_25_percent +
                               data_final$rent_15_to_20_percent + 
                               data_final$rent_10_to_15_percent +
                               data_final$rent_10_to_15_percent +
                               data_final$rent_under_10_percent) /
  data_final$origianl_total_pop


# Get cases/deaths per 1000
data_final$confirmed_cases_per1000 = data_final$confirmed_cases / 
  (data_final$origianl_total_pop)*1000
data_final$confirmed_deaths_per1000 = data_final$deaths / 
  (data_final$origianl_total_pop)*1000

# Get the percentages of each population group
data_final$white_pop_per = data_final$white_pop / data_final$origianl_total_pop
data_final$black_pop_per = data_final$black_pop / data_final$origianl_total_pop
data_final$asian_pop_per = data_final$asian_pop / data_final$origianl_total_pop
data_final$hispanic_pop_per = data_final$hispanic_pop / data_final$origianl_total_pop
data_final$amerindian_pop_per = data_final$amerindian_pop / data_final$origianl_total_pop
data_final$other_race_pop_per = data_final$other_race_pop / data_final$origianl_total_pop
data_final$two_or_more_races_pop_per = data_final$two_or_more_races_pop / data_final$origianl_total_pop
data_final$not_hispanic_pop_per = data_final$not_hispanic_pop / data_final$origianl_total_pop
