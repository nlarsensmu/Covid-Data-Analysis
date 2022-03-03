library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
library("gridExtra")
library("knitr")
library("ggpubr")

cases <- read_csv(".\\data\\COVID-19-20220207.csv")

# I would like to drill down into this graph to see what all is really in the big bin
ggplot(cases, mapping = aes(confirmed_cases)) + geom_histogram(bins = 20)
ggsave(".\\charts\\1HistCountiesByCase20bins.pdf")

ggplot(filter(cases, confirmed_cases < 100000), mapping=aes(confirmed_cases)) + geom_histogram(bins=20)
ggsave(".\\charts\\1HistCountiesByCase20bins-zoomed.pdf")

ggplot(filter(cases, confirmed_cases > 100000), mapping=aes(confirmed_cases)) + geom_histogram(bins=40)
ggsave(".\\charts\\1HistCountiesByCase20bins-zoomed2.pdf")


# Lets look at missing data for the following columns
plot.new
deaths_cases_per_county <- read.csv(".\\data\\query1_02_23_22.csv")
ggplot(deaths_cases_per_county, mapping = aes(deaths)) + 
  geom_histogram(bins = 20) + 
  labs(caption = "Figure 1") +
  ggtitle("Historgram of County Deaths")
ggsave(".\\charts\\1HistorgramOfCountyDeaths.jpeg")


deaths_cases_per_county["deaths_by_pop"] = deaths_cases_per_county$deaths / (deaths_cases_per_county$total_pop / 10000)
ggplot(deaths_cases_per_county, mapping = aes(deaths_by_pop)) + 
  geom_histogram(bins = 20) + 
  labs(caption = "Figure 1") +
  ggtitle("Historgram of County Deaths per 10,000 People")
ggsave(".\\charts\\1HistorgramOfCountyDeathsPer10000.jpeg")


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



covid_data_2021_by_month <- read_csv("./data/2020_2021_deaths_county_month.csv")
# create a tibble that is offset by one month
data_offset <- filter(covid_data_2021_by_month, year == 2020 | (year == 2021 & month <= 11))
data_final <- filter(covid_data_2021_by_month, (year == 2020 & month >= 2) | year == 2021)
test <- select(data_offset, month, year) %>% unique()
test[1, 1:2]
test[23, 1:2]
test <- select(data_final, month, year) %>% unique()
test[1, 1:2]
test[23, 1:2]
# Now we have a tibble that is offest from data_final, if we subtract cases
# and deaths we can get the new cases for each month
data_final$new_cases <- data_final$confirmed_cases - data_offset$confirmed_cases
data_final$new_deaths <- data_final$deaths - data_offset$deaths


data_final["cases_per_pop"] = data_final$confirmed_cases / (data_final$total_pop)
data_final["deaths_per_pop"] = data_final$deaths / (data_final$total_pop)
data_final["new_cases_per_pop"] = data_final$new_cases / (data_final$total_pop)
data_final["new_deaths_per_pop"] = data_final$new_deaths / (data_final$total_pop)
data_final["month_count"] = data_final$month + (data_final$year - 2020)*12 - 2
# Lets plot Harris, dallas, tarrant, bexar, and travis countines in TX on one plot

dallas_data <- data_final %>% 
  filter(data_final$county_name == "Dallas County" & data_final$state == "TX")

harris_data <- data_final %>% 
  filter(data_final$county_name == "Harris County" & data_final$state == "TX")

tarrant_data <- data_final %>% 
  filter(data_final$county_name == "Tarrant County" & data_final$state == "TX")

bexar_data <- data_final %>% 
  filter(data_final$county_name == "Bexar County" & data_final$state == "TX")

travis_data <- data_final %>% 
  filter(data_final$county_name == "Travis County" & data_final$state == "TX")


# Plot the top 5 counites cases
p1 <- ggplot(dallas_data, mapping = aes(month_count, y = cases, color = County)) + 
  geom_point(aes(y = dallas_data$cases_per_pop, col = "Dallas County")) +
  geom_point(aes(y = harris_data$cases_per_pop, col = "Harris County")) +
  geom_point(aes(y = tarrant_data$cases_per_pop, col = "Tarrant County")) +
  geom_point(aes(y = bexar_data$cases_per_pop, col = "Bexar County")) +
  geom_point(aes(y = travis_data$cases_per_pop, col = "Travis County")) +
  ylab("Cases per population") + xlab("Months since Fen 2020") +
  ggtitle("Total Cases by Month for top 5 Counties")

p2 <- ggplot(dallas_data, mapping = aes(month_count, y = cases, color = County)) + 
  geom_point(aes(y = dallas_data$new_cases_per_pop, col = "Dallas County")) +
  geom_point(aes(y = harris_data$new_cases_per_pop, col = "Harris County")) +
  geom_point(aes(y = tarrant_data$new_cases_per_pop, col = "Tarrant County")) +
  geom_point(aes(y = bexar_data$new_cases_per_pop, col = "Bexar County")) +
  geom_point(aes(y = travis_data$new_cases_per_pop, col = "Travis County")) +
  ylab("Cases per population") + xlab("Months since Feb 2020") +
  ggtitle("New Cases per Month for top 5 Counties")

p <- ggarrange(p1, p2, ncol = 1) +
  ggtitle("Distribution of Rent Burden") + 
  labs(caption = "Figure XX")
annotate_figure(p, top = text_grob("Cases Trends Since Feb 2020", 
                                   color = "Black", face = "bold", size = 14),
                bottom = text_grob("Figure XX", size = 10))

ggsave(".\\charts\\cases_trend.png", width = 6.5, height = 7)

# Plot the top 5 counites deaths
p1 <- ggplot(dallas_data, mapping = aes(month_count, y = cases, color = County)) + 
  geom_point(aes(y = dallas_data$deaths_per_pop, col = "Dallas County")) +
  geom_point(aes(y = harris_data$deaths_per_pop, col = "Harris County")) +
  geom_point(aes(y = tarrant_data$deaths_per_pop, col = "Tarrant County")) +
  geom_point(aes(y = bexar_data$deaths_per_pop, col = "Bexar County")) +
  geom_point(aes(y = travis_data$deaths_per_pop, col = "Travis County")) +
  ylab("Cases per population") + xlab("Months since Fen 2020") +
  ggtitle("Total Cases by Month for top 5 Counties")

p2 <- ggplot(dallas_data, mapping = aes(month_count, y = cases, color = County)) + 
  geom_point(aes(y = dallas_data$new_deaths_per_pop, col = "Dallas County")) +
  geom_point(aes(y = harris_data$new_deaths_per_pop, col = "Harris County")) +
  geom_point(aes(y = tarrant_data$new_deaths_per_pop, col = "Tarrant County")) +
  geom_point(aes(y = bexar_data$new_deaths_per_pop, col = "Bexar County")) +
  geom_point(aes(y = travis_data$new_deaths_per_pop, col = "Travis County")) +
  ylab("Cases per population") + xlab("Months since Feb 2020") +
  ggtitle("New Cases per Month for top 5 Counties")

p <- ggarrange(p1, p2, ncol = 1) +
  ggtitle("Distribution of Rent Burden") + 
  labs(caption = "Figure XX")
annotate_figure(p, top = text_grob("Deaths Trends Since Feb 2020", 
                                   color = "Black", face = "bold", size = 14),
                bottom = text_grob("Figure XX", size = 10))

ggsave(".\\charts\\deaths_trend.png", width = 6.5, height = 7)


# Lets plot the top 3 middle 3 and bottom 3 of entire US.
sorted <- arrange(data_final, total_pop)
numner_of_records = count(sorted)

pops <- unique(sorted$total_pop)
pops_count <- length(pops)
middle_1 = pops[pops_count/2-1]
middle_2 = pops[pops_count/2]
middle_3 = pops[pops_count/2+1]
lower_1 = pops[1]
lower_2 = pops[2]
lower_3 = pops[3]
top_1 = pops[pops_count]
top_2 = pops[pops_count-1]
top_3 = pops[pops_count-2]

middle_1_county <- unique(filter(data_final, total_pop == middle_1)$county_name)
middle_1_state <- unique(filter(data_final, total_pop == middle_1)$state)
middle_2_county <- unique(filter(data_final, total_pop == middle_2)$county_name)
middle_2_state <- unique(filter(data_final, total_pop == middle_2)$state)
middle_3_county <- unique(filter(data_final, total_pop == middle_3)$county_name)
middle_3_state <- unique(filter(data_final, total_pop == middle_3)$state)

lower_1_county <- unique(filter(data_final, total_pop == lower_1)$county_name)
lower_1_state <- unique(filter(data_final, total_pop == lower_1)$state)
lower_2_county <- unique(filter(data_final, total_pop == lower_2)$county_name)
lower_2_state <- unique(filter(data_final, total_pop == lower_2)$state)
lower_3_county <- unique(filter(data_final, total_pop == lower_3)$county_name)
lower_3_state <- unique(filter(data_final, total_pop == lower_3)$state)

top_1_county <- unique(filter(data_final, total_pop == top_1)$county_name)
top_1_state <- unique(filter(data_final, total_pop == top_1)$state)
top_2_county <- unique(filter(data_final, total_pop == top_2)$county_name)
top_2_state <- unique(filter(data_final, total_pop == top_2)$state)
top_3_county <- unique(filter(data_final, total_pop == top_3)$county_name)
top_3_state <- unique(filter(data_final, total_pop == top_3)$state)

ggplot(dallas_data, mapping = aes(month_count, y = cases, color = County)) + 
  geom_point(aes(y = 
                   filter(data_final, 
                          data_final$county_name ==  top_1_county&
                            data_final$state == top_1_state)$new_cases,
                 col = top_1_county)) +
  geom_point(aes(y = 
                   filter(data_final, 
                          data_final$county_name ==  top_2_county&
                            data_final$state == top_2_state)$new_cases,
                 col = top_2_county)) +
  geom_point(aes(y = 
                   filter(data_final, 
                          data_final$county_name ==  top_3_county&
                            data_final$state == top_3_state)$new_cases,
                 col = top_3_county)) +
  ylab("Cases per Population") + xlab("Months since 2020")  +
  ggtitle("Highest 3 Counties New Cases per Population")
ggsave(".\\charts\\highest3counties.png")

#plot together, dallas county is just a stand in for the right size of data.
ggplot(dallas_data, mapping = aes(month_count, y = cases, color = County)) + 
  geom_point(aes(y = 
                   filter(data_final, 
                          data_final$county_name ==  middle_1_county&
                            data_final$state == middle_1_state)$new_cases,
                 col = middle_1_county))  + 
  geom_point(aes(y = 
                   filter(data_final, 
                          data_final$county_name ==  middle_2_county&
                            data_final$state == middle_2_state)$new_cases,
                 col = middle_2_county))  + 
  geom_point(aes(y = 
                   filter(data_final, 
                          data_final$county_name ==  middle_3_county&
                            data_final$state == middle_3_state)$new_cases,
                 col = middle_3_county)) +ylab("New Cases per Population") + xlab("Months since 2020") +
  ggtitle("Median 3 Counties New Cases per Population")
ggsave(".\\charts\\median3Counties.png")

ggplot(dallas_data, mapping = aes(month_count, y = cases, color = County)) + 
  geom_point(aes(y = 
                   filter(data_final, 
                          data_final$county_name ==  lower_1_county&
                            data_final$state == lower_1_state)$new_cases,
                 col = lower_1_county)) +
  geom_point(aes(y = 
                   filter(data_final, 
                          data_final$county_name ==  lower_2_county&
                            data_final$state == lower_2_state)$new_cases,
                 col = lower_2_county)) +
  geom_point(aes(y = 
                   filter(data_final, 
                          data_final$county_name ==  lower_3_county&
                            data_final$state == lower_3_state)$new_cases,
                 col = lower_3_county))+ylab("Cases per Population") + xlab("Months since 2020")  +
  ggtitle("Lowest 3 Counties New Cases per Population")
ggsave(".\\charts\\lowest3counties.png")


# prepare cases to be what we need.

#normalize population
ggplot(data_final, mapping = aes(total_pop)) + geom_histogram(bins = 10, binwidth = 100000)  +
  ggtitle("Highest 3 Counties Cases per Million People") + 
  labs(caption = "Figure XX") + 
  ggtitle("Population Distribution")
ggsave(".\\charts\\populationHistorgram.jpeg")
data_final$origianl_total_pop = data_final$total_pop
data_final <- data_final %>% mutate_at(c("total_pop"), ~(scale(.) %>% as.vector))

mean(data_final$total_pop)
sd(data_final$total_pop)


ggplot(v, mapping = aes(total_pop)) + geom_histogram(binwidth = .5, bins = 10)+ 
  labs(caption = "Figure XX") + 
  ggtitle("Normalized Population Distribution")
ggsave(".\\charts\\populationHistorgramNorm.jpeg")

data_final["rent_under_10_percent_new"] = data_final$rent_under_10_percent / data_final$origianl_total_pop
data_final["rent_10_to_15_percent_new"] = data_final$rent_10_to_15_percent / data_final$origianl_total_pop
data_final["rent_15_to_20_percent_new"] = data_final$rent_15_to_20_percent / data_final$origianl_total_pop
data_final["rent_20_to_25_percent_new"] = data_final$rent_20_to_25_percent / data_final$origianl_total_pop
data_final["rent_25_to_30_percent_new"] = data_final$rent_25_to_30_percent / data_final$origianl_total_pop
data_final["rent_30_to_35_percent_new"] = data_final$rent_30_to_35_percent / data_final$origianl_total_pop
data_final["rent_35_to_40_percent_new"] = data_final$rent_35_to_40_percent / data_final$origianl_total_pop
data_final["rent_40_to_50_percent_new"] = data_final$rent_40_to_50_percent / data_final$origianl_total_pop
data_final["rent_over_50_percent_new"] = data_final$rent_over_50_percent / data_final$origianl_total_pop

# Impute rent costs to over_40 and under_40
p1 <- ggplot(data_final, mapping = aes(rent_under_10_percent_new)) + geom_boxplot() + 
  ggtitle("Renters Under 10% of Income")
p2 <- ggplot(data_final, mapping = aes(rent_10_to_15_percent_new)) + geom_boxplot() + 
  ggtitle("Renters Under 10%-15% of Income")
p3 <- ggplot(data_final, mapping = aes(rent_15_to_20_percent_new)) + geom_boxplot() + 
  ggtitle("Renters Under 15%-20% of Income")
p4 <- ggplot(data_final, mapping = aes(rent_20_to_25_percent_new)) + geom_boxplot() + 
  ggtitle("Renters Under 20%-25% of Income")
p5 <- ggplot(data_final, mapping = aes(rent_25_to_30_percent_new)) + geom_boxplot() + 
  ggtitle("Renters Under 25%-30% of Income")
p6 <- ggplot(data_final, mapping = aes(rent_30_to_35_percent_new)) + geom_boxplot() + 
  ggtitle("Renters Under 30%-35% of Income")
p7 <- ggplot(data_final, mapping = aes(rent_35_to_40_percent_new)) + geom_boxplot() + 
  ggtitle("Renters Under 35%-40% of Income")
p8 <- ggplot(data_final, mapping = aes(rent_40_to_50_percent_new)) + geom_boxplot() + 
  ggtitle("Renters Under 40%-50% of Income")
p9 <- ggplot(data_final, mapping = aes(rent_over_50_percent_new)) + geom_boxplot() + 
  ggtitle("Renters Over 50%of Income")

p <- ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9) +
  ggtitle("Distribution of Rent Burden") + 
  labs(caption = "Figure XX")
p
annotate_figure(p, top = text_grob("Distribution of Rent Burden", 
                                      color = "Black", face = "bold", size = 14),
                bottom = text_grob("Figure XX", size = 10))
ggsave(".\\charts\\rentBurden.jpeg")

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

p1 <- ggplot(data_final, mapping = aes(severe_burden)) + geom_boxplot() + 
  ggtitle("Renters Paying 50% or more on rent")
p2<- ggplot(data_final, mapping = aes(rent_burden)) + geom_boxplot() + 
  ggtitle("Renters Paying 30%-50% or more on rent")
p3<- ggplot(data_final, mapping = aes(no_rent_burden)) + geom_boxplot() + 
  ggtitle("Renters Paying 30% or less on rent")

p <- ggarrange(p1, p2, p3)
annotate_figure(p, top = text_grob("Distribution of rent Burdens by Classes", 
                                   color = "Black", face = "bold", size = 14),
                bottom = text_grob("Figure XX", size = 10))

ggsave(".\\charts\\DistributionRentBurdenClass.png")
