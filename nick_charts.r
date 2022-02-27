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



covid_data_2021_by_month <- read_csv("./data/2021_deaths_county_month.csv")
# Here we want to get the change in cases/deaths per month
data_2021 <- filter(covid_data_2021_by_month, year == 2021)
data_2022 <- filter(covid_data_2021_by_month, year == 2022)
data_joined <- inner_join(x=data_2021, y=data_2022, by=c("county_fips_code" = "county_fips_code"))
data_joined$first_week_deaths <- data_joined$deaths.y - data_joined$deaths.y
data_joined$first_week_cases <- data_joined$confirmed_cases.y - data_joined$confirmed_cases.x
data_joined$first_week_cases
data_joined


covid_data_2021_by_month["cases_per_mil"] = covid_data_2021_by_month$confirmed_cases / (covid_data_2021_by_month$total_pop / 1000000)
covid_data_2021_by_month["deaths_per_mil"] = covid_data_2021_by_month$deaths / (covid_data_2021_by_month$total_pop / 1000000)
covid_data_2021_by_month["month_count"] = covid_data_2021_by_month$month + (covid_data_2021_by_month$year - 2020)*12 - 12
# Lets plot Harris, dallas, tarrant, bexar, and travis countines in TX on one plot

dallas_data <- covid_data_2021_by_month %>% 
  filter(covid_data_2021_by_month$county_name == "Dallas County" & covid_data_2021_by_month$state == "TX")

harris_data <- covid_data_2021_by_month %>% 
  filter(covid_data_2021_by_month$county_name == "Harris County" & covid_data_2021_by_month$state == "TX")

tarrant_data <- covid_data_2021_by_month %>% 
  filter(covid_data_2021_by_month$county_name == "Tarrant County" & covid_data_2021_by_month$state == "TX")

bexar_data <- covid_data_2021_by_month %>% 
  filter(covid_data_2021_by_month$county_name == "Bexar County" & covid_data_2021_by_month$state == "TX")

travis_data <- covid_data_2021_by_month %>% 
  filter(covid_data_2021_by_month$county_name == "Travis County" & covid_data_2021_by_month$state == "TX")


# Plot the top 5 counites
ggplot(dallas_data, mapping = aes(month_count, y = cases, color = County)) + 
  geom_point(aes(y = dallas_data$cases_per_mil, col = "Dallas County")) +
  geom_point(aes(y = harris_data$cases_per_mil, col = "Harris County")) +
  geom_point(aes(y = tarrant_data$cases_per_mil, col = "Tarrant County")) +
  geom_point(aes(y = bexar_data$cases_per_mil, col = "Bexar County")) +
  geom_point(aes(y = travis_data$cases_per_mil, col = "Travis County")) +
  labs(caption = "Figure 1") + ylab("Cases per Million") + xlab("Months since 2020") +
  ggtitle("Cases per Month for top 5 Counties")
ggsave(".\\charts\\cases_per_month_by_county.jpeg")

ggplot(dallas_data, mapping = aes(month_count, y = cases, color = County)) + 
  geom_point(aes(y = dallas_data$deaths_per_mil, col = "Dallas County")) +
  geom_point(aes(y = harris_data$deaths_per_mil, col = "Harris County")) +
  geom_point(aes(y = tarrant_data$deaths_per_mil, col = "Tarrant County")) +
  geom_point(aes(y = bexar_data$deaths_per_mil, col = "Bexar County")) +
  geom_point(aes(y = travis_data$deaths_per_mil, col = "Travis County")) +
  labs(caption = "Figure 1") + ylab("Deaths per Million") + xlab("Months since 2020") +
  ggtitle("Deaths per Month for top 5 Counties")
ggsave(".\\charts\\deaths_per_month_by_county.jpeg")

# Lets plot the top 3 middle 3 and bottom 3
sorted <- arrange(covid_data_2021_by_month, total_pop)
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

middle_1_county <- unique(filter(covid_data_2021_by_month, total_pop == middle_1)$county_name)
middle_1_state <- unique(filter(covid_data_2021_by_month, total_pop == middle_1)$state)
middle_2_county <- unique(filter(covid_data_2021_by_month, total_pop == middle_2)$county_name)
middle_2_state <- unique(filter(covid_data_2021_by_month, total_pop == middle_2)$state)
middle_3_county <- unique(filter(covid_data_2021_by_month, total_pop == middle_3)$county_name)
middle_3_state <- unique(filter(covid_data_2021_by_month, total_pop == middle_3)$state)

lower_1_county <- unique(filter(covid_data_2021_by_month, total_pop == lower_1)$county_name)
lower_1_state <- unique(filter(covid_data_2021_by_month, total_pop == lower_1)$state)
lower_2_county <- unique(filter(covid_data_2021_by_month, total_pop == lower_2)$county_name)
lower_2_state <- unique(filter(covid_data_2021_by_month, total_pop == lower_2)$state)
lower_3_county <- unique(filter(covid_data_2021_by_month, total_pop == lower_3)$county_name)
lower_3_state <- unique(filter(covid_data_2021_by_month, total_pop == lower_3)$state)

top_1_county <- unique(filter(covid_data_2021_by_month, total_pop == top_1)$county_name)
top_1_state <- unique(filter(covid_data_2021_by_month, total_pop == top_1)$state)
top_2_county <- unique(filter(covid_data_2021_by_month, total_pop == top_2)$county_name)
top_2_state <- unique(filter(covid_data_2021_by_month, total_pop == top_2)$state)
top_3_county <- unique(filter(covid_data_2021_by_month, total_pop == top_3)$county_name)
top_3_state <- unique(filter(covid_data_2021_by_month, total_pop == top_3)$state)

test <- filter(covid_data_2021_by_month, 
       covid_data_2021_by_month$county_name ==  middle_2_county&
         covid_data_2021_by_month$state == middle_2_state)
#plot together, dallas county is just a stand in for the right size of data.
ggplot(dallas_data, mapping = aes(month_count, y = cases, color = County)) + 
  geom_point(aes(y = 
                   filter(covid_data_2021_by_month, 
                          covid_data_2021_by_month$county_name ==  middle_1_county&
                            covid_data_2021_by_month$state == middle_1_state)$cases_per_mil,
                 col = middle_1_county))  + 
  geom_point(aes(y = 
                   filter(covid_data_2021_by_month, 
                          covid_data_2021_by_month$county_name ==  middle_2_county&
                            covid_data_2021_by_month$state == middle_2_state)$cases_per_mil,
                 col = middle_2_county))  + 
  geom_point(aes(y = 
                   filter(covid_data_2021_by_month, 
                          covid_data_2021_by_month$county_name ==  middle_3_county&
                            covid_data_2021_by_month$state == middle_3_state)$cases_per_mil,
                 col = middle_3_county)) +ylab("Cases per Million") + xlab("Months since 2020") +
  ggtitle("Median 3 Counties Cases per Million People")
ggsave(".\\charts\\median3Counties.jpeg")

test <- filter(covid_data_2021_by_month, 
               covid_data_2021_by_month$county_name ==  lower_1_county&
                 covid_data_2021_by_month$state == lower_1_state)
test$cases_per_mil
test$total_pop

ggplot(dallas_data, mapping = aes(month_count, y = cases, color = County)) + 
  geom_point(aes(y = 
                   filter(covid_data_2021_by_month, 
                          covid_data_2021_by_month$county_name ==  lower_1_county&
                            covid_data_2021_by_month$state == lower_1_state)$cases_per_mil,
                 col = lower_1_county)) +
  geom_point(aes(y = 
                   filter(covid_data_2021_by_month, 
                          covid_data_2021_by_month$county_name ==  lower_2_county&
                            covid_data_2021_by_month$state == lower_2_state)$cases_per_mil,
                 col = lower_2_county)) +
  geom_point(aes(y = 
                   filter(covid_data_2021_by_month, 
                          covid_data_2021_by_month$county_name ==  lower_3_county&
                            covid_data_2021_by_month$state == lower_3_state)$cases_per_mil,
                 col = lower_3_county))+ylab("Cases per Million") + xlab("Months since 2020")  +
  ggtitle("Lowest 3 Counties Cases per Million People")
ggsave(".\\charts\\lowest3counties.jpeg")


ggplot(dallas_data, mapping = aes(month_count, y = cases, color = County)) + 
  geom_point(aes(y = 
                   filter(covid_data_2021_by_month, 
                          covid_data_2021_by_month$county_name ==  top_1_county&
                            covid_data_2021_by_month$state == top_1_state)$cases_per_mil,
                 col = top_1_county)) +
  geom_point(aes(y = 
                   filter(covid_data_2021_by_month, 
                          covid_data_2021_by_month$county_name ==  top_2_county&
                            covid_data_2021_by_month$state == top_2_state)$cases_per_mil,
                 col = top_2_county)) +
  geom_point(aes(y = 
                   filter(covid_data_2021_by_month, 
                          covid_data_2021_by_month$county_name ==  top_3_county&
                            covid_data_2021_by_month$state == top_3_state)$cases_per_mil,
                 col = top_3_county)) +
  ylab("Cases per Million") + xlab("Months since 2020")  +
  ggtitle("Highest 3 Counties Cases per Million People")
ggsave(".\\charts\\highest3counties.jpeg")


# prepare cases to be what we need.

#normalize population
ggplot(cases, mapping = aes(total_pop)) + geom_histogram(bins = 10, binwidth = 100000)  +
  ggtitle("Highest 3 Counties Cases per Million People") + 
  labs(caption = "Figure XX") + 
  ggtitle("Population Distribution")
ggsave(".\\charts\\populationHistorgram.jpeg")
cases_new <- cases %>% mutate_at(c("total_pop"), ~(scale(.) %>% as.vector))

mean(cases_new$total_pop)
sd(cases_new$total_pop)


ggplot(cases_new, mapping = aes(total_pop)) + geom_histogram(binwidth = .5, bins = 10)+ 
  labs(caption = "Figure XX") + 
  ggtitle("Normalized Population Distribution")
ggsave(".\\charts\\populationHistorgramNorm.jpeg")

cases_new["rent_under_10_percent_new"] = cases$rent_under_10_percent / cases$total_pop
cases_new["rent_10_to_15_percent_new"] = cases$rent_10_to_15_percent / cases$total_pop
cases_new["rent_15_to_20_percent_new"] = cases$rent_15_to_20_percent / cases$total_pop
cases_new["rent_20_to_25_percent_new"] = cases$rent_20_to_25_percent / cases$total_pop
cases_new["rent_25_to_30_percent_new"] = cases$rent_25_to_30_percent / cases$total_pop
cases_new["rent_30_to_35_percent_new"] = cases$rent_30_to_35_percent / cases$total_pop
cases_new["rent_35_to_40_percent_new"] = cases$rent_35_to_40_percent / cases$total_pop
cases_new["rent_40_to_50_percent_new"] = cases$rent_40_to_50_percent / cases$total_pop
cases_new["rent_over_50_percent_new"] = cases$rent_over_50_percent / cases$total_pop

# Impute rent costs to over_40 and under_40
p1 <- ggplot(cases_new, mapping = aes(rent_under_10_percent_new)) + geom_boxplot() + 
  ggtitle("Renters Under 10% of Income")
p2 <- ggplot(cases_new, mapping = aes(rent_10_to_15_percent_new)) + geom_boxplot() + 
  ggtitle("Renters Under 10%-15% of Income")
p3 <- ggplot(cases_new, mapping = aes(rent_15_to_20_percent_new)) + geom_boxplot() + 
  ggtitle("Renters Under 15%-20% of Income")
p4 <- ggplot(cases_new, mapping = aes(rent_20_to_25_percent_new)) + geom_boxplot() + 
  ggtitle("Renters Under 20%-25% of Income")
p5 <- ggplot(cases_new, mapping = aes(rent_25_to_30_percent_new)) + geom_boxplot() + 
  ggtitle("Renters Under 25%-30% of Income")
p6 <- ggplot(cases_new, mapping = aes(rent_30_to_35_percent_new)) + geom_boxplot() + 
  ggtitle("Renters Under 30%-35% of Income")
p7 <- ggplot(cases_new, mapping = aes(rent_35_to_40_percent_new)) + geom_boxplot() + 
  ggtitle("Renters Under 35%-40% of Income")
p8 <- ggplot(cases_new, mapping = aes(rent_40_to_50_percent_new)) + geom_boxplot() + 
  ggtitle("Renters Under 40%-50% of Income")
p9 <- ggplot(cases_new, mapping = aes(rent_over_50_percent_new)) + geom_boxplot() + 
  ggtitle("Renters Over 50%of Income")

p <- ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9) +
  ggtitle("Distribution of Rent Burden") + 
  labs(caption = "Figure XX")

annotate_figure(p, top = text_grob("Distribution of Rent Burden", 
                                      color = "Black", face = "bold", size = 14),
                bottom = text_grob("Figure XX", size = 10))
ggsave(".\\charts\\rentBurden.jpeg")
