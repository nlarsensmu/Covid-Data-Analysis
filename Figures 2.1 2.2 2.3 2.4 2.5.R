county_data = read.csv("county_data.csv")
county_data$white_per = county_data$white_pop/county_data$total_pop
county_data$black_per = county_data$black_pop/county_data$total_pop
county_data$hispanic_per = county_data$hispanic_pop/county_data$total_pop
county_data$asain_per = county_data$asian_including_hispanic /county_data$total_pop
county_data$amerindian_per = county_data$amerindian_pop/county_data$total_pop
county_data$other_per = county_data$other_race_pop/county_data$total_pop


p <- (ggplot(county_data, aes(y=white_per)) + geom_boxplot() + ylab("White %"))
p <- p + (ggplot(county_data, aes(y=black_per)) + geom_boxplot() + ylab("Black %"))
p <- p + (ggplot(county_data, aes(y=hispanic_per)) + geom_boxplot() + ylab("Hispanic %"))
p <- p + (ggplot(county_data, aes(y=asain_per)) + geom_boxplot() + ylab("Asain %"))
p <- p + (ggplot(county_data, aes(y=amerindian_per)) + geom_boxplot() + ylab("American Indian %"))
p <- p + (ggplot(county_data, aes(y=hispanic_per)) + geom_boxplot() + ylab("Other %"))
p <- p + labs(caption = "Figure 2.1") 
ggsave("Figure 2.1.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100
)
p

get_stats <- function(data) {
  print(mean(data))
  print(median(data))
  print(range(data))
  print(sd(data))
}

get_stats(county_data$white_per)
get_stats(county_data$black_per)
get_stats(county_data$hispanic_per)
get_stats(county_data$asain_per)
get_stats(county_data$amerindian_per)
get_stats(county_data$other_per)


covid_data = read.csv("year of 2021 by month.csv")  %>% arrange(year, month, county_fips_code)

#process to remove the rolling sum
covid_data_final = filter(covid_data, year == 2021 | year == 2022) %>% arrange(year, month, county_fips_code)
covid_data <- filter(covid_data, year != 2022) %>% arrange(year, month, county_fips_code)
length(covid_data_final$county_fips_code)
filter(covid_data_final, new_cases < 0)$county_fips_code


covid_data_final$new_cases <- covid_data_final$confirmed_cases - covid_data$confirmed_cases
covid_data_final$new_deaths <- covid_data_final$deaths - covid_data$deaths
covid_data_final <- filter(covid_data_final, year == 2021)

covid_data_final$new_cases_per <- covid_data_final$new_cases / covid_data_final$total_pop
covid_data_final$new_deaths_per <- covid_data_final$new_deaths / covid_data_final$total_pop
covid_data_final$confirmed_cases_per <- covid_data_final$confirmed_cases / covid_data_final$total_pop
covid_data_final$deaths_per <- covid_data_final$deaths / covid_data_final$total_pop

covid_data_final$month
covid_data_final$month <- as.factor(covid_data_final$month)
covid_data_final <- filter(covid_data_final, county_fips_code != '00000')
covid_data_final <- filter(covid_data_final, new_cases_per < 1)
covid_data_final <- filter(covid_data_final, new_cases_per > 0)
covid_data_final <- filter(covid_data_final, new_deaths_per < 1)
covid_data_final <- filter(covid_data_final, new_deaths_per > 0)

p <- (ggplot(covid_data_final, aes(x=month, y=new_cases_per)) + geom_boxplot() + ylab("New Cases"))
p <- p + scale_y_break(c(0.2,0.4))
p <- p + labs(caption = "Figure 2.2")
p
ggsave("Figure 2.2.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100
)


p <- (ggplot(covid_data_final, aes(x=month, y=new_deaths_per)) + geom_boxplot() + ylab("New Deaths"))
p <- p + scale_y_break(c(0.0045,0.089), scales = 0.1)
p <- p + labs(caption = "Figure 2.3")
p
ggsave("Figure 2.3.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100
)

p <- (ggplot(covid_data_final, aes(x=month, y=confirmed_cases_per)) + geom_boxplot() + ylab("Total Cases"))
p <- p + labs(caption = "Figure 2.4")
p
ggsave("Figure 2.4.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100
)

p <- (ggplot(covid_data_final, aes(x=month, y=deaths_per)) + geom_boxplot() + ylab("Total Deaths"))
p <- p + scale_y_break(c(0.02,0.08))
p <- p + labs(caption = "Figure 2.5")
p
ggsave("Figure 2.5.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100
)


(filter(covid_data_final, month == 1)$deaths_per * 100)  %>% get_stats()
(filter(covid_data_final, month == 1)$new_deaths_per * 100)  %>% get_stats()
(filter(covid_data_final, month == 1)$confirmed_cases_per * 100)  %>% get_stats()
(filter(covid_data_final, month == 1)$new_cases_per * 100 ) %>% get_stats()

