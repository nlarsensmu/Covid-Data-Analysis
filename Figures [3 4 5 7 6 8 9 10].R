library(tidyverse)
library(ggbreak)
library(ggplot2)

data = read.csv("first week of 2022 by county.csv")
print(colnames(data))
data_2021 <- filter(data, year == 2021)
data_2022 <- filter(data, year == 2022)
data_joined <- inner_join(x=data_2021, y=data_2022, by=c("county_fips_code" = "county_fips_code"))
data_joined$first_week_deaths <- data_joined$deaths.y - data_joined$deaths.y
data_joined$first_week_cases <- data_joined$confirmed_cases.y - data_joined$confirmed_cases.x
data_joined$first_week_cases


#figure 3 code
p <- ggplot(data_joined, aes(x=rent_over_50_percent.x, y=first_week_cases)) 
p <- p + geom_point(size=2, colour="darkgreen", shape=1)
p <- p + xlab("Population paying 50% + on Rent")
p <- p + ylab("Number of Cases") + ggtitle("Cases vs Population paying 50% + on Rent by County")
p <- p + scale_y_break(c(75000, 175000), scale=0.1)
p <- p + geom_smooth(method='lm', formula= y~x)
p
ggsave("High Rent vs Cases.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100
)

#figure 4 code 
data2 = read.csv("year of 2021 by month.csv")  %>% arrange(year, month, county_fips_code)

#process to remove the rolling sum
data2_final = filter(data2, year == 2021 | year == 2022) %>% arrange(year, month, county_fips_code)
data2 <- filter(data2, year != 2022) %>% arrange(year, month, county_fips_code)
length(data2_final$county_fips_code)
filter(data2_final, new_cases < 0)$county_fips_code)


data2_final$new_cases <- data2_final$confirmed_cases - data2$confirmed_cases
data2_final$new_deaths <- data2_final$deaths - data2$deaths

#verify the data looks correct, this is just dallas county
filter(data2_final, new_cases < 0) %>% select(new_cases, month, year, total_pop, county_fips_code)

data2_final <- filter(data2_final, year != 2022) %>% 
  filter(new_cases > 0) %>% 
  filter(deaths > 0)
data2_final
data2_final$percent_high_rent <- (
  data2_final$rent_over_50_percent + 
    data2_final$rent_40_to_50_percent +
    data2_final$rent_35_to_40_percent + 
    data2_final$rent_30_to_35_percent) / data2_final$total_pop
data2_final$deaths_per_case <- data2_final$deaths / data2_final$confirmed_cases

#figure 4 final 
p <- ggplot(data2_final, aes(x=percent_high_rent, y=confirmed_cases))
p <- p + geom_point(size=1, shape=1)
p <- p + xlab("Population paying 30% + on Rent")
p <- p + ylab("Number of Cases") + ggtitle("Cases vs Population paying 30% + on Rent by County")
p <- p + geom_smooth(method='lm', formula= y~x)
p
ggsave("Rent Burden vs Cases.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100
)
data2_final$y = data2_final$deaths
data2_final$x = data2_final$confirmed_cases

lm_eqn <- function(df){
  m <- lm(y ~ x, df)
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq))
  
  #b <- format(unname(coef(m)[2]), digits = 2)
  #r2 <- format(summary(m)$r.squared, digits = 3)
  #paste("slope = ",  b)
}

string <- lm_eqn(data2_final)
print(string, parse = TRUE)


#figure 5 code notice a different File name
p <- ggplot(data2_final, aes(x=x, y=y))
p <- p + geom_point(size=2, shape=1)
p <- p + xlab("Number of Cases")
p <- p + ylab("Number of Deaths") + ggtitle("Cases vs Deaths")
p <- p + scale_y_break(c(10000,20000), scale=0.1)
p <- p + geom_smooth(method='lm', formula= y~x)
p <- p + geom_text(x = 1500000, y = 1200, label = string, parse = FALSE)
p

ggsave("Cases vs Deaths.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100
)

data2 <- data
data2$percent_paying_large_rent = (data2$rent_40_to_50_percent + data2$rent_over_50_percent 
                                   + data2$rent_35_to_40_percent
                                   + data2$rent_30_to_35_percent) / data2$total_pop


#figure 7 code
data2 <- data2_final
data2$low_income_percentage = (data2$income_less_10000
                            + data2$income_10000_14999
                            + data2$income_15000_19999) / (data2_final$total_pop)
min(data2$low_income_percentage)
data2$low_income_percentage <- data2$low_income_percentage * 100
p <- ggplot(data2, aes(x=low_income_percentage)) + geom_histogram(colour="black", fill="grey") 
p <- p + geom_vline(aes(xintercept=6),
             color="blue", linetype="dashed", size=1)
p <- p + geom_vline(aes(xintercept=9),
                    color="red", linetype="dashed", size=1)
p
ggsave("Histogram Low.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100
)


p <- ggplot(data2, aes(x=confirmed_cases, y=deaths))
p <- p + geom_point(size=2, shape=1)
p <- p + xlab("Number of Cases")
p <- p + ylab("Number of Deaths") + ggtitle("Cases vs Deaths")
p


#figure 6 code

p <- ggplot(data2, aes(confirmed_cases, deaths)) + 
  geom_point(aes(colour = cut(low_income_percentage, c(-Inf, 6, 9, Inf)),, alpha=I(0.5)),
             size = 1) +
  scale_color_manual(name = "Low Income Percentage",
                     values = c("(-Inf,6]" = "red",
                                "(6,9]" = "blue",
                                "(9, Inf]" = "green"),
                     labels = c("<= 6%", "6% < low_income_percentage <= 9%", "> 9%"))
p <- p + xlab("Number of Cases")
p <- p + ylab("Number of Deaths") + ggtitle("Cases vs Deaths")
p
ggsave("Cases vs Deaths Color as income.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100
)
#figure 8 code
data2 <-  data2_final
data2 <-  data2 %>% group_by(county_fips_code, white_pop, black_pop, 
                             hispanic_pop, amerindian_pop, other_race_pop, total_pop) %>% summarize(

data2$white_pop <- data2$white_pop / data2$total_pop
data2$hispanic_pop <- data2$hispanic_pop / data2$total_pop
data2$amerindian_pop <- data2$amerindian_pop / data2$total_pop
data2$other_race_pop <- data2$other_race_pop / data2$total_pop
data2$black_pop <- data2$black_pop / data2$total_pop
data2$white_pop

p <- ggplot(data2, aes(x=white_pop)) + geom_histogram(colour="black", fill="blue")  
p <- p + ggplot(data2, aes(x=hispanic_pop)) + geom_histogram(colour="black", fill="red") + ggtitle("Demographic Histograms")
p <- p + ggplot(data2, aes(x=amerindian_pop)) + geom_histogram(colour="black", fill="green") 
p <- p + ggplot(data2, aes(x=other_race_pop)) + geom_histogram(colour="black", fill="purple") 
p <- p + ggplot(data2, aes(x=black_pop)) + geom_histogram(colour="black", fill="yellow") 
 
#p <- p + labs(caption = "Figure 8")
p

ggsave("Many histograms of Demographic.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100
)
length(data2$white_pop)
demo_data <- data.frame(values = c(data2$white_pop,                    
                                   data2$hispanic_pop,
                                   data2$amerindian_pop,
                                   data2$black_pop,
                                   data2$other_race_pop),
                   group = c(rep("White %", 36331),
                             rep("Hispanic %", 36331),
                             rep("American Indian %", 36331),
                             rep("Black %", 36331),
                             rep("Other %", 36331)))

#figure 9 code
data2$minority_pop <- data2$black_pop + data2$amerindian_pop + data2$hispanic_pop
p <- ggplot(data2, aes(x=minority_pop)) + geom_histogram(colour="black", fill="blue")  
p
data2$death_rate <- data2$deaths / data2$total_pop
p <- ggplot(data2, aes(x=minority_pop, y=death_rate))
p <- p + geom_point(size=2, shape=1, colour="darkgreen")
p <- p + xlab("Minority Population")
p <- p + ylab("Death Rate") + ggtitle("Minority Population vs Death Rate")
p <- p + geom_smooth(method = "lm", se = FALSE)
p <- p + scale_y_break(c(0.02, 0.8), scale=0.1)
p
ggsave("Minority vs Death Rate.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100
)

#figure 10 code
p <- ggplot(data2, aes(x=white_pop, y=death_rate))
p <- p + geom_point(size=2, shape=1, colour="darkgreen")
p <- p + xlab("White Population")
p <- p + ylab("Death Rate") + ggtitle("White Population vs Death Rate")
p <- p + geom_smooth(method = "lm", se = FALSE)
p <- p + scale_y_break(c(0.02, 0.08), scale=0.1)
p
ggsave("White vs Death Rate.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100
)

