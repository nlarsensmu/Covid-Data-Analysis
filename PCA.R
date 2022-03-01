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
covid_data_final$deaths_rate <- ifelse(
  covid_data_final$confirmed_cases != 0,
  covid_data_final$deaths / covid_data_final$confirmed_cases, 0)


colnames(covid_data_final)
select_data <- select(covid_data_final,
       rent_over_50_percent,
       rent_40_to_50_percent,
       rent_35_to_40_percent,
       rent_30_to_35_percent,
       total_pop,
       white_pop,
       black_pop,
       asian_pop,
       hispanic_pop,
       amerindian_pop,
       other_race_pop,
       commuters_by_public_transportation,
       poverty)


pca <- prcomp(select_data, scale. = TRUE)
print(pca)
pca_1_2 <- data.frame(pca$x[, 1:2])
plot(pca$x[,1], pca$x[,2])

PC1 <- pca$rotation[,1]
PC1_scores <- abs(PC1)
PC1_scores_ordered <- sort(PC1_scores, decreasing = TRUE)
names(PC1_scores_ordered)

pca_var <- pca$sdev^2
pca_var_perc <- round(pca_var/sum(pca_var) * 100, 1)
barplot(pca_var_perc, main = "Variation Plot", xlab = "PCs", ylab = "Percentage Variance", ylim = c(0, 100))



select_data_with_PCA <-  select_data
select_data_with_PCA$PC1 <- data.frame(pca$x)$PC1
select_data_with_PCA$PC2 <- data.frame(pca$x)$PC2
select_data_with_PCA$PC3 <- data.frame(pca$x)$PC3
select_data_with_PCA$deaths_rate <- covid_data_final$deaths_rate
select_data_with_PCA$confirmed_cases_per <- covid_data_final$confirmed_cases_per

p <- ggplot(select_data_with_PCA, aes(x=deaths_rate)) + 
  geom_histogram(colour="black", fill="grey",
                 breaks = c(0,0.01,0.0125,0.015,0.0175,0.02,0.025,0.0325,0.05,0.075))
p

ggsave("Histogram of Death Rate.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100
)

select_data_with_PCA$deaths_rate_catagorical <- 
  ifelse(select_data_with_PCA$deaths_rate < 0.015, "yellow",
         ifelse(select_data_with_PCA$deaths_rate < 0.02, "orange", "red"))
select_data_with_PCA$deaths_rate_catagorical <- as.factor(select_data_with_PCA$deaths_rate_catagorical)

p <- ggplot(select_data_with_PCA, aes(x=confirmed_cases_per)) + 
  geom_histogram(colour="black", fill="grey",
                 breaks = c(0,0.05,0.075,0.1,0.15,0.2,0.3))
p
ggsave("Histogram Cases as a Percentage.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100
)
select_data_with_PCA$confirmed_cases_per_catagorical <- 
  ifelse(select_data_with_PCA$confirmed_cases_per < 0.1, "yellow",
         ifelse(select_data_with_PCA$confirmed_cases_per < 0.2, "orange", "red"))




temp <- select_data_with_PCA
p1 <- ggplot(temp, aes(x=PC1, y=PC2)) + 
  geom_point(colour=temp$deaths_rate_catagorical, alpha = 0.1)
p2 <- ggplot(temp, aes(x=PC1, y=PC2)) + 
  geom_point(colour=temp$deaths_rate_catagorical, alpha = 0.1) + 
  labs(title = "PCA VS Death Rate")
p3 <- ggplot(temp, aes(x=PC3, y=PC2)) + 
  geom_point(colour=temp$deaths_rate_catagorical, alpha = 0.1) 
p1 + p2 + p3
ggsave("PCA vs Death Rate.png",  plot = (p1 + p2 + p3),  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100
)


temp <- select_data_with_PCA
p1 <- ggplot(temp, aes(x=PC1, y=PC2)) + 
  geom_point(colour=temp$confirmed_cases_per_catagorical)
p2 <- ggplot(temp, aes(x=PC1, y=PC2)) + 
  geom_point(colour=temp$confirmed_cases_per_catagorical, alpha = 0.1) + 
  labs(title = "PCA VS Cases as a Percentage")
p3 <- ggplot(temp, aes(x=PC3, y=PC2)) + 
  geom_point(colour=temp$confirmed_cases_per_catagorical, alpha = 0.1)
p1 + p2 + p3
ggsave("PCA vs Cases.png",  plot = (p1 + p2 + p3),  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100
)
