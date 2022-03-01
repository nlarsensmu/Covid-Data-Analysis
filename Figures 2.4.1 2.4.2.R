data = read.csv("first week of 2022 by county.csv")
print(colnames(data))
data_2021 <- filter(data, year == 2021)
data_2022 <- filter(data, year == 2022)




cor_TX <- cor(cases_TX_select[,-1])


data_sel <- select(data_2022, confirmed_cases, deaths, total_pop, 
                   median_income, rent_over_50_percent, rent_40_to_50_percent,white_pop)

# Figure 2.4.1 
cor_data_sel <- cor(data_sel)
p <- ggcorrplot(cor_data_sel, p.mat = cor_pmat(data_sel), insig = "blank", hc.order = TRUE)
p <- p + labs(caption = "Figure 2.4.1")
p
ggsave("Figure 2.4.1.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100
)


#Figure 2.4.2
data_sel <- select(data_2022, confirmed_cases, deaths, total_pop, 
                   median_income, 
                   rent_over_50_percent, 
                   rent_40_to_50_percent,
                   white_pop, black_pop, 
                   asian_pop, hispanic_pop, amerindian_pop, other_race_pop,
                   commuters_by_public_transportation
                   )

data_sel$rent_burden <- data_sel$rent_over_50_percent + data_sel$rent_40_to_50_percent
data_sel$minority_pop <- data_sel$black_pop + 
  data_sel$asian_pop + 
  data_sel$amerindian_pop + 
  data_sel$other_race_pop + 
  data_sel$hispanic_pop 

data_sel$rent_burden_per <- data_sel$rent_burden / data_sel$total_pop
data_sel$minority_per <- data_sel$minority_pop / data_sel$total_pop
data_sel$white_per <- data_sel$white_pop / data_sel$total_pop
data_sel$confirmed_cases_per <- data_sel$confirmed_cases / data_sel$total_pop
data_sel$deaths_per <- data_sel$deaths / data_sel$total_pop
data_sel$commuters_per <- data_sel$commuters_by_public_transportation
data_sel$deaths_per_case <- ifelse(data_sel$confirmed_cases == 0, 0, data_sel$deaths / data_sel$confirmed_cases)

data_sel_final <- select(data_sel, 
                         confirmed_cases, 
                         confirmed_cases_per, 
                         deaths, 
                         deaths_per, 
                         median_income, 
                         white_per,
                         minority_per,
                         total_pop,
                         commuters_per,
                         deaths_per_case,
                         rent_burden_per)


cor_data_sel <- cor(data_sel_final)
p <- ggcorrplot(cor_data_sel, p.mat = cor_pmat(data_sel_final), insig = "blank", hc.order = TRUE)
p <- p + labs(caption = "Figure 2.4.2")
p
ggsave("Figure 2.4.2.png",  plot = p,  device = "png",  
       scale = 1,  width = 1200,  height = 700,  units =  "px", dpi = 100
)

