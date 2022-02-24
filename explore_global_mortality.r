library(tidyverse)
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")

mortality <- read_csv(".\\data\\Global_Mobility_Report.csv")

unique(mortality$country_region)

mortality_us <- mortality %>% filter(mortality$country_region == "United States")
