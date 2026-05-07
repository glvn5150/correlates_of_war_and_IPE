library(readr)
library(dplyr)
library(ggplot2)
cow_arms_tech_long <- read_csv("cow_arms_tech_long.csv")
unique(cow_arms_tech_long$stateabb)

arms_country <- cow_arms_tech_long[2:8]
unique(arms_country$techname)
unique(arms_country$statename)

us_arms <- filter(arms_country, arms_country$stateabb == "USA")
chn_arms <- filter(arms_country, arms_country$stateabb == "CHN")
us_arms_clean <- data.frame(c(us_arms[5], us_arms[3], us_arms[7]))
chn_arms_clean <- data.frame(c(chn_arms[5], chn_arms[3], chn_arms[7]))
us_arms_clean_1860 <- filter(us_arms_clean, us_arms_clean$year > '1859' )

us_totuse_1860 <- us_arms_clean_1860$total_use
chn_totuse <- chn_arms_clean$total_use
year_chn_us <- chn_arms_clean$year

plot(x=year_chn_us, y=us_totuse_1860, type = "l", col='blue')
lines(x=year_chn_us, y=chn_totuse, col='red')
plot(us_arms_clean_1860$total_use, us_arms_clean_1860$total_use, type='l')
plot(us_totuse_1860, chn_totuse, type='l') 


filter(us_arms_clean_1860, us_arms_clean_1860$total_use > '15')
unique(us_arms_clean_1860$techname)
