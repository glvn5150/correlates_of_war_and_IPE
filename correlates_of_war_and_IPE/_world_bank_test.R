library(WDI)
library(wbstats)

WDIsearch("gdp.*constant")
gdp_data <- WDI(
  country = c("CN", "US"), 
  indicator = "NY.GDP.MKTP.KD", 
  start = 2000, 
  end = 2023
)

library(ggplot2)
ggplot(gdp_data, aes(x = year, y = NY.GDP.MKTP.KD, color = country)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "GDP Comparison (Constant 2015 US$)", y = "GDP") +
  theme_minimal()
