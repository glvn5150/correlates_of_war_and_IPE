library(dplyr)
library(tidyverse)
library(ggcorrplot)
install.packages("ggcorrplot")
install.packages("GGally")
library(GGally)
#--------------abbreviations----------------
#polyarch : To what extent is the ideal of electoral democracy in its fullest sense achieved?()
#--->"Closed Autocratic", 0.25 as "Autocratic", 0.5 as"Ambivalent", 0.75 as "Minimally Democratic", and 1.0 as "Democratic"##
#partidem : To what extent is the ideal of participatory democracy achieved?
#--->0.0 as "Closed Autocratic", 0.25 as "Autocratic", 0.5 as"Ambivalent", 0.75 as "Minimally Democratic", and 1.0 as "Democratic".
#mpi : To what extent is the electoral principle of democracy achieved?
#---> 0.0 as "Closed Autocratic", 0.25 as "Autocratic", 0.5 as"Ambivalent", 0.75 as "Minimally Democratic", and 1.0 as "Democratic".
#egaldem : To what extent is the ideal of egalitarian democracy achieved?
#---> 1 : equal rights distribution in all groups, 2: equal resources, 3: groups enjoy equal accsses to power
#v2caautmob: In this year, how frequent and large have events of mass mobilization for pro-autocraticaims been?


cow_set <- NMC_5_0 <- read.csv("NMC_5_0.csv")
v_dem <- readRDS("V-Dem-CY-Core-v16.rds")
pov_owid <- read.csv("share-of-population-in-extreme-poverty.csv")
unique(NMC_5_0$stateabb)
unique(v_dem$country_text_id)
unique(v_dem$country_name)
unique(v_dem$COWcode)
unique(NMC_5_0$ccode)


# Define the list of G20 members as they appear in your data
g20_list <- c(
  "Argentina", "Australia", "Brazil", "Canada", "China", 
  "France", "Germany", "India", "Indonesia", "Italy", 
  "Japan", "South Korea", "Mexico", "Russia", "Saudi Arabia", 
  "South Africa", "Türkiye", "United Kingdom", "United States of America"
)

cow_codes <- c("USA", "CAN", "MEX", "ARG", "BRA", "UKG", "FRN", "GMY", "ITA", 
               "RUS", "TUR", "SAU", "SAF", "IND", "CHN", "JPN", "ROK", "INS", "AUL")

vdem_g20_ids <- c("ARG", "AUS", "BRA", "CAN", "CHN", "FRA", "DEU", "IND", "IDN", 
                  "ITA", "JPN", "MEX", "RUS", "SAU", "ZAF", "KOR", "TUR", "GBR", "USA")

g20_vdem_subset <- v_dem[v_dem$country_text_id %in% vdem_g20_ids, ]

cow_g20_ids <- c("ARG", "AUL", "BRA", "CAN", "CHN", "FRN", "GMY", "IND", "INS", 
                 "ITA", "JPN", "MEX", "RUS", "SAU", "SAF", "ROK", "TUR", "UKG", "USA")

g20_nmc_subset <- NMC_5_0[NMC_5_0$stateabb %in% cow_g20_ids, ]


#--------------Indonesia's analysis----------------

v_dem_indo <- filter(v_dem, v_dem$country_name == "Indonesia")
v_dem_indo_clean <- filter(v_dem_indo, v_dem_indo$year > '1944')
View(v_dem_indo_clean)

indo_cow <- filter(cow_set, cow_set$stateabb == "INS")
milper_indo <- indo_cow$milper
milexp_indo <- indo_cow$milex
cinc_indo <- indo_cow$cinc
polyarch_indo <- v_dem_indo_clean$v2x_polyarchy
partidem_indo <- v_dem_indo_clean$v2x_partipdem

mpi <- v_dem_indo_clean$v2x_mpi$
text(polyarch_indo, partidem_indo, labels = v_dem_indo_clean$year, pos = 3, cex = 0.7)

year_labels <- v_dem_indo_clean$year
year_labels[as.numeric(year_labels) %% 5 != 0] <- "" 
text(polyarch, partidem, labels = year_labels, pos = 3, cex = 0.7)

plot(polyarch, partidem, main = "Indonesia: Electoral vs Participatory Democracy", # The Titlepch = 21,       
     bg = "skyblue",   
     col = "blue",     
     cex = 1.2) 
text(polyarch_indo, partidem_indo, labels = v_dem_indo_clean$year, pos = 3, cex = 0.7)
abline(reg_poly_parti, col = "red", lwd = 2)

length(polyarch)
length(partidem)

reg_poly_parti <- lm(partidem ~ polyarch)
summary(reg_poly_parti)

# Merge by year to ensure rows align perfectly
indo_combined <- v_dem %>%
  filter(country_name == "Indonesia", year >= 1944 & year <= 2015) %>%
  inner_join(cow_set %>% filter(stateabb == "INS"), by = "year") %>%
  select(
    Year = year,
    `Mil Personnel` = milper,
    `Mil Expend` = milex,
    CINC = cinc,
    Polyarchy = v2x_polyarchy,
    Parti_Dem = v2x_partipdem
  )

# Calculate the correlation matrix
# (use 'complete.obs' to handle any missing values in military data)
cor_matrix <- cor(indo_combined[,-1], use = "complete.obs") 

ggcorrplot(cor_matrix, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE, 
           title = "Indonesia (1944-2015): Military vs Democracy Correlation",
           colors = c("#E46726", "white", "#6D9EC1"))

# Merge by the numeric COW code (Indonesia is 850)
indo_merged <- merge(
  v_dem[v_dem$country_name == "Indonesia" & v_dem$year >= 1944 & v_dem$year <= 2015, ],
  cow_set[cow_set$ccode == 850, ],
  by = "year"
)

# Create a subset of just the variables you want to compare
plot_data <- indo_merged[, c("milper", "milex", "cinc", "v2x_partipdem", "v2x_polyarchy", "v2x_egaldem")]

# Rename columns for cleaner plot labels
colnames(plot_data) <- c("Mil_Pers", "Mil_Ex", "CINC", "Part_Dem", "Polyarchy", "Egal_Dem")

# Helper function to add year labels to the panels
text_panel <- function(x, y, ...) {
  points(x, y, pch = 20, col = rgb(0, 0, 1, 0.3)) # Plot points with transparency
  
  # Only label every 10th year to keep it readable
  years <- indo_merged$year
  label_mask <- years %% 10 == 0
  
  text(x[label_mask], y[label_mask], 
       labels = years[label_mask], 
       pos = 3, cex = 0.7, col = "darkred")
}

# Generate the matrix
pairs(plot_data, 
      lower.panel = text_panel, 
      upper.panel = panel.smooth, # Adds a trend line to the top half
      main = "Indonesia: Military vs Democracy (1944-2015)")

# Plot 1: Polyarchy vs Military Expenditure
plot(indo_merged$v2x_polyarchy, indo_merged$milex, 
     pch = 19, col = "steelblue", 
     xlab = "Polyarchy (Electoral Democracy)", 
     ylab = "Military Expenditure",
     main = "Indonesia: Polyarchy vs Military Expenditure (1944-2015)")

# Add a trend line
abline(lm(milex ~ v2x_polyarchy, data = indo_merged), col = "red", lwd = 2)

# Add year labels for every 10th year
with(indo_merged[indo_merged$year %% 10 == 0, ], 
     text(v2x_polyarchy, milex, labels = year, pos = 3, cex = 0.8, col = "darkred"))

# Plot 2: Polyarchy vs CINC
plot(indo_merged$v2x_polyarchy, indo_merged$cinc, 
     pch = 19, col = "darkgreen", 
     xlab = "Polyarchy (Electoral Democracy)", 
     ylab = "CINC (National Capability)",
     main = "Indonesia: Polyarchy vs CINC (1944-2015)")

# Add a trend line
abline(lm(cinc ~ v2x_polyarchy, data = indo_merged), col = "orange", lwd = 2)

# Add year labels for every 10th year
with(indo_merged[indo_merged$year %% 10 == 0, ], 
     text(v2x_polyarchy, cinc, labels = year, pos = 3, cex = 0.8, col = "black"))


# Plot 2: Polyarchy vs CINC
# Plot 1: Polyarchy vs Military Expenditure
plot(indo_merged$v2x_polyarchy, indo_merged$milex, 
     pch = 19, col = "steelblue", 
     xlab = "Polyarchy (Electoral Democracy)", 
     ylab = "Military Expenditure",
     main = "Indonesia: Polyarchy vs Military Expenditure (1944-2015)")

# Add a trend line
abline(lm(milex ~ v2x_polyarchy, data = indo_merged), col = "red", lwd = 2)

# Add year labels for every 10th year
with(indo_merged[indo_merged$year %% 10 == 0, ], 
     text(v2x_polyarchy, milex, labels = year, pos = 3, cex = 0.8, col = "darkred"))

# 1. Clean the poverty data (assuming the long column name from your image)
# We rename the column for easier coding
pov_clean <- pov_owid %>%
  filter(Code == "IDN") %>%
  select(year = Year, poverty = Share.of.population.in.poverty...3.a.day.) %>%
  filter(!is.na(poverty)) # Remove rows where poverty data doesn't exist

# 2. Merge with your existing indo_merged data
indo_poverty_analysis <- merge(indo_merged, pov_clean, by = "year")

# 3. View the time range we actually have data for
# Poverty data usually starts around 1980 for Indonesia
print(paste("Data range available:", min(indo_poverty_analysis$year), "-", max(indo_poverty_analysis$year)))

par(mfrow=c(1,2), mar=c(5,4,4,2))

# Plot 1: Polyarchy vs Poverty
# Expected: Higher democracy often correlates with lower extreme poverty
plot(indo_poverty_analysis$v2x_polyarchy, indo_poverty_analysis$poverty,
     pch = 19, col = "purple",
     xlab = "Polyarchy Index", ylab = "% in Extreme Poverty",
     main = "Indonesia: Democracy vs Poverty")

# Add Year labels
text(indo_poverty_analysis$v2x_polyarchy, indo_poverty_analysis$poverty, 
     labels = indo_poverty_analysis$year, pos = 3, cex = 0.7)
abline(lm(poverty ~ v2x_polyarchy, data = indo_poverty_analysis), col = "red", lty = 2)

# Plot 2: CINC vs Poverty
# Expected: Higher national capability usually correlates with lower poverty
plot(indo_poverty_analysis$cinc, indo_poverty_analysis$poverty,
     pch = 19, col = "darkorange",
     xlab = "National Capability (CINC)", ylab = "% in Extreme Poverty",
     main = "Indonesia: Capability vs Poverty")

# Add Year labels
text(indo_poverty_analysis$cinc, indo_poverty_analysis$poverty, 
     labels = indo_poverty_analysis$year, pos = 3, cex = 0.7)
abline(lm(poverty ~ cinc, data = indo_poverty_analysis), col = "red", lty = 2)


#------------G20 Analysis-------------
# Define G20 numeric codes
g20_codes <- c(2, 20, 70, 140, 160, 200, 220, 255, 325, 365, 640, 560, 670, 750, 710, 740, 732, 850, 900)

# Merge datasets and filter for a single year (e.g., 2012)
g20_data <- merge(v_dem[v_dem$year == 2012, ], 
                  cow_set[cow_set$year == 2012, ], 
                  by.x = "COWcode", by.y = "ccode")

# Subset for G20 only
g20_plot <- g20_data[g20_data$COWcode %in% g20_codes, ]

# Calculate your Ratios
# Ratio 1: Efficiency of Capability per Military Dollar
g20_plot$ratio_cinc_milex <- g20_plot$cinc * g20_plot$milex

# Ratio 2: Capability relative to Participatory Democracy
g20_plot$ratio_cinc_parti <- g20_plot$cinc * g20_plot$v2x_partipdem


# 1. Run the Linear Regressions on the logged values
reg_mil  <- lm(log(ratio_cinc_milex) ~ log(cinc), data = g20_plot)
reg_dem  <- lm(log(ratio_cinc_parti) ~ log(cinc), data = g20_plot)

# 2. View the summaries in your console
summary(reg_mil)
summary(reg_dem)

# 3. Create the Plots
par(mfrow=c(1,2), mar=c(5,4,4,2))

# --- Plot 1: Military ---
plot(log(g20_plot$cinc), log(g20_plot$ratio_cinc_milex), 
     pch = 19, col = "darkblue",
     main = "G20: Log-Weighted Military Power",
     xlab = "Log Total Capability (CINC)", 
     ylab = "Log(CINC × Mil-Expenditure)")

# Add the regression line
abline(reg_mil, col = "red", lwd = 2, lty = 2)

text(log(g20_plot$cinc), log(g20_plot$ratio_cinc_milex), 
     labels = g20_plot$stateabb, 
     pos = 3, cex = 0.8, col = "darkred")

# --- Plot 2: Democracy ---
plot(log(g20_plot$cinc), log(g20_plot$ratio_cinc_parti), 
     pch = 19, col = "darkgreen",
     main = "G20: Log-Weighted Democratic Power",
     xlab = "Log Total Capability (CINC)", 
     ylab = "Log(CINC × Partic. Dem)")

# Add the regression line
abline(reg_dem, col = "red", lwd = 2, lty = 2)

text(log(g20_plot$cinc), log(g20_plot$ratio_cinc_parti), 
     labels = g20_plot$stateabb, 
     pos = 3, cex = 0.8, col = "darkred")


unique(pov_owid$Code)

# Define the G20 codes used in the Poverty CSV
pov_g20_codes <- c("ARG", "AUS", "BRA", "CAN", "CHN", "FRA", "DEU", "IND", "IDN", 
                   "ITA", "JPN", "MEX", "RUS", "SAU", "ZAF", "KOR", "TUR", "GBR", "USA")

# Subset the poverty data for G20
pov_g20 <- pov_owid %>%
  filter(Code %in% pov_g20_codes) %>%
  select(year = Year, Code, poverty_rate = Share.of.population.in.poverty...3.a.day.)

# Because V-Dem also uses ISO codes (like the poverty set), 
# merge those two first by Code and Year:
vdem_pov <- merge(v_dem, pov_g20, by.x = c("year", "country_text_id"), by.y = c("year", "Code"))

# Then merge with the COW set using the numeric COWcode to avoid the UKG/GBR text mismatch
# 1. Standardize Poverty data column names using your unique Code list
# Indonesia = IDN, UK = GBR, Australia = AUS, Germany = DEU, etc.
pov_clean <- pov_owid %>%
  select(year = Year, Code, poverty_rate = Share.of.population.in.poverty...3.a.day.) %>%
  filter(!is.na(poverty_rate))

# 2. Define G20 numeric codes for merging (stable across datasets)
g20_codes <- c(2, 20, 70, 140, 160, 200, 220, 255, 325, 365, 
               640, 560, 670, 750, 710, 740, 732, 850, 900)

# 3. Merge V-Dem and Poverty first (both use ISO 3-letter codes)
vdem_pov <- merge(v_dem, pov_clean, by.x = c("year", "country_text_id"), by.y = c("year", "Code"))

# 4. Final Merge with COW Capability data using Numeric IDs
# Filter for 2012 (or your latest stable year) to ensure cross-sectional accuracy
g20_2012 <- merge(vdem_pov, cow_set, by.x = c("year", "COWcode"), by.y = c("year", "ccode")) %>%
  filter(COWcode %in% g20_codes, year == 2012)

# Calculate Multipliers
g20_2012$log_mil_power <- log(g20_2012$cinc * g20_2012$milex)
g20_2012$log_dem_power <- log(g20_2012$cinc * g20_2012$v2x_polyarchy)
g20_2012$log_pov_weight <- log(g20_2012$cinc * g20_2012$poverty_rate)

# Regression 1: Military Power
reg_mil <- lm(log_mil_power ~ log(cinc), data = g20_2012)

# Regression 2: Democratic Power (Highly volatile in 2026 data)
reg_dem <- lm(log_dem_power ~ log(cinc), data = g20_2012)

# Regression 3: Poverty-Weighted Capability
reg_pov <- lm(log_pov_weight ~ log(cinc), data = g20_2012)

# Review summaries
summary(reg_mil)
summary(reg_dem)
summary(reg_pov)

par(mfrow=c(1,3), mar=c(5,4,4,2))

# Plot A: Military Power
par(mfrow=c(1,3), mar=c(5,4,4,2))

# Plot A: Military Power
plot(log(g20_2012$cinc), g20_2012$log_mil_power, pch=19, col="darkblue",
     main="Log-Weighted Military Power", xlab="Log CINC", ylab="Log(CINC*Milex)")
abline(reg_mil, col="red", lwd=2, lty=2)
text(log(g20_2012$cinc), g20_2012$log_mil_power, labels=g20_2012$stateabb, pos=3, cex=0.8)

# Plot B: Democratic Power
plot(log(g20_2012$cinc), g20_2012$log_dem_power, pch=19, col="darkgreen",
     main="Log-Weighted Democracy", xlab="Log CINC", ylab="Log(CINC*Polyarchy)")
abline(reg_dem, col="red", lwd=2, lty=2)
text(log(g20_2012$cinc), g20_2012$log_dem_power, labels=g20_2012$stateabb, pos=3, cex=0.8)

# Plot C: Poverty Burden
plot(log(g20_2012$cinc), g20_2012$log_pov_weight, pch=19, col="purple",
     main="Log-Weighted Poverty", xlab="Log CINC", ylab="Log(CINC*Poverty)")
abline(reg_pov, col="red", lwd=2, lty=2)
text(log(g20_2012$cinc), g20_2012$log_pov_weight, labels=g20_2012$stateabb, pos=3, cex=0.8)


# 1. Poverty vs Polyarchy (Democracy)
# Does more democracy correlate with less poverty in the G20?
reg_pov_dem <- lm(log(poverty_rate) ~ log(v2x_polyarchy), data = g20_2012)

# 2. Poverty vs CINC (Capability)
# Does a more powerful/capable state have less poverty?
reg_pov_cinc <- lm(log(poverty_rate) ~ log(cinc), data = g20_2012)

# Check the results
summary(reg_pov_dem)
summary(reg_pov_cinc)


par(mfrow=c(1,2), mar=c(5,4,4,2))

# --- Plot 1: Poverty vs Polyarchy ---
plot(log(g20_2012$v2x_polyarchy), log(g20_2012$poverty_rate), 
     pch = 19, col = "purple",
     main = "G20: Democracy vs Poverty",
     xlab = "Log Polyarchy", ylab = "Log Poverty Rate")
abline(reg_pov_dem, col = "red", lwd = 2, lty = 2)
text(log(g20_2012$v2x_polyarchy), log(g20_2012$poverty_rate), 
     labels = g20_2012$stateabb, pos = 3, cex = 0.8)

# --- Plot 2: Poverty vs CINC ---
plot(log(g20_2012$cinc), log(g20_2012$poverty_rate), 
     pch = 19, col = "darkorange",
     main = "G20: Capability vs Poverty",
     xlab = "Log Total Capability (CINC)", ylab = "Log Poverty Rate")
abline(reg_pov_cinc, col = "red", lwd = 2, lty = 2)
text(log(g20_2012$cinc), log(g20_2012$poverty_rate), 
     labels = g20_2012$stateabb, pos = 3, cex = 0.8)

