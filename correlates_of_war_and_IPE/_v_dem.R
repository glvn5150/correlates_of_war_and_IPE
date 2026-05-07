library(dplyr)
library(tidyverse)

v_dem <- readRDS("V-Dem-CY-Core-v16.rds")
unique(v_dem$country_id)
unique(v_dem$country_name)

v_dem_indo <- filter(v_dem, v_dem$country_name == "Indonesia")
v_dem_indo_clean <- filter(v_dem_indo, v_dem_indo$year > '1944')
View(v_dem_indo_clean)


#polyarch : To what extent is the ideal of electoral democracy in its fullest sense achieved?()
#--->"Closed Autocratic", 0.25 as "Autocratic", 0.5 as"Ambivalent", 0.75 as "Minimally Democratic", and 1.0 as "Democratic"##
#partidem : To what extent is the ideal of participatory democracy achieved?
#--->0.0 as "Closed Autocratic", 0.25 as "Autocratic", 0.5 as"Ambivalent", 0.75 as "Minimally Democratic", and 1.0 as "Democratic".
#mpi : To what extent is the electoral principle of democracy achieved?
#---> 0.0 as "Closed Autocratic", 0.25 as "Autocratic", 0.5 as"Ambivalent", 0.75 as "Minimally Democratic", and 1.0 as "Democratic".

polyarch <- v_dem_indo_clean$v2x_polyarchy
partidem <- v_dem_indo_clean$v2x_partipdem
mpi <- v_dem_indo_clean$v2x_mpi$
text(polyarch, partidem, labels = v_dem_indo_clean$year, pos = 3, cex = 0.7)

year_labels <- v_dem_indo_clean$year
year_labels[as.numeric(year_labels) %% 5 != 0] <- "" 
text(polyarch, partidem, labels = year_labels, pos = 3, cex = 0.7)

plot(polyarch, partidem, main = "Indonesia: Electoral vs Participatory Democracy", # The Titlepch = 21,       
     bg = "skyblue",   
     col = "blue",     
     cex = 1.2) 
text(polyarch, partidem, labels = v_dem_indo_clean$year, pos = 3, cex = 0.7)
abline(reg_poly_parti, col = "red", lwd = 2)

  
length(polyarch)
length(partidem)
plot(polyarch, partidem)
abline(reg_poly_parti, col = "red", lwd = 2)


reg_poly_parti <- lm(partidem ~ polyarch)
summary(reg_poly_parti)
