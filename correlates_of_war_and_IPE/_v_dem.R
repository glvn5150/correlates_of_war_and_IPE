
v_dem <- readRDS("V-Dem-CY-Core-v16.rds")
unique(v_dem$country_id)
unique(v_dem$country_name)


indo <- filter(v_dem, v_dem$country_name == "Indonesia")
ts.plot(indo$v2x_polyarchy_sd)
ts.plot(indo$v2edplural)
ts.plot(indo$v2x_libdem)
