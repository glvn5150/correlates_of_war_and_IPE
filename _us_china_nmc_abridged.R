library(readr)
library(dplyr)
library(ggplot2)
library(tseries)
library(vars)
library(gmm)

#----load the US and chinese data----
NMC_60_abridged <- read_csv("NMC-60-abridged.csv")
View(NMC_60_abridged)

unique(NMC_60_abridged$stateabb)
us_military <- filter(NMC_60_abridged, NMC_60_abridged$stateabb == "USA" & NMC_60_abridged$year > "1950")
chn_military <- filter(NMC_60_abridged, NMC_60_abridged$stateabb == "CHN"& NMC_60_abridged$year > "1950")
unique(chn_military$year)
unique(us_military$year)
us_military

us_tpop<- us_military$tpop
us_milex <- us_military$milex
us_miper <- us_military$milper
us_cinc <- us_military$cinc
us_irst <- us_military$irst
us_pec <- us_military$pec

chn_tpop <- chn_military$tpop
chn_milex <- chn_military$milex
chn_miper <- chn_military$milper
chn_cinc <-chn_military$cinc
chn_pec <-chn_military$pec
chn_irst <-chn_military$irst

year <- us_military$year

par(mfrow=c(3,2))
plot(year, us_miper/max(us_miper), type='l', col='blue',ylab='military personel')
lines(year, chn_miper/max(chn_miper), type='l', col='red')
plot(year,us_milex/max(us_milex), col='blue', type='l',ylab='military expenditure')
lines(year, chn_milex/max(chn_milex), type='l', col='red',)
plot(year,us_tpop/max(us_tpop), col='blue', type='l',,ylab='total population')
lines(year, chn_milex/max(chn_milex), type='l', col='red')
plot(year,us_irst/max(us_irst), col='blue', type='l',ylab='iron and steel production')
lines(year, chn_irst/max(chn_irst), type='l', col='red')
plot(year,us_pec/max(us_pec), col='blue', type='l',ylab='primary energy consumption')
lines(year, chn_pec/max(chn_pec), type='l', col='red')
plot(year,us_cinc/max(us_cinc), col='blue', type='l',ylab='composite index of national capability')
lines(year, chn_cinc/max(chn_cinc), type='l', col='red')


#----vector autoregressions----
data_combined <- data.frame(
  us_cinc = us_cinc,
  chn_cinc = chn_cinc,
  us_milex = us_milex,
  us_milex = us_milex,
  us_pec = us_pec,
  us_tpop = us_tpop,
  us_miper = us_miper,
  us_irst = us_irst,
  chn_milex = chn_milex,
  chn_tpop = chn_tpop,
  chn_pec = chn_pec,
  chn_irst = chn_irst,
  chn_miper = chn_miper
)

y_var <- na.omit(data_combined[, c('us_cinc', 'chn_cinc')])
x_var <- na.omit(data_combined[, c("us_milex",  "us_pec", "us_irst","us_tpop", "us_miper",
                           "chn_milex", "chn_pec", "chn_irst", "chn_tpop", "chn_miper")])

lagselect <- VARselect(y_var, lag.max = 10, type = "const", exogen = x_var)
print(lagselect$selection)

optimal_lag <- lagselect$selection["AIC(n)"]
model_var <- VAR(y_var, p = optimal_lag, type = "const", exogen = x_var)
summary(model_var)

irf_res <- irf(model_var, impulse = "us_cinc", response = "chn_cinc", n.ahead = 20, boot = TRUE)
plot(irf_res, main = "Shock from US CINC to China CINC")

plot(ts(y_var$us_cinc), main="US CINC", col="blue")
acf(y_var$us_cinc, main="ACF US CINC")
plot(ts(y_var$chn_cinc), main="China CINC", col="red")
acf(y_var$chn_cinc, main="ACF China CINC")
plot(ts(x_var$chn_pec), main="China PEC", col="red")
lines(x_var$us_pec, main="US PEC", col="blue")
acf(x_var$chn_pec, main="ACF China CINC")

fevd_res <- fevd(model_var, n.ahead = 20)
print(fevd_res)
plot(fevd_res)

spec_res <- spectrum(y_var$us_cinc, log="no", main="Spectral Density: US CINC")
plot(spec_res$freq)
plot(spec_res$spec,col='red')

last_x <- tail(x_var, 1)
future_x <- do.call(rbind, replicate(10, last_x, simplify = FALSE))

pred <- predict(model_var, n.ahead = length(future_x), dumvar = future_x)
plot(pred, names = "us_cinc")
plot(pred, names = "chn_cinc")

hist(log(us_cinc))
hist(log(chn_cinc))
hist(log(us_milex))
hist(log(chn_milex))
hist(chn_pec)
hist(us_pec)

#diagnostics 
roots(model_var)
arch.test(model_var)
normality.test(model_var)
serial.test(model_var)
AIC(model_var)
shapiro.test(us_cinc)
shapiro.test(chn_cinc)

#qqplot
res <- residuals(model_var)
plot(res)
abline(h=mean(res))
qqnorm(res[, "us_cinc"], main = "qq us_cinc")
qqline(res[, "us_cinc"], col = "blue")
qqnorm(res[, "chn_cinc"], main = "qq chn_cinc")
qqline(res[, "chn_cinc"], col = "red")


gmm_moment <- function(theta, data){
  y <- data[-1]
  x <- data[-length(data)]
  eps <- y - theta[1] * x  # Simple AR(1) logic for GMM
  return(eps * x)
}

#-----gmm-------
gmm_fit <- gmm(gmm_moment, x = as.numeric(y_var$us_cinc), t0 = 0.1)
summary(gmm_fit)
fitted_vals <- fitted(gmm_fit)
residuals_gmm <- residuals(gmm_fit)

theta_hat <- coef(gmm_fit)
y_actual <- as.numeric(y_var$us_cinc[-1])
x_lagged <- as.numeric(y_var$us_cinc[-length(y_var$us_cinc)])
gmm_fitted <- theta_hat * x_lagged
gmm_resids <- y_actual - gmm_fitted
plot(y_actual, type="l", col="blue", lwd=2, main="GMM Manual Fit: US CINC", ylab="CINC")
lines(gmm_fitted, col="red", lty=2, lwd=2)
legend("topright", legend=c("Actual", "GMM Predicted"), col=c("blue", "red"), lty=c(1, 2))


gmm_fit_chn <- gmm(gmm_moment, x = as.numeric(y_var$chn_cinc), t0 = 0.1)
summary(gmm_fit_chn)
y_act_chn <- as.numeric(y_var$chn_cinc[-1])
x_lagged_chn <- as.numeric(y_var$chn_cinc[-length(y_var$chn_cinc)])
theta_hat_chn <- coef(gmm_fit_chn)
gmm_fitted_chn <- theta_hat_chn * x_lagged_chn
gmm_resids_chn <- y_act_chn - gmm_fitted_chn
plot(y_act_chn, type="l", col="blue", lwd=2, main="GMM Manual Fit: Chinese CINC", ylab="CINC")
lines(gmm_fitted_chn, col="red", lty=2, lwd=2)
legend("bottomright", legend=c("Actual", "GMM Predicted"), col=c("blue", "red"), lty=c(1, 2))
plot(gmm_resids_chn)
abline(h=mean(gmm_resids_chn))


#----causality----
causality_chn <- causality(model_var, cause = "chn_cinc")
print(causality_chn$Granger)
causality_us <- causality(model_var, cause = "us_cinc")
print(causality_us$Granger)
AIC(model_var)


