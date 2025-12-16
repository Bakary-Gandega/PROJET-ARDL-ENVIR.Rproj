R <- read.csv("C:/Users/HP/Documents/Portfolio R/BDD.csv", sep=";", dec=",")
View(R)

library(dplyr)

#selection des variables
Annees <- select(R, Annees) 
ln_PIB <- select(R, ln_PIB)
ln_IED <- select(R, ln_IED)
ln_Export  <- select(R, ln_Export )
ln_Inf <- select(R, ln_Inf)

# transformer les variables en series temporelles
Annees <- ts(Annees, start = 1985, end = 2022, frequency = 1)
ln_PIB <- ts(ln_PIB, start = 1985, end= 2022, frequency = 1)
ln_IED <- ts(ln_IED, start = 1985, end = 2022, frequency = 1 ) 
ln_Export <- ts(ln_Export, start = 1985, end = 2022, frequency = 1 ) 
ln_Inf <- ts(ln_Inf, start = 1985, end = 2022, frequency = 1 ) 

# regrouper les données en dataframe
D <- data.frame(Annees, ln_PIB, ln_IED, ln_Export, ln_Inf)

#visualiser
plot.ts(D)

#autocorrelation
acf(ln_PIB)
pacf(ln_PIB)

acf(ln_IED)
pacf(ln_IED)

acf(ln_Export)
pacf(ln_Export)

acf(ln_Inf)
pacf(ln_Inf)

library(tseries)
#Test de stationnarité
adf.test(ln_Export)
adf.test(diff(ln_Export))

adf.test(ln_IED)
adf.test(diff(ln_IED))

adf.test(ln_Inf)
adf.test(diff(ln_Inf))
adf.test(diff(diff(ln_Inf)))
adf.test(diff(diff(diff(ln_Inf))))

adf.test(ln_PIB)
adf.test(diff(ln_PIB))

library(ARDL)
#identification du modèle
#mon_model2 <- auto_ardl(ln_PIB ~ ln_Export + ln_IED + ln_Inf, D, max_order = 6) 
mon_model <- auto_ardl(ln_PIB ~ ln_Export + ln_IED + ln_Inf, D, max_lag_y = 6, max_lag_x = 6, max_order = 6) 

#analyse des lags
mon_model$best_order

#choix du modèle
C_model <- mon_model$best_model

#analyse des coeff du model
summary(C_model)

# Test de cointegration

#bounds_f_test(C_model)

    
#Analyse du residuel
mon_residu <- residuals(C_model)
options(scipen = 999)

mean(mon_residu)

#Normalité des résidus
shapiro.test(mon_residu)
jarque.bera.test(mon_residu)

#autoccorrelation des résidus
acf(mon_residu)
pacf(mon_residu)
