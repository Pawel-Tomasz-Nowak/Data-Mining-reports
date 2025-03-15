library(dplyr)
library(tidyr)


# ETAP 1
# Wczytywanie ramki danych.
dane <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

# Ile jest wierszy, a ile - zmiennych?
n.row <- nrow(dane) # 7043 wierszy.
n.col <- ncol(dane) # 21 kolumn.



# Dane zawierają sporo zmiennych jakościowych nieporzadkowych, które zostały błędnie wczytane jako zmienne typu napis. 
vars.to.cast <- c("gender", "SeniorCitizen", "Partner", "Dependents", "PhoneService", "MultipleLines",
                  "InternetService", "OnlineSecurity", "OnlineBackup", "DeviceProtection",
                  "TechSupport", "StreamingTV", "StreamingMovies", "Contract", "PaperlessBilling",
                  "PaymentMethod", "Churn")

# Dokonaj konwersji na typ `factor`.
dane[vars.to.cast] <- lapply(dane[vars.to.cast], as.factor)



# Znajdź cechy, które zawierają (standardowo kodowane) wartości brakujące
n.missing <- dane %>% 
  summarize(across(everything(), function(x) { sum(is.na(x)) } ) ) %>% 
  pivot_longer( everything(), names_to = "variable", values_to = "na.count" ) %>%
  filter( na.count > 0)

             
# Jest tylko jedna taka zmienna - "TotalCharges".


dane <- dane %>%
  drop_na(all_of(n.missing$variable)) %>% # Skasuj wiersze, które mają wartości brakujące dla zmiennych z n.missing$variable
  mutate(across(all_of(n.missing$variable), ~NULL))


