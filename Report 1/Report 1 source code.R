library(dplyr)
library(tidyr)
library(ggplot2)

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
# colSums(is.na(dane)) - ten sam efekt
             
# Jest tylko jedna taka zmienna - "TotalCharges".
# Obliczenie średniej dla TotalCharges, ignorując brakujące wartości
mean_value <- mean(dane$TotalCharges, na.rm = TRUE)

# Imputacja brakujących wartości średnią
dane$TotalCharges[is.na(dane$TotalCharges)] <- mean_value

# Wydobywanie zmiennych typu 'factor'
factors.vars <- names( select_if(dane, is.factor))


for (var in factors.vars){

  p <- ggplot(dane, aes(x = !!sym(var), fill = !!sym(var))) + geom_bar( ) +
    ggtitle(paste("Wykres słupkowy", var))

  print(p)

}

#Znajdź zmienne numeryczne ciągłe.
num.vars <- names( select_if(dane, is.numeric))


# ggplot(dane, aes(y = MonthlyCharges)) + geom_boxplot() +
#   ggtitle("Wykres pudełkowy dla MonthlyCharges")+
#   theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
# 
# 
# var <- "tenure"
# sd_sample <- sd(as.vector(dane[var]))
# 



#       
# p1 <- ggplot(dane, aes(x = tenure, y = MonthlyCharges)) + geom_point(color = "magenta") + geom_smooth(color = "black",
#                                                                                                       method = "lm")
# 
# p2 <- ggplot(dane, aes(x = tenure, y = TotalCharges)) + geom_point(color = "cyan") + geom_smooth(color = "black", 
#                                                                                                  method ="lm")
# 
# 
# p3 <- ggplot(dane, aes(x = tenure*MonthlyCharges, y = TotalCharges)) + geom_point(color = "orange") + geom_smooth(color = "black", 
#                                                                                                                   method ="lm")
# 
# print(p1)
# print(p2)
# print(p3)
# print(p4)

# Etap 3
# podział klientów na tych którzy odeszli i na tych którzy zostali
dane.lojalni <- subset(dane, Churn=="No")
dane.odeszli <- subset(dane, Churn=="Yes")

wskazniki <- function(X)
{
  wynik <- c(min(X), median(X), mean(X), max(X))
  names(wynik) <- c("min", "median", "mean", "max")
  return(wynik)
}



num.vars <- dane %>% select_if(is.numeric)
wskazniki_grup <- aggregate(x=num.vars, by=list(grupa=dane$Churn), FUN=wskazniki)
wskazniki_grup_t <- t(wskazniki_grup[,-1])  # Usunięcie kolumny 'grupa' i transpozycja wyniku
colnames(wskazniki_grup_t) <- c("Lojalni", "Odeszli") # Dodanie nazw kolumn
print(wskazniki_grup_t)

# wykresy słupkowe jeszcze do wybrania
factors.vars <- names(select_if(dane, is.factor))
for (var in factors.vars){
  
  p <- ggplot(dane, aes(x = !!sym(var), fill = Churn)) + geom_bar( ) +
    ggtitle(paste("Wykres słupkowy", var)) + 
  
  print(p)
  
} 

