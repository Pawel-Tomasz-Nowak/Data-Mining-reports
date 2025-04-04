library(datasets) # Importowanie bibliotek.
library(dplyr)
library(ggplot2)
library(GGally)
library(gridExtra)
library(arules)
data("iris") # Wczytywanie danych
df <- iris


num.cols <- colnames(df %>% select( where(is.numeric)))
# 
# violin.plot.box_maker <- function(var){
# ggplot(data = df, 
#        aes(x = Species, y = .data[[var]],fill = Species)) +
#   geom_violin(alpha = 0.2) +
#   geom_boxplot(width = 0.15)
# }
# 
# grid.arrange(violin.plot.box_maker("Sepal.Length"),
#              violin.plot.box_maker("Sepal.Width"),
#              violin.plot.box_maker("Petal.Length"),
#              violin.plot.box_maker("Petal.Width"))
# 
#              

var.discretization <- function(df, var){
  # Estimate the mean and sd of the variable (we'll use these values for fixed-type discretization)
  var.vec <- df[[var]]
  
  mean.est <- mean(var.vec)
  sd.est <- sd(var.vec)
  
  
  # Oparta na równej częstotliwości.
  var.discr.freq <- discretize(var.vec, 
                               method = "frequency", 
                               breaks = 3,
                               label = c("Krótka", "Średnia", "Długa"))
  
  # equal-width discretization
  var.discr.width <- discretize(var.vec, 
                                method = "interval", 
                                breaks = 3,
                                label = c("Krótka", "Średnia", "Długa"))
  
  # cluster discretization
  var.discr.cluster <- discretize(var.vec, 
                                  method = "cluster", 
                                  breaks = 3,
                                  label = c("Krótka", "Średnia", "Długa"))
  
  # user-specified breaks bounds.
  var.discr.user <- discretize(var.vec,
                               method = "fixed",
                               breaks = c(-Inf, mean.est-sd.est, mean.est+sd.est, Inf),
                               label = c("Krótka", "Średnia", "Długa"))

  
  
  # Store discretization result in a dataframe.
  df.discr_vals <- data.frame("equal-frequency" = var.discr.freq,
                              "equal-width" = var.discr.width,
                              "cluster-based" = var.discr.cluster,
                              "fixed-bounds"= var.discr.user
                              )
  
}

library(knitr)

# Przykładowy wektor liczb
vec <- c(1, 2, 3, 4, 5)

# Zamiana wektora na ramkę danych
df <- data.frame(Wartości = vec)

# Tworzenie tabeli za pomocą knitr::kable
kable(df, caption = "Tabela z wektora liczb")



