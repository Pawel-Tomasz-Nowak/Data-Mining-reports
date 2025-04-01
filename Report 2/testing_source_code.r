library(datasets) # Importowanie bibliotek.
library(dplyr)
library(ggplot2)
library(GGally)
library(gridExtra)
data("iris") # Wczytywanie danych
df <- iris


num.cols <- colnames(df %>% select( where(is.numeric)))

violin.plot.box_maker <- function(var){
ggplot(data = df, 
       aes(x = Species, y = .data[[var]],fill = Species)) +
  geom_violin(alpha = 0.3) +
  geom_boxplot(width = 0.1)
}

grid.arrange(violin.plot.box_maker("Sepal.Length"),
             violin.plot.box_maker("Sepal.Width"),
             violin.plot.box_maker("Petal.Length"),
             violin.plot.box_maker("Petal.Width"))

             

