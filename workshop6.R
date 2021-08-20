library(tidymodels)
data(ames)

ames$Sale_Price%>%head()

library(skimr)
skim(ames)

ames<-ames%>%mutate(Sale_Price=log10(Sale_Price))
