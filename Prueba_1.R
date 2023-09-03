
library(readxl)
Validacion <- read_excel("Validacion.xlsx")
View(Validacion)

lmHeight = lm(Conc~Abs1, data = Validacion) #Create the linear regression
summary(lmHeight) #Review the results
