
#FUNCTIONS CREATED ----


#pomps() fnc ----

bd <- iris
head(bd) #check variables

##manual form... ----

bd$Sepal.Length_p = (((bd$Sepal.Length) - (min(bd$Sepal.Length, na.rm = TRUE))) / ((max(bd$Sepal.Length, na.rm = TRUE)) - (min(bd$Sepal.Length, na.rm = TRUE))))*100
bd$Sepal.Width_p = (((bd$Sepal.Width) - (min(bd$Sepal.Width, na.rm = TRUE))) / ((max(bd$Sepal.Width, na.rm = TRUE)) - (min(bd$Sepal.Width, na.rm = TRUE))))*100
bd$Petal.Length_p = (((bd$Petal.Length) - (min(bd$Petal.Length, na.rm = TRUE))) / ((max(bd$Petal.Length, na.rm = TRUE)) - (min(bd$Petal.Length, na.rm = TRUE))))*100
bd$Petal.Width_p = (((bd$Petal.Width) - (min(bd$Petal.Width, na.rm = TRUE))) / ((max(bd$Petal.Width, na.rm = TRUE)) - (min(bd$Petal.Width, na.rm = TRUE))))*100

head(bd) #check new variables
summary(bd) #check descriptive data of the new variables

##automatic form...----

bd2 <- iris
head(bd2)

pomps <- function(data, variables) {
  for (var in variables) {
    # creates the name of the new variable with the extension _p
    new_var <- paste0(var, "_p")
    
    # Calculate the new variable using the formula provided
    data[[new_var]] <- ((data[[var]] - min(data[[var]], na.rm = TRUE)) / 
      (max(data[[var]], na.rm = TRUE) - min(data[[var]], na.rm = TRUE))) * 100
  }
  return(data)
}


bd2 <- pomps(data = bd2, variables = c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width'))

head(bd2) #check new variables


summary(bd); summary(bd2) #check that result is the same in both cases