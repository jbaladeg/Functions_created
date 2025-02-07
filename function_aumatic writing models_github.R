library(psych)
library(dplyr)
library(tidyr)
library(tibble)

# function in order to generate the model's structure for the CFA
write_model_cfa <- function(efa_objeto) {
  # Extract the load matrix from the EFA object and convert it to data.frame
    # With this data.frame we can create a new base with our variables and their loadings
  matriz_cargas <- as.data.frame(unclass(efa_objeto$loadings))
  
  # Convert the matrix into a long format
    # In this step, we transform the previous base in a long format, beacouse the factors will be the "factor variable"
  df <- matriz_cargas %>% 
    rownames_to_column("variable") %>% 
    pivot_longer(cols = -variable, names_to = "factor", values_to = "carga") %>% 
    filter(!is.na(carga))
   
  # Select factor with most positive load per variable 
    #in this step, the function is going to detect the best factor-location for each variable
  df_filtrado <- df %>% 
    group_by(variable) %>% 
    filter(carga == max(carga)) %>% 
    ungroup()
  
  # Gnerate text model
    #in this step, the function is going to write the model basing on the previuos conditions
  modelo <- df_filtrado %>% 
    group_by(factor) %>% 
    summarise(variables = paste(variable, collapse = " + ")) %>% 
    mutate(linea = paste0(factor, " =~ ", variables)) %>% 
    pull(linea) %>% 
    paste(collapse = "\n")
  
  return(modelo)
}


##################################################
#         EXAMPLE
##################################################

#1. read a data frame (bfi is public)
df <- bfi[,-c(26:28)]

#2. Make EFA

  #fisrt, parallel analysis find 6 factors
fa.parallel(df, fa = "fa")

  #create the EFA with 6 factors as parrallel analysis suggests
efa_result <- fa(df, nfactors = 6, rotate = "oblimin", fm = "ml")

  #Check the loadings of the variables in each factors
print(efa_result$loadings, sort=TRUE)


#3. write the model for the CFA automatocally 
modelo_cfa <- write_model_cfa(efa_result)
cat(modelo_cfa, "\n")

#4. now, you can use this result to create yor model for the CFA, just copy-paste and adapt:
model_CFA='
ML1 =~ C5 + E1 + E2 + N4
ML2 =~ N1 + N2 + N3 + N5
ML3 =~ C1 + C2 + C3 + E5
ML4 =~ E3 + O1 + O3 + O4
ML5 =~ A2 + A3 + A4 + A5
ML6 =~ A1 + C4 + E4 + O2 + O5
'
