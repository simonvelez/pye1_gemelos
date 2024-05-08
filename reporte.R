library(base)
# Determinar el número de registros y variables
num_registros <- nrow(twins_2_)
num_variables <- ncol(twins_2_)

# Determinar el número de registros con al menos un dato faltante
char_variables <- sum(sapply(twins_2_, is.character))

# Determinar el número de registros con información completa
registros_completos <- sum(complete.cases(twins_2_))

# Mostrar el reporte
cat("Número de registros:", num_registros, "\n")
cat("Número de variables:", num_variables, "\n")
cat("Registros con al menos un dato faltante:", char_variables, "\n")
cat("Registros con información completa:", registros_completos, "\n")
