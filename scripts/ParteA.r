# -----------------------------------------------------
# PART 1
# Exploratory and predictive analysis from observed data
# 0) Libraries and data
# 1) Description of users general behavior
# 2) Estimate effect of "sign_up" into "revenue"
# 3) Evaluating model's predictive capacity
# 4) Evaluation: ¿is there correlation or causality?
# 5)?Should there be an experiment to confirm findings?
# -----------------------------------------------------

# -----------------------------------------------------
# 0) Clean variables, libraries and data
# -----------------------------------------------------

# Libraries
require(pacman)
p_load(dplyr, tidyr, tidyverse, readr, ggplot2, corrplot)

# Change data name
db <- parte_a

# See and keep variables
vars <- colnames(db)

# Get categoric and non-categoric variables
categoric_vars <- sapply(db, is.factor)
non_categoric_vars <- !categoric_vars

# -----------------------------------------------------
# 1) Description of users general behaviour
# -----------------------------------------------------
# Summary statistics
summary(db)
lapply(db[, non_categoric_vars], summary)

# Histograms (Para no categóricas) (#TODO: falta ponerlo bonito)
histograma_f <-  function(var_name) {
  plot(density(db[[var_name]]), main = paste("Density of", var_name))
}
lapply(names(db)[non_categoric_vars], histograma_f)


# Box-wiskers (Para no categóricas) 
#TODO no sé si esto lo necesito realmente
box_f <-  function(var_name) {
  boxplot(db[[var_name]], main = paste("Boxplot of", var_name))
}
lapply(names(db)[non_categoric_vars], box_f)

# Log-Revenue
plot(density(log(db$Revenue)), main = "Density of log-Revenue")

# TODO: Considerar log(time_spent) por el box-plot

# Boxplots (Para categóricas)
#TODO no sé si esto lo necesito realmente; podría ser solo % de la población total
lapply(names(db)[categoric_vars], box_f)
  # Sumar todos los returning; hay 95k / 100k
  sum(db$is_returning_user == 1, na.rm = TRUE)

  # Sumar los sign_up; haay 77k / 100K
  sum(db$sign_up == 1, na.rm = TRUE)

# Boxplots categóricas vs revenue
# Todo: gráficas bien, pero imprime un montón de números 
box_f_cat_rev <-  function(var_name) {
  boxplot(log(db$Revenue) ~ db[[var_name]], main = paste("Boxplot of log(Revenue) by", var_name))
}
lapply(names(db)[categoric_vars], box_f_cat_rev)

# See pairs (log(revenue) vs continuas) (correlations)
str <- paste0("~", paste(names(db)[non_categoric_vars], collapse = "+"))
pairs(db[, names(db)[non_categoric_vars]])

# Categorics vs sign_up
cat_vs_cat <- function(var1, var2) {
  ggplot(db, aes_string(x = var1, fill = var2)) +
    geom_bar(position = "dodge") +
    labs(title = paste("Count of", var2, "by", var1))
}

lapply(names(db)[categoric_vars], function(var1) {
  cat_vs_cat(var1, "sign_up")
})



# -----------------------------------------------------
# 2) Estimate effect of "sign_up" into "revenue"
# -----------------------------------------------------

#TODO: si hay tiempo, puedo regresar usando lapply
# Simple linear regression model
model <- lm(log(Revenue) ~ sign_up, data = db)
summary(model)

# Controlling by all variables
model_controlled <- lm(log(Revenue) ~ sign_up + time_spent + past_sessions + 
                         device_type + is_returning_user + os_type, data = db)
summary(model_controlled)

# Controlar por interacciones entre vars fuertemente relacionadas a Revenue
# TODO: Rev vs time y vs past_Sessions sube al principio y luego baja
# Con device_type, os_type y sign_up no es tan claro; con returning_user si
model_controlled_revenue <- lm(log(Revenue) ~ sign_up + time_spent + past_sessions +
                                 device_type + is_returning_user + os_type +
                                 time_spent:past_sessions + time_spent:is_returning_user +
                                 past_sessions:is_returning_user,
                                 data = db)
summary(model_controlled_revenue)

# Controlar por interacciones con vars fuertemente relacionadas a sign_up
# TODO: sign vs device no parece tener (ver %'s), os_type si (ver %'s), 
# faltaría ver continuas vs sign_up
model_controlled_sign <- lm(log(Revenue) ~ sign_up + time_spent + past_sessions + 
                              device_type + is_returning_user + os_type +
                              sign_up:os_type, data = db)
summary(model_controlled_sign)

# Modelo final (quitando aquellas anteriores no significativas)

# -----------------------------------------------------
# 3) Evaluating model's predictive capacity
# -----------------------------------------------------

# Fijamos la semilla
set.seed(2025)

# Crear trainning data (30% of observations)
smp_size <- floor(0.3 * nrow(db))

# Creamos la columna de validación en la db para separar
validacion_ids <- sample(seq_len(nrow(db)), size = smp_size)
db$validacion <- 0
db$validacion[validacion_ids] <- 1

# test and trainning sets
data_test <- db %>% filter(validacion == 1)
data_train <- db %>% filter(validacion == 0)

# Crear parámetros y función k-fold
k <- 10
folds_i <- sample(rep(1:k, length.out = nrow(db)))

# Poner variables a los modelos para data_train
#TODO
models <- list(
  c("sign_up"),
  c("sign_up", "time_spent"),
  c("sign_up", "time_spent", "past_sessions")
)

predictor<-function(regresors){
  fmla<- formula(paste0("log(Revenue)",regresors))
  model <- lm(fmla,data = data_train)
  prediction_test <- predict(model, newdata = data_test)
  mse<-with(data_test,mean((log(Revenue)-prediction_test)^2))
  return(mse)
}

# Sacar MSE para los 
lapply(models, predictor)




# -----------------------------------------------------
# 4) Evaluation: ¿is there correlation or causality?
# -----------------------------------------------------

# -----------------------------------------------------
# 5) Should there be an experiment to confirm findings?
# -----------------------------------------------------


