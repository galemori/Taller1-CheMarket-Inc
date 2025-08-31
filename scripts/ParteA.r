
# -----------------------------------------------------
# PART 1
# Exploratory and predictive analysis from observed data
# 0) Libraries and data
# 1) Description of users general behavior
# 2) Estimate effect of "sign_up" into "revenue"
# 3) Logistic regression of variables into sign_up
# r) Evaluating model's predictive capacity
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
# TODO exportar

# Histograms (Para no categóricas) (#TODO: falta ponerlo bonito)
histograma_f <-  function(var_name) {
  plot(density(db[[var_name]]), main = paste("Density of", var_name))
}
lapply(names(db)[non_categoric_vars], histograma_f)

# Log-Revenue y sqrt-time (vemos que si mejoran y lo cambiamos en la db)
#TODO: exportar log-rev para anexo
plot(density(log(db$Revenue)), main = "Density of log-Revenue")
plot(density(sqrt(db$time_spent)), main = "Density sqrt-time")

db <- db %>% mutate(log_revenue = log(Revenue))
db <- db %>% mutate(sqrt_time_spent = sqrt(time_spent))


### Ya verificamos las variables. Seleccionemos las dependientes del modelo


# See pairs (log(revenue) vs continuas) (correlations)
# TODO exportar
str <- paste0("~", paste(names(db)[non_categoric_vars], collapse = "+"))
pairs(db[, names(db)[non_categoric_vars]])


# Box-wiskers (Para no categóricas) 
#TODO no sé si esto lo necesito realmente
# TODO exportar
box_f <-  function(var_name) {
  boxplot(db[[var_name]], main = paste("Boxplot of", var_name))
}
lapply(names(db)[non_categoric_vars], box_f)

# Boxplots categóricas vs revenue
# Todo: gráficas bien, pero imprime un montón de números 
# TODO exportar
box_f_cat_rev <-  function(var_name) {
  boxplot(db$log_revenue ~ db[[var_name]], main = paste("Boxplot of log(Revenue) by", var_name))
}
lapply(names(db)[categoric_vars], box_f_cat_rev)




#
# Función: Boxplot + diferencia de medias
box_f_cat_rev <- function(var_name) {
  # Boxplot
  boxplot(db$log_revenue ~ db[[var_name]], 
          main = paste("Boxplot of log(Revenue) by", var_name))
  
  # Diferencia de medias
  means <- tapply(db$log_revenue, db[[var_name]], mean, na.rm = TRUE)
  diffs <- combn(means, 2, FUN = function(x) diff(rev(x))) # diferencias entre categorías

  # Graficar resultados con ggplot2 y ponlo bonito
  df <- data.frame(Category = names(means), Mean = means)
  ggplot(df, aes(x = Category, y = Mean)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Mean of log(Revenue) by", var_name))
}

# Aplicar a todas las categóricas
lapply(names(db)[categoric_vars], box_f_cat_rev)




# Función: medias + IC95% y gráfico con barras + errorbars
plot_mean_diff <- function(var_name) {
  
  # Calcular medias e IC95%
  summary_df <- db %>%
    group_by(!!sym(var_name)) %>%
    summarise(
      mean_rev = mean(log_revenue, na.rm = TRUE),
      sd_rev   = sd(log_revenue, na.rm = TRUE),
      n        = n(),
      .groups = "drop"
    ) %>%
    mutate(
      se = sd_rev / sqrt(n),
      ci_low = mean_rev - 1.96 * se,
      ci_high = mean_rev + 1.96 * se
    )
  
  # Gráfico: barras + error bars en negro
  ggplot(summary_df, aes(x = !!sym(var_name), y = mean_rev, fill = !!sym(var_name))) +
    geom_col(width = 0.6, alpha = 0.8) +   # Barras
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), 
                  width = 0.2, color = "black", size = 0.8) +  # Intervalos en negro
    labs(
      title = paste("Media de log(Revenue) por", var_name),
      x = var_name,
      y = "Mean log(Revenue)"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}

# Aplicar la función a todas las variables categóricas
plots <- lapply(names(db)[categoric_vars], plot_mean_diff)

# Mostrar el primero como ejemplo
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
# TODO: exportar


# Categorics vs sign_up
# TODO exportar
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
model <- lm(log_revenue ~ sign_up, data = db)
# TODO exportar
summary(model)

# Controlling by all variables
model_controlled <- lm(log(Revenue) ~ sign_up + sqrt_time_spent + past_sessions + 
                         device_type + is_returning_user + os_type, data = db)
# TODO exportar
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
# 3) Logistic regression of variables into sign_up
# -----------------------------------------------------

# Quitamos revenue del set de variables explicativas
vars <- setdiff(names(db), c("sign_up", "revenue"))

# Fórmula dinámica
fml <- as.formula(paste("sign_up ~", paste(vars, collapse = " + ")))

# Ajustamos el modelo logístico
logit_model <- glm(fml, data = db, family = binomial(link = "logit"))

# Resumen del modelo
summary(logit_model)

# Odds ratios en vez de coeficientes logit
exp(coef(logit_model))

# Intervalos de confianza de los odds ratios
exp(confint(logit_model))

# -----------------------------------------------------
# 4) Evaluating model's predictive capacity
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




