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

# Log-Revenue
plot(density(log(db$Revenue)), main = "Density of log-Revenue")

# Box-wiskers (Para no categóricas) 
#TODO no sé si esto lo necesito realmente
box_f <-  function(var_name) {
  boxplot(db[[var_name]], main = paste("Boxplot of", var_name))
}
lapply(names(db)[non_categoric_vars], box_f)

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

# See pairs (correlations)
str <- paste0("~", paste(names(db)[non_categoric_vars], collapse = "+"))
pairs(db[, names(db)[non_categoric_vars]])

# Pairwise scatterplots
ggpairs(df[, numeric_vars])

# Bar plot for dummy/categorical variables
# Example: mean of a numeric variable by a categorical variable
agg <- aggregate(df$numeric_var ~ df$categorical_var, FUN = mean)
barplot(agg[,2], names.arg = agg[,1], main = "Mean of numeric_var by categorical_var")

# Or using ggplot2
ggplot(df, aes(x = categorical_var, y = numeric_var)) +
  stat_summary(fun = mean, geom = "bar") +
  labs(title = "Mean of numeric_var by categorical_var")



# -----------------------------------------------------
# 2) Estimate effect of "sign_up" into "revenue"
# -----------------------------------------------------

# Simple linear regression model
model <- lm(log(Revenue) ~ sign_up, data = df)
summary(model)

# Controlling by all variables
model_controlled <- lm(log(Revenue) ~ sign_up + time_spent + past_sessions + device_type + is_returning_user, data = df)
summary(model_controlled)

# Controlar solo por vars relacionadas a Revenue
model_controlled_revenue <- lm(log(Revenue) ~ sign_up + time_spent + past_sessions, data = df)
summary(model_controlled_revenue)

# Controlar solo por vars relacionadas a sign_up
model_controlled_sign <- lm(log(Revenue) ~ sign_up + time_spent + past_sessions, data = df)
summary(model_controlled_sign)

# -----------------------------------------------------
# 3) Evaluating model's predictive capacity
# -----------------------------------------------------

# -----------------------------------------------------
# 4) Evaluation: ¿is there correlation or causality?
# -----------------------------------------------------

# -----------------------------------------------------
# 5) Should there be an experiment to confirm findings?
# -----------------------------------------------------


