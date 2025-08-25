# -----------------------------------------------------
# PART 1
# Exploratory and predictive analysis from observed data
# 0) Good practices, clean variables, libraries and data
# 1) Description of users general behaviour
# 2) Estimate effect of "sign_up" into "revenue"
# 3) Evaluating model's predictive capacity
# 4) Evaluation: ¿is there correlation or causality?
# 5)?Should there be an experiment to confirm findings?
# -----------------------------------------------------

# -----------------------------------------------------
# 0) Good practices, clean variables and libraries
# -----------------------------------------------------

# Clean environment and libraries
rm(list = ls())

require(pacman)
p_load(dplyr, tidyr, readr, ggplot2, corrplot)

# Load data
db <- read_csv("data/cleaned_data.csv")

# See variables
vars <- colnames(db)

# -----------------------------------------------------
# 1) Description of users general behaviour
# -----------------------------------------------------
# Summary statistics
summary(db)

# Get numeric variables
numeric_vars <- sapply(df, is.numeric)

# See pairs
#pairs(~IngresosSemanales+TiempoPromedioServicio+ExperienciaEstilista+PrecioServicioPromedio, data=DatosTarea5_punto2)


# Correlogram for numeric variables
cor_matrix <- cor(df[, numeric_vars], use = "complete.obs")
corrplot(cor_matrix, method = "circle")

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


