# -----------------------------------------------------
# PART 0
# This script downloads data for both following parts
# 0) Good practices, clean variables and libraries
# 1) d
# 2) Save the data in a structure
# 3) Export the data to a .csv file
# -----------------------------------------------------

# -----------------------------------------------------
# 0) Good practices, clean variables and libraries
# -----------------------------------------------------

# Clean environment and libraries
rm(list = ls())

require(pacman)
p_load(rvest, dplyr, tidyr, readr, httr, jsonlite)


# -----------------------------------------------------
# PART A
# -----------------------------------------------------


# Load data for part A and b (relative path)
parte_a <- readRDS("Parte_A.rds")

# Verify no null data
colSums(is.na(parte_a))

# Define categoric variable "device_type"
parte_a <- parte_a %>%
  mutate(device_type = as.factor(device_type)) %>%
  mutate(is_returning_user = as.factor(is_returning_user)) %>%
  mutate(sign_up = as.factor(sign_up))

## Faltaría para OS_type si se agrega al final #TODO


## Graficas 
ggplot(data = Dta_p1, aes(x = device_type, y = Revenue)) +
  stat_boxplot(aes(group = device_type), geom = "errorbar", width = 0.3,
               color = c("#10a53dFF","#3a5e8cFF", "#541352FF"), size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = device_type),
               color = c("#10a53dFF","#3a5e8cFF", "#541352FF"), fill = c("#10a53dFF","#3a5e8cFF", "#541352FF"),
               size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,10), breaks = seq(0,300,100)) +
  labs(x = "Device type",y = "Revenue")+
  theme_bw()

ggplot(data = Dta_p1, aes(x = os_type, y = Revenue)) +
  stat_boxplot(aes(group = os_type), geom = "errorbar", width = 0.3,
               color = c("#10a53dFF","#3a5e8cFF", "#541352FF"), size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = os_type),
               color = c("#10a53dFF","#3a5e8cFF", "#541352FF"), fill = c("#10a53dFF","#3a5e8cFF", "#541352FF"),
               size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,10), breaks = seq(0,300,100)) +
  labs(x = "Device type",y = "Revenue")+
  theme_bw()


ggplot(data = Dta_p1, aes(x = time_spent, y = Revenue)) +
  stat_boxplot(aes(group = os_type), geom = "errorbar", width = 0.3,
               color = c("#10a53dFF","#3a5e8cFF", "#541352FF"), size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = os_type),
               color = c("#10a53dFF","#3a5e8cFF", "#541352FF"), fill = c("#10a53dFF","#3a5e8cFF", "#541352FF"),
               size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,10), breaks = seq(0,300,100)) +
  labs(x = "Device type",y = "Revenue")+
  theme_bw()

ggplot(data = Dta_p1, aes(x = sign_up, y = Revenue)) +
  stat_boxplot(aes(group = sign_up), geom = "errorbar", width = 0.3,
               color = c("#10a53dFF","#3a5e8cFF"), size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = sign_up),
               color = c("#10a53dFF","#3a5e8cFF"), fill = c("#10a53dFF","#3a5e8cFF"),
               size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,10), breaks = seq(0,300,100)) +
  labs(x = "Sign up",y = "Revenue")+
  theme_bw()







# -----------------------------------------------------
# PART B
# -----------------------------------------------------
parte_b <- readRDS("Parte_B.rds")

# Verify no null data
colSums(is.na(parte_b))
