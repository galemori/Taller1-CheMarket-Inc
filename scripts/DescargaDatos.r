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

# -----------------------------------------------------
# PART B
# -----------------------------------------------------
parte_b <- readRDS("Parte_B.rds")

# Verify no null data
colSums(is.na(parte_b))
