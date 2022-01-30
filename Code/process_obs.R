rm(list = ls())
library(easypackages)
libraries("ggplot2","tidyverse","ggstatsplot","deSolve")

# Path where Data on John P. usuari cluster data is:
# /home/usuaris/j.palmer/research/mosquito_model_data_prep/data/proc/mosquito_alert_raw_reports.Rds
Path <- "/Users/celsaaraujobarja/Documents/mosquito_alert_raw_reports.Rds"
df_data <- readRDS(Path)
df_data_filt <- df_data %>%  group_by(user)
df_data <-  df_data[,c(1,3)]


Path <-  "~/MAD_MODEL/SUR_MODEL/data/register_data_tigausers.csv"
df_reg <-  read.csv(Path, colClasses = c("character", "character"))
colnames(df_reg) <-  c("user","reg_date")

df_merg <-  merge(df_data,df_reg, by="user" )