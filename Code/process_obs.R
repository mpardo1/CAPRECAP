rm(list = ls())
library(easypackages)
libraries("ggplot2","tidyverse","ggstatsplot","deSolve")

# Path where Data on John P. usuari cluster data is:
# /home/usuaris/j.palmer/research/mosquito_model_data_prep/data/proc/mosquito_alert_raw_reports.Rds
Path <- "~/CAPRECAP/data/mosquito_alert_raw_reports.Rds"
df_data <- readRDS(Path)
df_data <-  df_data[,c(3,5,9)]


Path <-  "~/MAD_MODEL/SUR_MODEL/data/register_data_tigausers.csv"
df_reg <-  read.csv(Path, colClasses = c("character", "character"))
colnames(df_reg) <-  c("user","reg_date")

# Join two df by user code:
df_merg <-  merge(df_data,df_reg, by="user" )


# sort by user code and date:
df_filt <- df_merg[order(df_merg$user, df_merg$server_upload_time),]
df_filt <- df_sort %>% filter(type == "bite" | type == "adult")
count_dist <- df_filt %>% group_by(user) %>% summarize(count=n())
# Take the users with more than N reports.
N<- 90
count_dist <- count_dist %>% filter(count > N)

df_merg <-  merge(df_filt,count_dist, by="user" )
df_merg$report_date <- as.Date(substr(df_merg$server_upload_time,1,10), "%Y-%m-%d")
df_merg$server_upload_time <- NULL
df_merg$reg_date <- as.Date(substr(df_merg$reg_date,1,10), "%Y-%m-%d")
df_merg <- df_merg[,c(1,3,2,5,4)]
df_group <- df_merg %>% group_by(user, report_date) %>%
  summarise(type = min(type), reg_date = min(reg_date))

min_date <- min(df_group$reg_date)
max_date <- max(max(df_group$reg_date, df_group$report_date))
vec_date <- seq(min_date,max_date,by="days")
df_datereg <- data.frame(date_id = seq(1, length(vec_date),1), reg_date = vec_date)
df_daterep <- data.frame(date_id_1 = seq(1, length(vec_date),1), report_date = vec_date)
vec_user <- unique(df_group$user)
df_user <- data.frame(user_id = seq(1, length(vec_user),1), user = vec_user)
df_merg <-  merge(df_group,df_user, by="user" )
# df_merg$coinc <- ifelse(df_merg$reg_date == df_merg$report_date ,1,0)

df_merg <-  merge(df_merg,df_datereg, by="reg_date" )
df_merg <-  merge(df_merg,df_daterep, by="report_date" )

max_ind <- max(max(df_datereg$date_id, df_daterep$date_id_1))
mat <- matrix(0, ncol = max_ind, nrow = max(df_user$user_id))
l <- length(df_merg$report_date)
for(i in c(1:l)){
  if(df_merg$type[i] == "adult"){
    # print("adult")
    mat[df_merg$user_id[i], df_merg$date_id_1[i]]  <- 1
  }else{
    mat[df_merg$user_id[i], df_merg$date_id_1[i]]  <- 4
  }
 
  if(df_merg$date_id[i] == df_merg$date_id_1[i]){
    print(paste0("i:",i))
    mat[df_merg$user_id[i], df_merg$date_id_1[i]] <- 3
  }else if(mat[df_merg$user_id[i], df_merg$date_id[i]] != 3){
    mat[df_merg$user_id[i], df_merg$date_id[i]] <- 2
  }
}

obs <- as.data.frame(mat)
colnames(obs) <- as.character(vec_date)


df_sort <- df_merg[order(df_merg$user_id, df_merg$date_id_1),]