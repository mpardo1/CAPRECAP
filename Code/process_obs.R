rm(list = ls())
library(easypackages)
libraries("ggplot2","tidyverse","ggstatsplot","deSolve")

# Path where Data on John P. usuari cluster data is:
# /home/usuaris/j.palmer/research/mosquito_model_data_prep/data/proc/mosquito_alert_raw_reports.Rds
Path <- "~/CAPRECAP/data/mosquito_alert_raw_reports.Rds"
df_data <- readRDS(Path)
df_data <-  df_data[,c(3,4,5,9,2)]
df_data$report_date <- as.Date(substr(df_data$server_upload_time,1,10), "%Y-%m-%d")
df_data <-  df_data[order(df_data$user,df_data$report_id,df_data$report_date),]
df_data[which(df_data$user == "01025433-6FE6-40E2-9B3C-978964911C46"),]


# Filter the users which have edit the reports:
user_edit_rep <-  df_data %>% group_by(user,report_id) %>% summarize(count=n(), max_version = max(version_number))
df_data[which(df_data$user == "00012362-528A-496B-BFE5-06E61D2642F9" & df_data$report_id == "TkEH"),]

# Register data:
Path <-  "~/MAD_MODEL/SUR_MODEL/data/register_data_tigausers.csv"
df_reg <-  read.csv(Path, colClasses = c("character", "character"))
colnames(df_reg) <-  c("user","reg_date")

  
# Check the difference in the user_id between the 2 datasets, now there are many that are not in 
# df_ref since I have not updated this file.
no_reg_user <-  setdiff(df_data$user,df_reg$user)
dismatch_el <- length(no_reg_user)
length(unique(df_data$user))
length(unique(df_reg$user))
# Check if a particular user_id is in the register list,
df_reg[which(df_reg$user == "332be43c-59d9-426b-9d4a-519d813d2f0e"),]
df_data[which(df_data$user %in% no_reg_user),]
# Join two df by user code:
df_merg <-  merge(df_data,df_reg, by="user" )


# sort by user code and date:
df_filt <- df_merg[order(df_merg$user, df_merg$server_upload_time),]
# If we want to consider bites and adults:
# df_filt <- df_filt %>% filter((type == "bite" | type == "adult") & version_number == 0)
# If we want to consider just adult mosquitoes.
df_filt <- df_filt %>% filter(type == "adult" & version_number == 0)
df_filt$version_number <-  NULL
count_dist <- df_filt %>% group_by(user) %>% summarize(count=n())
# Take the users with more than N reports.
# N<- 90
# count_dist <- count_dist %>% filter(count > N)

# Count the number of reports associated with each user:
df_filt$report_weekday <- weekdays(df_filt$report_date)
df_filt$server_upload_time <- NULL
# Tip meritxel: take 
day_in <-  c("Saturday","Sunday", "Thursday" ,"Friday")
df_merg <-  df_filt[which(df_filt$report_weekday %in% day_in)  ,] 
df_merg$reg_date <- as.Date(substr(df_merg$reg_date,1,10), "%Y-%m-%d")
df_group <- df_merg %>% group_by(user, report_date, type) %>%
  summarise(type = min(type), reg_date = min(reg_date), report_id = min(report_id))

df_group <-  df_group[which(lubridate::year(df_group$report_date) == 2019 &
                              lubridate::year(df_group$reg_date) == 2019 &
                              lubridate::month(df_group$report_date) > 2 &
                              lubridate::month(df_group$report_date) < 11),]

min_date <- min(min(df_group$reg_date, df_group$report_date))
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
# loop with just adults:
for(i in c(1:l)){
  if(df_merg$type[i] == "adult"){
    # print("adult")
    mat[df_merg$user_id[i], df_merg$date_id_1[i]]  <- 1
  }
  if(df_merg$date_id[i] == df_merg$date_id_1[i]){
    print(paste0("i:",i))
    mat[df_merg$user_id[i], df_merg$date_id_1[i]] <- 3
  }else if(mat[df_merg$user_id[i], df_merg$date_id[i]] != 3){
        mat[df_merg$user_id[i], df_merg$date_id[i]] <- 2
  }
}

# Select a 50 observations randomly:
max_row <-  nrow(mat)
N <-  50
rand_vec <-  sample(1:max_row,N)
mat_filt <-  mat[rand_vec,]
Path <- paste0("~/CAPRECAP/Output/observations_example_", min_date,"_", max_date,".dat")
write.table(mat_filt,Path, row.names = FALSE, col.names = FALSE, sep="")

# Loop with bites and mosquito adults:
# for(i in c(1:l)){
#   if(df_merg$type[i] == "adult"){
#     # print("adult")
#     mat[df_merg$user_id[i], df_merg$date_id_1[i]]  <- 1
#   }else{
#     mat[df_merg$user_id[i], df_merg$date_id_1[i]]  <- 4
#   }
#  
#   if(df_merg$date_id[i] == df_merg$date_id_1[i]){
#     print(paste0("i:",i))
#     mat[df_merg$user_id[i], df_merg$date_id_1[i]] <- 3
#   }else if(mat[df_merg$user_id[i], df_merg$date_id[i]] != 3){
#     mat[df_merg$user_id[i], df_merg$date_id[i]] <- 2
#   }
# }

obs <- as.data.frame(mat)
colnames(obs) <- as.character(vec_date)


df_sort <- df_merg[order(df_merg$user_id, df_merg$date_id_1),]