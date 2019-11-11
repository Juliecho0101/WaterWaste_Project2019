# generate random sd_water > water_csv 
water_sd_new <- data.frame(water_i_sd = runif(202,4.06,5.32),
                     water_o_sd = runif(202,3.87,5.06)
                     )
write.table(water_sd_new,"water_sd.txt", sep=",",col.names = TRUE, row.names = FALSE)

# original csv file (sample from Heo)
library(readr)
library(dplyr)

db_sample_water <- read_csv("db_upload_water_csv.csv")

db_sample_water <- db_sample_water %>% filter(!is.na(date))

db_sample_water <- as.data.frame(cbind(db_sample_water,ecph_add_startend[,35:36]))

# 
library(truncnorm)

time_csv <- as.data.frame(as.POSIXct("2019-11-11 09:00:00")) 
time_temp <- as.data.frame(as.POSIXct("2019-11-11 09:00:00"))

water_i_csv <- as.data.frame(11.11) 
water_i_temp <- as.data.frame(11.11)

water_o_csv <- as.data.frame(11.11)
water_o_temp <- as.data.frame(11.11)

my_water_data <- data.frame(date = as.POSIXct("2019-11-11 09:00:00"),
                      water_i = 11.11,
                      water_o = 11.11,
                       )

for(i in 1:nrow(db_sample_water)){
        time_temp <- as.data.frame(seq(db_sample_water$datetime[i],db_sample_water$endtime[i], by ="10 min"))
        colnames(time_temp) <- colnames(time_csv)
        time_csv <- rbind(time_csv,time_temp) #시계열 시퀀
        
        water_i_temp <- as.data.frame(rtruncnorm(n=nrow(time_temp),a=db_sample_water$water_i_min[i],b=db_sample_water$water_i_max[i],mean = db_sample_water$water_i_mean[i],sd = db_sample_water$water_i_sd[i]))
        colnames(water_i_temp) <- colnames(water_i_csv)
        water_i_csv <-　rbind(water_i_csv,water_i_temp) # water_in 
        
        water_o_temp <- as.data.frame(rtruncnorm(n=nrow(time_temp),a=db_sample_water$water_o_min[i],b=db_sample_water$water_o_max[i],mean = db_sample_water$water_o_mean[i],sd = db_sample_water$water_o_sd[i]))
        colnames(water_o_temp) <- colnames(water_o_csv)
        water_o_csv <-　rbind(water_o_csv,water_o_temp) # water_out
        
        my_water_data <- cbind(time_csv,water_i_csv,water_o_csv) 
}

## save results of for loops 
water_final <- my_water_data

water_final <-water_final[-1,]

## dataframe column name change
names(water_final) <- c("DATE","WATER_LITER_IN","WATER_LITER_OUT")                       
water_final <- water_final %>% mutate(GET_DATE = format(DATE,"%Y%m%d%H%M%S"))

head(water_final)

# save the results 
write.table(water_final,"water_final.txt", sep=",",col.names = TRUE, row.names = FALSE)
