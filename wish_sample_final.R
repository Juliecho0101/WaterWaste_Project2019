library(lubridate)
library(readxl)
library(readr)
#1.generate machine operation start time randomly
## runif(n,min,max) 
a1 <- as.POSIXct('2019-01-01 08:00:00',origin="1970-01-01")
a2 <- as.POSIXct('2019-01-01 10:00:00',origin="1970-01-01")
op_random <- runif(202,a1,a2)
op_start <- strftime(as.POSIXct(op_random,origin="1970-01-01"),format="%H:%M:%S")
op_start <- as.data.frame(op_start)

write.xlsx(op_start,"op_start.xlsx")

#샘플데이터 받은 것 저장 
library(readr)
library(dplyr)
db_sample_origin <- read_csv("db_upload_ecph_csv.csv")

db_sample_origin <- db_sample_origin %>% filter(!is.na(date))

summary(db_sample_origin)

#일자별 날짜로우 생성  --nias 과제에서 했었음. 

date_day <- as.vector(db_sample_origin$date)
date_cnt <- as.vector(db_sample_origin$row)
date_day_cnt <- rep(as.Date(date_day,origin="1970-01-01"),date_cnt)  #172887
rm(date_day_cnt)

# 일별 운영시간 동안의 
library(lubridate)
class(op_start)
rowcnt <- as.data.frame(db_sample_origin$row)
optime_cnt <- cbind(db_sample_origin$date,op_start,rowcnt)
colnames(optime_cnt)[3] <- "time_cnt"
colnames(optime_cnt)[1] <- "date"

optime_cnt$op_start <- as.character(optime_cnt$op_start)

optime_cnt <- optime_cnt %>% mutate(datetime = as.POSIXct(paste(optime_cnt$date,optime_cnt$op_start),format="%Y-%m-%d %H:%M:%S"))

#finish time 추가 
optime_cnt <- optime_cnt %>% mutate(endtime = datetime+minutes(time_cnt))

str(optime_cnt)

##############################  seLee 코드 반영 19.11.11
# times: 생성하고자하는 시계열 데이터셋 기본틀 (변수: times = POSIXct("2019-10-01 10:00"))
# temp: for문 안에서 rbind할 임시 데이터셋
times <- as.data.frame(as.POSIXct("2019-11-11 09:00:00")) 
#colnames(times)[1] <- "seq"
temp <- as.data.frame(as.POSIXct("2019-11-11 09:00:00"))


#colnames(temp)[1] <- "seq"

for(i in 1:nrow(optime_cnt)){
        temp <- as.data.frame(seq(optime_cnt$datetime[i],optime_cnt$endtime[i], by ="min"))
        colnames(temp) <- colnames(times)
        times <- rbind(times,temp)
}

# 컬럼명 바꾸고 
colnames(times)[1] <- "time_seq"
times <-  times[-1,]
times2<- as.data.frame(times)


# origin csv (1일 1row)에 start_time, end_time 컬럼 추가.  cbind 사용할 것
ecph_add_startend <- cbind(db_sample_origin,optime_cnt[,3:5])

# 번외편 ,,, SD
sd_new <- data.frame(ec_i_sdnew = runif(202,1.932,2.137),
                     ec_o_sdnew = runif(202,1.429,1.502),
                     ph_i_sdnew = runif(202,0.270,0.287),
                     ph_o_sdnew = runif(202,0.270,0.274),
                     bar_sdnew = runif(202,0.04,0.053),
                     t_i_sdnew = runif(202,0.442,0.482),
                     t_o_sdnew = runif(202,0.754,0.784))

#sd 수정 
ecph_add_startend$ec_i_sd <- sd_new$ec_i_sdnew
ecph_add_startend$ec_o_sd <- sd_new$ec_o_sdnew
ecph_add_startend$ph_I_sd <- sd_new$ph_i_sdnew
ecph_add_startend$ph_o_sd <- sd_new$ph_o_sdnew
ecph_add_startend$bar_sd <- sd_new$bar_sdnew 
ecph_add_startend$t_i_sd <- sd_new$t_i_sdnew
ecph_add_startend$t_o_sd <- sd_new$t_o_sdnew

# 위의 for loop 만든 것 처럼 한개 생성해서   ;;  test : ss_add
library(truncnorm)

time_csv <- as.data.frame(as.POSIXct("2019-11-11 09:00:00")) 
time_temp <- as.data.frame(as.POSIXct("2019-11-11 09:00:00"))

elec_csv <- as.data.frame(8.62) 
elec_temp <- as.data.frame(8.62)

ec_i_csv <- as.data.frame(3.30)
ec_i_temp <- as.data.frame(3.30)

ec_o_csv <- as.data.frame(3.30)
ec_o_temp <- as.data.frame(3.30)

ph_I_csv <- as.data.frame(3.30)
ph_I_temp <- as.data.frame(3.30)

ph_o_csv <- as.data.frame(3.30)
ph_o_temp <- as.data.frame(3.30)

bar_csv <- as.data.frame(3.30)
bar_temp <- as.data.frame(3.30)

t_i_csv <- as.data.frame(3.30)
t_i_temp <- as.data.frame(3.30)

t_o_csv <- as.data.frame(3.30)
t_o_temp <- as.data.frame(3.30)

my_data <- data.frame(date = as.POSIXct("2019-11-11 09:00:00"),
                         elec = 1.00,
                         ec_i = 1.00,
                         ec_o = 1.00,
                         ph_I = 1.00,
                         ph_o = 1.00,
                         bar = 1.00,
                         t_i = 1.00,
                         t_o = 1.00 )

for(i in 1:nrow(ecph_add_startend)){
        time_temp <- as.data.frame(seq(ecph_add_startend$datetime[i],ecph_add_startend$endtime[i], by ="min"))
        colnames(time_temp) <- colnames(time_csv)
        time_csv <- rbind(time_csv,time_temp) #시계열 시퀀
        
        elec_temp <- as.data.frame(seq(ecph_add_startend$elec_min[i],ecph_add_startend$elec_max[i],length.out = nrow(time_temp)))
        colnames(elec_temp) <- colnames(elec_csv)
        elec_csv <- rbind(elec_csv,elec_temp) #전력량
                
        ec_i_temp <- as.data.frame(rtruncnorm(n=nrow(time_temp),a=ecph_add_startend$ec_i_min[i],b=ecph_add_startend$ec_i_max[i]
                                              ,mean = ecph_add_startend$ec_i_mean[i],sd = ecph_add_startend$ec_i_sd[i]))
        colnames(ec_i_temp) <- colnames(ec_i_csv)
        ec_i_csv <-　rbind(ec_i_csv,ec_i_temp) #전기전도도_유입수 
        
        ec_o_temp <- as.data.frame(rtruncnorm(n=nrow(time_temp),a=ecph_add_startend$ec_o_min[i],b=ecph_add_startend$ec_o_max[i]
                                              ,mean = ecph_add_startend$ec_o_mean[i],sd = ecph_add_startend$ec_o_sd[i]))
        colnames(ec_o_temp) <- colnames(ec_o_csv)
        ec_o_csv <-　rbind(ec_o_csv,ec_o_temp)  #전기전도도_처리수 
        
        ph_I_temp <- as.data.frame(rtruncnorm(n=nrow(time_temp),a=ecph_add_startend$ph_I_min[i],b=ecph_add_startend$ph_I_max[i],mean = ecph_add_startend$ph_I_mean[i],sd = ecph_add_startend$ph_I_sd[i]))
        colnames(ph_I_temp) <- colnames(ph_I_csv)
        ph_I_csv <-　rbind(ph_I_csv,ph_I_temp) # pH_in 
        
        ph_o_temp <- as.data.frame(rtruncnorm(n=nrow(time_temp),a=ecph_add_startend$ph_o_min[i],b=ecph_add_startend$ph_o_max[i]
                                              ,mean = ecph_add_startend$ph_o_mean[i],sd = ecph_add_startend$ph_o_sd[i]))
        colnames(ph_o_temp) <- colnames(ph_o_csv)
        ph_o_csv <-　rbind(ph_o_csv,ph_o_temp) # pH_out
        
        bar_temp <- as.data.frame(rtruncnorm(n=nrow(time_temp),a=ecph_add_startend$bar_max[i],b=ecph_add_startend$bar_min[i]
                                              ,mean = ecph_add_startend$bar_mean[i],sd = ecph_add_startend$bar_sd[i]))
        colnames(bar_temp) <- colnames(bar_csv)
        bar_csv <-　rbind(bar_csv,bar_temp) #bar 
        
        t_i_temp <- as.data.frame(rtruncnorm(n=nrow(time_temp),a=ecph_add_startend$t_i_min[i],b=ecph_add_startend$t_i_max[i]
                                              ,mean = ecph_add_startend$t_i_mean[i],sd = ecph_add_startend$t_i_sd[i]))
        colnames(t_i_temp) <- colnames(t_i_csv)
        t_i_csv <-　rbind(t_i_csv,t_i_temp) #tempeature_in
        
        t_o_temp <- as.data.frame(rtruncnorm(n=nrow(time_temp),a=ecph_add_startend$t_o_min[i],b=ecph_add_startend$t_o_max[i]
                                             ,mean = ecph_add_startend$t_o_mean[i],sd = ecph_add_startend$t_o_sd[i]))
        colnames(t_o_temp) <- colnames(t_o_csv)
        t_o_csv <-　rbind(t_o_csv,t_o_temp) #tempeature_out
        
         
        my_data <- cbind(time_csv, elec_csv, ec_i_csv,ec_o_csv, ph_I_csv, ph_o_csv,bar_csv,t_i_csv,t_o_csv) 
}

## save results of for loops 
ecph_final <- my_data

ecph_final <-ecph_final[-1,]

## dataframe column name change
#names(ecph_final) <- c("date","elec","ec_i","ec_o","ph_I","ph_o","bar","t_i","t_o")
names(ecph_final) <- c("DATE","TPOW","EC_VAL_I","EC_VAL_O","PH_VAL_I","PH_VAL_O","PD2_VALUE","EC_T_I","EC_T_O","GET_DATE")                            
## Posixct to chr 
ecph_final <- ecph_final %>% mutate(GET_DATE = format(DATE,"%Y%m%d%H%M%S"))

head(ecph_final)
## save the results 
write.table(ecph_final,"ecph_final.txt", sep=",",col.names = TRUE, row.names = FALSE)
