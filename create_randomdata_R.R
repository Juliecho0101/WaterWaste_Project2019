# 일정한 시간간격 생성하여 컬럼별 난수데이터 생성하기
data1 <- as.Date("2019-08-01")
#z1 <- seq.POSIXt(as.POSIXct(data1), as.POSIXct(data1+7), by = "1 min")
df1 <- data.frame(GET_DATE =seq.POSIXt(as.POSIXct(data1), as.POSIXct(data1+7), by = "1 min"),
                  VC_VAL = rtruncnorm(n=10081, a=100, b=300, mean=220, sd=20),
                  AC_VAL = rtruncnorm(n=10081, a=0, b=1, mean=0.6, sd=1),
                  TPOW  = rtruncnorm(n=10081, a=50, b=100, mean=80, sd=10),
                  PAVR = rtruncnorm(n=10081, a=5, b=12, mean=8, sd=1),
                  EC_VAL_I = rtruncnorm(n=10081, a=0.01, b=50, mean=20, sd=10),
                  EC_VAL_O = rtruncnorm(n=10081, a=0.01, b=50, mean=20, sd=10),
                  EC_T_I = rtruncnorm(n=10081, a=0.01, b=50, mean=15, sd=10),
                  EC_T_O = rtruncnorm(n=10081, a=0.01, b=50, mean=15, sd=10),
                  PD2_VALUE = rtruncnorm(n=10081, a=-5, b=5, mean=0, sd=1))

# 데이터 확인
summary(df1)
str(df1)
head(df1)

# 현재 datetime인 GET_DATE > YYYYmmddhhMMss
library(dplyr)
df1 <- df1 %>% mutate(DATE = format(GET_DATE,"%Y%m%d%H%M%S"))
write.xlsx(df1,"DATA_ECPH.xlsx")

