#create dataframe
elec <- data.frame(elec = c(119.394,199.367,198.535,190.333,186.971,198.023),
                    bar = c(0.049974,0.012633,0.008229,-0.004431,-0.008142,0.015348))
diff <-as.data.frame(c(10.386002,10.642529,10.530488,
                        10.559434,10.641665,10.504206))
# add column **cbind **
elec <- cbind(elec,diff)

# change column name ** names **
names(elec)[3] <- c("ec")

# read.csv
sluge <- read.csv("bukang.csv")
totalelec <- cbind(totalelec,sluge)

# correlation
cor(totalelec[1:3,]$bar,totalelec[1:3,]$sludge)
cor(totalelec[4:6,]$bar,totalelec[4:6,]$sludge)