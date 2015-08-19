library(reshape)

i100 <- read.table("i100.csv", sep=',', header=TRUE, stringsAsFactors=FALSE)
i100$Date <- as.Date(i100$Date, "%Y/%m/%d")
i100 <- rename(i100, c(Close="i100_close", Net="i100_net"))

tj100 <- read.table("tj100.csv", sep=',', header=TRUE, stringsAsFactors=FALSE)
tj100$Date <- as.Date(tj100$Date, "%Y-%m-%d")
tj100 <- rename(tj100, c(Close="tj100_close", Net="tj100_net"))

hs300 <- read.table("hs300.csv", sep=',', header=TRUE, stringsAsFactors=FALSE)
hs300$Date <- as.Date(hs300$Date, "%Y-%m-%d")
hs300 <- rename(hs300, c(Net="hs300_net", Close="hs300_close"))

ss000001 <- read.table("ss000001.csv", sep=',', header=TRUE, stringsAsFactors=FALSE)
ss000001$Date <- as.Date(ss000001$Date, "%Y-%m-%d")
ss000001 <- rename(ss000001, c(Close="ss000001_close"))

candidate <- read.table("candidate_210004.csv", sep=',', header=TRUE, stringsAsFactors=FALSE)
candidate$Date <- as.Date(candidate$Date, "%Y-%m-%d")
candidate <- rename(candidate, c(Net="candidate_net"))


cmp_df <- merge(i100, tj100, by='Date')
cmp_df <- merge(cmp_df, hs300, by='Date')
cmp_df <- merge(cmp_df, ss000001, by='Date')
cmp_df <- merge(cmp_df, candidate, by='Date')

cmp_df <- na.omit(cmp_df)

cmp_df <- cmp_df[order(cmp_df$Date),]

start <- 1

cmp_df$i100_index_income <- (cmp_df$i100_close / cmp_df[start, 'i100_close'] - 1) * 100
cmp_df$i100_fund_income <- (cmp_df$i100_net /cmp_df[start, 'i100_net'] - 1) * 100
cmp_df$tj100_index_income <- (cmp_df$tj100_close / cmp_df[start, 'tj100_close'] - 1) * 100
cmp_df$tj100_fund_income <- (cmp_df$tj100_net / cmp_df[start, 'tj100_net'] - 1) * 100
cmp_df$hs300_index_income <-(cmp_df$hs300_close / cmp_df[start, 'hs300_close'] - 1) * 100
cmp_df$hs300_fund_income <- (cmp_df$hs300_net / cmp_df[start, 'hs300_net'] - 1) * 100
cmp_df$ss000001_index_income <- (cmp_df$ss000001_close / cmp_df[start, 'ss000001_close'] - 1) * 100
cmp_df$candidate_fun_income <- (cmp_df$candidate_net / cmp_df[start, 'candidate_net'] - 1) * 100


max4p <- max(cmp_df$i100_index_income, cmp_df$i100_fund_income, cmp_df$tj100_index_income, cmp_df$tj100_fund_income, cmp_df$hs300_index_income, cmp_df$hs300_fund_income, cmp_df$ss000001_index_income, cmp_df$candidate_fun_income)
min4p <- min(cmp_df$i100_index_income, cmp_df$i100_fund_income, cmp_df$tj100_index_income, cmp_df$tj100_fund_income, cmp_df$hs300_index_income, cmp_df$hs300_fund_income, cmp_df$ss000001_index_income, cmp_df$candidate_fun_income)

plot(cmp_df$i100_index_income, type='b', col='red', xaxt='n', xlab='Date', ylab='Income', ylim=c(min4p, max4p))
lines(cmp_df$tj100_index_income, type='b', col='blue')
lines(cmp_df$i100_fund_income, type='b', col='pink')
lines(cmp_df$tj100_fund_income, type='b', col='grey')
lines(cmp_df$hs300_index_income, type='b', col='maroon')
lines(cmp_df$hs300_fund_income, type='b', col='yellow')
lines(cmp_df$ss000001_index_income, type='b', col='green')
lines(cmp_df$candidate_fun_income, type='b', col='black')
axis(1, 1:length(cmp_df$Date), labels=cmp_df$Date)
