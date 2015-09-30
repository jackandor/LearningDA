library(reshape)

preprocess_index <- function (filename, name, dateformat='%Y-%m-%d') {
  df <- read.table(filename, sep=',', header=TRUE, stringsAsFactors=FALSE)
  df$Date <- as.Date(df$Date, dateformat)
  close <- paste(name, 'close', sep='_')
  df <- rename(df, c(Close=close))
  df <- df[order(df$Date),]
  index_rate <- paste(name, 'index', 'rate', sep='_')
  for (i in 2:length(df$Date))
    df[i, index_rate] <- (df[i, close] / df[i - 1, close] - 1) * 100
  df[1, index_rate] <- 0
  return(df)
}

preprocess_fund <- function (filename, name, dateformat='%Y-%m-%d') {
  df <- read.table(filename, sep=',', header=TRUE, stringsAsFactors=FALSE)
  df$Date <- as.Date(df$Date, dateformat)
  net <- paste(name, 'net', sep='_')
  df <- rename(df, c(Net=net))
  df <- df[order(df$Date),]
  fund_rate <- paste(name, 'fund', 'rate', sep='_')
  for (i in 2:length(df$Date)) df[i, fund_rate] <- (df[i, net] / df[i - 1, net] - 1) * 100
  df[1, fund_rate] <- 0
  return(df)
}

preprocess_index_and_fund <- function(index_filename, fund_filename, name, dateformat='%Y-%m-%d') {
  df_index <- preprocess_index(index_filename, name, dateformat=dateformat)
  df_fund <- preprocess_fund(fund_filename, name, dateformat=dateformat)
  return(merge(df_index, df_fund, by='Date'))
}

income <- function(name, typeName1, typeName2, df, startDate=df[1, "Date"]) {
  var1_name <- paste(name, typeName1, sep='_')
  var2_name <- paste(name, typeName2, 'income', sep='_')
  df_date <- df[which(df[, "Date"] >= startDate), "Date"]
  df_income <- (df[which(df[,"Date"] >= startDate), var1_name] / df[which(df[,"Date"] == startDate), var1_name] - 1) * 100
  newdf <- data.frame(df_date, df_income)
  newdf <- rename(newdf, c(df_date="Date", df_income=var2_name))
  return(newdf)
}

stat_index_and_fund <- function(name, df, startDate=df[1, "Date"]) {
  index_rate <- paste(name, 'index', 'rate', sep='_')
  fund_rate <- paste(name[[1]][1], 'fund', 'rate', sep='_')
  df <- na.omit(df)
  dev.new()
  plot(df[,index_rate], type='b', col='red', xaxt='n', xlab='Date', ylab='Rate')
  lines(df[,fund_rate], type='b', col='blue')
  axis(1, 1:length(df$Date), labels=df$Date)

  close <- paste(name, 'close', sep='_')
  net <- paste(name, 'net', sep='_')
  fix_rate <- paste(name, 'fix', 'rate', sep='_')
  df[,fix_rate] <- df[,close]/df[,net]
  dev.new()
  plot(df[,fix_rate], xaxt='n', xlab='Date', ylab='Rate')
  axis(1, 1:length(df$Date), labels=df$Date)

  df_index_income <- income(name, 'close', 'index', df, startDate=startDate)
  df_fund_income <- income(name, 'net', 'fund', df, startDate=startDate)
  newdf <- merge(df_index_income, df_fund_income, by='Date')

  max4p <- max(newdf[, 2], newdf[, 3])
  min4p <- min(newdf[, 2], newdf[, 3])
  dev.new()
  plot(newdf[, 2], type='b', col='red', xaxt='n', xlab='Date', ylab='Income', ylim=c(min4p, max4p))
  lines(newdf[, 3], type='b', col='blue')
  axis(1, 1:length(newdf$Date), labels=newdf$Date)

  tmp <- df[which(df[,"Date"] >= startDate),fund_rate] - df[which(df[,"Date"] >= startDate),index_rate]
  print(paste(name, 'mean is', mean(tmp)))
  print(paste(name, 'sd is', sd(tmp)))
}

myFundIncome <- function(df, fund, fundCode, netName) {
  fundAmount <- paste(fundCode, 'Amount', sep='_')
  fundInvestment <- paste(fundCode, 'Investment', sep='_')
  myfund_income <- paste(fundCode, 'myfund_income', sep='_')
  myfund_investment <- paste(fundCode, 'myfund_investment', sep='_')
  myfund_income_rate <- paste(fundCode, 'myfund_income_rate', sep='_')
  for (i in 1:length(df$Date)) {
    tmp <- fund[fund$Date <= df[i,'Date'], ]
    df[i, myfund_income] <- (tmp[order(tmp$Date, decreasing=T),][1, fundAmount] * df[i, netName])
    df[i, myfund_investment] <- tmp[order(tmp$Date, decreasing=T),][1, fundInvestment]
    df[i, myfund_income_rate] <- (df[i, myfund_income] / df[i, myfund_investment] - 1) * 100
    if (is.nan(df[i, myfund_income_rate])) df[i, myfund_income_rate] <- 0
  }
  return(df)
}

i100 <- preprocess_index_and_fund('sz399415.csv', 'fund_001113.csv', 'i100')
tj100 <- preprocess_index_and_fund('h30537.csv', 'fund_001243.csv', 'tj100')
hs300 <- preprocess_index_and_fund('sz399300.csv', 'fund_000961.csv', 'hs300')
#stat_index_and_fund('i100', i100, startDate=as.Date('2015-06-03'))
#stat_index_and_fund('tj100', tj100, startDate=as.Date('2015-06-03'))
#stat_index_and_fund('hs300', hs300)
ss000001 <- preprocess_index('ss000001.csv', 'ss000001')
fund_210004 <- preprocess_fund('fund_210004.csv', 'fund_210004')
fund_540003 <- preprocess_fund('fund_540003.csv', 'fund_540003')
start <- as.Date('2015-06-03')
cmp_df <- income('i100', 'net', 'fund', i100, startDate=start)
cmp_df <- merge(cmp_df, income('tj100', 'net', 'fund', tj100, startDate=start), by="Date")
cmp_df <- merge(cmp_df, income('hs300', 'net', 'fund', hs300, startDate=start), by="Date")
cmp_df <- merge(cmp_df, income('ss000001', 'close', 'index', ss000001, startDate=start), by="Date")
cmp_df <- merge(cmp_df, income('fund_210004', 'net', 'fund', fund_210004, startDate=start), by="Date")
cmp_df <- merge(cmp_df, income('fund_540003', 'net', 'fund', fund_540003, startDate=start), by="Date")

net_df <- i100[, c('Date', 'i100_net')]
net_df <- merge(net_df, tj100[, c('Date', 'tj100_net')], by='Date')
net_df <- merge(net_df, hs300[, c('Date', 'hs300_net')], by='Date')
net_df <- na.omit(net_df)

#calculate my fund income
myfund <- read.table('myfund.csv', sep=',', header=TRUE, stringsAsFactors=FALSE)
myfund$Fund <- sprintf('%06d', myfund$Fund)
myfund$Fund <- as.factor(myfund$Fund)
md <- melt(myfund, id=(c('Date', 'Fund')))
tmp <- cast(md, Date~Fund+variable, sum)
tmp[,c('000961_Amount')] <- cumsum(tmp[,c('000961_Amount')])
tmp[,c('001113_Amount')] <- cumsum(tmp[,c('001113_Amount')])
tmp[,c('001243_Amount')] <- cumsum(tmp[,c('001243_Amount')])
tmp[,c('000961_Investment')] <- cumsum(tmp[,c('000961_Investment')])
tmp[,c('001113_Investment')] <- cumsum(tmp[,c('001113_Investment')])
tmp[,c('001243_Investment')] <- cumsum(tmp[,c('001243_Investment')])
myfund <- tmp

hs300_myfund_income_rate <- myFundIncome(hs300, tmp, '000961', 'hs300_net')
print(paste('hs300 fund income rate is:', hs300_myfund_income_rate[length(hs300_myfund_income_rate$Date),'000961_myfund_income_rate']))
cmp_df <- merge(cmp_df, hs300_myfund_income_rate[, c('Date', '000961_myfund_income_rate')], by="Date")
i100_myfund_income_rate <- myFundIncome(i100, tmp, '001113', 'i100_net')
print(paste('i100 fund income rate is:', i100_myfund_income_rate[length(i100_myfund_income_rate$Date),'001113_myfund_income_rate']))
cmp_df <- merge(cmp_df, i100_myfund_income_rate[, c('Date', '001113_myfund_income_rate')], by="Date")
tj100_myfund_income_rate <- myFundIncome(tj100, tmp, '001243', 'tj100_net')
print(paste('tj100 fund income rate is:', tj100_myfund_income_rate[length(tj100_myfund_income_rate$Date),'001243_myfund_income_rate']))
cmp_df <- merge(cmp_df, tj100_myfund_income_rate[, c('Date', '001243_myfund_income_rate')], by="Date")

#plot all income
max4p <- max(cmp_df$i100_fund_income, cmp_df$tj100_fund_income, cmp_df$hs300_fund_income, cmp_df$ss000001_index_income, cmp_df$fund_210004_fund_income, cmp_df$fund_540003_fund_income)
min4p <- min(cmp_df$i100_fund_income, cmp_df$tj100_fund_income, cmp_df$hs300_fund_income, cmp_df$ss000001_index_income, cmp_df$fund_210004_fund_income, cmp_df$fund_540003_fund_income)

dev.new()

plot(cmp_df$i100_fund_income, type='b', col='violet', xaxt='n', xlab='Date', ylab='Income', ylim=c(min4p, max4p))
lines(cmp_df$tj100_fund_income, type='b', col='blue')
lines(cmp_df$hs300_fund_income, type='b', col='yellow')
lines(cmp_df$ss000001_index_income, type='b', col='green')
lines(cmp_df$fund_210004_fund_income, type='b', col='grey')
lines(cmp_df$fund_540003_fund_income, type='b', col='black')
axis(1, 1:length(cmp_df$Date), labels=cmp_df$Date)
