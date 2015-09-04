library(reshape)

preprocess <- function(filename, dateformat='%Y-%m-%d', index=FALSE, fund=TRUE) {
  df <- read.table(filename, sep=',', header=TRUE, stringsAsFactors=FALSE)
  df$Date <- as.Date(df$Date, dateformat)
  name <- strsplit(filename, '.', fixed=TRUE)
  close <- paste(name[[1]][1], 'close', sep='_')
  net <- paste(name[[1]][1], 'net', sep='_')
  df <- rename(df, c(Close=close))
  df <- rename(df, c(Net=net))

  df <- df[order(df$Date),]
  if(index) {
    index_rate <- paste(name[[1]][1], 'index', 'rate', sep='_')
    for (i in 2:length(df$Date))
      df[i, index_rate] <- (df[i, close] / df[i - 1, close] - 1) * 100
    df[1, index_rate] <- 0
  }

  if(fund) {
    fund_rate <- paste(name[[1]][1], 'fund', 'rate', sep='_')
    for (i in 2:length(df$Date)) df[i, fund_rate] <- (df[i, net] / df[i - 1, net] - 1) * 100
    df[1, fund_rate] <- 0
  }

  return(df)
}

fund_income <- function(name, df, startDate=df[1, "Date"]) {
  net <- paste(name, 'net', sep='_')
  fund_income <- paste(name, 'fund', 'income', sep='_')
  df_date <- df[which(df[, "Date"] >= startDate), "Date"]
  df_fund_income <- (df[which(df[,"Date"] >= startDate), net] / df[which(df[,"Date"] == startDate), net] - 1) * 100
  newdf <- data.frame(df_date, df_fund_income)
  newdf <- rename(newdf, c(df_date="Date", df_fund_income=fund_income))
  return(newdf)
}

index_income <- function(name, df, startDate=df[1, "Date"]) {
  close <- paste(name, 'close', sep='_')
  index_income <- paste(name, 'index', 'income', sep='_')
  df_date <- df[which(df[, "Date"] >= startDate), "Date"]
  df_index_income <- (df[which(df[,"Date"] >= startDate), close] / df[which(df[,"Date"] == startDate), close] - 1) * 100
  newdf <- data.frame(df_date, df_index_income)
  newdf <- rename(newdf, c(df_date="Date", df_index_income=index_income))
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

  df_index_income <- index_income(name, df, startDate=startDate)
  df_fund_income <- fund_income(name, df, startDate=startDate)
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

i100 <- preprocess('i100.csv', dateformat='%Y/%m/%d', index=TRUE)
tj100 <- preprocess('tj100.csv', index=TRUE)
hs300 <- preprocess('hs300.csv', index=TRUE)
#stat_index_and_fund('i100', i100, startDate=as.Date('2015-06-03'))
#stat_index_and_fund('tj100', tj100, startDate=as.Date('2015-06-03'))
#stat_index_and_fund('hs300', hs300)
ss000001 <- preprocess('ss000001.csv', index=TRUE, fund=FALSE)
fund_210004 <- preprocess('fund_210004.csv')
fund_540003 <- preprocess('fund_540003.csv')
start <- as.Date('2015-06-03')
cmp_df <- fund_income('i100', i100, startDate=start)
cmp_df <- merge(cmp_df, fund_income('tj100', tj100, startDate=start), by="Date")
cmp_df <- merge(cmp_df, fund_income('hs300', hs300, startDate=start), by="Date")
cmp_df <- merge(cmp_df, index_income('ss000001', ss000001, startDate=start), by="Date")
cmp_df <- merge(cmp_df, fund_income('fund_210004', fund_210004, startDate=start), by="Date")
cmp_df <- merge(cmp_df, fund_income('fund_540003', fund_540003, startDate=start), by="Date")

max4p <- max(cmp_df$i100_fund_income, cmp_df$tj100_fund_income, cmp_df$hs300_fund_income, cmp_df$ss000001_index_income, cmp_df$fund_210004_fund_income, cmp_df$fund_540003_fund_income)
min4p <- min(cmp_df$i100_fund_income, cmp_df$tj100_fund_income, cmp_df$hs300_fund_income, cmp_df$ss000001_index_income, cmp_df$fund_210004_fund_income, cmp_df$fund_540003_fund_income)

dev.new()

plot(cmp_df$i100_fund_income, type='b', col='red', xaxt='n', xlab='Date', ylab='Income', ylim=c(min4p, max4p))
lines(cmp_df$tj100_fund_income, type='b', col='blue')
lines(cmp_df$hs300_fund_income, type='b', col='yellow')
lines(cmp_df$ss000001_index_income, type='b', col='green')
lines(cmp_df$fund_210004_fund_income, type='b', col='grey')
lines(cmp_df$fund_540003_fund_income, type='b', col='black')
axis(1, 1:length(cmp_df$Date), labels=cmp_df$Date)
