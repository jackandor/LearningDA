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

plot_index_and_fund <- function(name, df, start=1) {
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

  index_income <- paste(name, 'index', 'income', sep='_')
  fund_income <- paste(name, 'fund', 'income', sep='_')
  df[,index_income] <- (df[,close] / df[start, close] - 1) * 100
  df[,fund_income] <- (df[,net] / df[start, net] - 1) * 100
  max4p <- max(df[,index_income], df[,fund_income])
  min4p <- min(df[,index_income], df[,fund_income])
  dev.new()
  plot(df[,index_income], type='b', col='red', xaxt='n', xlab='Date', ylab='Income', ylim=c(min4p, max4p))
  lines(df[,fund_income], type='b', col='blue')
  axis(1, 1:length(df$Date), labels=df$Date)
}

i100 <- preprocess('i100.csv', dateformat='%Y/%m/%d', index=TRUE)
tj100 <- preprocess('tj100.csv', index=TRUE)
hs300 <- preprocess('hs300.csv', index=TRUE)
plot_index_and_fund('i100', i100)
plot_index_and_fund('tj100', tj100)
plot_index_and_fund('hs300', hs300)
ss000001 <- preprocess('ss000001.csv', index=TRUE, fund=FALSE)
candidate_210004 <- preprocess('candidate_210004.csv')
candidate_540003 <- preprocess('candidate_540003.csv')
