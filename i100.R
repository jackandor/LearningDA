i100 <- read.table("i100.csv", sep=',', header=TRUE, stringsAsFactors=FALSE)
i100$Date <- as.Date(i100$Date, "%Y/%m/%d")
i100 <- i100[order(i100$Date),]

for (i in 2:length(i100$Date)) i100[i, "IndexRate"] <- (i100[i, "Close"] / i100[i - 1, "Close"] - 1) * 100
i100[1, "IndexRate"] <- 0
for (i in 2:length(i100$Date)) i100[i, "FundRate"] <- (i100[i, "Net"] / i100[i - 1, "Net"] - 1) * 100
i100[1, "FundRate"] <- 0

plot(i100$IndexRate, type='b', col='red', xaxt='n', xlab='Date', ylab='Rate')
lines(i100$FundRate, type='b', col='blue')
axis(1, 1:length(i100$Date), labels=i100$Date)

i100$FixRate <- i100$Close/i100$Net
dev.new()
plot(i100$FixRate, xaxt='n', xlab='Date', ylab='Rate')
axis(1, 1:length(i100$Date), labels=i100$Date)

start <- 1
i100$IndexIncome <- (i100$Close / i100[start, 'Close'] - 1) * 100
i100$FundIncome <- (i100$Net /i100[start, 'Net'] - 1) * 100
max4p <- max(i100$IndexIncome, i100$FundIncome)
min4p <- min(i100$IndexIncome, i100$FundIncome)
dev.new()
plot(i100$IndexIncome, type='b', col='red', xaxt='n', xlab='Date', ylab='Income', ylim=c(min4p, max4p))
lines(i100$FundIncome, type='b', col='blue')
axis(1, 1:length(i100$Date), labels=i100$Date)

