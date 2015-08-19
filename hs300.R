hs300 <- read.table("hs300.csv", sep=',', header=TRUE, stringsAsFactors=FALSE)
hs300 <- na.omit(hs300)
hs300$Date <- as.Date(hs300$Date, "%Y-%m-%d")
hs300 <- hs300[order(hs300$Date),]

for (i in 2:length(hs300$Date)) hs300[i, "IndexRate"] <- (hs300[i, "Close"] / hs300[i - 1, "Close"] - 1) * 100
hs300[1, "IndexRate"] <- 0
for (i in 2:length(hs300$Date)) hs300[i, "FundRate"] <- (hs300[i, "Net"] / hs300[i - 1, "Net"] - 1) * 100
hs300[1, "FundRate"] <- 0

plot(hs300$IndexRate, type='b', col='red', xaxt='n', xlab='Date', ylab='Rate')
lines(hs300$FundRate, type='b', col='blue')
axis(1, 1:length(hs300$Date), labels=hs300$Date)

hs300$FixRate <- hs300$Close/hs300$Net
dev.new()
plot(hs300$FixRate, xaxt='n', xlab='Date', ylab='Rate')
axis(1, 1:length(hs300$Date), labels=hs300$Date)

start <- 1
hs300$IndexIncome <- (hs300$Close / hs300[start, 'Close'] - 1) * 100
hs300$FundIncome <- (hs300$Net /hs300[start, 'Net'] - 1) * 100
max4p <- max(hs300$IndexIncome, hs300$FundIncome)
min4p <- min(hs300$IndexIncome, hs300$FundIncome)
dev.new()
plot(hs300$IndexIncome, type='b', col='red', xaxt='n', xlab='Date', ylab='Income', ylim=c(min4p, max4p))
lines(hs300$FundIncome, type='b', col='blue')
axis(1, 1:length(hs300$Date), labels=hs300$Date)

print(mean(hs300$FundRate - hs300$IndexRate))
print(sd(hs300$FundRate - hs300$IndexRate))

