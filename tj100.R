tj100 <- read.table("tj100.csv", sep=',', header=TRUE, stringsAsFactors=FALSE)
tj100$Date <- as.Date(tj100$Date, "%Y-%m-%d")
tj100 <- tj100[order(tj100$Date),]
tj100 <- na.omit(tj100)

for (i in 2:length(tj100$Date)) tj100[i, "IndexRate"] <- (tj100[i, "Close"] / tj100[i - 1, "Close"] - 1) * 100
tj100[1, "IndexRate"] <- 0
for (i in 2:length(tj100$Date)) tj100[i, "FundRate"] <- (tj100[i, "Net"] / tj100[i - 1, "Net"] - 1) * 100
tj100[1, "FundRate"] <- 0

plot(tj100$IndexRate, type='b', col='red', xaxt='n', xlab='Date', ylab='Rate')
lines(tj100$FundRate, type='b', col='blue')
axis(1, 1:length(tj100$Date), labels=tj100$Date)

tj100$FixRate <- tj100$Close/tj100$Net
dev.new()
plot(tj100$FixRate, xaxt='n', xlab='Date', ylab='Rate')
axis(1, 1:length(tj100$Date), labels=tj100$Date)

start <- 1
tj100$IndexIncome <- (tj100$Close / tj100[start, 'Close'] - 1) * 100
tj100$FundIncome <- (tj100$Net /tj100[start, 'Net'] - 1) * 100
max4p <- max(tj100$IndexIncome, tj100$FundIncome)
min4p <- min(tj100$IndexIncome, tj100$FundIncome)
dev.new()
plot(tj100$IndexIncome, type='b', col='red', xaxt='n', xlab='Date', ylab='Income', ylim=c(min4p, max4p))
lines(tj100$FundIncome, type='b', col='blue')
axis(1, 1:length(tj100$Date), labels=tj100$Date)
