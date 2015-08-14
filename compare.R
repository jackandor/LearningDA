i100 <- read.table("i100.csv", sep=',', header=TRUE, stringsAsFactors=FALSE)
i100$Date <- as.Date(i100$Date, "%Y/%m/%d")
i100 <- i100[order(i100$Date),]
i100 <- na.omit(i100)

tj100 <- read.table("tj100.csv", sep=',', header=TRUE, stringsAsFactors=FALSE)
tj100$Date <- as.Date(tj100$Date, "%Y-%m-%d")
tj100 <- tj100[order(tj100$Date),]

cmp_df <- merge(i100, tj100, by='Date')

start <- 1
cmp_df$i100_index_income <- (cmp_df$Close.x / cmp_df[start, 'Close.x'] - 1) * 100
cmp_df$i100_fund_income <- (cmp_df$Net.x /cmp_df[start, 'Net.x'] - 1) * 100
cmp_df$tj100_index_income <- (cmp_df$Close.y / cmp_df[start, 'Close.y'] - 1) * 100
cmp_df$tj100_fund_income <- (cmp_df$Net.y / cmp_df[start, 'Net.y'] - 1) * 100

max4p <- max(cmp_df$i100_index_income, cmp_df$i100_fund_income, cmp_df$tj100_index_income, cmp_df$tj100_fund_income)
min4p <- min(cmp_df$i100_index_income, cmp_df$i100_fund_income, cmp_df$tj100_index_income, cmp_df$tj100_fund_income)

plot(cmp_df$i100_index_income, type='b', col='red', xaxt='n', xlab='Date', ylab='Income', ylim=c(min4p, max4p))
lines(cmp_df$tj100_index_income, type='b', col='blue')
lines(cmp_df$i100_fund_income, type='b', col='pink')
lines(cmp_df$tj100_fund_income, type='b', col='grey')
axis(1, 1:length(i100$Date), labels=i100$Date)
