# xts-dygraph tests
require(dplyr)
require(dygraphs)
require(xts)

temp <- filter(operas, Title=="Così fan tutte")
receipts <- xts(temp$Receipts,
                as.POSIXct(temp$Date), "UTC")
colnames(receipts) <- "kr"
extra <- as.POSIXct("1790-02-12")
receipts <- merge.xts(receipts, extra, tzone="UTC")
receipts

dygraph(receipts) %>% 
  dyOptions(labelsUTC = TRUE)