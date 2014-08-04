install.packages("rjson")
library(jsonlite)
setwd("~/Fieldwork SPH/GitHub/H-HybridCE")
json_file <- "~/Fieldwork SPH/GitHub/H-HybridCE/jsonoutput.json"
jdat <- fromJSON(json_file)
df <- jdat$rows
df <- data.frame(df, stringsAsFactors=FALSE)
colnames(df) <- c("Day", "Views")
df$Day <- as.Date(df$Day)
df$Views <- as.numeric(df$Views)
df <- df[order(df$Day),]

png("YTchannelviews.png", width=960)
plot(df, type="n")
 plot(df$Day,  df$Views, type="h", axes=F, xlab="", ylab="Channel Views", main="YouTube Channel Views by Day")
 lines(lowess(df$Day, df$Views,f=.1) )
 axis.Date(1, at=seq(df$Day[1], df$Day[112],length.out=28), format="%b-%d", las=2)
axis(2, at=seq(0, 40, by=10))
mtext(text="Time (days)", side=1, line=3.75)
box()
graphics.off()
