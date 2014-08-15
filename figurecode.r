vidata <- read.csv("F1data.csv")
vidata$Day <- as.Date(vidata$Day, format="%m/%d/%Y")
for (i in 2:5){
class(vidata[,i]) <- "numeric"}
vidata$views <- rowSums(vidata[,2:5])

longdata <- reshape(vidata,
                varying = c("L2", "L3", "L4", "L5"),
                v.names = "views",
                timevar = "lecture",
                times = c("L2", "L3", "L4", "L5"),
                direction = "long")
longdata <- subset(longdata, select=-c(id))

require(ggplot2)
longdata$lecture <- factor(longdata$lecture, levels=c("L2","L3","L4","L5"), 
       labels=c("Lecture 2", "Lecture 3", "Lecture 4", "Lecture 5"), ordered=TRUE)
p <- ggplot(longdata, aes(x=Day, y=views, fill=lecture)) + geom_area(position="stack")
j <- p + geom_line(aes(ymax=views), position="stack") + theme_bw() + 
        theme(plot.background=element_blank(), 
              panel.grid.major.x=element_blank(),
              panel.grid.major.y=element_line(size=0.1, color="gray"),
              panel.grid.minor=element_blank()) + scale_fill_hue(name="Video Lecture")
k <- j + xlab("Date") + ylab("Number of views") + ggtitle("YouTube Video View Patterns per Lecture") +
        geom_vline(xintercept=as.numeric(longdata$Day[31]), lty=4, lwd=1)
print(k)


