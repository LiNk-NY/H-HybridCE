## YouTube Data Preprocess

vidata <- read.csv("data-raw/Figure2YouTubeDataRaw.csv")
vidata$views <- rowSums(vidata[,2:5])

longdata <- reshape(vidata,
                    varying = c("L2", "L3", "L4", "L5"),
                    v.names = "views",
                    timevar = "lecture",
                    times = c("L2", "L3", "L4", "L5"),
                    direction = "long")

rownames(longdata) <- NULL

longdata <- subset(longdata, select = -c(id))

longdata$lecture <- factor(longdata$lecture, levels = c("L2","L3","L4","L5"),
                           labels = c("Lecture 2", "Lecture 3",
                                    "Lecture 4", "Lecture 5"),
                           ordered = TRUE)

write.csv(longdata, file = "data/Figure2YouTubeData.csv", row.names = FALSE)
