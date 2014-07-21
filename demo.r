##Marcel Ramos
##YouTube viewing habits

setwd("~/Fieldwork SPH/GitHub/H-HybridCE/")
getwd()
dir()
demo <- read.csv("~/Fieldwork SPH/Data/demographics.csv", header=TRUE)
demo <- demo[,c("Gender", "Age.group", "Percentage")]
demo$Percentage <- as.numeric(unlist(strsplit(as.character(demo$Percentage), split="0000%", fixed=TRUE)))
summary(demo$Age.group)
demo$AgeG <- factor(demo$Age.group, levels=c("18-24", "25-34", "35-44", "45-54", "55-64", "65-"))
demo

library(lattice)
png("Demo_by_AG.png")
barchart(demo$Percentage~demo$AgeG, data=demo, groups=demo$Gender, auto.key=list(space="right"), main="Demographics by Age Group", xlab="Age Group", ylab="Percentage")
graphics.off()

devs <- read.csv("~/Fieldwork SPH/Data/devices.csv", header=TRUE)
devs$devper <- round(devs$Views/sum(devs$Views)*100, 2)
devs

library(xlsx)
cols <- 1:44
cols <- cols[-c(13,17,30,33,35,36)]

colnamesfile <- read.csv("~/Fieldwork SPH/Data/Codebook1.csv", header=FALSE, colClasses=c("character", "character"), na.strings="NA")

survey <- read.xlsx("~/Fieldwork SPH/Data/PH750-2 online tech 07-09.xlsx", sheetIndex=1, header=TRUE, colIndex=cols, colClasses=NA)

compl <- complete.cases(colnamesfile)
colnames(survey) <- colnamesfile$V2[compl]
chars <- c(8,15,32,33,35,36)
for (i in chars) {
        class(survey[,i]) <- "character"
}

sapply(survey,class)

names(survey)
head(survey,10)

table(survey$EpiBiosStatus)

table(survey$MedPrefBIOS)

mean(survey$Travelminutes)
sd(survey$Travelminutes)

png("traveltime.png")
plot(survey$Travelminutes, type="h", pch=0, ylab="Travel Time (to and from in Minutes)", xlab="", col=survey$EpiBiosStatus)
points(survey$Travelminutes, pch=0)
abline(h=mean(survey$Travelminutes), lty=3)
abline(h=100.1622-65.80665, lty=4)
abline(h=100.1622+65.80665, lty=4)
legend("top", legend=c("Both", "Biostatistics","Epidemiology"), col=c("black", "red", "green"), lty=1)
graphics.off()

boxplot(survey$Travelminutes, type="h")

