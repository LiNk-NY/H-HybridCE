##Marcel Ramos
##YouTube viewing habits

setwd("~/GitHub/H-HybridCE/")
demo <- read.csv("~/Fieldwork SPH/Data/demographics.csv", header=TRUE)
demo <- demo[,c("Gender", "Age.group", "Percentage")]
demo$Percentage <- as.numeric(unlist(strsplit(as.character(demo$Percentage), split="0000%", fixed=TRUE)))
summary(demo$Age.group)
demo$AgeG <- factor(demo$Age.group, levels=c("18-24", "25-34", "35-44", "45-54", "55-64", "65-"))
demo

library(lattice)
barchart(demo$Percentage~demo$AgeG, data=demo, groups=demo$Gender, auto.key=list(space="right"), main="Demographics by Age Group", xlab="Age Group", ylab="Percentage")

devs <- read.csv("~/Fieldwork SPH/Data/devices.csv", header=TRUE)
devs$devper <- round(devs$Views/sum(devs$Views)*100, 2)
devs

library(xlsx)
cols <- 1:44
cols <- cols[-c(13,17,30,33,35,36)]

colnamesfile <- read.csv("~/Fieldwork SPH/Data/Codebook1.csv", header=FALSE, colClasses=c("character", "character"), na.strings="NA")

survey <- read.xlsx("~/Fieldwork SPH/Data/PH750-2 online tech 07-09.xlsx", sheetIndex=1, header=TRUE, colIndex=cols, colClasses=NA, stringsAsFactors=FALSE)
compl <- complete.cases(colnamesfile)
colnames(survey) <- colnamesfile$V2[compl]
chars <- c(8,15,32,33,35,36)
for (i in chars) {
        class(survey[,i]) <- "character"
}

summary(survey)

survey$EpiBiosStatus[1] <- "PH750"
survey$EpiBiosStatus[24] <- "PH750"
survey$EpiBiosStatus <- factor(survey$EpiBiosStatus, labels=c("Bios", "Epi"))

#Totals
table(survey$EpiBiosStatus)

#Method/Medium Preference by Class
table(survey$MedPrefBIOS, survey$EpiBiosStatus) 
table(survey$MedPrefEPI, survey$EpiBiosStatus)

#Matrix of preferences #assuming online for PH752 == Online after class is over
prefs <- matrix(c(12,6,9,5,5,0), ncol=2, byrow=TRUE)
rownames(prefs) <- c("In person", "Online after class", "Online during class")
colnames(prefs) <- c("Bios", "Epi")
prefs

#Course format choice if participant were to re-take course in the future
Hchoice <- table(survey$FFormatThis, survey$EpiBiosStatus)
colSums(Hchoice)
OtherChoice <- table(survey$FFormatOther, survey$EpiBiosStatus)
colSums(OtherChoice)


mean(survey$Travelminutes)
sd(survey$Travelminutes)

png("traveltime.png")
plot(survey$Travelminutes, type="h", pch=0, ylab="Travel Time (to and from in Minutes)", xlab="", col=survey$EpiBiosStatus)
points(survey$Travelminutes, pch=0)
abline(h=mean(survey$Travelminutes), lty=3)
abline(h=100.1622-65.80665, lty=4)
abline(h=100.1622+65.80665, lty=4)
legend("top", legend=c("Biostatistics","Epidemiology"), col=c("black","red"), lty=1)
graphics.off()

boxplot(survey$Travelminutes, type="h")

#Reason for preferences
!is.na(survey$MedPrefRBIOS)
!is.na(survey$PrefEPIR)

Reasons <- ifelse(!is.na(survey$MedPrefRBIOS)==TRUE, survey$MedPrefRBIOS, survey$PrefEPIR)
RC1 <- c("Interactivity", "Interactivity", NA, "Convenience", "Interactivity", "AvoidCommute", "LearnPref", "Convenience", "LearnPref", "LearnPref",
         "Interactivity", "LearnPref", "Interactivity", "Convenience", "AvoidCommute", NA, "AvoidCommute", NA, "Interactivity", "LearnPref",
         "LearnPref", "Convenience", "Convenience", "AvoidCommute", "LearnPref", "Interactivity", "Convenience", "Interactivity", "AvoidCommute", "LearnPref",
         "Convenience", "LearnPref", "AvoidCommute", "LearnPref", "LearnPref", "LearnPref", "LearnPref")
survey$RC1 <- factor(RC1, levels=c("Interactivity", "Convenience", "AvoidCommute", "LearnPref"), labels=c("Interactivity", "Convenience", "AvoidCommute", "LearnPref"))

RC2 <- c("LearnPref", NA, NA, NA, NA, "Interactivity", NA, NA, "AvoidCommute", NA, 
         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
         NA, NA, NA, "Interactivity", NA, NA, NA, NA, "Convenience", NA, 
         NA, NA, "Convenience", NA, "Interactivity", NA, "Convenience" )
survey$RC2 <- factor(RC2,levels=c("Interactivity", "Convenience", "AvoidCommute", "LearnPref"), labels=c("Interactivity", "Convenience", "AvoidCommute", "LearnPref"))

table(survey$Race)
library(plyr)
survey$Race2 <- revalue(survey$Race, c("Asian"="Other", "Prefer not to respond"="Other", "South Asian/European Jew"="Other"))
survey$Race2 <- factor(survey$Race2, levels=c("Other", "Hispanic", "Non-Hispanic black", "Non-Hispanic white"))

survey$Gender[11] <- NA
survey$Gender <- factor(survey$Gender, levels=c("Female", "Male"))
table(survey$Gender, survey$EpiBiosStatus)
table(survey$Race2, survey$EpiBiosStatus)
by(survey$Age, survey$EpiBiosStatus, FUN=function(x) {c(m = mean(x, na.rm=TRUE), s = sd(x, na.rm=TRUE))})
by(survey$Travelminutes, survey$EpiBiosStatus, FUN=function(x) {c(m = mean(x, na.rm=TRUE), s = sd(x, na.rm=TRUE))})

#Subsetting for BIOS students
biosub <- subset(survey, survey$EpiBiosStatus=="Bios")
nrow(biosub)

#barplot(comb, horiz=TRUE, col=gray.colors(4),beside=TRUE, legend.text=rownames(comb), main="Reasons for Medium Preference", xlab="Frequency Endorsed")

#Primary and secondary reason for course pref selection
comb <-  table(biosub$RC1, biosub$MedPrefBIOS) + table(biosub$RC2, biosub$MedPrefBIOS)
comb
colnames(comb) <- c("In-person", "Synchronous", "Asynchronous")
rownames(comb) <- c("Interactivity", "Convenience", "Avoid Commuting", "Learning Preference")
comb

#Figure 2
barplot(comb, col=gray.colors(4), beside=TRUE, legend.text=rownames(comb), main="Reasons for Course Format Preference", xlab="Course Format Preferences", ylab="Frequency Endorsed") 

#Age by Preference
summary( with(biosub, aov(Age~ MedPrefBIOS, data=biosub)))
#Travel Time by Preference
summary( with(biosub, aov(Travelminutes~ MedPrefBIOS, data=biosub)))
#Gender by Preference
with(biosub, fisher.test(Gender, MedPrefBIOS))
#Race by Preference
with(biosub, fisher.test(Race2, MedPrefBIOS))
#Pref Reason by Preference
with(biosub, fisher.test(RC1, MedPrefBIOS))


summary(with(biosub, aov(Travelminutes~ RC1, data=biosub)))
