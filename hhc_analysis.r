##Marcel Ramos
##YouTube viewing habits
library(xlsx)
cols <- 1:44
cols <- cols[-c(13,17,30,33,35,36)]

colnamesfile <- read.csv("../../Data/Codebook1.csv", header=FALSE, colClasses=c("character", "character"), na.strings="NA")
survey <- read.xlsx("../../Data/PH750-2 online tech 07-09.xlsx", sheetIndex=1, header=TRUE, colIndex=cols, stringsAsFactors=FALSE)

compl <- complete.cases(colnamesfile)
colnames(survey) <- colnamesfile$V2[compl]

#Students taking both were considered to be BIOS only
survey$EpiBiosStatus[c(1,24)] <- "PH750"
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

plot(survey$Travelminutes, type="h", pch=0, ylab="Travel Time (to and from in Minutes)", xlab="", main="Student Travel Times by Course", col=survey$EpiBiosStatus)
points(survey$Travelminutes, pch=0)
with(survey, abline(h=mean(Travelminutes), lty=3))
with(survey, abline(h=mean(Travelminutes)-sd(Travelminutes), lty=4))
with(survey, abline(h=mean(Travelminutes)+sd(Travelminutes), lty=4))
legend("top", legend=c("Biostatistics","Epidemiology"), col=c("black","red"), lty=1)
#graphics.off()

#boxplot(survey$Travelminutes, type="h")

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

#checking reason labels - Primary Reason, BIOS Reasons, EPI Reasons, Secondary Reason 
with(survey, paste(RC1, Reasons, RC2)) 

table(survey$Race)
library(plyr)
survey$Race2 <- revalue(survey$Race, c("Asian"="Other", "Prefer not to respond"="Other", "South Asian/European Jew"="Other"))
survey$Race2 <- factor(survey$Race2, levels=c("Other", "Hispanic", "Non-Hispanic black", "Non-Hispanic white"))

survey$Gender[11] <- NA
survey$Gender <- factor(survey$Gender, levels=c("Female", "Male"))
table(survey$Gender, survey$EpiBiosStatus); prop.table(table(survey$Gender, survey$EpiBiosStatus), 2)
table(survey$Race2, survey$EpiBiosStatus); prop.table(table(survey$Race2, survey$EpiBiosStatus),2)
by(survey$Age, survey$EpiBiosStatus, FUN=function(x) {c(m = mean(x, na.rm=TRUE), s = sd(x, na.rm=TRUE))})
by(survey$Travelminutes, survey$EpiBiosStatus, FUN=function(x) {c(m = mean(x, na.rm=TRUE), s = sd(x, na.rm=TRUE))})

#Subsetting for BIOS students
biosub <- subset(survey, survey$EpiBiosStatus=="Bios")
nrow(biosub)

#barplot(comb, horiz=TRUE, col=gray.colors(4),beside=TRUE, legend.text=rownames(comb), main="Reasons for Medium Preference", xlab="Frequency Endorsed")

#Primary and secondary reason for course pref selection
comb <-  table(biosub$RC1, biosub$MedPrefBIOS) + table(biosub$RC2, biosub$MedPrefBIOS)
comb
colnames(comb) <- c("In-person", "Asynchronous", "Synchronous")
rownames(comb) <- c("Interactivity", "Convenience", "Avoid Commuting", "Learning Preference")
comb

#Figure 2 - use pdf() for final graphic
cols2 <- c("gray75", "gray55", "gray35", "gray15")
png("PrefReasons.png")
barplot(comb, col=cols2, beside=TRUE, legend.text=rownames(comb), main="Primary and Secondary Reasons for Course Format Preference", xlab="Course Format Preferences", ylab="Frequency Endorsed") 
graphics.off()
#Age by Preference
hist(survey$Age)
with(biosub, kruskal.test(Age~ factor(MedPrefBIOS), data=biosub))
with(biosub, by(Age, MedPrefBIOS, FUN=function(x) {c(m = mean(x, na.rm=TRUE), sd = sd(x, na.rm=TRUE))}) )


#Travel Time by Preference
hist(survey$Travelminutes)
with(biosub, kruskal.test(Travelminutes~ factor(MedPrefBIOS), data=biosub))
with(biosub, by(Travelminutes, MedPrefBIOS, FUN=function(x) {c(m = mean(x, na.rm=TRUE), sd = sd(x, na.rm=TRUE))}))
#Gender by Preference
with(biosub, fisher.test(Gender, MedPrefBIOS))
with(biosub, xtabs(~Gender+MedPrefBIOS))
#percentages by row
prop.table(with(biosub, xtabs(~Gender+MedPrefBIOS)),1)

#Race by Preference
with(biosub, fisher.test(Race2, MedPrefBIOS))
with(biosub, xtabs(~Race2+MedPrefBIOS))
#table with row percentages
prop.table(with(biosub, xtabs(~Race2+MedPrefBIOS)),1)

#Pref Reason by Preference
with(biosub, fisher.test(RC1, MedPrefBIOS))$p.value
with(biosub, xtabs(~RC1+MedPrefBIOS))
#column percentages
prop.table(with(biosub, xtabs(~RC1+MedPrefBIOS)),2)

with(biosub, kruskal.test(Travelminutes~ RC1, data=biosub))

