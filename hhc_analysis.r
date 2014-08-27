##Marcel Ramos
##YouTube viewing habits
library(xlsx)
cols <- 1:44
cols <- cols[-c(13,17,30,33,35,36)]

colnamesfile <- read.csv("~/Desktop/Data/Codebook1.csv", header=FALSE, colClasses=c("character", "character"), na.strings="NA")
survey <- read.xlsx("~/Desktop/Data/PH750-2 online tech 07-09.xlsx", sheetIndex=1, header=TRUE, colIndex=cols, stringsAsFactors=FALSE)

compl <- complete.cases(colnamesfile)
colnames(survey) <- colnamesfile$V2[compl]

#Students taking both were considered to be BIOS only
survey$EpiBiosStatus[c(1,24)] <- "PH750"
survey$EpiBiosStatus <- factor(survey$EpiBiosStatus, labels=c("Bios", "Epi"))

#Totals
table(survey$EpiBiosStatus)


#Course format choice if participant were to re-take course in the future
Hchoice <- table(survey$FFormatThis, survey$EpiBiosStatus)
colSums(Hchoice)
OtherChoice <- table(survey$FFormatOther, survey$EpiBiosStatus)
colSums(OtherChoice)

par(family="serif")
plot(survey$Travelminutes, type="h", pch=0, ylab="Travel Time (to and from in Minutes)", xlab="", main="Student Travel Times by Course", col=survey$EpiBiosStatus, axes=F)
axis(side=2, las=2); axis(side=1) ; box()
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

###     Subsetting for BIOS students    ###
biosub <- subset(survey, survey$EpiBiosStatus=="Bios")

###     TABLE 1         ###
by(survey$Age, survey$EpiBiosStatus, FUN=function(x) {c(m = mean(x, na.rm=TRUE), s = sd(x, na.rm=TRUE))})
table(survey$Gender, survey$EpiBiosStatus); prop.table(table(survey$Gender, survey$EpiBiosStatus), 2)
table(survey$Race2, survey$EpiBiosStatus); prop.table(table(survey$Race2, survey$EpiBiosStatus),2)
by(survey$Travelminutes, survey$EpiBiosStatus, FUN=function(x) {c(m = mean(x, na.rm=TRUE), s = sd(x, na.rm=TRUE))})

#Course Preference by Class
table(survey$MedPrefBIOS, survey$EpiBiosStatus) ; table(survey$MedPrefEPI, survey$EpiBiosStatus)

###     TABLE 2         ###
#assuming online for PH752 == Online after class is over
prefs <- matrix(c(12,6,9,5,5,0), ncol=2, byrow=TRUE)
rownames(prefs) <- c("In person", "Online after class", "Online during class")
colnames(prefs) <- c("Bios", "Epi")
tot <- matrix(rep(c(24,11),3), ncol=2, byrow=TRUE)
prefs; prefs/tot

###     FIGURE 1        ###
### YouTube Viewing Patterns ###
vidata <- read.csv("F1data.csv")
vidata$Day <- as.Date(vidata$Day, format="%m/%d/%Y")
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
cols1 <- c("Lecture 2"="gray75", "Lecture 3"="gray55", "Lecture 4"="gray35", "Lecture 5"="gray15")

p <- ggplot(longdata, aes(x=Day, y=views, fill=lecture)) + geom_area(position="stack")
j <- p + geom_line(aes(ymax=views), position="stack") + theme_bw() + scale_fill_manual(values=cols1) +
        theme(plot.background=element_blank(),
              panel.grid.major.x=element_blank(),
              panel.grid.major.y=element_line(size=0.1, color="gray"),
              panel.grid.minor=element_blank())
k <- j + xlab("Date") + ylab("Number of views") + ggtitle("YouTube Video View Patterns per Lecture") +
        geom_vline(xintercept=as.numeric(longdata$Day[31]), lty=4, lwd=1)  + labs(fill="Online Lecture \n Videos")

print(k)
#dev.copy(png, filename="YTviews.png", width=960)
#dev.off()


###     FIGURE 2        ###
#Reasons for course pref selection
comb <-  table(biosub$RC1, biosub$MedPrefBIOS) + table(biosub$RC2, biosub$MedPrefBIOS)
colnames(comb) <- c("In-person", "Asynchronous", "Synchronous")
rownames(comb) <- c("Interactivity", "Convenience", "Avoid Commuting", "Learning Preference")
totals <- matrix(rep(c(12,9,5),4), byrow=TRUE, ncol=3)
comb; comb/totals

#Figure 2 - use pdf() for final graphic
cols2 <- c("gray75", "gray55", "gray35", "gray15")
png("PrefReasons.png")
#pdf("PrefReasons.pdf", width=6, height=6, paper="special")
par(family="serif")
barplot(comb, col=cols2, beside=TRUE, legend.text=rownames(comb), main="Reasons for Course Format Preference", xlab="Course Format Preferences", ylab="Frequency endorsed", axes=F)
axis(side=2, las=2)
graphics.off()


###     TABLE 3         ###
#Age by Preference
require(RColorBrewer)
hist(biosub$Age, breaks=20, density=15, col=brewer.pal(5, "Set1"))
with(biosub, by(Age, MedPrefBIOS, FUN=function(x) {c(m = mean(x, na.rm=TRUE), sd = sd(x, na.rm=TRUE))}) )
with(biosub, kruskal.test(Age~ factor(MedPrefBIOS), data=biosub))
#Travel Time by Preference
hist(survey$Travelminutes, breaks=20, density=15, col=brewer.pal(5, "Dark2"))
with(biosub, by(Travelminutes, MedPrefBIOS, FUN=function(x) {c(m = mean(x, na.rm=TRUE), sd = sd(x, na.rm=TRUE))}))
with(biosub, kruskal.test(Travelminutes~ factor(MedPrefBIOS), data=biosub))
#Gender by Preference
with(biosub, xtabs(~Gender+MedPrefBIOS))
prop.table(with(biosub, xtabs(~Gender+MedPrefBIOS)),1) #percentages by row
with(biosub, fisher.test(Gender, MedPrefBIOS))
#Race by Preference
with(biosub, fisher.test(Race2, MedPrefBIOS))
with(biosub, xtabs(~Race2+MedPrefBIOS))
prop.table(with(biosub, xtabs(~Race2+MedPrefBIOS)),1) #table with row percentages
#Reasons for Preference with in-group percentages and Fisher p-value
comb; comb/totals ; print("Fisher's Exact test p-value");  fisher.test(comb)$p.value

with(biosub, xtabs(~STEMstat+MedPrefBIOS));prop.table(with(biosub, xtabs(~STEMstat+MedPrefBIOS)),1 )
with(biosub, fisher.test(STEMstat,MedPrefBIOS))

with(biosub, xtabs(~HybridHist+MedPrefBIOS)) ; prop.table(with(biosub, xtabs(~HybridHist+MedPrefBIOS)),1 )
with(biosub, fisher.test(HybridHist,MedPrefBIOS))

with(biosub, xtabs(~CChild+MedPrefBIOS)) ; prop.table(with(biosub, xtabs(~CChild+MedPrefBIOS)),1 )
with(biosub, fisher.test(CChild,MedPrefBIOS))

with(biosub, xtabs(~FTPT+MedPrefBIOS)) ; prop.table(with(biosub, xtabs(~FTPT+MedPrefBIOS)),1 )
with(biosub, fisher.test(FTPT,MedPrefBIOS))

with(biosub, xtabs(~ConfLStatBIOS+MedPrefBIOS)) ; prop.table(with(biosub, xtabs(~ConfLStatBIOS+MedPrefBIOS)),1 )
with(biosub, fisher.test(ConfLStatBIOS,MedPrefBIOS))


#list of possible covars -- STEMstat HybridHist CChild FTPT ConfLStatBIOS            (Education, Program, WHours, JobStat)
str(biosub[,-grep("epi", names(biosub), ignore.case=T)])



#Alt result
with(biosub, kruskal.test(Travelminutes~ RC1, data=biosub))

biosub$Interactivity <- biosub$RC1=="Interactivity" | biosub$RC2=="Interactivity"
biosub$Interactivity[is.na(biosub$Interactivity)] <- FALSE

biosub$AvoidCommute <- biosub$RC1=="AvoidCommute" | biosub$RC2=="AvoidCommute"
biosub$AvoidCommute[is.na(biosub$AvoidCommute)] <- FALSE

boxplot(biosub$Travelminutes ~ biosub$MedPrefBIOS=="In person")

with(biosub, kruskal.test(Travelminutes ~ AvoidCommute, data=biosub))
boxplot(Travelminutes ~ AvoidCommute, data=biosub, notch=TRUE)
