## Marcel Ramos
##YouTube viewing habits

library(xlsx)
colnums <- 1:44
colnums <- colnums[-c(13,17,30,33,35,36)]

colnamesfile <- read.csv("data-raw/SurveyCodebook.csv", header = FALSE,
                         colClasses = c("character", "character"),
                         na.strings = "NA")
survey <- read.xlsx("data-raw/PH750-2SurveyData.xlsx",
                    sheetIndex = 1, header = TRUE,
                    colIndex = colnums, stringsAsFactors = FALSE)

compl <- complete.cases(colnamesfile)
colnames(survey) <- colnamesfile$V2[compl]

#Students taking both were considered to be BIOS only
survey$EpiBiosStatus[c(1,24)] <- "PH750"
survey$EpiBiosStatus <- factor(survey$EpiBiosStatus, labels = c("Bios", "Epi"))
#boxplot(survey$Travelminutes, type = "h")

# Reason for preferences
Reasons <- ifelse(!is.na(survey$MedPrefRBIOS) == TRUE,
                  survey$MedPrefRBIOS, survey$PrefEPIR)

# Coding responses manually
RC1 <- c("Interactivity", "Interactivity", NA, "Convenience", "Interactivity",
         "AvoidCommute", "LearnPref", "Convenience", "LearnPref", "LearnPref",
         "Interactivity", "LearnPref", "Interactivity", "Convenience",
         "AvoidCommute", NA, "AvoidCommute", NA, "Interactivity", "LearnPref",
         "LearnPref", "Convenience", "Convenience", "AvoidCommute", "LearnPref",
         "Interactivity", "Convenience", "Interactivity", "AvoidCommute",
         "LearnPref", "Convenience", "LearnPref", "AvoidCommute", "LearnPref",
         "LearnPref", "LearnPref", "LearnPref")

survey$RC1 <- factor(RC1, levels = c("Interactivity", "Convenience",
                                   "AvoidCommute", "LearnPref"),
                     labels = c("Interactivity", "Convenience",
                                "AvoidCommute", "LearnPref"))

RC2 <- c("LearnPref", NA, NA, NA, NA, "Interactivity", NA, NA, "AvoidCommute",
         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
         "Interactivity", NA, NA, NA, NA, "Convenience", NA, NA, NA,
         "Convenience", NA, "Interactivity", NA, "Convenience" )

survey$RC2 <- factor(RC2, levels = c("Interactivity", "Convenience",
                                     "AvoidCommute", "LearnPref"),
                     labels = c("Interactivity", "Convenience",
                                "AvoidCommute", "LearnPref"))

#checking reason labels - Primary Reason, BIOS Reasons, EPI Reasons, Secondary Reason
with(survey, paste(RC1, RC2, Reasons))

table(survey$Race)
require(plyr)
survey$Race2 <- revalue(survey$Race, c("Asian" = "Other",
                                       "Prefer not to respond" = "Other",
                                       "South Asian/European Jew" = "Other"))
survey$Race2 <- factor(survey$Race2,
                       levels = c("Other", "Hispanic", "Non-Hispanic black",
                                  "Non-Hispanic white"))
survey$Job <- revalue(survey$JobStat,
                      c("No" = "No/No Answer",
                        "Prefer not to respond" = "No/No Answer"))
survey$Job <- factor(survey$Job, levels = c("No/No Answer", "Full-time", "Part-time"))
survey$Gender[11] <- NA
survey$Gender <- factor(survey$Gender, levels = c("Female", "Male"))

numberSummaries <- function(number) {
    c(MED = median(number, na.rm = TRUE),
      MIN = min(number, na.rm = TRUE),
      MAX = max(number, na.rm = TRUE))
}

meanSD <- function(number) {
    c(m = mean(number, na.rm = TRUE),
      sdev  = sd(number, na.rm = TRUE))
}

###     TABLE 1         ###
by(survey$Age, survey$EpiBiosStatus, FUN = meanSD)
by(survey$Age, survey$EpiBiosStatus, FUN = numberSummaries)

table(survey$Gender, survey$EpiBiosStatus)
prop.table(table(survey$Gender, survey$EpiBiosStatus), 2)

table(survey$Race2, survey$EpiBiosStatus)
prop.table(table(survey$Race2, survey$EpiBiosStatus),2)

by(survey$Travelminutes, survey$EpiBiosStatus, FUN = meanSD)

by(survey$Travelminutes, survey$EpiBiosStatus, FUN = numberSummaries)

# Course Preference by Class
table(survey$MedPrefBIOS, survey$EpiBiosStatus)
table(survey$MedPrefEPI, survey$EpiBiosStatus)

# assuming online for PH752 == Online after class is over
prefs <- matrix(c(12,6,9,5,5,0), ncol = 2, byrow = TRUE)
rownames(prefs) <- c("In person", "Online after class", "Online during class")
colnames(prefs) <- c("Bios", "Epi")
tot <- matrix(rep(c(24,11),3), ncol = 2, byrow = TRUE)
prefs; prop.table(prefs, 2)

fpre <- table(survey$RC1, survey$EpiBiosStatus) + table(survey$RC2, survey$EpiBiosStatus)
fpre
fpre/matrix(c(rep(26,4), rep(11,4)), byrow = FALSE, ncol = 2)

# Course format choice if participant were to re-take course
survey$retake <- revalue(survey$FFormatThis,
                         c("6 in-person only, 6 0nline only" = "Other",
                           "Option of each class in-person or recorded (not just online)" = "Other",
                           "Eight sessions in-person only, four sessions online only." = "8 online, 4 in-person",
                           "I would like to have the option of attending each class in-person or online." = "Option of in-person or online",
                           "Classes are in-person only" = "In-person", "Classes are online only" = "Online only",
                           "Classes are in person and recorded for viewing after the lecture is over" = "Other"))

table(survey$retake, survey$EpiBiosStatus)
prop.table(with(survey, xtabs(~retake+EpiBiosStatus)),2)

### YouTube Viewing Patterns ###
vidata <- read.csv("data-raw/Figure2YouTubeData.csv")
vidata$Day <- as.Date(vidata$Day, format = "%m/%d/%Y")
vidata$views <- rowSums(vidata[,2:5])

longdata <- reshape(vidata,
                    varying = c("L2", "L3", "L4", "L5"),
                    v.names = "views",
                    timevar = "lecture",
                    times = c("L2", "L3", "L4", "L5"),
                    direction = "long")
longdata <- subset(longdata, select = -c(id))

library(ggplot2)
library(grid)
library(RColorBrewer)

longdata$lecture <- factor(longdata$lecture, levels = c("L2","L3","L4","L5"),
                           labels = c("Lecture 2", "Lecture 3",
                                    "Lecture 4", "Lecture 5"),
                           ordered = TRUE)
cols0 <- c("Lecture 2" = "gray85", "Lecture 3" = "gray65",
           "Lecture 4" = "gray45", "Lecture 5" = "gray25")

###     FIGURE 2        ###
# YouTube Lecture Viewing Patterns
postscript("Figures/YouTubeViewsF2.eps", width = 8, height = 4,
           paper = "special", horizontal = FALSE, family = "Times")

p <- ggplot(longdata, aes(x = Day, y = views, fill = lecture)) +
    geom_area(position = "stack") + xlab("Date") + ylab("Number of views") +
    labs(fill = "Online Lecture \n Videos") +
    geom_vline(xintercept = as.numeric(longdata$Day[31]), lty = 4, lwd = 1) +
    geom_line(aes(ymax = views), position = "stack") +
    scale_fill_manual(values = cols0) +
    theme_bw() + theme(plot.background = element_blank(),
                       plot.margin = unit(c(0.5,0,0.5,0), "cm"),
                       panel.grid.major.x = element_blank(),
                       panel.grid.major.y = element_line(size = 0.1, color = "gray"),
                       panel.grid.minor = element_blank(),
                       axis.text.x = element_text(size = 14),
                       axis.text.y = element_text(size = 14),
                       legend.title = element_text(size = 14),
                       legend.text = element_text(size = 14),
                       text = element_text(size = 16))
print(p)

dev.off()

# dev.copy(png, filename = "YTviews.png", width = 960); dev.off()
# dev.copy(pdf, file = "YTviews.pdf", height = 4, width = 8, paper = "special")
# dev.off()

###     Subsetting for BIOS students    ###
biosub <- subset(survey, survey$EpiBiosStatus == "Bios")

#Totals
table(survey$EpiBiosStatus)

## Exploratory Plot
par(family = "serif")

plot(biosub$Travelminutes, type = "h",
     ylab = "Travel Time (to and from in Minutes)",
     xlab = "", main = "BIOS Student Travel Times by Preference", axes = F,
     col = factor(biosub$MedPrefBIOS), lwd = 2)
axis(side = 2, las = 2)
axis(side = 1)
box()

points(biosub$Travelminutes, pch = 0)
with(biosub, abline(h = mean(Travelminutes), lty = 3))
with(biosub, abline(h = mean(Travelminutes) - sd(Travelminutes), lty = 4))
with(biosub, abline(h = mean(Travelminutes) + sd(Travelminutes), lty = 4))
legend("top", legend = c("In-person", "Asynchronous", "Synchronous"),
       col = 1:3, lty = 1)
#graphics.off()

###     FIGURE 1        ###
#Reasons for course pref selection
comb <-  table(biosub$RC1, biosub$MedPrefBIOS) +
    table(biosub$RC2, biosub$MedPrefBIOS)
colnames(comb) <- c("In-person", "Asynchronous", "Synchronous")
rownames(comb) <- c("Interactivity", "Convenience",
                    "Avoid Commuting", "Learning Preference")
totals <- matrix(rep(c(12,9,5),4), byrow = TRUE, ncol = 3)
comb/totals

# Figure 1 - encapsulatedpostscript for final graphic

postscript("Figures/FormatPrefF1.eps", width = 6, height = 6,
           paper = "special", horizontal = FALSE, family = "Times")

cols1 <- brewer.pal(4, "Greys")
dens <- seq(10,40, length.out = 4)
barplot(comb, col = cols1, beside = TRUE, legend.text = rownames(comb),
        xlab = "", xaxt = "n", ylab = "", axes = FALSE)
axis(side = 1, labels = colnames(comb), at = c(3,8,13), line = -.75, tick = FALSE)
mtext("Frequency endorsed", side = 2, at = 3.5, line = 2, cex = 1.2)
mtext("Course Format Preferences", side = 1, at = 8, line = 1.5, cex = 1.2)
axis(side = 2, las = 2)

dev.off()

# graphics.off()

###     TABLE 2         ###
# Age by Preference
hist(biosub$Age, breaks = 20, density = c(seq(5, 25, 5)),
     col = brewer.pal(5, "Set1"), main = "Historgram", xlab = "Age (yrs.)")
with(biosub, by(Age, MedPrefBIOS, FUN = meanSD))

with(biosub, by(Age, MedPrefBIOS, FUN = numberSummaries))

with(biosub, kruskal.test(Age~ factor(MedPrefBIOS), data = biosub))
sampAgeMed <- with(biosub, median(Age, na.rm = TRUE))
MedBin1 <- ifelse(biosub$Age>sampAgeMed, "Yes", "No")
fisher.test(table(MedBin1,biosub$MedPrefBIOS))$p.value

#Travel Time by Preference
hist(survey$Travelminutes, breaks = 20, density = 25,
     col = brewer.pal(5, "Dark2"))
with(biosub, by(Travelminutes, MedPrefBIOS, FUN = meanSD))

with(biosub, by(Travelminutes, MedPrefBIOS, FUN = numberSummaries))

with(biosub, kruskal.test(Travelminutes~ factor(MedPrefBIOS), data = biosub))
sampTMed <- with(biosub, median(Travelminutes, na.rm = TRUE))
MedBin2 <- ifelse(biosub$Travelminutes>sampTMed, "Yes", "No")
fisher.test(table(MedBin2, biosub$MedPrefBIOS))

#Gender by Preference
with(biosub, xtabs(~Gender+MedPrefBIOS))
prop.table(with(biosub, xtabs(~Gender+MedPrefBIOS)),1) #percentages by row
with(biosub, fisher.test(Gender, MedPrefBIOS))
#Race by Preference
with(biosub, fisher.test(Race2, MedPrefBIOS))
with(biosub, xtabs(~Race2+MedPrefBIOS))
prop.table(with(biosub, xtabs(~Race2+MedPrefBIOS)),1) #table with row percentages
#Reasons for Preference with in-group percentages and Fisher p-value
comb
comb/totals
print("Fisher's Exact test p-value")
fisher.test(comb)$p.value

#Preference by STEM (ScienceTechnologyEngineeringMathematics) status
with(biosub, xtabs(~STEMstat+MedPrefBIOS))
prop.table(with(biosub, xtabs(~STEMstat+MedPrefBIOS)),1 )

with(biosub, fisher.test(STEMstat,MedPrefBIOS))

#Preference by HybridCourse History
biosub$HybridHist[biosub$HybridHist >= 2] <- 2
biosub$HybridHist <- factor(biosub$HybridHist, levels = c(0, 1, 2),
                            labels = c("Zero", "One", "Two+") )
with(biosub, xtabs(~HybridHist+MedPrefBIOS))
prop.table(with(biosub, xtabs(~ HybridHist + MedPrefBIOS)),1 )
with(biosub, fisher.test(HybridHist, MedPrefBIOS))

#Preference by Responsible for care of children?
with(biosub, xtabs(~CChild+MedPrefBIOS))
prop.table(with(biosub, xtabs(~CChild+MedPrefBIOS)),1 )

with(biosub, fisher.test(CChild,MedPrefBIOS))

#Preference by Confidence Level in Stats
with(biosub, xtabs(~ConfLStatBIOS+MedPrefBIOS))
prop.table(with(biosub, xtabs(~ConfLStatBIOS+MedPrefBIOS)),1 )

with(biosub, fisher.test(ConfLStatBIOS,MedPrefBIOS))

#Preference by Job Status (recoded)
with(biosub, xtabs(~Job+MedPrefBIOS))
prop.table(with(biosub, xtabs(~Job+MedPrefBIOS)),1 )

with(biosub, fisher.test(Job,MedPrefBIOS))

# list of possible covars -- STEMstat HybridHist CChild ConfLStatBIOS JobStat
# (FTPT, Education, Program, WHours)
str(biosub[,-grep("epi", names(biosub), ignore.case = T)])

# Preference by Full-time or Part-time student --- Mention
# with(biosub, xtabs(~FTPT+MedPrefBIOS))
# prop.table(with(biosub, xtabs(~FTPT+MedPrefBIOS)),1 )

#with(biosub, fisher.test(FTPT,MedPrefBIOS))

#Work Hours
#with(biosub, by(WHours, MedPrefBIOS, FUN = function(x) {c(m = mean(x, na.rm = TRUE), sdev = sd(x, na.rm = TRUE))}))
#with(biosub, kruskal.test(WHours~factor(MedPrefBIOS), data = biosub))


#Alt results
with(biosub, kruskal.test(Travelminutes~ RC1, data = biosub))

biosub$Interactivity <- biosub$RC1 == "Interactivity" | biosub$RC2 == "Interactivity"
biosub$Interactivity[is.na(biosub$Interactivity)] <- FALSE

biosub$AvoidCommute <- biosub$RC1 == "AvoidCommute" | biosub$RC2 == "AvoidCommute"
biosub$AvoidCommute[is.na(biosub$AvoidCommute)] <- FALSE

boxplot(biosub$Travelminutes ~ biosub$MedPrefBIOS == "In person")

with(biosub, kruskal.test(Travelminutes ~ AvoidCommute, data = biosub))
boxplot(Travelminutes ~ AvoidCommute, data = biosub, notch = TRUE)
