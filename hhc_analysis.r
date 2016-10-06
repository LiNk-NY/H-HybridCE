## Marcel Ramos
## YouTube viewing habits
library(ggplot2)
library(grid)
library(RColorBrewer)
library(xlsx)
library(plyr)

survey <- read.csv("data/PH750-2SurveyData.csv", stringsAsFactors = TRUE)

numberSummaries <- function(number) {
    c(MIN = min(number, na.rm = TRUE),
      MED = median(number, na.rm = TRUE),
      MAX = max(number, na.rm = TRUE))
}

meanSD <- function(number) {
    round(
    c(m = mean(number, na.rm = TRUE),
      sdev  = sd(number, na.rm = TRUE)), 1)
}

###     Subsetting for BIOS students    ###
biosub <- subset(survey, survey$EpiBiosStatus == "Bios")

###     TABLE 1       ###
by(survey$Age, survey$EpiBiosStatus, FUN = meanSD)
# by(survey$Age, survey$EpiBiosStatus, FUN = numberSummaries)

table(survey$Gender, survey$EpiBiosStatus)
round(prop.table(table(survey$Gender, survey$EpiBiosStatus), 2), 2)*100

table(survey$Race2, survey$EpiBiosStatus)
round(prop.table(table(survey$Race2, survey$EpiBiosStatus),2), 2)

by(survey$Travelminutes, survey$EpiBiosStatus, FUN = meanSD)

by(survey$Travelminutes, survey$EpiBiosStatus, FUN = numberSummaries)

# Course Preference by Class

# online for PH752 == Online after class is over
# Add missing category in 2nd Factor

survey$MedPrefEPI2 <- factor(survey$MedPrefEPI2,
                             levels =
                                 c("In person", "Synchronous", "Asynchronous"))
prefs <- cbind(Bios = table(survey$MedPrefBIOS2),
               Epi = table(survey$MedPrefEPI2))
prefs
prop.table(prefs, 2)

fpre <- table(survey$RC1, survey$EpiBiosStatus) + table(survey$RC2, survey$EpiBiosStatus)
fpre/colSums(prefs)

table(survey$retake, survey$EpiBiosStatus)
prop.table(with(survey, xtabs(~retake+EpiBiosStatus)),2)

###     YouTube Viewing Patterns     ###
###             FIGURE 2             ###
### YouTube Lecture Viewing Patterns ###

longdata <- read.csv("data/Figure2YouTubeData.csv", stringsAsFactors = FALSE)
longdata$Day <- as.Date(longdata$Day, format = "%m/%d/%Y")

cols0 <- c("Lecture 2" = "gray85", "Lecture 3" = "gray65",
           "Lecture 4" = "gray45", "Lecture 5" = "gray25")

postscript("Figures/YouTubeViewsF2.eps", width = 8, height = 4,
           paper = "special", horizontal = FALSE, family = "Times")

p <- ggplot(longdata, aes(x = Day, y = views, fill = lecture)) +
    geom_area(position = "stack") + xlab("Date") + ylab("Number of views") +
    labs(fill = "Online Lecture \n Videos") +
    geom_vline(xintercept = as.numeric(as.Date.character("2014-03-03")),
               lty = 4, lwd = 1) +
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


#Totals
table(survey$EpiBiosStatus)

###     FIGURE 1        ###
#Reasons for course pref selection
comb <-  table(biosub$RC1, biosub$MedPrefBIOS2) +
    table(biosub$RC2, biosub$MedPrefBIOS2)
comb <- comb[,c("In person", "Asynchronous", "Synchronous")]
comb <- comb[c("Interactivity", "Convenience",
                    "AvoidCommute", "LearnPref"),]
rownames(comb) <- c("Interactivity", "Convenience", "Avoiding Commute",
                    "Learning Preference")
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
with(biosub, by(Age, MedPrefBIOS, FUN = meanSD))
with(biosub, by(Age, MedPrefBIOS, FUN = numberSummaries))

with(biosub, kruskal.test(Age~ factor(MedPrefBIOS), data = biosub))
sampAgeMed <- with(biosub, median(Age, na.rm = TRUE))
MedBin1 <- ifelse(biosub$Age>sampAgeMed, "Yes", "No")
fisher.test(table(MedBin1, biosub$MedPrefBIOS))

#Travel Time by Preference
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
cat("Fisher's Exact test p-value =", fisher.test(comb)$p.value)

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

# Preference by Full-time or Part-time student --- Mention
with(biosub, xtabs(~FTPT+MedPrefBIOS))
prop.table(with(biosub, xtabs(~FTPT+MedPrefBIOS)),1 )
with(biosub, fisher.test(FTPT,MedPrefBIOS))

# Work Hours
with(biosub, by(WHours, MedPrefBIOS, FUN = function(x) {c(m = mean(x, na.rm = TRUE), sdev = sd(x, na.rm = TRUE))}))
with(biosub, kruskal.test(WHours~factor(MedPrefBIOS), data = biosub))
