## Marcel Ramos
## YouTube viewing habits
library(lattice)
library(xlsx)

demo <- read.csv("data-raw/YouTubeDemographics.csv", header = TRUE)
demo <- demo[,c("Gender", "Age.group", "Percentage")]
demo$percentage <- as.numeric(unlist(strsplit(as.character(demo$Percentage),
                                              split = "0000%", fixed = TRUE)))
summary(demo$Age.group)
demo$AgeG <- factor(demo$Age.group, levels =
                    c("18-24", "25-34", "35-44", "45-54", "55-64", "65-"))

barchart(demo$percentage~demo$AgeG, data = demo,
         groups = demo$Gender,
         auto.key = list(space = "right"),
         main = "Demographics by Age Group",
         xlab = "Age Group", ylab = "Percentage")

devs <- read.csv("data-raw/YouTubeDevices.csv", header = TRUE)
devs
devs$devper <- round(devs$Views/sum(devs$Views)*100, 2)

survey <- read.xlsx("data-raw/PH750-2SurveyDataRaw.xlsx", sheetIndex = 1,
                    header = TRUE)

table(survey$What.was.your.preferred.way.to.attend.lectures.for.this.course.)

