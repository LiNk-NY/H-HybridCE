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

devs <- read.csv("data-raw/devices.csv", header = TRUE)
devs
devs$devper <- round(devs$Views/sum(devs$Views)*100, 2)

survey <- read.xlsx("data-raw/PH750-2SurveyData.xlsx", sheetIndex = 1,
                    header = TRUE)

table(survey$What.was.your.preferred.way.to.attend.lectures.for.this.course.)

## Exploratory Graphs
plot(survey$How.many.minutes.of.total.travel.time.does.it.take.for.you.to.attend.a.class.on.campus..including.time.spent.both.getting.to.and.from.class.., type = "h", pch = 0)
points(survey$How.many.minutes.of.total.travel.time.does.it.take.for.you.to.attend.a.class.on.campus..including.time.spent.both.getting.to.and.from.class.., pch = 0)
meanMinutes <- mean(survey$How.many.minutes.of.total.travel.time.does.it.take.for.you.to.attend.a.class.on.campus..including.time.spent.both.getting.to.and.from.class..)
sdMinutes <- sd(survey$How.many.minutes.of.total.travel.time.does.it.take.for.you.to.attend.a.class.on.campus..including.time.spent.both.getting.to.and.from.class..)
abline(h = meanMinutes, lty = 3)
abline(h = meanMinutes - sdMinutes, lty = 4)
abline(h = meanMinutes + sdMinutes, lty = 4)

boxplot(survey$How.many.minutes.of.total.travel.time.does.it.take.for.you.to.attend.a.class.on.campus..including.time.spent.both.getting.to.and.from.class.., type = "h")

