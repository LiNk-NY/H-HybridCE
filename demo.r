## Marcel Ramos
## YouTube viewing habits

demo <- read.csv("data-raw/demographics.csv", header=TRUE)
demo <- demo[,c("Gender", "Age.group", "Percentage")]
demo$percentage <- as.numeric(unlist(strsplit(as.character(demo$Percentage), split="0000%", fixed=TRUE)))
summary(demo$Age.group)
demo$AgeG <- factor(demo$Age.group, levels=c("18-24", "25-34", "35-44", "45-54", "55-64", "65-"))
demo

library(lattice)
barchart(demo$percentage~demo$AgeG, data=demo, groups=demo$Gender, auto.key=list(space="right"), main="Demographics by Age Group", xlab="Age Group", ylab="Percentage")

devs <- read.csv("data-raw/devices.csv", header=TRUE)
devs
devs$devper <- round(devs$Views/sum(devs$Views)*100, 2)


library(xlsx)
survey <- read.xlsx("data-raw/PH750-2 online tech 07-09.xlsx", sheetIndex=1, header=TRUE)
head(survey,10)
table(survey$What.was.your.preferred.way.to.attend.lectures.for.this.course.)
plot(survey$How.many.minutes.of.total.travel.time.does.it.take.for.you.to.attend.a.class.on.campus..including.time.spent.both.getting.to.and.from.class.., type="h", pch=0)
points(survey$How.many.minutes.of.total.travel.time.does.it.take.for.you.to.attend.a.class.on.campus..including.time.spent.both.getting.to.and.from.class.., pch=0)
abline(h=mean(survey$How.many.minutes.of.total.travel.time.does.it.take.for.you.to.attend.a.class.on.campus..including.time.spent.both.getting.to.and.from.class..), lty=3)
abline(h=100.1622-65.80665, lty=4)
abline(h=100.1622+65.80665, lty=4)

boxplot(survey$How.many.minutes.of.total.travel.time.does.it.take.for.you.to.attend.a.class.on.campus..including.time.spent.both.getting.to.and.from.class.., type="h")

mean(survey$How.many.minutes.of.total.travel.time.does.it.take.for.you.to.attend.a.class.on.campus..including.time.spent.both.getting.to.and.from.class..)
sd(survey$How.many.minutes.of.total.travel.time.does.it.take.for.you.to.attend.a.class.on.campus..including.time.spent.both.getting.to.and.from.class..)
sd(survey$How.many.minutes.of.total.travel.time.does.it.take.for.you.to.attend.a.class.on.campus..including.time.spent.both.getting.to.and.from.class..)
