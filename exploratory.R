## Exploratory Graphs
source("demo.r")
plot(survey$How.many.minutes.of.total.travel.time.does.it.take.for.you.to.attend.a.class.on.campus..including.time.spent.both.getting.to.and.from.class.., type = "h", pch = 0)
points(survey$How.many.minutes.of.total.travel.time.does.it.take.for.you.to.attend.a.class.on.campus..including.time.spent.both.getting.to.and.from.class.., pch = 0)
meanMinutes <- mean(survey$How.many.minutes.of.total.travel.time.does.it.take.for.you.to.attend.a.class.on.campus..including.time.spent.both.getting.to.and.from.class..)
sdMinutes <- sd(survey$How.many.minutes.of.total.travel.time.does.it.take.for.you.to.attend.a.class.on.campus..including.time.spent.both.getting.to.and.from.class..)
abline(h = meanMinutes, lty = 3)
abline(h = meanMinutes - sdMinutes, lty = 4)
abline(h = meanMinutes + sdMinutes, lty = 4)

boxplot(survey$How.many.minutes.of.total.travel.time.does.it.take.for.you.to.attend.a.class.on.campus..including.time.spent.both.getting.to.and.from.class.., type = "h")

## Exploratory Plot
source("hhc_analysis.r")
par(family = "serif")

plot(biosub$Travelminutes, type = "h",
     ylab = "Travel Time (to and from in Minutes)",
     xlab = "", main = "BIOS Student Travel Times by Preference", axes = F,
     col = factor(biosub$MedPrefBIOS), lwd = 2)
axis(side = 2, las = 2)
axis(side = 1)

points(biosub$Travelminutes, pch = 0)
with(biosub, abline(h = mean(Travelminutes), lty = 3))
with(biosub, abline(h = mean(Travelminutes) - sd(Travelminutes), lty = 4))
with(biosub, abline(h = mean(Travelminutes) + sd(Travelminutes), lty = 4))
legend(x = 11, y = 225, legend = c("In-person", "Asynchronous", "Synchronous"),
       col = 1:3, lty = 1)

# graphics.off()

hist(biosub$Age, breaks = 20, density = c(seq(5, 25, 5)),
     col = brewer.pal(5, "Set1"), main = "Historgram", xlab = "Age (yrs.)")

hist(survey$Travelminutes, breaks = 20, density = 25,
     col = brewer.pal(5, "Dark2"))

with(biosub, kruskal.test(Travelminutes~ RC1, data = biosub))

biosub$Interactivity <- biosub$RC1 == "Interactivity" | biosub$RC2 == "Interactivity"
biosub$Interactivity[is.na(biosub$Interactivity)] <- FALSE

biosub$AvoidCommute <- biosub$RC1 == "AvoidCommute" | biosub$RC2 == "AvoidCommute"
biosub$AvoidCommute[is.na(biosub$AvoidCommute)] <- FALSE

boxplot(biosub$Travelminutes ~ biosub$MedPrefBIOS == "In person")

with(biosub, kruskal.test(Travelminutes ~ AvoidCommute, data = biosub))
boxplot(Travelminutes ~ AvoidCommute, data = biosub, notch = TRUE)

# list - STEMstat HybridHist CChild ConfLStatBIOS JobStat
# (FTPT, Education, Program, WHours)
str(biosub[,-grep("epi", names(biosub), ignore.case = T)])
