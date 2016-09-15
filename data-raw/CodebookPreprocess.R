# Preprocess Survey Codebook

codebook <- read.csv("data-raw/SurveyCodebook.csv", header = FALSE,
                     colClasses = c("character", "character"),
                     na.strings = "NA")
names(codebook) <- c("LongQItem", "ShortQItem")

write.csv(codebook, file = "data/SurveyCodebook.csv")
