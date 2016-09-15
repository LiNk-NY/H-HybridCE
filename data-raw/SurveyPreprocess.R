# Survey Dataset Preprocessing

colnums <- 1:44
colnums <- colnums[-c(13,17,30,33,35,36)]

survey <- read.xlsx("data-raw/PH750-2SurveyDataRaw.xlsx",
                    sheetIndex = 1, header = TRUE,
                    colIndex = colnums, stringsAsFactors = FALSE)

codebook <- read.csv("data/SurveyCodebook.csv",
                         colClasses = c("character", "character"),
                         na.strings = "NA")

compl <- complete.cases(codebook)
colnames(survey) <- colnamesfile$V2[compl]

survey$EpiBiosStatus[c(1,24)] <- "PH750"
survey$EpiBiosStatus <- factor(survey$EpiBiosStatus, labels = c("Bios", "Epi"))

Reasons <- ifelse(!is.na(survey$MedPrefRBIOS),
                  survey$MedPrefRBIOS, survey$PrefEPIR)

survey$Race2 <- revalue(survey$Race, c("Asian" = "Other",
                                       "Prefer not to respond" = "Other",
                                       "South Asian/European Jew" = "Other"))
survey$Race2 <- factor(survey$Race2,
                       levels = c("Non-Hispanic white", "Non-Hispanic black",
                                  "Hispanic", "Other"))
survey$Job <- revalue(survey$JobStat,
                      c("No" = "No/No Answer",
                        "Prefer not to respond" = "No/No Answer"))
survey$Job <- factor(survey$Job, levels = c("No/No Answer", "Full-time", "Part-time"))
survey$Gender[11] <- NA
survey$Gender <- factor(survey$Gender, levels = c("Female", "Male"))

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

survey$MedPrefEPI2 <- factor(survey$MedPrefEPI, levels =
                                 c("In person", "Online",
                                   "Online after class is over"),
                             labels = c("In person", "Synchronous",
                                        "Asynchronous"))
survey$MedPrefBIOS2 <- factor(survey$MedPrefBIOS, levels = c("In person",
                                                            "Online while class is occurring",
                                                            "Online after class is over"),
                             labels = c("In person", "Synchronous", "Asynchronous"))

# Course format choice if participant were to re-take course
survey$retake <- revalue(survey$FFormatThis,
                         c("6 in-person only, 6 0nline only" = "Other",
                           "Option of each class in-person or recorded (not just online)" = "Other",
                           "Eight sessions in-person only, four sessions online only." = "8 online, 4 in-person",
                           "I would like to have the option of attending each class in-person or online." = "Option of in-person or online",
                           "Classes are in-person only" = "In-person", "Classes are online only" = "Online only",
                           "Classes are in person and recorded for viewing after the lecture is over" = "Other"))

write.csv(survey, "data/PH750-2SurveyData.csv")
