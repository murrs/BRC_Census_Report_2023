library(data.table)
library(survey)
library(anesrake)

#Read in random samplle data, create age categorical variable
randomSample2023 <- fread("allRandomSamples2023.tsv", sep = "\t")
randomSample2023[, age := 2023 - `What is the year of your birth (1)`]
randomSample2023[, ageCat5 := ifelse(between(age, 0, 29), "0-29",
                                     ifelse(between(age, 30, 39), "30-39",
                                            ifelse(between(age, 40, 49), "40-49",
                                                   ifelse(between(age, 50, 59), "50-59",
                                                          ifelse(age >= 60, "60+", NA)))))]
randomSample2022 <- fread("C:\\Users\\ashev\\Documents\\census\\report_2022\\allRandomSamples2022.tsv", sep = "\t")
#Read in cleaned online survey data
online2023 <- fread("cleaned2023.tsv", sep = "\t")
online2023 <- do.call(data.table, lapply(online2023, function(x){
  ifelse(x == "", NA, x)}))

#Create "population" data sets for raking based off random sample
totPop = 72000
# early.ipf <- data.frame(early.ipf = c("Early", "Not early"), freq = (c(918, 2593)))
# early.ipf$freq <- early.ipf$freq / sum(early.ipf$freq) * totPop
# early.target <- early.ipf$freq / totPop
# names(early.target) <- early.ipf$early.ipf

nbburns.ipf <- data.frame(nbburns.ipf = c("Virgin", "1", "2", "3-4", "5-7", "8+"),
                          freq = c(sum(randomSample2023$`Number of burns (TOTAL YEARS) (4b)` == 1 | 
                                         (is.na(randomSample2023$`Number of burns (TOTAL YEARS) (4b)`) &
                                            !is.na(randomSample2023$`What was the first language you learned (5)`)), na.rm = TRUE),
                                   sum(randomSample2023$`Number of burns (TOTAL YEARS) (4b)` == 2, na.rm = TRUE),
                                   sum(randomSample2023$`Number of burns (TOTAL YEARS) (4b)` == 4, na.rm = TRUE),
                                   sum((randomSample2023$`Number of burns (TOTAL YEARS) (4b)` >= 4) &
                                         (randomSample2023$`Number of burns (TOTAL YEARS) (4b)` <= 5), na.rm = TRUE),
                                   sum((randomSample2023$`Number of burns (TOTAL YEARS) (4b)` >= 6) &
                                         (randomSample2023$`Number of burns (TOTAL YEARS) (4b)` <= 8), na.rm = TRUE),
                                   sum(randomSample2023$`Number of burns (TOTAL YEARS) (4b)` >= 9, na.rm = TRUE)))
nbburns.ipf$freq <- nbburns.ipf$freq / sum(nbburns.ipf$freq) * totPop
nbburns.target <- nbburns.ipf$freq / totPop
names(nbburns.target) <- nbburns.ipf$nbburns.ipf

gender.ipf <- data.frame(gender.ipf = c("Female", "Male", "Other"),
                         freq = c(sum(randomSample2023$`What is your current gender (3)` == 1, na.rm = TRUE),
                                  sum(randomSample2023$`What is your current gender (3)` == 2, na.rm = TRUE),
                                  sum(randomSample2023$`What is your current gender (3)` %in% c(3, 7, 9), na.rm = TRUE)))
gender.ipf$freq <- gender.ipf$freq / sum(gender.ipf$freq) * totPop
gender.target <- gender.ipf$freq / totPop
names(gender.target) = gender.ipf$gender.ipf

age.ipf <- data.frame(age.ipf = c("0-29", "30-39", "40-49", "50-59", "60+"),
                      freq = c(sum(randomSample2023$ageCat5 == "0-29", na.rm = TRUE),
                               sum(randomSample2023$ageCat5 == "30-39", na.rm = TRUE),
                               sum(randomSample2023$ageCat5 == "40-49", na.rm = TRUE),
                               sum(randomSample2023$ageCat5 == "50-59", na.rm = TRUE),
                               sum(randomSample2023$ageCat5 == "60+", na.rm = TRUE)))
age.ipf$freq <- age.ipf$freq / sum(age.ipf$freq) * totPop
age.target <- age.ipf$freq / totPop
names(age.target) = age.ipf$age.ipf

english.ipf <- data.frame(english.ipf = c("English", "Other language"),
                          freq = c(sum(randomSample2023$`What was the first language you learned (5)` == 1, na.rm = TRUE),
                                   sum(randomSample2023$`What was the first language you learned (5)` %in% (2:9), na.rm = TRUE)))
english.ipf$freq <- english.ipf$freq / sum(english.ipf$freq) * totPop
english.target <- english.ipf$freq / totPop
names(english.target) = english.ipf$english.ipf


foreign.ipf <- data.frame(foreign.ipf = c("US", "Other country"),
                          freq = c(sum(randomSample2023$`Where do you usually reside (2)` %in% (1:3), na.rm = TRUE),
                                   sum(randomSample2023$`Where do you usually reside (2)` %in% (4:6), na.rm = TRUE)))
foreign.ipf$freq <- foreign.ipf$freq / sum(foreign.ipf$freq) * totPop
foreign.target <- foreign.ipf$freq / totPop
names(foreign.target) = foreign.ipf$foreign.ipf

notEligible <- sum(randomSample2023$`Are you eligible to vote (6a)` == 2,
                   na.rm = TRUE)
party.ipf <- data.frame(party.ipf = c("Not eligible", "Democrat", "Green", 
                                      "Libertarian", "Republican", "Other", "None"),
                        freq = c(notEligible,
                                 sum(randomSample2023$`With which political party are you currently affiliated (6c)` == 1, na.rm = TRUE),
                                 sum(randomSample2023$`With which political party are you currently affiliated (6c)` == 2, na.rm = TRUE),
                                 sum(randomSample2023$`With which political party are you currently affiliated (6c)` == 3, na.rm = TRUE),
                                 sum(randomSample2023$`With which political party are you currently affiliated (6c)` == 4, na.rm = TRUE),
                                 sum(randomSample2023$`With which political party are you currently affiliated (6c)` == 5, na.rm = TRUE),
                                 sum(randomSample2023$`With which political party are you currently affiliated (6c)` == 6, na.rm = TRUE)))
party.ipf$freq <- party.ipf$freq / sum(party.ipf$freq) * totPop
party.target <- party.ipf$freq / totPop
names(party.target) = party.ipf$party.ipf


#Create online survey variables for raking
online2023[, early.ipf := ifelse(grepl("((pre)|(Before))", firstArrivedBRC), "Early", 
                                 ifelse(grepl("[A-Za-z0-9]+", firstArrivedBRC), "Not early", NA))]
online2023[, early.ipf := as.factor(early.ipf)]

online2023[, nbburns.ipf := ifelse(nburns == 1, "Virgin",
                                   ifelse(nburns == 2, "1",
                                          ifelse(nburns == 3, "2",
                                                 ifelse(between(nburns, 4, 5), "3-4",
                                                        ifelse(between(nburns, 6, 8), "5-7",
                                                               ifelse(nburns >= 9, "8+", NA))))))]
online2023[, nbburns.ipf := as.factor(nbburns.ipf)]

online2023[, gender.ipf := ifelse(grepl("(?<!(e))([Mm]ale)", currentGender, perl = TRUE), "Male",
                                  ifelse(grepl("[Ff]emale", currentGender), "Female",
                                         ifelse(grepl("[A-Z]+", currentGender), "Other", NA)))]
online2023[, gender.ipf := as.factor(gender.ipf)]

online2023[, age.ipf := ifelse(between(age, 0, 29), "0-29",
                               ifelse(between(age, 30, 39), "30-39",
                                      ifelse(between(age, 40, 49), "40-49",
                                             ifelse(between(age, 50, 59), "50-59",
                                                    ifelse(age >= 60, "60+", NA)))))]
online2023[, age.ipf := as.factor(age.ipf)]

online2023[, english.ipf := ifelse(firstLanguage == "English", "English",
                                   ifelse(grepl("[A-Z]+", firstLanguage), "Other language", NA))]
online2023[, english.ipf := as.factor(english.ipf)]

online2023[, foreign.ipf := ifelse(reside %in% c("Within California", 
                                                 "Within Nevada", 
                                                 "Other location within U.S."),
                                   "US", ifelse(grepl("[A-Z]+", reside),
                                                "Other country", NA))]
online2023[, foreign.ipf := as.factor(foreign.ipf)]

online2023[, party.ipf := ifelse(eligibleVoteUS == "no", "Not eligible",
                                 ifelse(politicalParty == "Democratic Party", "Democrat",
                                        ifelse(politicalParty == "Green Party", "Green",
                                               ifelse(politicalParty == "Libertarian Party", "Libertarian",
                                                      ifelse(politicalParty == "Republican Party", "Republican",
                                                             ifelse(politicalParty == "Other US Party", "Other",
                                                                    ifelse(politicalParty == "None or unaffiliated", "None", NA)))))))]
online2023[, party.ipf := as.factor(party.ipf)]



rake2023 <- anesrake(inputter = list(#early.ipf = early.target,
                                     nbburns.ipf = nbburns.target,
                                     gender.ipf = gender.target,
                                     age.ipf = age.target,
                                     english.ipf = english.target,
                                     foreign.ipf = foreign.target,
                                     party.ipf = party.target),
                     dataframe = online2023,
                     caseid = online2023$responseID,
                     type = "nolim",
                     cap = 5)

online2023$weights <- rake2023$weightvec
fwrite(online2023, file = "census2023_cleaned_weighted.tsv", sep = "\t")
