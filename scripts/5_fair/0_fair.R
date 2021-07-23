data_fair <- data_demographics[,1]
data_fair$identifier <- data_raw$`Survey Instance Id`
data_fair$knowledge = data_demographics$fair_knowledge
data_fair$profession_group <- data_demographics$profession_group

data_fair$effort_fair <- (data_raw$effort_f + data_raw$effort_a + data_raw$effort_i + data_raw$effort_r) > 0

# Effort
data_fair$effort_fair <- factor(
  data_fair$effort_fair,
  levels = c(FALSE, TRUE),
  labels = c(
    "No",
    "Yes"
  )
)

# Effort
data_fair$effort_f <- factor(
  data_raw$effort_f,
  levels = c(0, 1, 2, 3),
  labels = c(
    "No, not at all",
    "Yes, very little effort",
    "Yes, some effort",
    "Yes, a lot of effort"
  ),
  ordered = TRUE
)

data_fair$effort_a <- factor(
  data_raw$effort_a,
  levels = c(0, 1, 2, 3),
  labels = c(
    "No, not at all",
    "Yes, very little effort",
    "Yes, some effort",
    "Yes, a lot of effort"
  ),
  ordered = TRUE
)

data_fair$effort_i <- factor(
  data_raw$effort_i,
  levels = c(0, 1, 2, 3),
  labels = c(
    "No, not at all",
    "Yes, very little effort",
    "Yes, some effort",
    "Yes, a lot of effort"
  ),
  ordered = TRUE
)

data_fair$effort_r <- factor(
  data_raw$effort_r,
  levels = c(0, 1, 2, 3),
  labels = c(
    "No, not at all",
    "Yes, very little effort",
    "Yes, some effort",
    "Yes, a lot of effort"
  ),
  ordered = TRUE
)

data_fair$letter_f <- tolower(trim(data_raw$fair_word_individual_f))
data_fair$letter_a <- tolower(trim(data_raw$fair_word_individual_a))
data_fair$letter_i <- tolower(trim(data_raw$fair_word_individual_i))
data_fair$letter_r <- tolower(trim(data_raw$fair_word_individual_r))

data_fair$description_fair <- tolower(trim(data_raw$fair_definition))
data_fair$description_f <- tolower(trim(data_raw$fair_description_individual_f))
data_fair$description_a <- tolower(trim(data_raw$fair_description_individual_a))
data_fair$description_i <- tolower(trim(data_raw$fair_description_individual_i))
data_fair$description_r <- tolower(trim(data_raw$fair_description_individual_r))

data_fair$effort_f_description <- tolower(trim(data_raw$effort_f_description))
data_fair$effort_a_description <- tolower(trim(data_raw$effort_a_description))
data_fair$effort_i_description <- tolower(trim(data_raw$effort_i_description))
data_fair$effort_r_description <- tolower(trim(data_raw$effort_r_description))

data_fair_researchers <- subset(data_fair, data_fair$profession_group == "Researcher")


data_fair_export <- data_fair[, c("identifier", 
                                  "letter_f", 
                                  "letter_a", 
                                  "letter_i", 
                                  "letter_r", 
                                  "description_fair",
                                  "description_f",
                                  "description_a",
                                  "description_i",
                                  "description_r"
                                  )]

data_fair_selection <- data_fair_researchers

data_fair_definition_export <- rbind(
  data.frame(
    aspect = "F",
    name = "Findable",
    definition = as.data.frame(table(data_fair_selection$letter_f))$Var1,
    freq = as.data.frame(table(data_fair_selection$letter_f))$Freq
  ),
  data.frame(
    aspect = "A",
    name = "Accessible",
    definition = as.data.frame(table(data_fair_selection$letter_a))$Var1,
    freq = as.data.frame(table(data_fair_selection$letter_a))$Freq
  ),
  data.frame(
    aspect = "I",
    name = "Interoperable",
    definition = as.data.frame(table(data_fair_selection$letter_i))$Var1,
    freq = as.data.frame(table(data_fair_selection$letter_i))$Freq
  ),
  data.frame(
    aspect = "R",
    name = "Reusable",
    definition = as.data.frame(table(data_fair_selection$letter_r))$Var1,
    freq = as.data.frame(table(data_fair_selection$letter_r))$Freq
  )
)

data_fair_definition_export <- data_fair_definition_export[!is.na(data_fair_definition_export$definition),]
write.csv(data_fair_definition_export, file="exports/fair_definitions.csv", row.names=FALSE, na="")


data_fair_description_export <- rbind(
  data.frame(
    aspect = "FAIR",
    name = "FAIR",
    description = as.data.frame(table(data_fair_selection$description_fair))$Var1,
    freq = as.data.frame(table(data_fair_selection$description_fair))$Freq
  ),
  data.frame(
    aspect = "F",
    name = "Findable",
    description = as.data.frame(table(data_fair_selection$description_f))$Var1,
    freq = as.data.frame(table(data_fair_selection$description_f))$Freq
  ),
  data.frame(
    aspect = "A",
    name = "Accessible",
    description = as.data.frame(table(data_fair_selection$description_a))$Var1,
    freq = as.data.frame(table(data_fair_selection$description_a))$Freq
  ),
  data.frame(
    aspect = "I",
    name = "Interoperable",
    description = as.data.frame(table(data_fair_selection$description_i))$Var1,
    freq = as.data.frame(table(data_fair_selection$description_i))$Freq
  ),
  data.frame(
    aspect = "R",
    name = "Reusable",
    description = as.data.frame(table(data_fair_selection$description_r))$Var1,
    freq = as.data.frame(table(data_fair_selection$description_r))$Freq
  )
)

data_fair_description_export <- data_fair_description_export[!is.na(data_fair_description_export$description),]
write.csv(data_fair_description_export, file="exports/fair_description.csv", row.names=FALSE, na="")



data_fair_effort <- data_fair[, c("Castor Record ID", "effort_f", "effort_a", "effort_i", "effort_r", "profession_group")]
data_fair_effort <- as.data.frame(data_fair_effort[complete.cases(data_fair_effort), ])

#data_fair_effort <- subset(data_fair_effort, data_fair_effort$)

row.names(data_fair_effort) <- data_fair_effort$`Castor Record ID`
data_fair_effort[1] <- NULL

data_fair_effort$did_effort <- (as.numeric(data_fair_effort$effort_f) - 1 > 0) |
  (as.numeric(data_fair_effort$effort_a) - 1 > 0) |
  (as.numeric(data_fair_effort$effort_i) - 1 > 0) |
  (as.numeric(data_fair_effort$effort_r) - 1 > 0)

data_fair_effort$did_effort_all <- (as.numeric(data_fair_effort$effort_f) - 1 > 0) &
  (as.numeric(data_fair_effort$effort_a) - 1 > 0) &
  (as.numeric(data_fair_effort$effort_i) - 1 > 0) &
  (as.numeric(data_fair_effort$effort_r) - 1 > 0)

data_fair_effort$did_some_to_more_effort <- (as.numeric(data_fair_effort$effort_f) - 1 > 1) |
  (as.numeric(data_fair_effort$effort_a) - 1 > 1) |
  (as.numeric(data_fair_effort$effort_i) - 1 > 1) |
  (as.numeric(data_fair_effort$effort_r) - 1 > 1)


data_fair_effort$did_some_to_more_effort_all <- (as.numeric(data_fair_effort$effort_f) - 1 > 1) &
  (as.numeric(data_fair_effort$effort_a) - 1 > 1) &
  (as.numeric(data_fair_effort$effort_i) - 1 > 1) &
  (as.numeric(data_fair_effort$effort_r) - 1 > 1)

#heard_of_fair
table(data_raw$heard_of_fair)

# know_fair_definition
table(data_raw$know_fair_definition)

# fair_definition
unique(data_raw$fair_definition)

data_fair$fair4health <- factor(
  data_raw$fair4health,
  levels = c(1, 0, 3),
  labels = c(
    "Positive",
    "Negative",
    "I have insufficient knowledge of the FAIR principles to answer this question"
  ),
  ordered = TRUE
)


write.csv(data_fair$fair4health, file="exports/fair4health.csv", row.names=FALSE)


data_fair_effort_export <- rbind(
  data.frame(
    aspect = "F",
    name = "Findable",
    definition = as.data.frame(table(data_raw$effort_f_description))$Var1,
    freq = as.data.frame(table(data_raw$effort_f_description))$Freq
  ),
  data.frame(
    aspect = "A",
    name = "Accessible",
    definition = as.data.frame(table(data_raw$effort_a_description))$Var1,
    freq = as.data.frame(table(data_raw$effort_a_description))$Freq
  ),
  data.frame(
    aspect = "I",
    name = "Interoperable",
    definition = as.data.frame(table(data_raw$effort_i_description))$Var1,
    freq = as.data.frame(table(data_raw$effort_i_description))$Freq
  ),
  data.frame(
    aspect = "R",
    name = "Reusable",
    definition = as.data.frame(table(data_raw$effort_r_description))$Var1,
    freq = as.data.frame(table(data_raw$effort_r_description))$Freq
  )
)

write.csv(data_fair_effort_export, file="exports/fair_effort.csv", row.names=FALSE)



data_fair_effort_researchers <- subset(data_fair_effort, data_fair_effort$profession_group == "Researcher")
#table(data_fair_effort_researchers$did_effort)
#table(data_fair_effort_researchers$did_some_to_more_effort)

cat("Researchers\n")
cat(paste(
  "Effort (any aspect): ",
  format(round(table(data_fair_effort_researchers$did_effort)["TRUE"] / nrow(data_fair_effort_researchers) * 100, 1), nsmall = 1),
  "%\n",
  sep=""
))
cat(paste(
  "Some to more effort (any aspect): ",
  format(round(table(data_fair_effort_researchers$did_some_to_more_effort)["TRUE"] / nrow(data_fair_effort_researchers) * 100, 1), nsmall = 1),
  "%\n",
  sep=""
))
cat(paste(
  "Effort (all aspects): ",
  format(round(table(data_fair_effort_researchers$did_effort_all)["TRUE"] / nrow(data_fair_effort_researchers) * 100, 1), nsmall = 1),
  "%\n",
  sep=""
))
cat(paste(
  "Some to more effort (all aspects): ",
  format(round(table(data_fair_effort_researchers$did_some_to_more_effort_all)["TRUE"] / nrow(data_fair_effort_researchers) * 100, 1), nsmall = 1),
  "%\n",
  sep=""
))

data_fair_effort_support <- subset(data_fair_effort, data_fair_effort$profession_group == "Support")
#table(data_fair_effort_support$did_effort)
#table(data_fair_effort_support$did_some_to_more_effort)


cat("\n\nSupport\n")
cat(paste(
  "Effort (any aspect): ",
  format(round(table(data_fair_effort_support$did_effort)["TRUE"] / nrow(data_fair_effort_support) * 100, 1), nsmall = 1),
  "%\n",
  sep=""
))
cat(paste(
  "Some to more effort (any aspect): ",
  format(round(table(data_fair_effort_support$did_some_to_more_effort)["TRUE"] / nrow(data_fair_effort_support) * 100, 1), nsmall = 1),
  "%\n",
  sep=""
))
cat(paste(
  "Effort (all aspects): ",
  format(round(table(data_fair_effort_support$did_effort_all)["TRUE"] / nrow(data_fair_effort_support) * 100, 1), nsmall = 1),
  "%\n",
  sep=""
))
cat(paste(
  "Some to more effort (all aspects): ",
  format(round(table(data_fair_effort_support$did_some_to_more_effort_all)["TRUE"] / nrow(data_fair_effort_support) * 100, 1), nsmall = 1),
  "%\n",
  sep=""
))
