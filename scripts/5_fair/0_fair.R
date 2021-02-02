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

data_fair_definition_export <- rbind(
  data.frame(aspect="F", name="Findable", definition = unique(data_fair$letter_f)),
  data.frame(aspect="A", name="Accessible", definition = unique(data_fair$letter_a)),
  data.frame(aspect="I", name="Interoperable", definition = unique(data_fair$letter_i)),
  data.frame(aspect="R", name="Reusable", definition = unique(data_fair$letter_r))
)

data_fair_definition_export <- data_fair_definition_export[!is.na(data_fair_definition_export$definition),]
write.csv(data_fair_definition_export, file="exports/fair_definitions.csv", row.names=FALSE, na="")


data_fair_description_export <- rbind(
  data.frame(aspect="FAIR", name="FAIR", description = unique(data_fair$description_fair)),
  data.frame(aspect="F", name="Findable", description = unique(data_fair$description_f)),
  data.frame(aspect="A", name="Accessible", description = unique(data_fair$description_a)),
  data.frame(aspect="I", name="Interoperable", description = unique(data_fair$description_i)),
  data.frame(aspect="R", name="Reusable", description = unique(data_fair$description_r))
)

data_fair_description_export <- data_fair_description_export[!is.na(data_fair_description_export$description),]
write.csv(data_fair_description_export, file="exports/fair_description.csv", row.names=FALSE, na="")



data_fair_effort <- data_fair[, c("Castor Record ID", "effort_f", "effort_a", "effort_i", "effort_r", "profession_group")]
data_fair_effort <- as.data.frame(data_fair_effort[complete.cases(data_fair_effort), ])

#data_fair_effort <- subset(data_fair_effort, data_fair_effort$)

row.names(data_fair_effort) <- data_fair_effort$`Castor Record ID`
data_fair_effort[1] <- NULL

#heard_of_fair
table(data_raw$heard_of_fair)

# know_fair_definition
table(data_raw$know_fair_definition)

# fair_definition
unique(data_raw$fair_definition)
