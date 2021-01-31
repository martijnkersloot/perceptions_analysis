data_fair <- data_demographics[,1]
data_fair$identifier <- data_raw$`Survey Instance Id`
data_fair$knowledge = data_demographics$fair_knowledge

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

data_fair$letter_f_correct <- NA 
data_fair$letter_a_correct <- NA 
data_fair$letter_i_correct <- NA 
data_fair$letter_r_correct <- NA 

fair_letter_f <- c(
  "findable",
  "find",
  "findability",
  "finding",
  "finable",
  "findable, ,"
)

fair_letter_a <- c(
  "accessible",
  "accesible",
  "accessibility",
  "accessable",
  "accesability",
  "accesable",
  "accessabil",
  "accesinle",
  "assessible",
  "accesibility"
)

fair_letter_i <- c(
  "interoperable",
  "interoperability",
  "interopable",
  "interoperable or something"
)

fair_letter_r <- c(
  "reausable",
  "reusable",
  "reusing",
  "re usable",
  "reusability",
  "reuse",
  "re-useable",
  "reuseable",
  "re-use ...",
  "reus",
  "reuasable"
)

data_fair[!is.na(data_fair$letter_f),]$letter_f_correct <- data_fair[!is.na(data_fair$letter_f),]$letter_f %in% fair_letter_f
data_fair[!is.na(data_fair$letter_a),]$letter_a_correct <- data_fair[!is.na(data_fair$letter_a),]$letter_a %in% fair_letter_a
data_fair[!is.na(data_fair$letter_i),]$letter_i_correct <- data_fair[!is.na(data_fair$letter_i),]$letter_i %in% fair_letter_i
data_fair[!is.na(data_fair$letter_r),]$letter_r_correct <- data_fair[!is.na(data_fair$letter_r),]$letter_r %in% fair_letter_r

data_fair$description_fair <- tolower(trim(data_raw$fair_definition))
data_fair$description_f <- tolower(trim(data_raw$fair_description_individual_f))
data_fair$description_a <- tolower(trim(data_raw$fair_description_individual_a))
data_fair$description_i <- tolower(trim(data_raw$fair_description_individual_i))
data_fair$description_r <- tolower(trim(data_raw$fair_description_individual_r))


# data_fair[!is.na(data_fair$description_f) & is.na(data_fair$letter_f_correct),]$letter_f_correct <- data_fair[!is.na(data_fair$description_f) & is.na(data_fair$letter_f_correct),]$description_f %in% fair_letter_f
# data_fair[!is.na(data_fair$description_a) & is.na(data_fair$letter_a_correct),]$letter_a_correct <- data_fair[!is.na(data_fair$description_a) & is.na(data_fair$letter_a_correct),]$description_a %in% fair_letter_a
# data_fair[!is.na(data_fair$description_i) & is.na(data_fair$letter_i_correct),]$letter_i_correct <- data_fair[!is.na(data_fair$description_i) & is.na(data_fair$letter_i_correct),]$description_i %in% fair_letter_i
# data_fair[!is.na(data_fair$description_r) & is.na(data_fair$letter_r_correct),]$letter_r_correct <- data_fair[!is.na(data_fair$description_r) & is.na(data_fair$letter_r_correct),]$description_r %in% fair_letter_r

data_fair$profession_group <- data_demographics$profession_group

data_fair_effort <- data_fair[, c("Castor Record ID", "effort_f", "effort_a", "effort_i", "effort_r", "profession_group")]
data_fair_effort <- as.data.frame(data_fair_effort[complete.cases(data_fair_effort), ])

#data_fair_effort <- subset(data_fair_effort, data_fair_effort$)

row.names(data_fair_effort) <- data_fair_effort$`Castor Record ID`
data_fair_effort[1] <- NULL

data_fair_effort_table <- rbind(
  data.frame(aspect = "F", count = table(data_fair_effort$effort_f, data_fair_effort$profession_group)),
  data.frame(aspect = "A", count = table(data_fair_effort$effort_a, data_fair_effort$profession_group)),
  data.frame(aspect = "I", count = table(data_fair_effort$effort_i, data_fair_effort$profession_group)),
  data.frame(aspect = "R", count = table(data_fair_effort$effort_r, data_fair_effort$profession_group))
)


data_fair_effort_table$aspect <- factor(
  data_fair_effort_table$aspect,
  levels = c("F", "A", "I", "R"),
  labels = c(
    "Findable",
    "Accessible",
    "Interoperable",
    "Reusable"
  )
)

colnames(data_fair_effort_table) <- c("aspect", "effort", "profession_group", "freq")
#data_fair_effort_table$percentage <- data_fair_effort_table$freq / nrow(data_fair_effort) * 100
#data_fair_effort_table$rounded <- abs(round(data_fair_effort_table$percentage))

data_fair_effort_table$percentage <- NA
data_fair_effort_table$rounded <- NA


for(group in unique(data_fair_effort_table$profession_group)) {
  group_subset <- data_fair_effort_table$profession_group == group
  data_fair_effort_table[group_subset,]$percentage <- data_fair_effort_table[group_subset,]$freq / nrow(subset(data_fair_effort, data_fair_effort$profession_group == group)) * 100
}

#data_fair_effort_table$percentage <- data_fair_effort_table$freq / nrow(data_fair_effort) * 100
data_fair_effort_table$rounded <- abs(round(data_fair_effort_table$percentage))

# Make percentages negative for 'No'
data_fair_effort_table$percentage[data_fair_effort_table$effort == "No, not at all"] = -data_fair_effort_table$percentage[data_fair_effort_table$effort == "No, not at all"]

#heard_of_fair
table(data_raw$heard_of_fair)

# know_fair_definition
table(data_raw$know_fair_definition)

# fair_definition
unique(data_raw$fair_definition)