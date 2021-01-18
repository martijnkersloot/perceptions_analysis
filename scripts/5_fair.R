library(likert)

data_fair <- data_demographics[,1]
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

data_fair[!is.na(data_fair$letter_f),]$letter_f_correct <- data_fair[!is.na(data_fair$letter_f),]$letter_f %in% c(
  "findable",
  "find",
  "findability",
  "finding",
  "finable",
  "findable, ,"
)

data_fair[!is.na(data_fair$letter_a),]$letter_a_correct <- data_fair[!is.na(data_fair$letter_a),]$letter_a %in% c(
  "accessible",
  "accesible",
  "accessibility",
  "accessable",
  "accesability",
  "accesable",
  "accessabil",
  "accesinle",
  "assessible" 
)

data_fair[!is.na(data_fair$letter_i),]$letter_i_correct <- data_fair[!is.na(data_fair$letter_i),]$letter_i %in% c(
  "interoperable",
  "interoperability",
  "interopable",
  "interoperable or something"
)

data_fair[!is.na(data_fair$letter_r),]$letter_r_correct <- data_fair[!is.na(data_fair$letter_r),]$letter_r %in% c(
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

data_fair_effort <- data_fair[, c(1, 3:7)]
data_fair_effort <- as.data.frame(data_fair_effort[complete.cases(data_fair_effort), ])

row.names(data_fair_effort) <- data_fair_effort$`Castor Record ID`
data_fair_effort[1] <- NULL

data_fair_effort_table <- rbind(
  data.frame(aspect = "F", count = table(data_fair_effort$effort_f)),
  data.frame(aspect = "A", count = table(data_fair_effort$effort_a)),
  data.frame(aspect = "I", count = table(data_fair_effort$effort_i)),
  data.frame(aspect = "R", count = table(data_fair_effort$effort_r))
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

colnames(data_fair_effort_table) <- c("aspect", "effort", "freq")
data_fair_effort_table$percentage <- data_fair_effort_table$freq / nrow(data_fair_effort) * 100
data_fair_effort_table$rounded <- abs(round(data_fair_effort_table$percentage))

# Make percentages negative for 'No'
data_fair_effort_table$percentage[data_fair_effort_table$effort == "No, not at all"] = -data_fair_effort_table$percentage[data_fair_effort_table$effort == "No, not at all"]

#heard_of_fair
table(data_raw$heard_of_fair)


# know_fair_definition
table(data_raw$know_fair_definition)



# fair_definition

unique(data_raw$fair_definition)




rm(trim)
