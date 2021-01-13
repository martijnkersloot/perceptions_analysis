library(kableExtra)
library(compareGroups)

data_demographics <- data_raw[,2]

data_demographics$age <- factor(
  data_raw$age,
  levels = c(1, 2, 3, 4, 5, 0),
  labels = c("< 30",
             "30 - 39",
             "40 - 49",
             "50 - 59",
             "≥ 60",
             "Rather not tell")
)

# Workplace
# Workplace Other
# Country

data_demographics$primary_institution <- factor(
  data_raw$institute_type_primary,
  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 0),
  labels = c(
    "Academic hospital",
    "University",
    "Teaching hospital",
    "Hospital",
    "Biotech company",
    "Pharma company",
    "Medical device company",
    "Contract research organization",
    "Other"
  )
)

data_demographics$profession <- factor(
  data_raw$profession_academia,
  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 0),
  labels = c(
    "Data Steward",
    "Research nurse",
    "PhD candidate",
    "Post-doc",
    "Assistant professor",
    "Associate professor",
    "Professor",
    "Other academic staff",
    "Other"
  )
)

data_demographics$research_experience <- factor(
  data_raw$experience,
  levels = c(1, 2, 3, 4, 5),
  labels = c("< 1 year",
             "1 - 2 years",
             "2 - 4 years",
             "4 - 6 years",
             "≥ 6 years")
)

# table1(~ age + primary_institution + profession + research_experience, 
#        data = data_demographics, 
#        droplevels = TRUE, 
#        render.missing = NULL)


# Heard of FAIR
# Yes: 1
# No: 0
# Don't know / not sure: 99

# Definition/Description of FAIR
# Only shown if Yes or Don't know / not sure
# Yes: 1
# No: 0

# data_demographics$fair_heard_of <- data_raw$heard_of_fair
# data_demographics$fair_know_definition <- data_raw$know_fair_definition


# New variable: Knowledge of FAIR
data_demographics$fair_knowledge <- NA

# No if : Don't know definition/description / Not heard of FAIR 
data_demographics$fair_knowledge[data_raw$heard_of_fair == 0] <- 0
data_demographics$fair_knowledge[data_raw$know_fair_definition == 0] <- 0

# Yes if: Know definition/description
data_demographics$fair_knowledge[data_raw$heard_of_fair == 1 & data_raw$know_fair_definition == 1] <- 1


data_demographics$fair_knowledge <- factor(
  data_demographics$fair_knowledge,
  levels = c(1, 0),
  labels = c("Knowledge of FAIR",
             "No knowledge of FAIR")
)

data_demographics <- subset(data_demographics, !is.na(data_demographics$fair_knowledge))


attr(data_demographics$age, "label") <- "Age"
attr(data_demographics$profession, "label") <- "Profession"
attr(data_demographics$primary_institution, "label") <- "Primary Institution"
attr(data_demographics$research_experience, "label") <- "Research experience"
attr(data_demographics$fair_knowledge, "label") <- "Knowledge of FAIR"


res <- compareGroups(
  fair_knowledge ~ age + primary_institution + profession + research_experience,
  data_demographics
)

restab <- createTable(
  res,
  show.all = TRUE,
  show.p.overall = FALSE,
  all.last = TRUE
)

export2md(restab, header.labels = c(all = "All"))

# descrTable(data_demographics)
