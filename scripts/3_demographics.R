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

# Introduce new level: "Data manager"
data_demographics$profession <- factor(
  data_raw$profession_academia,
  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 0, 20),
  labels = c(
    "Data Steward",
    "Research nurse",
    "PhD candidate",
    "Post-doc",
    "Assistant professor",
    "Associate professor",
    "Professor",
    "Other academic staff",
    "Other",
    "Other: Data Manager"
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

# Yes if: Heard of FAIR & Know definition/description
data_demographics$fair_knowledge[data_raw$heard_of_fair == 1 & data_raw$know_fair_definition == 1] <- 1


data_demographics$fair_knowledge <- factor(
  data_demographics$fair_knowledge,
  levels = c(1, 0),
  labels = c("Knowledge of FAIR",
             "No knowledge of FAIR")
)

attr(data_demographics$age, "label") <- "Age"
attr(data_demographics$profession, "label") <- "Profession"
attr(data_demographics$primary_institution, "label") <- "Primary Institution"
attr(data_demographics$research_experience, "label") <- "Research experience"
attr(data_demographics$fair_knowledge, "label") <- "Knowledge of FAIR"



data_demographics$profession_other <-
  apply(data_raw[, c("profession_academia_other", "profession_nonacademia")],
        1,
        function(x) {
          x[!is.na(x)][1]
        })

data_demographics_data_managers <- grepl("datamanager", gsub("\\s", "", tolower(data_demographics$profession_other)), fixed = TRUE)
data_demographics_phd <- grepl("phd", gsub("\\s", "", tolower(data_demographics$profession_other)), fixed = TRUE)
data_demographics_consultant <- grepl("consultant", gsub("\\s", "", tolower(data_demographics$profession_other)), fixed = TRUE)
data_demographics_support <- grepl("researchsupport", gsub("\\s", "", tolower(data_demographics$profession_other)), fixed = TRUE)
data_demographics_coordinator <- grepl("coordinator", gsub("\\s", "", tolower(data_demographics$profession_other)), fixed = TRUE)
data_demographics_assistant <- grepl("research-assistant", gsub("\\s", "", tolower(data_demographics$profession_other)), fixed = TRUE)
data_demographics_junior <- grepl("juniorresearcher", gsub("\\s", "", tolower(data_demographics$profession_other)), fixed = TRUE)
data_demographics_manager <- grepl("trialmanager", gsub("\\s", "", tolower(data_demographics$profession_other)), fixed = TRUE)

data_demographics$profession[data_demographics_data_managers] <- "Other: Data Manager"
data_demographics$profession[data_demographics_phd] <- "PhD candidate"

profession_group_support <- c("Data Steward", "Other: Data Manager")
profession_group_researcher <- c("PhD candidate", "Post-doc", "Assistant professor", "Associate professor", "Professor")
#data_demographics$profession_group <- 

data_demographics$profession_group <- NA
data_demographics$profession_group[data_demographics$profession %in% profession_group_support] <- "Support"
data_demographics$profession_group[data_demographics$profession %in% profession_group_researcher] <- "Researcher"
data_demographics$profession_group[data_demographics_junior] <- "Researcher"
data_demographics$profession_group[data_demographics_consultant] <- "Support"
data_demographics$profession_group[data_demographics_coordinator] <- "Support"
data_demographics$profession_group[data_demographics_assistant] <- "Support"
data_demographics$profession_group[data_demographics_support] <- "Support"
data_demographics$profession_group[data_demographics_manager] <- "Support"
data_demographics$profession_group[!is.na(data_demographics$profession) & is.na(data_demographics$profession_group)] <- "Other" 
data_demographics$profession_group[!is.na(data_demographics$profession_other) & is.na(data_demographics$profession_group)] <- "Other" 

data_demographics$profession_group <- factor(data_demographics$profession_group)
attr(data_demographics$profession_group, "label") <- "Profession group"

data_demographics_no_na <- subset(data_demographics, !is.na(data_demographics$fair_knowledge))

