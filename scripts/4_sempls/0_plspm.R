perceptions_path_edgelist <- rbind(
  c("attitude", "intention_to_act"),
  c("awareness", "perceived_usefulness"),
  c("compatibility", "attitude"),
  c("experienced_usefulness", "perceived_usefulness"),
  c("external_influence", "subjective_norm"),
  c("facilitating_conditions", "perceived_behavioral_control"),
  c("intention_to_act", "behavior"),
  c("interpersonal_influence", "subjective_norm"),
  c("perceived_behavioral_control", "intention_to_act"),
  c("perceived_ease_of_use", "perceived_usefulness"),
  c("perceived_ease_of_use", "attitude"),
  c("perceived_risk", "intention_to_act"),
  c("perceived_usefulness", "attitude"),
  c("self_efficacy", "perceived_behavioral_control"),
  c("situational_normality", "intention_to_act"),
  c("structural_assurance", "intention_to_act"),
  c("subjective_norm", "intention_to_act")
)

# Outer model / blocks
perceptions_path_matrix <- rbind(
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
  c(0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
)

colnames(perceptions_path_matrix) <- c(
  "awareness",
  "compatibility",
  "experienced_usefulness",
  "external_influence",
  "facilitating_conditions",
  "interpersonal_influence",
  "perceived_ease_of_use",
  "perceived_risk",
  "perceived_usefulness",
  "self_efficacy",
  "situational_normality",
  "structural_assurance",
  "subjective_norm",
  "perceived_behavioral_control",
  "attitude",
  "intention_to_act",
  "behavior"
)

rownames(perceptions_path_matrix) <- colnames(perceptions_path_matrix)

perceptions_structural_model_names <- as.data.frame(rbind(
  c("compatibility", "Compatibility"),
  c("awareness", "Awareness"),
  c("experienced_usefulness", "Experienced usefulness"),
  c("perceived_usefulness", "Perceived usefulness"),
  c("perceived_ease_of_use", "Perceived ease of use"),
  c("attitude", "Attitude"),
  c("interpersonal_influence", "Interpersonal influence"),
  c("external_influence", "External influence"),
  c("subjective_norm", "Subjective norm"),
  c("self_efficacy", "Self-efficacy"),
  c("facilitating_conditions", "Facilitating conditions"),
  c("perceived_behavioral_control", "Perceived behavioral control"),
  c("situational_normality", "Situational normality"),
  c("structural_assurance", "Structural assurance"),
  c("perceived_risk", "Perceived risk"),
  c("intention_to_act", "Intention to act"),
  c("behavior", "Behavior")
))

colnames(perceptions_structural_model_names) <- c("short", "full")
row.names(perceptions_structural_model_names) <- perceptions_structural_model_names$short
perceptions_structural_model_names[1] <- NULL

measurement_model <- list(
  generate_block("awareness", 4),
  generate_block("compatibility", 1),
  generate_block("experiencedusefulness", 6),
  generate_block("externalinfluence", 2),
  generate_block("facilitatingconditions", 4),
  generate_block("interpersonalinfluence", 4),
  generate_block("perceivedeaseofuse", 1),
  generate_block("perceivedrisk", 2),
  generate_block("perceivedusefulness", 6),
  generate_block("selfefficacy", 10),
  generate_block("situationalnormality", 1),
  generate_block("structuralassurance", 4),
  generate_block("subjectivenorm", 1),
  generate_block("perceivedbehavioralcontrol", 1),
  generate_block("attitude", 4),
  generate_block("intentiontoact", 4),
  generate_block("behavior", 4)
  # c(generate_block("behavior", 4), "effort_f", "effort_a", "effort_i", "effort_r")
)

data_plspm <- data_raw[, c("Castor Record ID", unlist(measurement_model))]

data_plspm$fair_knowledge <- data_demographics$fair_knowledge
data_plspm$profession_group <- data_demographics$profession_group

# data_plspm$effort_f <- data_raw$effort_f
# data_plspm$effort_a <- data_raw$effort_a
# data_plspm$effort_i <- data_raw$effort_i
# data_plspm$effort_r <- data_raw$effort_r


data_plspm <- as.data.frame(subset(data_plspm, data_raw$`Survey Progress` == 100))

# Only include researchers
#data_plspm <- as.data.frame(subset(data_plspm, data_plspm$profession_group != "Other"))

data_plspm <- as.data.frame(subset(data_plspm, data_plspm$profession_group == "Researcher"))

data_plspm$experiencedusefulness_1[is.na(data_plspm$experiencedusefulness_1)] <- 0
data_plspm$experiencedusefulness_2[is.na(data_plspm$experiencedusefulness_2)] <- 0
data_plspm$experiencedusefulness_3[is.na(data_plspm$experiencedusefulness_3)] <- 0
data_plspm$experiencedusefulness_4[is.na(data_plspm$experiencedusefulness_4)] <- 0
data_plspm$experiencedusefulness_5[is.na(data_plspm$experiencedusefulness_5)] <- 0
data_plspm$experiencedusefulness_6[is.na(data_plspm$experiencedusefulness_6)] <- 0

# Path matrix
perceptions_model <- plspm(data_plspm, perceptions_path_matrix, measurement_model)

summary(perceptions_model)

data_plspm$profession_group <- factor(data_plspm$profession_group)
#plspm.groups(perceptions_model, data_plspm$profession_group, reps=50)
plspm.groups(perceptions_model, data_plspm$profession_group, method="permutation")

# plspm.groups(perceptions_model, data_plspm$fair_knowledge)

perceptions_cor <- cor(data_plspm[,2:60])
all_observed <- colnames(data_plspm[,2:60])

data_plspm_raw <- data_plspm[,2:60]


#rm(generate_block, perceptions_path_matrix)

htmt <- data.frame(latent1=character(),
                   latent2=character(),
                   htmt=numeric()
)


for(latent1 in measurement_model) {
  #print (latent1)
  name1 <- strsplit(latent1[1], "_")[[1]][1]
  
  cor_monotrait1 <- cor(data_plspm_raw[, latent1], data_plspm_raw[, latent1])
  
  htmt <- rbind(htmt, c(
    name1, 
    name1,
    NA
  ))
  
  for(latent2 in measurement_model) {
    name2 <- strsplit(latent2[1], "_")[[1]][1]
    
    if(name1 != name2) {
      cor_monotrait2 <- cor(data_plspm_raw[, latent2], data_plspm_raw[, latent2])
      
      cor_heterotrait <- cor(data_plspm_raw[, latent1], data_plspm_raw[, latent2])
      
      
      htmt <- rbind(htmt, c(
        name1, 
        name2,
        mean(cor_heterotrait) / sqrt(mean(cor_monotrait1) * mean(cor_monotrait2))
      ))
    }
  }
}

colnames(htmt) <- c("latent1", "latent2", "htmt")

htmt$htmt <- formatC(as.numeric(htmt$htmt), digits = 3)
htmt <- htmt[order(htmt$latent1, htmt$latent2),]

htmt_final <- pivot_wider(htmt, names_from = latent2, values_from = htmt)
