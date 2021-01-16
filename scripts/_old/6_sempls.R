library(semPLS)
library(DiagrammeR)
library(dplyr)

structural_model <- rbind(
  c("compatibility", "attitude"), 
  c("awareness", "perceived_usefulness"), 
  c("experienced_usefulness", "perceived_usefulness"), 
  c("perceived_usefulness", "attitude"), 
  c("perceived_ease_of_use", "perceived_usefulness"), 
  c("perceived_ease_of_use", "attitude"), 
  c("attitude", "intention_to_act"), 
  c("interpersonal_influence", "subjective_norm"), 
  c("external_influence", "subjective_norm"), 
  c("subjective_norm", "intention_to_act"), 
  c("self_efficacy", "perceived_behavioural_control"), 
  c("facilitating_conditions", "perceived_behavioural_control"), 
  c("perceived_behavioural_control", "intention_to_act"), 
  c("situational_normality", "intention_to_act"), 
  c("structural_assurance", "intention_to_act"), 
  c("perceived_risk", "intention_to_act"), 
  c("intention_to_act", "behavior")
)

colnames(structural_model) <- c("source", "target")

measurement_model <- rbind(
  c("compatibility", "compatibility_1"), 
  c("awareness", "awareness_1"), 
  c("awareness", "awareness_2"), 
  c("awareness", "awareness_3"), 
  c("awareness", "awareness_4"), 
  c("perceived_usefulness", "perceivedusefulness_1"), 
  c("perceived_usefulness", "perceivedusefulness_2"), 
  c("perceived_usefulness", "perceivedusefulness_3"), 
  c("perceived_usefulness", "perceivedusefulness_4"), 
  c("perceived_usefulness", "perceivedusefulness_5"), 
  c("perceived_usefulness", "perceivedusefulness_6"), 
  c("experienced_usefulness", "experiencedusefulness_1"), 
  c("experienced_usefulness", "experiencedusefulness_2"), 
  c("experienced_usefulness", "experiencedusefulness_3"), 
  c("experienced_usefulness", "experiencedusefulness_4"), 
  c("experienced_usefulness", "experiencedusefulness_5"), 
  c("experienced_usefulness", "experiencedusefulness_6"), 
  c("perceived_ease_of_use", "perceivedeaseofuse_1"), 
  c("attitude", "attitude_1"), 
  c("attitude", "attitude_2"), 
  c("attitude", "attitude_3"), 
  c("attitude", "attitude_4"), 
  c("interpersonal_influence", "interpersonalinfluence_1"), 
  c("interpersonal_influence", "interpersonalinfluence_2"), 
  c("interpersonal_influence", "interpersonalinfluence_3"), 
  c("interpersonal_influence", "interpersonalinfluence_4"), 
  c("external_influence", "externalinfluence_1"), 
  c("external_influence", "externalinfluence_2"), 
  c("subjective_norm", "subjectivenorm_1"), 
  c("self_efficacy", "selfefficacy_1"), 
  c("self_efficacy", "selfefficacy_2"), 
  c("self_efficacy", "selfefficacy_3"), 
  c("self_efficacy", "selfefficacy_4"), 
  c("self_efficacy", "selfefficacy_5"), 
  c("self_efficacy", "selfefficacy_6"), 
  c("self_efficacy", "selfefficacy_7"), 
  c("self_efficacy", "selfefficacy_8"), 
  c("self_efficacy", "selfefficacy_9"), 
  c("self_efficacy", "selfefficacy_10"), 
  c("facilitating_conditions", "facilitatingconditions_1"), 
  c("facilitating_conditions", "facilitatingconditions_2"), 
  c("facilitating_conditions", "facilitatingconditions_3"), 
  c("facilitating_conditions", "facilitatingconditions_4"), 
  c("perceived_behavioural_control", "perceivedbehavioralcontrol_1"), 
  c("situational_normality", "situationalnormality_1"), 
  c("structural_assurance", "structuralassurance_1"), 
  c("structural_assurance", "structuralassurance_2"), 
  c("structural_assurance", "structuralassurance_3"), 
  c("structural_assurance", "structuralassurance_4"), 
  c("perceived_risk", "perceivedrisk_1"), 
  c("perceived_risk", "perceivedrisk_2"), 
  c("intention_to_act", "intentiontoact_1"), 
  c("intention_to_act", "intentiontoact_2"), 
  c("intention_to_act", "intentiontoact_3"), 
  c("intention_to_act", "intentiontoact_4"), 
  c("behavior", "behavior_1"), 
  c("behavior", "behavior_2"), 
  c("behavior", "behavior_3"), 
  c("behavior", "behavior_4")
)

colnames(measurement_model) <- c("source", "target")

structural_model_names <- as.data.frame(rbind(
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
  c("perceived_behavioural_control", "Perceived behavioural control"), 
  c("situational_normality", "Situational normality"), 
  c("structural_assurance", "Structural assurance"), 
  c("institutional_trust", "Institutional trust"), 
  c("perceived_risk", "Perceived risk"), 
  c("intention_to_act", "Intention to act"), 
  c("behavior", "Behavior")
))

colnames(structural_model_names) <- c("short", "full")
row.names(structural_model_names) <- structural_model_names$short
structural_model_names[1] <- NULL

data_completed <- as.data.frame(subset(data_raw, data_raw$`Survey Progress` == 100))

perceptions_model <- plsm(data = data_completed, strucmod = structural_model, measuremod = measurement_model)


# Fix experienced thing
perceptions_fit <- sempls(model = perceptions_model, data = data_completed, wscheme = "centroid")


# indicator reliability | Outer loadings | > 0.708
perceptions_fit$outer_loadings

# convergent validity | Average Variance Extracted (AVE) | > 0.5
communality(perceptions_fit)


# internal consistency | rho | > 0.60
dgrho(perceptions_fit)

plsWeights(perceptions_fit)
plsLoadings(perceptions_fit)

#mvpairs(model = perceptions_model, data = data_completed, LVs = "behavior")

pathDiagram(perceptions_fit, 
            file = "exports/model-full", 
            full = TRUE, 
            edge.labels = "both", 
            output.type = "graphics", 
            digits = 2, 
            graphics.fmt = "pdf"
)

pathDiagram(perceptions_fit, 
            file = "exports/model", 
            full = FALSE, 
            edge.labels = "both", 
            output.type = "graphics", 
            digits = 2, 
            graphics.fmt = "pdf"
)



# Bootstrapping
set.seed(123)
perceptions_bootstrapped <- bootsempls(perceptions_fit, nboot = 500, start = "ones", verbose = FALSE)
perceptions_bootstrapped

#adjusted bootstrap percentile (BCa)
perceptions_bootstrapped_summary <- summary(perceptions_bootstrapped, type = "bca", level = 0.95)
perceptions_bootstrapped_summary


perceptions_bootstrapped_outcomes <- perceptions_bootstrapped_summary$table
perceptions_bootstrapped_outcomes$path <- attr(perceptions_bootstrapped$t, "path")
perceptions_bootstrapped_outcomes$significant <- perceptions_bootstrapped_outcomes$Lower * perceptions_bootstrapped_outcomes$Upper >= 0

# R Squared
rSquared(perceptions_fit)

# 
redundancy(perceptions_fit)
# 
# 
gof(perceptions_fit)
# 
# 
