library(seminr)

measurements <- constructs(
  composite("compatibility", single_item("compatibility_1")),
  composite("awareness", multi_items("awareness_", 1:4)),
  composite("perceived_usefulness", multi_items("perceivedusefulness_", 1:6)),
  composite("experienced_usefulness", multi_items("experiencedusefulness_", 1:6)),
  composite("perceived_ease_of_use", single_item("perceivedeaseofuse_1")),
  composite("attitude", multi_items("attitude_", 1:4)),
  composite("interpersonal_influence", multi_items("interpersonalinfluence_", 1:4)),
  composite("external_influence", multi_items("externalinfluence_", 1:2)),
  composite("subjective_norm", single_item("subjectivenorm_1")),
  composite("self_efficacy", multi_items("selfefficacy_", 1:10)),
  composite("facilitating_conditions", multi_items("facilitatingconditions_", 1:4)),
  composite("perceived_behavioral_control", single_item("perceivedbehavioralcontrol_1")),
  composite("situational_normality", single_item("situationalnormality_1")),
  composite("structural_assurance", multi_items("structuralassurance_", 1:4)),
  composite("perceived_risk", multi_items("perceivedrisk_", 1:2)),
  composite("intention_to_act", multi_items("intentiontoact_", 1:4)),
  composite("behavior", multi_items("behavior_", 1:4))
)

structure <- relationships(
  paths(from = c("compatibility", "perceived_usefulness"), to = "attitude"),
  paths(from = c("awareness", "experienced_usefulness"), to = "perceived_usefulness"),
  paths(from = "perceived_ease_of_use", to = c("perceived_usefulness", "attitude")),
  paths(from = "attitude", to = "intention_to_act"),
  paths(from = "interpersonal_influence", to = "subjective_norm"),
  paths(from = "external_influence", to = "subjective_norm"),
  paths(from = "subjective_norm", to = "intention_to_act"),
  paths(from = c("self_efficacy", "facilitating_conditions"), to = "perceived_behavioral_control"),
  paths(from = "perceived_behavioral_control", to = "intention_to_act"),
  paths(from = "situational_normality", to = "intention_to_act"),
  paths(from = "structural_assurance", to = "intention_to_act"),
  paths(from = "perceived_risk", to = "intention_to_act"),
  paths(from = "intention_to_act", to = "behavior")
)


data_completed <- as.data.frame(subset(data_raw, data_raw$`Survey Progress` == 100))


pls_model <- estimate_pls(data = data_completed, 
                          measurement_model = measurements, 
                          structural_model = structure)
