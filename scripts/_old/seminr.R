# 1) a data set, 2) an inner model, and 3) an outer model.

library(seminr)

measurement_model <- constructs(
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

structural_model <- relationships(
  paths(from = "compatibility", to = c("attitude")),
  paths(from = "awareness", to = c("perceived_usefulness")),
  paths(from = "experienced_usefulness", to = c("perceived_usefulness")),
  paths(from = "perceived_usefulness", to = c("attitude")),
  paths(from = "perceived_ease_of_use", to = c("perceived_usefulness", "attitude")),
  paths(from = "attitude", to = c("intention_to_act")),
  paths(from = "interpersonal_influence", to = c("subjective_norm")),
  paths(from = "external_influence", to = c("subjective_norm")),
  paths(from = "subjective_norm", to = c("intention_to_act")),
  paths(from = "self_efficacy", to = c("perceived_behavioural_control")),
  paths(from = "facilitating_conditions", to = c("perceived_behavioural_control")),
  paths(from = "perceived_behavioural_control", to = c("intention_to_act")),
  paths(from = "situational_normality", to = c("intention_to_act")),
  paths(from = "structural_assurance", to = c("intention_to_act")),
  # paths(from = "situational_normality", to = c("institutional_trust")),
  # paths(from = "structural_assurance", to = c("institutional_trust")),
  # paths(from = "institutional_trust", to = c("intention_to_act", "perceived_risk")),
  paths(from = "perceived_risk", to = c("intention_to_act")),
  paths(from = "intention_to_act", to = c("behavior"))
)


completed_survey_data <- as.data.frame(subset(survey_data, survey_data$`Survey Progress` == 100))

mobi_pls <- estimate_pls(data = completed_survey_data,
                         measurement_model = measurement_model,
                         structural_model = structural_model)
summary(mobi_pls)
plot_scores(mobi_pls)



# inner_model_labels <- c("compatibility",
#             "awareness",
#             "experienced_usefulness",
#             "perceived_usefulness",
#             "perceived_ease_of_use",
#             "attitude",
#             "interpersonal_influence",
#             "external_influence",
#             "subjective_norm",
#             "self_efficacy",
#             "facilitating_conditions",
#             "perceived_behavioural_control",
#             "situational_normality",
#             "structural_assurance",
#             "institutional_trust",
#             "perceived_risk",
#             "intention_to_act",
#             "behaviour")
# 
# colnames(inner_model) <- inner_model_labels
# rownames(inner_model) <- inner_model_labels
# 
# innerplot(inner_model)


