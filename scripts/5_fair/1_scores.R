data_scores <- data_raw[, c("Castor Record ID", unlist(measurement_model))]

data_scores$fair_knowledge <- data_demographics$fair_knowledge
data_scores$profession <- data_demographics$profession
data_scores$age <- data_demographics$age
data_scores$research_experience <- data_demographics$research_experience
data_scores$profession_group <- data_demographics$profession_group
data_scores$umc <- data_raw$hospital_department_1
#data_scores <- as.data.frame(subset(data_scores, data_raw$`Survey Progress` == 100))

# Only include researchers & support
# data_scores <- as.data.frame(subset(data_scores, data_scores$profession_group == "Researcher"))

data_scores <- as.data.frame(subset(data_scores, data_scores$profession_group != "Other"))

data_scores$awareness <- rowMeans(data_scores[, generate_block("awareness", 4)])
data_scores$compatibility <- data_scores[, generate_block("compatibility", 1)]
data_scores$experienced_usefulness <- rowMeans(data_scores[, generate_block("experiencedusefulness", 6)])
data_scores$external_influence <- rowMeans(data_scores[, generate_block("externalinfluence", 2)])
data_scores$facilitating_conditions <- rowMeans(data_scores[, generate_block("facilitatingconditions", 4)])
data_scores$interpersonal_influence <- rowMeans(data_scores[, generate_block("interpersonalinfluence", 4)])
data_scores$perceived_ease_of_use <- data_scores[, generate_block("perceivedeaseofuse", 1)]
data_scores$perceived_risk <- rowMeans(data_scores[, generate_block("perceivedrisk", 2)])
data_scores$perceived_usefulness <- rowMeans(data_scores[, generate_block("perceivedusefulness", 6)])
data_scores$self_efficacy <- rowMeans(data_scores[, generate_block("selfefficacy", 10)])
data_scores$situational_normality <- data_scores[, generate_block("situationalnormality", 1)]
data_scores$structural_assurance <- rowMeans(data_scores[, generate_block("structuralassurance", 4)])
data_scores$subjective_norm <- data_scores[, generate_block("subjectivenorm", 1)]
data_scores$perceived_behavioral_control <- data_scores[, generate_block("perceivedbehavioralcontrol", 1)]
data_scores$attitude <- rowMeans(data_scores[, generate_block("attitude", 4)])
data_scores$intention_to_act <- rowMeans(data_scores[, generate_block("intentiontoact", 4)])
data_scores$behavior <- rowMeans(data_scores[, generate_block("behavior", 4)])

data_scores <- subset(data_scores,select = !(names(data_scores) %in% unlist(measurement_model)))


# table_scores <- createTable(
#   compareGroups(
#     profession_group ~ awareness + compatibility + experienced_usefulness + external_influence + 
#       facilitating_conditions + interpersonal_influence + perceived_ease_of_use + perceived_risk +
#       perceived_usefulness + self_efficacy + situational_normality + structural_assurance + 
#       subjective_norm + perceived_behavioral_control + attitude + intention_to_act + 
#       behavior,
#     data_scores
#   ),
#   show.all = TRUE,
#   # show.p.overall = FALSE,
#   all.last = TRUE
# )

data_scores_all <- data_scores
data_scores_all$experienced_usefulness[is.na(data_scores_all$experienced_usefulness)] <- -1
#data_scores_all$experienced_usefulness[is.na(data_scores_all$experienced_usefulness)] <- 0
data_scores_all$umc <- NULL

data_scores_all <- as.data.frame(data_scores_all[complete.cases(data_scores_all), ])

data_scores_all$experienced_usefulness[data_scores_all$experienced_usefulness == -1] <- NA

data_scores_knowledge <- subset(data_scores_all, data_scores_all$fair_knowledge == "Knowledge of FAIR")
data_scores_no_knowledge <- subset(data_scores_all, data_scores_all$fair_knowledge == "No knowledge of FAIR")


table_scores <- data.frame(variable=character(),
                           researchers_all_mean=numeric(),
                           researchers_all_sd=numeric(),
                           researchers_knowledge_mean=numeric(),
                           researchers_knowledge_sd=numeric(),
                           researchers_no_knowledge_mean=numeric(),
                           researchers_no_knowledge_sd=numeric(),
                           researchers_p=numeric(),
                           support_all_mean=numeric(),
                           support_all_sd=numeric()
)

for(variable in rownames(perceptions_structural_model_names)) {
  
  temp_scores_knowledge_var <- data_scores_knowledge[, variable]
  temp_scores_no_knowledge_var <- data_scores_no_knowledge[, variable]
  
  researcher_temp_scores_all_var <- subset(data_scores_all, data_scores_all$profession_group == "Researcher")[, variable]
  support_temp_scores_all_var <- subset(data_scores_all, data_scores_all$profession_group == "Support")[, variable]
  
  test <- wilcox.test(data_scores_all[, variable] ~ data_scores_all$fair_knowledge)
  
  table_scores <- rbind(table_scores, c(
    variable,
    mean(researcher_temp_scores_all_var,na.rm=TRUE),
    sd(researcher_temp_scores_all_var,na.rm=TRUE),
    mean(temp_scores_knowledge_var,na.rm=TRUE),
    sd(temp_scores_knowledge_var,na.rm=TRUE),
    mean(temp_scores_no_knowledge_var,na.rm=TRUE),
    sd(temp_scores_no_knowledge_var,na.rm=TRUE),
    test$p.value,
    mean(support_temp_scores_all_var,na.rm=TRUE),
    sd(support_temp_scores_all_var,na.rm=TRUE)
  ))
}

colnames(table_scores) <-
  c(
    "variable",
    "researchers_all_mean",
    "researchers_all_sd",
    "researchers_knowledge_mean",
    "researchers_knowledge_sd",
    "researchers_no_knowledge_mean",
    "researchers_no_knowledge_sd",
    "researchers_p",
    "support_all_mean",
    "support_all_sd"
  )

table_scores$researchers_p <- p.adjust(table_scores$researchers_p, method="bonferroni")

table_scores$researchers_p_star <- symnum(
  as.numeric(table_scores$researchers_p),
  corr = FALSE,
  na = FALSE,
  cutpoints = c(0, 0.001, 0.01, 0.05, 1),
  symbols = c("***", "**", "*", "")
)

table_scores$researchers_p <- scales::pvalue(as.numeric(table_scores$researchers_p))


table_scores$researchers_knowledge_mean <- format(as.numeric(table_scores$researchers_knowledge_mean), digits = 3)
table_scores$researchers_knowledge_sd <- format(as.numeric(table_scores$researchers_knowledge_sd), digits = 2)
table_scores$researchers_no_knowledge_mean <- format(as.numeric(table_scores$researchers_no_knowledge_mean), digits = 3)
table_scores$researchers_no_knowledge_sd <- format(as.numeric(table_scores$researchers_no_knowledge_sd), digits = 2)
table_scores$researchers_all_mean <- format(as.numeric(table_scores$researchers_all_mean), digits = 3)
table_scores$researchers_all_sd <- format(as.numeric(table_scores$researchers_all_sd), digits = 2)
table_scores$support_all_mean <- format(as.numeric(table_scores$support_all_mean), digits = 3)
table_scores$support_all_sd <- format(as.numeric(table_scores$support_all_sd), digits = 2)

rownames(table_scores) <- table_scores$variable


table_scores <- merge(table_scores, perceptions_structural_model_names, by=0)
table_scores_ave <- merge(table_scores, perceptions_fit_ave, by="variable")

table_scores <- table_scores[, c(
  "full",
  "researchers_all_mean",
  "researchers_all_sd",
  "researchers_knowledge_mean",
  "researchers_knowledge_sd",
  "researchers_no_knowledge_mean",
  "researchers_no_knowledge_sd",
  "researchers_p",
  "researchers_p_star",
  "support_all_mean",
  "support_all_sd"
)]
  
table_scores_ave <- table_scores_ave[, c(
  "full",
  "ave"
)]

table_scores$researchers_p = cell_spec(
  table_scores$researchers_p, 
  format = "latex", 
  bold = table_scores$researchers_p_star != ""
)

table_scores <- table_scores[order(table_scores$full), ]
table_scores_ave <- table_scores_ave[order(table_scores_ave$full), ]

print(
  kable(table_scores,
        col.names = c("Variable", "Mean", "SD", "Mean", "SD", "Mean", "SD", "p", "", "Mean", "SD" ),
        row.names = FALSE,
        escape = FALSE,
        format="latex", booktabs = T
  ) %>%
    kable_styling() %>%
    column_spec(3, border_right = T) %>%
    column_spec(5, border_right = T) %>%
    column_spec(7, border_right = T) %>%
    column_spec(9, border_right = T) %>%
    add_header_above(c(" ", "All" = 2, "Knowledge of FAIR" = 2, "No knowledge of FAIR" = 2, " " = 2, "All" = 2)) %>%
    add_header_above(c(" ", "Researchers" = 8, "Support staff" = 2))
)

print(
  kable(table_scores_ave,
        col.names = c("Variable", "AVE" ),
        row.names = FALSE,
        escape = FALSE,
        format="html", booktabs = T
  ) %>%
    kable_styling() 
)
