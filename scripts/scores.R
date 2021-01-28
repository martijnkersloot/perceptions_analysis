#data_scores <- data_scores[, c("Castor Record ID", "fair_knowledge", "profession_group")]


data_scores <- data_raw[, c("Castor Record ID", unlist(measurement_model))]

data_scores$fair_knowledge <- data_demographics$fair_knowledge
data_scores$profession <- data_demographics$profession
data_scores$age <- data_demographics$age
data_scores$research_experience <- data_demographics$research_experience
data_scores$profession_group <- data_demographics$profession_group
data_scores$umc <- data_raw$hospital_department_1
#data_scores <- as.data.frame(subset(data_scores, data_raw$`Survey Progress` == 100))

# Only include researchers & support
data_scores <- as.data.frame(subset(data_scores, data_scores$profession_group == "Researcher"))

#data_scores <- as.data.frame(subset(data_scores, data_scores$profession_group != "Other"))

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


table_scores <- createTable(
  compareGroups(
    profession_group ~ awareness + compatibility + experienced_usefulness + external_influence + 
      facilitating_conditions + interpersonal_influence + perceived_ease_of_use + perceived_risk +
      perceived_usefulness + self_efficacy + situational_normality + structural_assurance + 
      subjective_norm + perceived_behavioral_control + attitude + intention_to_act + 
      behavior,
    data_scores
  ),
  show.all = TRUE,
  # show.p.overall = FALSE,
  all.last = TRUE
)


table_scores <- createTable(
  compareGroups(
    fair_knowledge ~ awareness + compatibility + experienced_usefulness + external_influence + 
      facilitating_conditions + interpersonal_influence + perceived_ease_of_use + perceived_risk +
      perceived_usefulness + self_efficacy + situational_normality + structural_assurance + 
      subjective_norm + perceived_behavioral_control + attitude + intention_to_act + 
      behavior,
    data_scores
  ),
  show.all = TRUE,
  # show.p.overall = FALSE,
  all.last = TRUE
)

export2md(table_scores, header.labels = c(all = "All"))

data_scores_with_umcs <- subset(data_scores, !is.na(data_scores$umc))
data_scores_with_umcs <- subset(data_scores_with_umcs, data_scores_with_umcs$profession_group != "Other")

data_scores_with_umcs_researchers <- subset(data_scores_with_umcs, data_scores_with_umcs$profession_group == "Researcher")

data_scores_comparison <- data.frame(variable=character(),
                                         p=numeric()) 


for(variable in rownames(perceptions_structural_model_names)) {
#  test <- kruskal.test(data_scores_with_umcs[, variable] ~ data_scores_with_umcs$umc)
 # test <- kruskal.test(data_scores_with_umcs_researchers[, variable] ~ data_scores_with_umcs_researchers$umc)
  test <- wilcox.test(data_scores_with_umcs[, variable] ~ data_scores_with_umcs$fair_knowledge)
  print(test)
  
  
  data_scores_comparison <- rbind(data_scores_comparison, c(variable, test$p.value))
}

colnames(data_scores_comparison) <- c("variable", "p")


data_scores_comparison$p <- as.numeric(data_scores_comparison$p)

data_scores_comparison$p_star <- symnum(
  data_scores_comparison$p,
  corr = FALSE,
  na = FALSE,
  cutpoints = c(0, 0.001, 0.01, 0.05, 1),
  symbols = c("***", "**", "*", " ")
)

data_scores_comparison$p <- scales::pvalue(as.numeric(data_scores_comparison$p))


# pairwise.wilcox.test(data_scores_with_umcs_researchers$structural_assurance, data_scores_with_umcs_researchers$umc,
#                      p.adjust.method = "BH", exact = FALSE)
# 
# pairwise.wilcox.test(data_scores_with_umcs$external_influence, data_scores_with_umcs$research_experience,
#                      p.adjust.method = "BH", exact = FALSE)







data_scores_all <- data_scores
data_scores_all$experienced_usefulness[is.na(data_scores_all$experienced_usefulness)] <- -1
#data_scores_all$experienced_usefulness[is.na(data_scores_all$experienced_usefulness)] <- 0
data_scores_all$umc <- NULL

data_scores_all <- as.data.frame(data_scores_all[complete.cases(data_scores_all), ])

data_scores_all$experienced_usefulness[data_scores_all$experienced_usefulness == -1] <- NA

data_scores_knowledge <- subset(data_scores_all, data_scores_all$fair_knowledge == "Knowledge of FAIR")
data_scores_no_knowledge <- subset(data_scores_all, data_scores_all$fair_knowledge == "No knowledge of FAIR")


table_scores <- data.frame(variable=character(),
                           all_mean=numeric(),
                           all_sd=numeric(),
                           knowledge_mean=numeric(),
                           knowledge_sd=numeric(),
                           no_knowledge_mean=numeric(),
                           no_knowledge_sd=numeric(),
                           p=numeric()
)

for(variable in rownames(perceptions_structural_model_names)) {
  
  temp_scores_knowledge_var <- data_scores_knowledge[, variable]
  temp_scores_no_knowledge_var <- data_scores_no_knowledge[, variable]
  temp_scores_all_var <- data_scores_all[, variable]
  
  test <- wilcox.test(data_scores_all[, variable] ~ data_scores_all$fair_knowledge)
  
  table_scores <- rbind(table_scores, c(
    variable,
    mean(temp_scores_all_var,na.rm=TRUE),
    sd(temp_scores_all_var,na.rm=TRUE),
    mean(temp_scores_knowledge_var,na.rm=TRUE),
    sd(temp_scores_knowledge_var,na.rm=TRUE),
    mean(temp_scores_no_knowledge_var,na.rm=TRUE),
    sd(temp_scores_no_knowledge_var,na.rm=TRUE),
    test$p.value
  ))
  
  #data_demographics_knowledge[, variable]
  
  #print(variable)
}

colnames(table_scores) <-
  c(
    "variable",
    "all_mean",
    "all_sd",
    "knowledge_mean",
    "knowledge_sd",
    "no_knowledge_mean",
    "no_knowledge_sd",
    "p"
  )



table_scores$p_star <- symnum(
  as.numeric(table_scores$p),
  corr = FALSE,
  na = FALSE,
  cutpoints = c(0, 0.001, 0.01, 0.05, 1),
  symbols = c("***", "**", "*", " ")
)

table_scores$p <- scales::pvalue(as.numeric(table_scores$p))


table_scores$knowledge_mean <- format(as.numeric(table_scores$knowledge_mean), digits = 3)
table_scores$knowledge_sd <- format(as.numeric(table_scores$knowledge_sd), digits = 2)
table_scores$no_knowledge_mean <- format(as.numeric(table_scores$no_knowledge_mean), digits = 3)
table_scores$no_knowledge_sd <- format(as.numeric(table_scores$no_knowledge_sd), digits = 2)
table_scores$all_mean <- format(as.numeric(table_scores$all_mean), digits = 3)
table_scores$all_sd <- format(as.numeric(table_scores$all_sd), digits = 2)

table_scores_ave <- merge(table_scores, perceptions_fit_ave, by="variable")
rownames(table_scores_ave) <- table_scores_ave$variable


table_scores_ave <- merge(table_scores_ave, perceptions_structural_model_names, by=0)

table_scores_ave <- table_scores_ave[, c(
  "full",
  "all_mean",
  "all_sd",
  "ave",
  "knowledge_mean",
  "knowledge_sd",
  "no_knowledge_mean",
  "no_knowledge_sd",
  "p"
)]

options(knitr.table.format = "latex")
#options(knitr.table.format = "html")

#table_scores_ave$p = cell_spec(table_scores_ave$p,format = "latex", bold = table_scores_ave$p  <= 0.05)
table_scores_ave$p = cell_spec(table_scores_ave$p, format = "html", bold = table_scores_ave$p  <= 0.05)

kable(table_scores_ave,
      col.names = c("Variable", "Mean", "SD", "AVE", "Mean", "SD", "Mean", "SD", "p" ),
      row.names = FALSE,
      escape = FALSE
) %>%
  kable_styling() %>%
  column_spec(4, border_right = T) %>%
  column_spec(6, border_right = T) %>%
  column_spec(8, border_right = T) %>%
  add_header_above(c(" ", "All researchers" = 3, "Knowledge of FAIR" = 2, "No knowledge of FAIR" = 2, " "))
