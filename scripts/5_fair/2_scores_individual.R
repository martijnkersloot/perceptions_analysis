data_scores_individual <- data_raw[, c("Castor Record ID", unlist(measurement_model))]

data_scores_individual$fair_knowledge <- data_demographics$fair_knowledge
data_scores_individual$profession <- data_demographics$profession
data_scores_individual$age <- data_demographics$age
data_scores_individual$research_experience <- data_demographics$research_experience
data_scores_individual$profession_group <- data_demographics$profession_group
data_scores_individual$umc <- data_raw$hospital_department_1

data_scores_individual <- as.data.frame(subset(data_scores_individual, data_scores_individual$profession_group != "Other"))


#data_scores_individual <- data_scores
#data_scores_individual$experienced_usefulness[is.na(data_scores_individual$experienced_usefulness)] <- -1
#data_scores_individual$experienced_usefulness[is.na(data_scores_individual$experienced_usefulness)] <- 0
data_scores_individual$umc <- NULL

data_scores_individual <- as.data.frame(data_scores_individual[complete.cases(data_scores_individual), ])

#data_scores_individual$experienced_usefulness[data_scores_individual$experienced_usefulness == -1] <- NA

data_scores_individual_knowledge <- subset(data_scores_individual, data_scores_individual$fair_knowledge == "Knowledge of FAIR")
data_scores_individual_no_knowledge <- subset(data_scores_individual, data_scores_individual$fair_knowledge == "No knowledge of FAIR")


table_scores_individual <- data.frame(variable=character(),
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

for(variable in unlist(measurement_model)) {
  
  temp_scores_knowledge_var <- data_scores_individual_knowledge[, variable]
  temp_scores_no_knowledge_var <- data_scores_individual_no_knowledge[, variable]
  
  researcher_temp_scores_all_var <- subset(data_scores_individual, data_scores_individual$profession_group == "Researcher")[, variable]
  support_temp_scores_all_var <- subset(data_scores_individual, data_scores_individual$profession_group == "Support")[, variable]
  
  test <- wilcox.test(data_scores_individual[, variable] ~ data_scores_individual$fair_knowledge)
  
  table_scores_individual <- rbind(table_scores_individual, c(
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

colnames(table_scores_individual) <-
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

table_scores_individual$researchers_p <- p.adjust(table_scores_individual$researchers_p, method="bonferroni")

table_scores_individual$researchers_p_star <- symnum(
  as.numeric(table_scores_individual$researchers_p),
  corr = FALSE,
  na = FALSE,
  cutpoints = c(0, 0.001, 0.01, 0.05, 1),
  symbols = c("***", "**", "*", "")
)

table_scores_individual$researchers_p <- scales::pvalue(as.numeric(table_scores_individual$researchers_p))


table_scores_individual$researchers_knowledge_mean <- format(as.numeric(table_scores_individual$researchers_knowledge_mean), digits = 3)
table_scores_individual$researchers_knowledge_sd <- format(as.numeric(table_scores_individual$researchers_knowledge_sd), digits = 2)
table_scores_individual$researchers_no_knowledge_mean <- format(as.numeric(table_scores_individual$researchers_no_knowledge_mean), digits = 3)
table_scores_individual$researchers_no_knowledge_sd <- format(as.numeric(table_scores_individual$researchers_no_knowledge_sd), digits = 2)
table_scores_individual$researchers_all_mean <- format(as.numeric(table_scores_individual$researchers_all_mean), digits = 3)
table_scores_individual$researchers_all_sd <- format(as.numeric(table_scores_individual$researchers_all_sd), digits = 2)
table_scores_individual$support_all_mean <- format(as.numeric(table_scores_individual$support_all_mean), digits = 3)
table_scores_individual$support_all_sd <- format(as.numeric(table_scores_individual$support_all_sd), digits = 2)

rownames(table_scores) <- table_scores_individual$variable

table_scores <- merge(table_scores, perceptions_structural_model_names, by=0)

table_scores_individual <- table_scores_individual[, c(
  "variable",
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
  

table_scores_individual$researchers_p = cell_spec(
  table_scores_individual$researchers_p, 
  format = "html", 
  bold = table_scores_individual$researchers_p_star != ""
)

table_scores_individual <- table_scores_individual[order(table_scores_individual$variable), ]

table_scores_individual$variable <- str_replace(table_scores_individual$variable, "_", "")

print(
  kable(table_scores_individual,
        col.names = c("Observed variable", "Mean", "SD", "Mean", "SD", "Mean", "SD", "p", "", "Mean", "SD" ),
        row.names = FALSE,
        escape = FALSE,
        format="html", booktabs = T
  ) %>%
    kable_styling() %>%
    column_spec(3, border_right = T) %>%
    column_spec(5, border_right = T) %>%
    column_spec(7, border_right = T) %>%
    column_spec(9, border_right = T) %>%
    add_header_above(c(" ", "All" = 2, "Knowledge of FAIR" = 2, "No knowledge of FAIR" = 2, " " = 2, "All" = 2)) %>%
    add_header_above(c(" ", "Researchers" = 8, "Support staff" = 2))
)
