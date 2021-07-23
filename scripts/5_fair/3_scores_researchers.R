data_scores_researchers <- data_raw[, c("Castor Record ID", unlist(measurement_model))]

data_scores_researchers$fair_knowledge <- data_demographics$fair_knowledge
data_scores_researchers$profession <- data_demographics$profession
data_scores_researchers$age <- data_demographics$age
data_scores_researchers$research_experience <- data_demographics$research_experience
data_scores_researchers$profession_group <- data_demographics$profession_group

data_scores_researchers <- as.data.frame(subset(data_scores_researchers, data_scores_researchers$profession_group == "Researcher"))
# data_scores_researchers <- as.data.frame(subset(data_scores_researchers, data_scores_researchers$profession_group == "Support"))
data_scores_researchers <- as.data.frame(data_scores_researchers[complete.cases(data_scores_researchers), ])

#data_scores_researchers$experienced_usefulness[data_scores_researchers$experienced_usefulness == -1] <- NA

# data_scores_researchers <- subset(data_scores_researchers, data_scores_researchers$fair_knowledge == "Knowledge of FAIR")
#data_scores_researchers <- subset(data_scores_researchers, data_scores_researchers$fair_knowledge == "No knowledge of FAIR")


table_scores_researchers <- data.frame(variable=character(),
                                       # strongly_disagree_n=numeric(),
                                       strongly_disagree_p=numeric(),
                                       # disagree_n=numeric(),
                                       disagree_p=numeric(),
                                       # neutral_n=numeric(),
                                       neutral_p=numeric(),
                                       # agree_n=numeric(),
                                       agree_p=numeric(),
                                       # strongly_agree_n=numeric(),
                                       strongly_agree_p=numeric()
)

for(variable in unlist(measurement_model)) {
  
  temp_scores_var <- data_scores_researchers[, variable]

  table_scores_researchers <- rbind(table_scores_researchers, c(
    variable,
    # length(which(temp_scores_var == 1)),
    length(which(temp_scores_var == 1)) / nrow(data_scores_researchers),
    # length(which(temp_scores_var == 2)),
    length(which(temp_scores_var == 2)) / nrow(data_scores_researchers),
    # length(which(temp_scores_var == 3)),
    length(which(temp_scores_var == 3)) / nrow(data_scores_researchers),
    # length(which(temp_scores_var == 4)),
    length(which(temp_scores_var == 4)) / nrow(data_scores_researchers),
    # length(which(temp_scores_var == 5)),
    length(which(temp_scores_var == 5)) / nrow(data_scores_researchers)
  ))
}

colnames(table_scores_researchers) <-
  c(
    "variable",
    # "strongly_disagree_n",
    "strongly_disagree_p",
    # "disagree_n",
    "disagree_p",
    # "neutral_n",
    "neutral_p",
    # "agree_n",
    "agree_p",
    # "strongly_agree_n",
    "strongly_agree_p"
  )

table_scores_researchers$strongly_disagree_p <- formatC(as.numeric(table_scores_researchers$strongly_disagree_p) * 100, digits = 3)
table_scores_researchers$disagree_p <- formatC(as.numeric(table_scores_researchers$disagree_p) * 100, digits = 3)
table_scores_researchers$neutral_p <- formatC(as.numeric(table_scores_researchers$neutral_p) * 100, digits = 3)
table_scores_researchers$agree_p <- formatC(as.numeric(table_scores_researchers$agree_p) * 100, digits = 3)
table_scores_researchers$strongly_agree_p <- formatC(as.numeric(table_scores_researchers$strongly_agree_p) * 100, digits = 3)


table_scores_researchers <- table_scores_researchers[order(table_scores_researchers$variable), ]

table_scores_researchers$variable <- str_replace(table_scores_researchers$variable, "_", "")

print(
  kable(table_scores_researchers,
        col.names = c("Observed variable", "%", "%", "%", "%", "%" ),
        row.names = FALSE,
        escape = FALSE,
        format="html", booktabs = T
  ) %>%
    kable_styling() %>%
    # column_spec(3, border_right = T) %>%
    # column_spec(5, border_right = T) %>%
    # column_spec(7, border_right = T) %>%
    # column_spec(9, border_right = T) %>%
    add_header_above(c(" ", "Strongly disagree" = 1, "Disagree" = 1, "Neutral" = 1, "Agree" = 1, "Strongly agree" = 1))
)
