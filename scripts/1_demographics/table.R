# Table 1: Demographics

data_demographics_knowledge <- subset(data_demographics, data_demographics$fair_knowledge == "Knowledge of FAIR")
data_demographics_no_knowledge <- subset(data_demographics, data_demographics$fair_knowledge == "No knowledge of FAIR")

table_demographics <- data.frame(variable=character(),
                                 group=character(),
                                 sub_group=character(),
                                 all_n=numeric(),
                                 all_p=numeric(),
                                 knowledge_n=numeric(),
                                 knowledge_p=numeric(),
                                 no_knowledge_n=numeric(),
                                 no_knowledge_p=numeric()
                                 )

for(group in levels(data_demographics$profession_group)) {
  variable <- "profession_group"
  option <- group
  
  knowledge_n <- nrow(subset(data_demographics_knowledge, data_demographics_knowledge[, variable] == option))
  no_knowledge_n <- nrow(subset(data_demographics_no_knowledge, data_demographics_no_knowledge[, variable] == option))
  all_n <- nrow(subset(data_demographics, data_demographics[, variable] == option))
  
  variable <- "profession"
  
  table_demographics <- rbind(table_demographics, c(
    variable,
    group,
    NA,
    all_n,
    all_n / nrow(data_demographics),
    knowledge_n,
    knowledge_n / nrow(data_demographics),
    no_knowledge_n,
    no_knowledge_n / nrow(data_demographics)
  ))
  
  if(group != "Other") {
    for(option in unique(as.character(subset(data_demographics, data_demographics$profession_group == group)$profession))) {
      knowledge_n <-
        nrow(subset(
          data_demographics_knowledge,
          (data_demographics_knowledge[, variable] == option & data_demographics_knowledge$profession_group == group)
        ))
      no_knowledge_n <-
        nrow(subset(
          data_demographics_no_knowledge,
          (data_demographics_no_knowledge[, variable] == option & data_demographics_no_knowledge$profession_group == group)
        ))
      all_n <-
        nrow(subset(
          data_demographics, 
          (data_demographics[, variable] == option & data_demographics$profession_group == group)
      ))
      
      
      table_demographics <- rbind(table_demographics, c(
        variable,
        group,
        option,
        all_n,
        all_n / nrow(data_demographics),
        knowledge_n,
        knowledge_n / nrow(data_demographics),
        no_knowledge_n,
        no_knowledge_n / nrow(data_demographics)
      ))
    }
  }
}



for(variable in c("age", "primary_institution", "research_experience")) {
  for(option in levels(data_demographics[[variable]])) {
    if(!is.na(option)) {
      knowledge_n <- nrow(subset(data_demographics_knowledge, data_demographics_knowledge[, variable] == option))
      no_knowledge_n <- nrow(subset(data_demographics_no_knowledge, data_demographics_no_knowledge[, variable] == option))
      all_n <- nrow(subset(data_demographics, data_demographics[, variable] == option))

      table_demographics <- rbind(table_demographics, c(
        variable,
        option,
        NA,
        all_n,
        all_n / nrow(data_demographics),
        knowledge_n,
        knowledge_n / nrow(data_demographics),
        no_knowledge_n,
        no_knowledge_n / nrow(data_demographics)
      ))
    }
  }
}

table_demographics <- rbind(c(
  NA,
  "Total respondents",
  NA,
  nrow(data_demographics),
  1,
  nrow(data_demographics_knowledge),
  nrow(data_demographics_knowledge) / nrow(data_demographics),
  nrow(data_demographics_no_knowledge),
  nrow(data_demographics_no_knowledge) / nrow(data_demographics)
), table_demographics)

colnames(table_demographics) <- c(
  "variable",
  "group",
  "sub_group",
  "all_n",
  "all_p",
  "knowledge_n",
  "knowledge_p",
  "no_knowledge_n",
  "no_knowledge_p"
)

table_demographics$all_p <- format(as.numeric(table_demographics$all_p) * 100, digits = 1, trim=TRUE)
table_demographics$knowledge_p <- format(as.numeric(table_demographics$knowledge_p) * 100, digits = 1, trim=TRUE)
table_demographics$no_knowledge_p <- format(as.numeric(table_demographics$no_knowledge_p) * 100, digits = 1, trim=TRUE)

table_demographics$all_p <- paste("(", table_demographics$all_p, ")", sep="")
table_demographics$knowledge_p <- paste("(", table_demographics$knowledge_p, ")", sep="")
table_demographics$no_knowledge_p <- paste("(", table_demographics$no_knowledge_p, ")", sep="")

table_demographics <- subset(table_demographics, table_demographics$all_n > 0)

indentation_1 <- which(is.na(table_demographics$sub_group))
indentation_2 <- which(!is.na(table_demographics$sub_group))

table_demographics$sub_group[is.na(table_demographics$sub_group)] <- table_demographics[is.na(table_demographics$sub_group),]$group

print(
  kable(
    table_demographics[, 3:ncol(table_demographics)],
    # HTML: col.names = c(" ", "n", "(%)", "n", "(%)", "n", "(%)" ),
    col.names = c(" ", "n", "(\\%)", "n", "(\\%)", "n", "(\\%)"),
    row.names = FALSE,
    escape = FALSE,
    booktabs = T
  ) %>%
    kable_styling() %>%
    pack_rows(
      index = c(
        " " = 1,
        "Profession" = length(table_demographics$variable[table_demographics$variable == "profession"]) - 1,
        "Age" = length(table_demographics$variable[table_demographics$variable == "age"]) - 1,
        "Primary Institution" = length(table_demographics$variable[table_demographics$variable == "primary_institution"])  - 1,
        "Research experience" = length(table_demographics$variable[table_demographics$variable == "research_experience"]) - 1
      ),
      indent = FALSE,
      bold = FALSE,
      label_row_css = "",
      latex_gap_space = "0.3em"
    )  %>%
    add_indent(indentation_1, 1)  %>%
    add_indent(indentation_2, 2) %>%
    add_header_above(c(
      " ",
      "All" = 2,
      "Knowledge of FAIR" = 2,
      "No knowledge of FAIR" = 2
    ))
  # add_header_above(c(" ", "All researchers" = 3, "Knowledge of FAIR" = 2, "No knowledge of FAIR" = 2, " "))
)