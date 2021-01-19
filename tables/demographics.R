# Table 1: Demographics

table_demographics <- createTable(
  compareGroups(
    fair_knowledge ~ age + primary_institution + profession + research_experience,
    data_demographics
  ),
  show.all = TRUE,
  show.p.overall = FALSE,
  all.last = TRUE
)

export2md(table_demographics, header.labels = c(all = "All"))

rm(table_demographics)