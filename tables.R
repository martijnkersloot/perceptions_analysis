
# Table 1: Demographics
res <- compareGroups(
  fair_knowledge ~ age + primary_institution + profession + research_experience,
  data_demographics
)

restab <- createTable(
  res,
  show.all = TRUE,
  show.p.overall = FALSE,
  all.last = TRUE
)

export2md(restab, header.labels = c(all = "All"))
