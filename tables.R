library(kableExtra)
library(reshape)
library(rapportools)

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

rm(res, restab)

# Table 2: Data sharing

export2md(rbind(
  "Shares research data" = table4_shares,
  "Data sharing methods" = table4_method,
  "Data sharing with" = table4_with
))

