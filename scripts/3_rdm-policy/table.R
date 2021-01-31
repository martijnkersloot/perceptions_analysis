# Table: Researcher aware of RDM policy

table_rdm <- createTable(
  compareGroups(
    ~ policy_aware + policy_contact + policy_fair,
    data_rdm
  ),
  show.all = TRUE,
  show.p.overall = FALSE,
  all.last = TRUE,
  show.n = FALSE
)

print(
  export2md(table_rdm)
)