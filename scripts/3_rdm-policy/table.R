# Table: Researcher aware of RDM policy

table_rdm <- createTable(
  compareGroups(
    profession_group ~ policy_aware + policy_contact + policy_fair,
    data_rdm
  ),
  show.all = FALSE,
  show.p.overall = FALSE,
  all.last = TRUE,
  show.n = FALSE
)

print(
  export2md(table_rdm, format="latex")
)
