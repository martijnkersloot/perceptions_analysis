data_rdm <- data_demographics[,1]

data_rdm$policy_aware <- factor(
  data_raw$policy_aware,
  levels = c(1, 2, 0, 99),
  labels = c(
    "Organization has policy",
    "Organization does not have policy",
    "Not aware of this policy",
    "Does not know"
  )
)

data_rdm$policy_contact <- factor(
  data_raw$policy_contact,
  levels = c(1, 2, 0),
  labels = c(
    "A specific person",
    "A specific department",
    "Does not know"
  )
)

data_rdm$policy_fair <- factor(
  data_raw$policy_fair,
  levels = c(1, 0, 99),
  labels = c(
    "Part of RDM policy",
    "Not part of RDM policy",
    "Does not know"
  )
)


attr(data_rdm$policy_aware, "label") <- "Aware of RDM policy"
attr(data_rdm$policy_contact, "label") <- "Who to contact regarding RDM policy"
attr(data_rdm$policy_fair, "label") <- "FAIR"

data_rdm$profession_group <- data_demographics$profession_group
data_rdm <- subset(data_rdm, data_rdm$profession_group == "Researcher")
data_rdm <- as.data.frame(data_rdm[complete.cases(data_rdm), ])

