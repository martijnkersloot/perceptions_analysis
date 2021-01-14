library(likert)

data_fair <- data_raw[,2]

# effort

data_fair$effort_f <- factor(
  data_raw$effort_f,
  levels = c(0, 1, 2, 3),
  labels = c(
    "No, not at all",
    "Yes, very little effort",
    "Yes, some effort",
    "Yes, a lot of effort"
  ),
  ordered = TRUE
)

data_fair$effort_a <- factor(
  data_raw$effort_a,
  levels = c(0, 1, 2, 3),
  labels = c(
    "No, not at all",
    "Yes, very little effort",
    "Yes, some effort",
    "Yes, a lot of effort"
  ),
  ordered = TRUE
)

data_fair$effort_i <- factor(
  data_raw$effort_i,
  levels = c(0, 1, 2, 3),
  labels = c(
    "No, not at all",
    "Yes, very little effort",
    "Yes, some effort",
    "Yes, a lot of effort"
  ),
  ordered = TRUE
)

data_fair$effort_r <- factor(
  data_raw$effort_r,
  levels = c(0, 1, 2, 3),
  labels = c(
    "No, not at all",
    "Yes, very little effort",
    "Yes, some effort",
    "Yes, a lot of effort"
  ),
  ordered = TRUE
)

data_fair_effort <- as.data.frame(data_fair[complete.cases(data_fair), ])

row.names(data_fair_effort) <- data_fair_effort$`Castor Record ID`
data_fair_effort[1] <- NULL

data_fair_effort_table <- rbind(
  data.frame(aspect = "F", count = table(data_fair_effort$effort_f)),
  data.frame(aspect = "A", count = table(data_fair_effort$effort_a)),
  data.frame(aspect = "I", count = table(data_fair_effort$effort_i)),
  data.frame(aspect = "R", count = table(data_fair_effort$effort_r))
)


data_fair_effort_table$aspect <- factor(
  data_fair_effort_table$aspect,
  levels = c("F", "A", "I", "R"),
  labels = c(
    "Findable",
    "Accessible",
    "Interoperable",
    "Reusable"
  )
)

colnames(data_fair_effort_table) <- c("aspect", "effort", "freq")
data_fair_effort_table$percentage <- data_fair_effort_table$freq / nrow(data_fair_effort) * 100
data_fair_effort_table$rounded <- abs(round(data_fair_effort_table$percentage))

# Make percentages negative for 'No'
data_fair_effort_table$percentage[data_fair_effort_table$effort == "No, not at all"] = -data_fair_effort_table$percentage[data_fair_effort_table$effort == "No, not at all"]

# ggplot(data_fair_effort_table,
#        aes(
#          fill = effort,
#          y = percentage,
#          x = reverse.levels(aspect)
#        )) +
#   geom_bar(position = position_stack(reverse = TRUE),
#            stat = "identity",
#            width = 0.5) +
#   coord_flip() +
#   scale_y_continuous(
#     breaks = seq(-100, 100, 10),
#     limits = c(-60, 60),
#     labels = abs(seq(-100, 100, 10))
#   ) +
#   labs(title = "Effort spent in making research data FAIR, per FAIR aspect\n", x = "FAIR aspect\n", y = "\nPercentage (%)") +
#   theme_minimal() +
#   geom_hline(yintercept = 0) +
#   theme(legend.position = "bottom",
#             legend.box = "vertical",
#         axis.title.y=element_blank()
#   ) + 
#   geom_text(aes(label = paste0(rounded, "%"),
#             position = position_stack(vjust = 0.5), size = 2),
#     labels = c("No effort", "Very little effort", "Some effort", "A lot of effort"),
#     color="white")


ggplot(data_fair_effort_table,
       aes(
         fill = effort,
         y = percentage,
         x = reverse.levels(aspect)
       )) +
  geom_bar(position = position_stack(reverse = TRUE),
           stat = "identity",
           width = 0.5) +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(-100, 100, 10),
    limits = c(-60, 60),
    labels = abs(seq(-100, 100, 10))
  ) +
  labs(title = "Effort spent in making research data FAIR, per FAIR aspect\n", x = "FAIR aspect\n", y = "\nPercentage (%)") +
  theme_minimal() +
  geom_hline(yintercept = 0) +
  scale_fill_manual(
    name = "Effort",
    values = c("#ca0020", "#c1c1c1", "#92c5de", "#0571b0"),
    labels = c("No effort", "Very little effort", "Some effort", "A lot of effort")
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    axis.title.y = element_blank(),
    legend.title=element_blank()
  ) +
  geom_text(
    aes(label = paste0(rounded, "%")),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    size = 2.5,
    color = "white"
  )



  # geom_hline(yintercept=seq(-60, 60, 10), color="white", size=0.1)


#data_fair_effort_likert <- likert(data_fair_effort)

# ggplot(data_fair_effort, aes(fill=condition, y=value, x=specie)) + 
#   geom_bar(position="stack", stat="identity")



# plot(data_fair_effort_likert,
#      type="heat",
#      low.color = "white",
#      high.color = "blue",
#      text.color = "black",
#      text.size = 4,
#      wrap = 50)




#heard_of_fair
table(data_raw$heard_of_fair)


# know_fair_definition
table(data_raw$know_fair_definition)



# fair_definition

unique(data_raw$fair_definition)








## RDM

#policy_aware
table(data_raw$policy_aware)

table(data_raw$policy_contact)

table(data_raw$policy_fair)