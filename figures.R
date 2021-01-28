# Plots

ggplot(completed_per_umc, aes(x=Var1, y=Freq)) +
  geom_bar(stat = "identity", color = "black", fill = "grey") +
  labs(title = "Frequency by UMC\n", x = "\nUMC", y = "Frequency\n") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()

ggplot(completed_per_source, aes(x=Var1, y=Freq)) +
  geom_bar(stat = "identity", color = "black", fill = "grey") +
  labs(title = "Frequency by source\n", x = "\nSource", y = "Frequency\n") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()


data_fair_effort_table_no_other <- subset(data_fair_effort_table, data_fair_effort_table$profession_group != "Other", drop = TRUE)
rownames(data_fair_effort_table_no_other) <- NULL

levels(data_fair_effort_table_no_other$profession_group) <- c("Other", "Researcher", "Support staff")

ggplot(data_fair_effort_table_no_other,
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
    limits = c(-70, 70),
    labels = abs(seq(-100, 100, 10))
  ) +
  # labs(title = "Effort spent in making research data FAIR, per FAIR aspect\n", x = "FAIR aspect\n", y = "\nPercentage (%)") +
  labs(x = "FAIR aspect\n", y = "\nPercentage (%)") +
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
  ) + facet_wrap(~ profession_group, ncol = 1)
