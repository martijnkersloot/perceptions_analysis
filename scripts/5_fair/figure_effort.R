library(gridExtra)
library(gtable)

data_fair_effort_table <- rbind(
  data.frame(aspect = "F", count = table(data_fair_effort$effort_f, data_fair_effort$profession_group)),
  data.frame(aspect = "A", count = table(data_fair_effort$effort_a, data_fair_effort$profession_group)),
  data.frame(aspect = "I", count = table(data_fair_effort$effort_i, data_fair_effort$profession_group)),
  data.frame(aspect = "R", count = table(data_fair_effort$effort_r, data_fair_effort$profession_group))
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

colnames(data_fair_effort_table) <- c("aspect", "effort", "profession_group", "freq")
#data_fair_effort_table$percentage <- data_fair_effort_table$freq / nrow(data_fair_effort) * 100
#data_fair_effort_table$rounded <- abs(round(data_fair_effort_table$percentage))

data_fair_effort_table$percentage <- NA
data_fair_effort_table$rounded <- NA


for(group in unique(data_fair_effort_table$profession_group)) {
  group_subset <- data_fair_effort_table$profession_group == group
  data_fair_effort_table[group_subset,]$percentage <- data_fair_effort_table[group_subset,]$freq / nrow(subset(data_fair_effort, data_fair_effort$profession_group == group)) * 100
}

data_fair_effort_table$rounded <- abs(round(data_fair_effort_table$percentage))

# Make percentages negative for 'No'
data_fair_effort_table$percentage[data_fair_effort_table$effort == "No, not at all"] = -data_fair_effort_table$percentage[data_fair_effort_table$effort == "No, not at all"]


data_fair_effort_table_no_other <- subset(data_fair_effort_table, data_fair_effort_table$profession_group != "Other", drop = TRUE)
rownames(data_fair_effort_table_no_other) <- NULL

levels(data_fair_effort_table_no_other$profession_group) <- c("Researchers", "Research support staff", "Other")

xs <- split(data_fair_effort_table_no_other,f = data_fair_effort_table_no_other$profession_group)

figure_y_labels <-  paste0(abs(seq(-100, 100, 10)), "%")
figure_y_labels[c(FALSE, TRUE)] <- ""


plot_effort_researcher <- ggplot(
    xs$Researcher,
    aes(
      fill = effort,
      y = percentage,
      x = reverse.levels(aspect)
    )
  ) +
    geom_bar(
      position = position_stack(reverse = TRUE),
      stat = "identity",
      width = 0.5
    ) +
    coord_flip() +
    scale_y_continuous(
      breaks = seq(-100, 100, 10),
      limits = c(-55, 75),
      labels = figure_y_labels
    ) +
    # labs(title = "Effort spent in making research data FAIR, per FAIR aspect\n", x = "FAIR aspect\n", y = "\nPercentage (%)") +
    labs(x = "FAIR aspect\n", y = "\nPercentage (%)") +
    # theme_minimal() +
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
      axis.title.x = element_blank(),
      # axis.title.x = element_text(size=9.5),
      legend.title = element_blank(),
      panel.border = element_rect(colour = "gray", fill=NA, size=1),
      panel.background = element_blank(), 
      panel.grid = element_blank(),
      panel.spacing.x = unit(2,"line")
    ) +
    geom_shadowtext(
      aes(label = paste0(rounded, "%")),
      position = position_stack(vjust = 0.5, reverse = TRUE),
      size = 2.5,
      color = "white",
      bg.r = 0.1,
      alpha = 0.1
    ) +
    geom_text(
      aes(label = paste0(rounded, "%")),
      position = position_stack(vjust = 0.5, reverse = TRUE),
      size = 2.5,
      color = "white"
    ) + facet_wrap( ~ profession_group, ncol = 2)

legend = gtable_filter(ggplot_gtable(ggplot_build(plot_effort_researcher)), "guide-box")

plot_effort_researcher <- plot_effort_researcher + theme(legend.position="none")

plot_effort_support <- plot_effort_researcher %+% xs$`Research support staff` + scale_x_discrete(position = "top")

plot_effort_researcher <- plot_effort_researcher + theme(plot.margin = unit(c(0, .75, 0, 0), "cm"))
plot_effort_support <- plot_effort_support + theme(plot.margin = unit(c(0, 0, 0, .75), "cm"))

grid.arrange(arrangeGrob(plot_effort_researcher, plot_effort_support, ncol = 2),
             legend,
             heights=c(1.1, 0.2))


