library(gridExtra)
library(gtable)
library(tidyr)
#library(forcats)
library(plyr)
library(shadowtext)


data_fair_selfefficacy_table <- rbind(
  data.frame(aspect = "FAIR", who="myself", table_scores_researchers[table_scores_researchers$variable == "selfefficacy1", c(2:6)]),
  data.frame(aspect = "FAIR", who="help", table_scores_researchers[table_scores_researchers$variable == "selfefficacy2", c(2:6)]),
  data.frame(aspect = "F", who="myself", table_scores_researchers[table_scores_researchers$variable == "selfefficacy3", c(2:6)]),
  data.frame(aspect = "F", who="help", table_scores_researchers[table_scores_researchers$variable == "selfefficacy4", c(2:6)]),
  data.frame(aspect = "A", who="myself", table_scores_researchers[table_scores_researchers$variable == "selfefficacy5", c(2:6)]),
  data.frame(aspect = "A", who="help", table_scores_researchers[table_scores_researchers$variable == "selfefficacy6", c(2:6)]),
  data.frame(aspect = "I", who="myself", table_scores_researchers[table_scores_researchers$variable == "selfefficacy7", c(2:6)]),
  data.frame(aspect = "I", who="help", table_scores_researchers[table_scores_researchers$variable == "selfefficacy8", c(2:6)]),
  data.frame(aspect = "R", who="myself", table_scores_researchers[table_scores_researchers$variable == "selfefficacy9", c(2:6)]),
  data.frame(aspect = "R", who="help", table_scores_researchers[table_scores_researchers$variable == "selfefficacy10", c(2:6)])
)

data_fair_selfefficacy_table <- data_fair_selfefficacy_table %>% 
  pivot_longer(cols=c(3:7), values_to = "percentage")

data_fair_selfefficacy_table$name <- str_replace(data_fair_selfefficacy_table$name, "_p", "")
data_fair_selfefficacy_table$percentage <- as.numeric(data_fair_selfefficacy_table$percentage)

data_fair_selfefficacy_table$aspect <- factor(
  data_fair_selfefficacy_table$aspect,
  levels = c("FAIR", "F", "A", "I", "R"),
  labels = c(
    "FAIR",
    "Findable",
    "Accessible",
    "Interoperable",
    "Reusable"
  )
)

data_fair_selfefficacy_table$rounded <- abs(round(data_fair_selfefficacy_table$percentage))

# Make percentages negative for 'No'
data_fair_selfefficacy_table$percentage[data_fair_selfefficacy_table$name == "strongly_disagree"] = -data_fair_selfefficacy_table$percentage[data_fair_selfefficacy_table$name == "strongly_disagree"]
data_fair_selfefficacy_table$percentage[data_fair_selfefficacy_table$name == "disagree"] = -data_fair_selfefficacy_table$percentage[data_fair_selfefficacy_table$name == "disagree"]

# Center neutral
data_fair_selfefficacy_table$percentage[data_fair_selfefficacy_table$name == "neutral"] <- data_fair_selfefficacy_table$percentage[data_fair_selfefficacy_table$name == "neutral"] / 2

data_fair_selfefficacy_table_neutral <- data_fair_selfefficacy_table[data_fair_selfefficacy_table$name == "neutral", ]
data_fair_selfefficacy_table_neutral$rounded <- NA
data_fair_selfefficacy_table_neutral$percentage <- -data_fair_selfefficacy_table_neutral$percentage

data_fair_selfefficacy_table <- rbind(data_fair_selfefficacy_table, data_fair_selfefficacy_table_neutral)


data_fair_selfefficacy_table$who <- factor(
  data_fair_selfefficacy_table$who,
  levels = c("myself", "help"),
  labels = c(
    "Without help",
    "With help"
  )
)

data_fair_selfefficacy_table$name <- factor(
  data_fair_selfefficacy_table$name,
  levels = c("strongly_disagree", "disagree", "neutral", "agree", "strongly_agree"),
  labels = c(
    "Strongly disagree",
    "Disagree",
    "Neutral",
    "Agree",
    "Strongly agree"
  )
  # ordered=TRUE
)

xs <- split(data_fair_selfefficacy_table,f = data_fair_selfefficacy_table$who)

figure_y_labels <-  paste0(abs(seq(-100, 100, 10)), "%")
figure_y_labels[c(FALSE, TRUE)] <- ""

plot.labels <- c("**Without help** I am able to make my research data more ...", "**With help** I am able to make my research data more ...")
names(plot.labels) <- c("Without help", "With help")

plot_selfefficacy_bythemselves <- ggplot(
  xs$`Without help`,
    aes(
      fill = name,
      y = percentage,
      x = reverse.levels(aspect)
    )
  ) +
  geom_bar(
    position = position_stack(reverse = FALSE),
    data = . %>% filter((percentage < 0)),
    stat = "identity",
    width = 0.5
  ) +
  geom_bar(
    position = position_stack(reverse = TRUE),
    data = . %>% filter((percentage > 0)),
    stat = "identity",
    width = 0.5
  ) +
    coord_flip() +
    scale_y_continuous(
      breaks = seq(-100, 100, 10),
      limits = c(-55, 90),
      labels = figure_y_labels
    ) +
    # labs(title = "Effort spent in making research data FAIR, per FAIR aspect\n", x = "FAIR aspect\n", y = "\nPercentage (%)") +
    labs(x = "FAIR aspect\n", y = "\nPercentage (%)") +
    # theme_minimal() +
    geom_hline(yintercept = 0) +
    scale_fill_manual(
      name = "Effort",
      values = c("#e36c33", "#edad88", "#c1c1c1", "#92c5de", "#0571b0"),
      labels = c(
        "Strongly disagree",
        "Disagree",
        "Neutral",
        "Agree",
        "Strongly agree"
      ),
      drop=FALSE
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
    aes(label = ifelse(rounded != 0, paste0(rounded, "%"), "")),
    position = position_stack(vjust = 0.5, reverse = FALSE),
    data = . %>% filter((percentage < 0)),
    size = 2.5,
    color = "white",
    bg.r = 0.1,
    alpha = 0.1
  ) + 
  geom_shadowtext(
    aes(label = ifelse(rounded != 0, paste0(rounded, "%"), "")),
    position = position_stack(vjust = 0, reverse = TRUE),
    data = . %>% filter((percentage > 0) & name == "Neutral"),
    size = 2.5,
    color = "white",
    bg.r = 0.1,
    alpha = 0.1
  ) + 
  geom_shadowtext(
    aes(label = ifelse((rounded != 0 & name != "Neutral"), paste0(rounded, "%"), "")),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    data = . %>% filter((percentage > 0)),
    size = 2.5,
    color = "white",
    bg.r = 0.1,
    alpha = 0.1
  ) + 
  geom_text(
    aes(label = ifelse(rounded != 0, paste0(rounded, "%"), "")),
    position = position_stack(vjust = 0.5, reverse = FALSE),
    data = . %>% filter((percentage < 0)),
    size = 2.5,
    color = "white"
  ) + 
  geom_text(
    aes(label = ifelse(rounded != 0, paste0(rounded, "%"), "")),
    position = position_stack(vjust = 0, reverse = TRUE),
    data = . %>% filter((percentage > 0) & name == "Neutral"),
    size = 2.5,
    color = "white"
  ) + 
  geom_text(
    aes(label = ifelse((rounded != 0 & name != "Neutral"), paste0(rounded, "%"), "")),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    data = . %>% filter((percentage > 0)),
    size = 2.5,
    color = "white",
  ) + 
    facet_wrap( ~ who, ncol = 2, labeller = labeller(who = plot.labels))

legend = gtable_filter(ggplot_gtable(ggplot_build(plot_selfefficacy_bythemselves)), "guide-box")

plot_selfefficacy_bythemselves <- plot_selfefficacy_bythemselves + theme(legend.position="none") + 
  theme(strip.text.x = ggtext::element_markdown())

plot_selfefficacy_withhelp <- plot_selfefficacy_bythemselves %+% xs$`With help` + scale_x_discrete(position = "top")

plot_selfefficacy_bythemselves <- plot_selfefficacy_bythemselves + theme(plot.margin = unit(c(0, .75, 0, 0), "cm"))
plot_selfefficacy_withhelp <- plot_selfefficacy_withhelp + theme(plot.margin = unit(c(0, 0, 0, .75), "cm"))

grid.arrange(arrangeGrob(plot_selfefficacy_bythemselves, plot_selfefficacy_withhelp, ncol = 2),
             legend,
             heights=c(1.1, 0.2))


