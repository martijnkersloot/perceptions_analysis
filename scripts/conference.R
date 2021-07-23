require(ggpubr)
library(gridExtra)
library(grid)

data_demographics_researchers <- subset(data_demographics, data_demographics$profession_group == "Researcher")

data_demographics_researchers$institute <- substr(data_demographics_researchers$`Castor Record ID`, 4, nchar(data_demographics_researchers$`Castor Record ID`) - 6)

data_demographics_researchers$heard_of_fair <- factor(
  data_demographics_researchers$heard_of_fair,
  levels = c(1, 0),
  labels = c(
    "Yes",
    "No"
  )
)

attr(data_demographics_researchers$heard_of_fair, "label") <- "Heard of the FAIR principles"

data_demographics_researchers$know_fair_definition <- factor(
  data_demographics_researchers$know_fair_definition,
  levels = c(1, 0),
  labels = c(
    "Yes",
    "No"
  )
)

data_demographics_researchers$know_aspect_fair <- factor(
(!is.na(data_fair_researchers$letter_f) | !is.na(data_fair_researchers$letter_a) | !is.na(data_fair_researchers$letter_i) | !is.na(data_fair_researchers$letter_r)),
levels = c(TRUE, FALSE),
labels = c(
  "Yes",
  "No"
)
)


data_demographics_researchers$heard_of_fair[is.na(data_demographics_researchers$heard_of_fair)] <- "No"
data_demographics_researchers$know_fair_definition[is.na(data_demographics_researchers$know_fair_definition)] <- "No"

attr(data_demographics_researchers$know_fair_definition, "label") <- "Knows definition of the FAIR principles"
attr(data_demographics_researchers$know_aspect_fair, "label") <- "Knows description of at least one aspect of the FAIR principles"


table_demographics_researchers <- createTable(compareGroups(~ heard_of_fair + know_fair_definition + know_aspect_fair + primary_institution + profession + research_experience, data = data_demographics_researchers), hide.no = "no", show.n=F)

export2latex(table_demographics_researchers)



#data_fair_researchers


know_correct_values <- c(76, 76, 40, 68, 75, 72, 33, 51)

know_correct_values <- c(76, 76, 40, 68, 75, 72, 33, 51)

know_correct_percentage <- round(know_correct_values / nrow(data_demographics_researchers) * 100, 1)

know_correct <- data.frame(
  aspect = factor(
    c("F", "A", "I", "R"),
    levels = c("F", "A", "I", "R"),
    labels=    c("F\n(Findable)", "A\n(Accessible)", "I\n(Interoperable)", "R\n(Reusable)"),
    ordered=T
  ),
  type = factor(
    c("know", "know", "know", "know", "correct", "correct", "correct", "correct"),
    levels = c("know", "correct"),
    labels = c(
      "Claims to know meaning",
      "Gave correct meaning"
    ),
    ordered=T
  ),
  value = know_correct_values,
  percentage = know_correct_percentage
)

ggplot(
  know_correct,
  aes(
    fill = type,
    y = percentage,
    x = aspect
  )
)+ 
  geom_bar(position="dodge", stat="identity", colour="gray") +
  labs(x = "FAIR aspect\n", y = "\nPercentage (%)") +
  scale_fill_manual(
    name = "Effort",
    values = c("grey20", "grey90"),
    labels = c("Claims to know meaning", "Gave correct meaning")
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
  scale_y_continuous(expand = c(0,0),
    breaks = seq(0, 100, 10),
    limits = c(0, 60),
    labels = paste0(abs(seq(0, 100, 10)), "%")
  ) +
  geom_text(
    position = position_dodge2(width = 0.9, preserve = "single"),
    aes(label = paste0(percentage, "%")),
    size = 3,
    color = "gray40", vjust=-1
  ) + 
  ggtitle("Researchers' knowledge of the meaning\nof the letters of FAIR") 

descriptions_values = c(17, 1, 20, 9, 26, 2, 11, 4, 15, 1)
descriptions_percentages <- round(descriptions_values / c(70, 70, 44, 44, 49, 49, 22, 22, 41, 41) * 100, 1)

descriptions <- data.frame(
  aspect = factor(
    c("FAIR", "F", "A", "I", "R"),
    levels = c("FAIR", "F", "A", "I", "R"),
    labels=    c("FAIR\n", "F\n(Findable)", "A\n(Accessible)", "I\n(Interoperable)", "R\n(Reusable)"),
    ordered=T
  ),
  type = factor(
    c("human", "machine", "human", "machine", "human", "machine", "human", "machine", "human", "machine"),
    levels = c("human", "machine"),
    labels = c(
      "Human-readability",
      "Machine-readability"
    ),
    ordered=T
  ),
  value = descriptions_values,
  percentage = descriptions_percentages
)

ggplot(
  descriptions,
  aes(
    fill = type,
    y = percentage,
    x = aspect
  )
)+ 
  geom_bar(position="dodge", stat="identity", colour="gray") +
  labs(x = "FAIR aspect\n", y = "\nPercentage (%)") +
  scale_fill_manual(
    name = "Effort",
    values = c("grey20", "grey90"),
    labels = c(
      "Human-readability   ",
      "Machine-readability"
    )
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
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0, 100, 10),
                     limits = c(0, 60),
                     labels = paste0(abs(seq(0, 100, 10)), "%")
  ) +
  geom_text(
    position = position_dodge2(width = 0.9, preserve = "single"),
    aes(label = paste0(percentage, "%")),
    size = 3,
    color = "gray40", vjust=-1
  ) + 
  ggtitle("Mentions of human- and machine-readability\nin researchers' descriptions")

ggarrange(know_correct_plot, descriptions_plot, labels=c("1", "2"), hjust=-0.8)

# 5 x 4

library(readr)
Assessment_Effort <- read_csv("data/Assessment - Effort, with ID.csv")
Assessment_Effort$aspect <- factor(
  Assessment_Effort$aspect,
  levels = c("F", "A", "I", "R"),
  ordered=T
)

#Assessment_Effort <- subset(Assessment_Effort, Assessment_Effort$ID %in% data_fair_effort_export2$id)
Assessment_Effort <- subset(Assessment_Effort, Assessment_Effort$ID %in% data_demographics_researchers$`Castor Record ID`)



effort_table <- as.data.frame(table(Assessment_Effort$aspect, Assessment_Effort$Category))


effort_table2 <- as.data.frame(table(Assessment_Effort$ID, Assessment_Effort$Category))
effort_table2 <- subset(effort_table2, effort_table2$Freq > 0)
effort_table2_id <- as.data.frame(table(effort_table2$Var1))
effort_table2_id <- subset(effort_table2_id, effort_table2_id$Freq > 0)
effort_table2 <- as.data.frame(table(effort_table2$Var2))

effort_table3 <- as.data.frame(table(Assessment_Effort$aspect, Assessment_Effort$ID))
effort_table3 <- subset(effort_table3, effort_table3$Freq > 0)
effort_table3 <- as.data.frame(table(effort_table3$Var1))


effort_table <- reshape(effort_table, idvar="Var2", timevar="Var1", direction="wide")

effort_table$total <- effort_table$Freq.F + effort_table$Freq.A + effort_table$Freq.I + effort_table$Freq.R


effort_table$Perc.F <- format(round(effort_table$Freq.F / sum(effort_table$Freq.F) * 100, 1), nsmall = 1, trim=TRUE)
effort_table$Perc.A <- format(round(effort_table$Freq.A / sum(effort_table$Freq.A) * 100, 1), nsmall = 1, trim=TRUE)
effort_table$Perc.I <- format(round(effort_table$Freq.I / sum(effort_table$Freq.I) * 100, 1), nsmall = 1, trim=TRUE)
effort_table$Perc.R <- format(round(effort_table$Freq.R / sum(effort_table$Freq.R) * 100, 1), nsmall = 1, trim=TRUE)

effort_table$Perc.total <- effort_table$total / (sum(effort_table$Freq.F) + sum(effort_table$Freq.A) + sum(effort_table$Freq.I) + sum(effort_table$Freq.R))


effort_table$Perc.F <- paste("(", effort_table$Perc.F, ")", sep="")
effort_table$Perc.A <- paste("(", effort_table$Perc.A, ")", sep="")
effort_table$Perc.I <- paste("(", effort_table$Perc.I, ")", sep="")
effort_table$Perc.R <- paste("(", effort_table$Perc.R, ")", sep="")

effort_table <- effort_table[, c("Var2", "Freq.F", "Perc.F", "Freq.A", "Perc.A", "Freq.I", "Perc.I", "Freq.R", "Perc.R", "total")]

####

data_fair_effort_export2 <- rbind(
  data.frame(
    aspect = "F",
    definition = data_fair_researchers$effort_f_description,
    id = data_fair_researchers$`Castor Record ID`
  ),
  data.frame(
    aspect = "A",
    definition = data_fair_researchers$effort_a_description,
    id = data_fair_researchers$`Castor Record ID`
  ),
  data.frame(
    aspect = "I",
    definition = data_fair_researchers$effort_i_description,
    id = data_fair_researchers$`Castor Record ID`
  ),
  data.frame(
    aspect = "R",
    definition = data_fair_researchers$effort_r_description,
    id = data_fair_researchers$`Castor Record ID`
  )
)

data_fair_effort_export2$aspect <- factor(
  data_fair_effort_export2$aspect,
  levels = c("F", "A", "I", "R"),
  ordered=T
)

data_fair_effort_export2 <- data_fair_effort_export2[order(data_fair_effort_export2$aspect, data_fair_effort_export2$definition),]
data_fair_effort_export2 <- subset(data_fair_effort_export2, !is.na(data_fair_effort_export2$definition))

write.csv(effort_table, file="exports/effort_table.csv", row.names=FALSE)
write.csv(data_fair_effort_export2, file="exports/fair_effort_ids.csv", row.names=FALSE)
write.csv(effort_table2, file="exports/effort_table2.csv", row.names=FALSE)

nrow(data_fair_researchers[data_fair_researchers$effort_f != "No, not at all",])
nrow(data_fair_researchers[data_fair_researchers$effort_a != "No, not at all",])
nrow(data_fair_researchers[data_fair_researchers$effort_i != "No, not at all",])
nrow(data_fair_researchers[data_fair_researchers$effort_r != "No, not at all",])


nrow(data_fair_researchers[(data_fair_researchers$effort_f != "No, not at all" | data_fair_researchers$effort_a != "No, not at all" | data_fair_researchers$effort_i != "No, not at all" | data_fair_researchers$effort_r != "No, not at all"), ])
nrow(data_fair_researchers[(!is.na(data_fair_researchers$effort_f_description) | !is.na(data_fair_researchers$effort_a_description) | !is.na(data_fair_researchers$effort_i_description) | !is.na(data_fair_researchers$effort_r_description)), ])


write.csv(data_raw, file="exports/data_export.csv", row.names=F, na="")
