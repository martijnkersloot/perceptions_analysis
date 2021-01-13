progress_per_source <- as.data.frame(table(data_raw$institute, data_raw$`Survey Progress`))
progress_per_umc <- as.data.frame(table(data_raw$hospital_department_1, data_raw$`Survey Progress`, useNA="ifany"))

# Calculate percentage of filled in / not filled in
total <- nrow(data_raw)
fully_filled_in <- sum(subset(progress_per_source, progress_per_source$Var2 == 100)$Freq)
p_fully_filled_in <- sum(subset(progress_per_source, progress_per_source$Var2 == 100)$Freq) / total * 100
not_fully_filled_in <- total - fully_filled_in

completed_per_umc <- subset(progress_per_umc, progress_per_umc$Var2 == 100, select=c(1, 3))
completed_per_source <- subset(progress_per_source, progress_per_source$Var2 == 100, select=c(1, 3))

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