print(
  ggplot(completed_per_umc, aes(x = Var1, y = Freq)) +
    geom_bar(stat = "identity", color = "black", fill = "grey") +
    labs(title = "Frequency by UMC\n", x = "\nUMC", y = "Frequency\n") +
    scale_y_continuous(expand = c(0, 0)) +
    theme_classic()
)

print(
  ggplot(completed_per_source, aes(x = Var1, y = Freq)) +
    geom_bar(stat = "identity", color = "black", fill = "grey") +
    labs(title = "Frequency by source\n", x = "\nSource", y = "Frequency\n") +
    scale_y_continuous(expand = c(0, 0)) +
    theme_classic()
)

print(
  ggplot(completed_per_day, aes(x = Var1, y = cumsum(Freq))) + geom_line() +
    labs(title = "Completed surveys over time\n", x = "\nDate", y = "Completed surveys\n") +
    scale_x_date(
      date_breaks = "1 month",
      date_labels =  "%b %Y",
      limits = as.Date(c("2020-11-27", "2021-03-01"))
    ) +
    theme_classic()
)