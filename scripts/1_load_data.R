file <- paste("data/Researcher_s_perceptions_of_the__Questionnaire_export_", date, ".csv", sep="")

library(readr)
library(ggplot2)
library(stringr)

data_raw <- read_delim(file, ";", escape_double = FALSE, trim_ws = TRUE)

data_raw$institute <- substr(data_raw$`Castor Record ID`, 4, nchar(data_raw$`Castor Record ID`) - 6)

comments <- subset(data_raw, !is.na(data_raw$comments), select=c("Castor Record ID", "Survey Creation Date", "comments"))

rm(date, file)
