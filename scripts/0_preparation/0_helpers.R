library(kableExtra)
library(reshape)
library(rapportools)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(compareGroups)
library(plspm)
library(reshape)
library(scales)
library(likert)
library(tidyr)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

generate_block <- function (variable, count) {
  paste(variable, "_", seq(1:count), sep="")
}