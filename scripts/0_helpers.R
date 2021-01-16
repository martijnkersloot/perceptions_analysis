trim <- function (x) gsub("^\\s+|\\s+$", "", x)

generate_block <- function (variable, count) {
  paste(variable, "_", seq(1:count), sep="")
}
