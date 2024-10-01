Start.of.Month <- function(x) {
  as.Date(format(x, "%Y-%m-01"))
}

End.of.Month <- function(x) {
  Start.of.Month(Start.of.Month(x) + 35) - 1
}