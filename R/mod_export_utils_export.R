createfilter <- function(x, y) {
  val <- unlist(stringr::str_split(x, " ... "))
  paste0("dplyr::between(",
         paste(y, val[1], val[2], sep = ", "),
         ")")
}

pastevector  <- function(x, y, aslist = FALSE) {
  paste0(x,
         ifelse(aslist, " = list(", " = c("),
         paste0("\"", y, "\"", collapse = ", "),
         ")")
}
