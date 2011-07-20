as.Date <- function (x, ...) {
  if (is.numeric(x)) zoo:::as.Date.numeric(x, ...)
  else UseMethod("as.Date")
}

as.Date.numeric <- function (x, origin, ...) {
  if (missing(origin)) origin <- "1970-01-01"
  if (identical(origin, "0000-00-00")) origin <- as.Date("0000-01-01", ...) - 1
  as.Date(origin, ...) + x
}
