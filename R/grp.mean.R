#' Make a mean dataframe by group from data with concurrent controls
#' step 2
#'
#' @param data original data
#' @param group a factor name with grouping information
#' @param filter.std a logical vector, default value is TRUE.
#' @param scale a logical vector, default value is TRUE.
#'
#' @return the grp.mean dataframe
#' @export
#' @example
#' data <- read.csv("genus.Syn.csv", header = T, row.names = 1)
#' genus.syn.mean <- grp.mean(data, "group")

grp.mean <- function(data, group,
                     filter.std = TRUE,
                     scale = TRUE){
  f <- formula(paste0(".", "~", "group"))
  grp.m <- aggregate(f, data = data, FUN = mean)
  rownames(grp.m) <- grp.m[, 1]
  grp.m <- grp.m[, -1]
  grp.m <- t(grp.m)
  grp.m <- as.data.frame(grp.m)
  if (filter.std) {
    grp.m <- filter.std(grp.m)
  }
  if (scale) {
    grp.m <- t(scale(t(grp.m)))
  }
  class(grp.m) <- c("grp.mean", "matrix", "array")
  return(grp.m)
}



