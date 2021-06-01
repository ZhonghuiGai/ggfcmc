#' Make a mean dataframe by group from data with concurrent controls
#'
#' @param data original data
#' @param group a factor with grouping information
#' @param filter.std a logical vevtor, default value is TRUE.
#'
#' @return the grp.mean dataframe
#' @export
#'

grp.mean <- function(data, group, filter.std = TRUE){
  f <- formula(paste0(".", "~", "group"))
  grp.m <- aggregate(f, data = data, FUN = mean)
  rownames(grp.m) <- grp.m[, 1]
  grp.m <- grp.m[, -1]
  grp.m <- t(grp.m)
  grp.m <- as.data.frame(grp.m)
  if (filter.std) {
    grp.m <- filter.std(grp.m)
  }
  return(grp.m)
}



