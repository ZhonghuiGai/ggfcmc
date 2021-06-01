#' Estimate the fuzzifier step 3
#'
#' @param df a dataframe of "grp.mean" class
#'
#' @return a numeric
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' m <- mestimate(scaledata)
mestimate <- function(df){
  N <- dim(df)[[1]]
  D <- dim(df)[[2]]
  m.sj <- 1 + (1418/N + 22.05)*D^(-2) + (12.33/N +0.243)*D^(-0.0406*log(N) - 0.1134)
  return(m.sj)
}
