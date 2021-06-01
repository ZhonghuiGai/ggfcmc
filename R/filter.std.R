#' Filter data
#'
#' @param data dataframe
#' @param min.std min standard deviation
#'
#' @return a filted data
#' @export
#'
#' @author Zhonghui Gai

filter.std <- function (data, min.std =0){
  tmp <- logical(dim(data)[1])
  if (is.numeric(min.std)) {
    data <- data
    for (i in 1:length(tmp)) {
      tmp[i] <- sd(data[i, ], na.rm = TRUE)
    }
    index <- tmp > min.std
    cat(paste(sum(!index), "variables excluded.\n"))
  }
  return(data[index, ])
}
