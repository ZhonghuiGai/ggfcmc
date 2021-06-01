#' Function for soft clustering based on fuzzy c-means.
#'
#' @param df The data matrix where columns correspond to variables
#'  and rows to observations.
#' @param centers Number of clusters or initial values for cluster centers.
#' @param m A number greater than 1 giving the degree of fuzzification.
#' @param ... other parameter for cmeans in e1071 pkg.
#'
#' @return
#' @export
#'
#' @author Zhonghui Gai
#' @importFrom e1071 cmeans
#' @examples
#' ggfcmc(genus.syn.mean, centers = 8, m = mestimate(genus.syn.mean))
ggfcmc <- function (df, centers, m, ...){
  cm <- cmeans(df, centers = centers, method = "cmeans", m = m, ...)
  return(cm)
}
