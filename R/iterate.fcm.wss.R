#' Fuzzy c-means: choose the clusters number
#'
#' @param df a dataframe of "grp.mean" class
#' @param m a numeric abtained by mestimate function
#'
#' @return a plot
#' @export
#'
#' @author Zhonghui Gai
#' @importFrom e1071 cmeans
#' @examples
#' iterate.fcm.wss(scaledata, mestimate(scaledata))

iterate.fcm.wss <- function(df, m){
  totss <- numeric()
  for (i in 2:20){
    FCMresults <- cmeans(df,centers=i,m=m)
    totss[i] <- sum(sumsqr(df,FCMresults$cluster))
  }
  # return(totss)
  p <- plot(1:20, totss[1:20],
       type="b", xlab="Number of Clusters", ylab="wss")
  return(p)
}

sumsqr <- function(x, clusters){
  sumsqr <- function(x) sum(scale(x, scale = FALSE)^2)
  wss <- sapply(split(as.data.frame(x), clusters), sumsqr)
  return(wss)
}



