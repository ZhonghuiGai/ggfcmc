#' Draw the centroid profiles step 5
#'
#' @param data result from ggcfmc or cmeans
#'
#' @return
#' @export
#'
#' @author Zhonghui Gai
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_line xlab ylab theme_bw theme
#' element_text element_blank
#' @examples
#' ggcenterplot(data = fcmc)
ggcenterplot <- function(data){
  if (!inherits(data, "fclust")) {
    stop("df must be the class of 'fclust'")
  }
   fcm.centroid <- data$centers
   fcm.centroid <- data.frame(fcm.centroid)
   fcm.centroid$cluster <- paste0("cluster ", rownames(fcm.centroid))
   fcm.centroid <- reshape2::melt(data = fcm.centroid,
                                  id.var = "cluster")
  p <- ggplot(data = fcm.centroid, aes(x = variable, y = value,
                                       group = cluster,
                                       colour = cluster)) +
    geom_line() + xlab(NULL) + ylab("Relative abundance") +
    theme_bw() + theme(axis.text = element_text(face = "bold", size = 12),
                       axis.title = element_text(face = "bold", size = 14),
                       legend.title = element_blank())
  return(p)
}
