#' PCA plot based on ggplot2 using the stats::prcomp function
#'
#' @param mean.data A data frame containing the first collumn as the grouping information
#' @param group the result of fcmc, default value is fcmc$cluster
#' @param pc Which PC to present, one of 12, 13, 23, the default value is 12
#' @param level default value is 0.68
#'
#' @return
#' @export
#'
#' @author ZhonghuiGai
#' @examples
#' fcmcPCA(genus.syn.mean, fcmc$cluster, pc = 12, level = 0.95) +
#'  ggsci::scale_color_aaas()
fcmcPCA <- function(mean.data, group, pc = 12,
                  level = 0.68){
  if (inherits(mean.data, "grp.mean") & all(rownames(mean.data) == rownames(group))) {
    data <- data.frame(group = as.factor(group), mean.data)
  }else{
    stop("The class of mean.data must be 'grp.mean', and the rownames must bu identical!")
    }
  pca <- stats::prcomp(data[, -1], scale. = TRUE)
  site <- pca$x[, 1:3]
  eig <- summary(pca)$importance[2,]*100
  eig <- round(eig, 1)
  if(all(rownames(site) == rownames(data))){
    pca.data <- data.frame(cluster = data$group, site)
    colnames(pca.data)[2:4] <- paste0("PC", 1:3)
  }
  if (pc == 12) {
    x <- "PC1"
    y <- "PC2"
    x.lab <- paste0(x, ": ", eig[1], "%")
    y.lab <- paste0(y, ": ", eig[2], "%")
  }else if (pc == 13) {
    x <- "PC1"
    y <- "PC3"
    x.lab <- paste0(x, ": ", eig[1], "%")
    y.lab <- paste0(y, ": ", eig[3], "%")
  }else if (pc == 23) {
    x <- "PC2"
    y <- "PC3"
    x.lab <- paste0(x, ": ", eig[2], "%")
    y.lab <- paste0(y, ": ", eig[3], "%")
  }
  p <- ggplot(data = pca.data, aes_string(x = x, y = y, color = "cluster")) +
    geom_point(aes(color = cluster, shape = cluster),
               size = 1.5, alpha = 1) +
    stat_ellipse(level = level,  linetype = 1,
                 geom = "polygon", alpha = 0.1,
                 aes(fill = cluster), show.legend = FALSE,
                 size = 0.2) +
    xlab(x.lab) + ylab(y.lab)
  p <- p + theme(panel.grid = element_line(color = 'gray90', size = 0.1),
                 panel.background = element_rect(color = 'gray60',
                                                 fill = 'transparent', size = 1),
                 axis.text = element_text(size = 12, face = "bold", color = "black"),
                 axis.text.x = element_text(colour = "black", size = 12, face = "bold"),
                 axis.title = element_text(size = 12, face = "bold"),
                 legend.text = element_text(size = 10, face = "bold"),
                 legend.title = element_text(size = 12, face = "bold"),
                 legend.position = "right",
                 panel.border = element_rect(colour = "black", fill = "transparent"),
                 legend.background = element_rect(fill = "transparent"),
                 legend.key = element_rect(fill = "transparent"))
  return(p)
}
