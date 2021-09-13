#' Draw heat map of the correlation of centers about the fcmc result
#'
#' @param data the fcmc$centers matrix
#'
#' @return
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' a ggfcmc result
#' fcmc.center.heatmap(t(fcmc$centers))
fcmc.center.heatmap <- function(data){
  colnames(data) <- paste0("C", colnames(data))
  cor <- psych::corr.test(data, method = "spearman")
  cor.r <- cor$r
  cor.p <- cor$p
  cor.p <- ifelse(cor.p < 0.001, "***",
                  ifelse(cor.p >= 0.001 & cor.p < 0.01, "**",
                         ifelse(cor.p >= 0.01 & cor.p < 0.05, "*", " ")))
  reorder <- function(data = cor.r){
    cor.r.tmp <- 1-data
    cor.r.tmp <- cor.r.tmp/2
    dd <- as.dist(cor.r.tmp)
    hc <- hclust(dd)
    ind2 <<- hc$order
  }
  cor.r.ind <- cor.r
  cor.p.ind <- cor.p
  if(TRUE){
    reorder()
    cor.r.ind <- cor.r.ind[ind2, ind2]
    cor.p.ind <- cor.p.ind[ind2, ind2]
  }
  if(TRUE){
    ind <- !lower.tri(cor.r.ind)
    cor.r.ind[ind] <- NA
    cor.p.ind[ind] <- NA
  }
  p <- ggplot(reshape2::melt(cor.r.ind, na.rm = TRUE), aes(Var1, Var2)) +
    geom_tile(aes(fill = value), colour = "white") + theme_minimal()
  p <- p + scale_fill_gradient2(name="Value", limits = c(-1, 1),
                         low = "#3c9eff", mid = "white",
                         high = "#ff445d",  midpoint = 0.01) +
   scale_y_discrete(position = "right", expand = c(0, 0)) +
   scale_x_discrete(expand = c(0, 0))
  p <- p + xlab(NULL) + ylab(NULL) + coord_fixed(ratio = 1)
  p <- p + theme(panel.grid = element_line(color = "white", size = 0.1),
          panel.background = element_rect(color = 'gray99',
                                          fill = 'transparent', size = 1),
          axis.text = element_text(size = 10, face = "bold"),
          legend.title = element_text(size = 8, face = "bold"),
          panel.border = element_rect(color = "white", fill = 'transparent'),
          legend.position = c(0.3, 0.8), legend.direction = "horizontal")
  if(TRUE){
    cor.both <- paste0(cor.p.ind, "\n", round(cor.r.ind, 2))
    cor.both <- matrix(cor.both, nrow = nrow(cor.r))
    colnames(cor.both) <- colnames(cor.r.ind)
    rownames(cor.both) <- rownames(cor.r.ind)
    cor.both[ind] <- NA
    p <- p + geom_text(data = reshape2::melt(cor.both, na.rm = TRUE),
                aes(Var1, Var2, label = value), color = "black",
                size = 3, fontface = "bold") +
      guides(fill = guide_colorbar(barwidth = 6,
                                   barheight = 0.5,
                                   raster = T,
                                   ticks = FALSE,
                                   nbin = 10,
                                   label.position = "top",
                                   title.position = "bottom",
                                   title = "Spearman correlation",
                                   label.theme = element_text(angle = -45,
                                                              size = 6,
                                                              face = "bold",
                                                              vjust = 0.5,
                                                              hjust = 0)))
  }
  return(p)
}


