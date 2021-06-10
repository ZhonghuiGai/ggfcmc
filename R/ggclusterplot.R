#' Draw the cluster plots
#'
#' @param data scaled original mean data created by grp.mean function.
#' @param fcm a aojedct created from ggfcmc or cmeans functions.
#'
#' @return cluster photoes
#' @export
#'
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_line xlab ylab theme_bw theme
#' element_text element_blank scale_colour_gradientn
#' @author Zhonghui Gai
#' @examples
ggclusterplot <- function(data, fcm, ncol){
  fcm.df <- data.frame(data) # scaled original mean data
  fcm.df$variables <- row.names(fcm.df)
  cs <- cluster.size(fcm)
  n <- sapply(fcm$cluster, function(x){
    cs[x]
  })
  # bind cluster assinment
  fcm.df$cluster <- paste0("cluster ", fcm$cluster, "\n", "n = ", n)
  #fetch the membership for each variable/top scoring cluster facet_wrap
  fcm.df$membership <-
    sapply(1:length(fcm.df$cluster), function(row){
      clust <- fcm$cluster[row]
      fcmc$membership[row,clust]
    })
  cluster.df <- melt(data = fcm.df,
                               id.var = c("cluster", "variables", "membership"))
  # order the dataframe by score
  # cluster.df <- cluster.df[order(cluster.df$membership), ]
  p <-
    ggplot(cluster.df, aes(x = variable, y= value)) +
    geom_line(aes(colour = membership, group = variables)) +
    scale_colour_gradientn(colours = c('blue1', 'red2')) +
    # this adds the core
    # geom_line(data=core, aes(grp,value, group=cluster),
    # color="black", inherit.aes=FALSE) +
    xlab(NULL) +
    ylab("Relative abundance") +
    facet_wrap(vars(cluster), scales = "free_y", ncol = ncol) +
    theme(panel.grid = element_blank(),
          panel.background = element_rect(color = 'gray60',
                                          fill = 'transparent', size = 1),
          axis.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 12, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          strip.text = element_text(face = "bold", color = "black",
                                    size = 8),
          strip.background = element_rect(fill = "gray90", colour = "gray60"),
          legend.title.align = 0.5,
          legend.text = element_text(size = 10, face = "bold"),
          legend.title = element_text(size = 10, face = "bold"),
          strip.switch.pad.wrap = unit(5, "cm"))
  return(p)
}

cluster.size <- function(fcm){
  cs <- fcm$size
  names(cs) <- 1:length(cs)
  return(cs)
}
