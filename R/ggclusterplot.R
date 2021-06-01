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
ggclusterplot <- function(data, fcm){
  fcm.df <- data.frame(data) # scaled original mean data
  fcm.df$variables <- row.names(fcm.df)
  fcm.df$cluster <- fcmc$cluster # bind cluster assinment
  #fetch the membership for each variable/top scoring cluster facet_wrap
  fcm.df$membership <-
    sapply(1:length(fcm.df$cluster), function(row){
      clust <- fcm.df$cluster[row]
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
    facet_wrap(vars(cluster), scales = "free_y", ncol = 4)
  return(p)
}
