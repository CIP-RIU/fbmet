#' gg_biplot
#'
#' Creates a modified biplot. The modification is re-sizing dots by trait value.
#'
#' @param model a model as from the agricolae function AMMI
#' @param add_labels whether the biplot should show labels (more time consuming)
#' @param distance the relative distance at which to draw the circle, default: 0.5
#' @param ranges give xlim and ylim ranges in plot units; default is NULL. Allows re-
#'  sizing.
#' @import ggplot2
#' @import ggrepel
#' @export
#'
#' @example inst/examples/ex_gg_biplot.R
#'
gg_biplot <- function(model, add_labels = TRUE, distance  =  .5, ranges = NULL){
  #distance = .9
  stopifnot(class(model) == 'AMMI')
  dat = model$biplot
  #dat = model

  env = dat[dat$type=="ENV", ]
  pc1 = model$analysis$percent[1]
  pc2 = model$analysis$percent[2]

  G <- subset(model$biplot, model$biplot$type == "GEN")
  x <- G$PC1
  y <- G$PC2
  d <- sqrt(x^2 + y^2)
  r <- distance * max(d)

  tt <- seq(0, 2 * pi , length = 100)
  df <- data.frame(x = sin(tt) * r, y = cos(tt) * r)

  gg <- ggplot(dat, aes(x = dat$PC1, y = dat$PC2, color = dat$type)) +
    ggtitle(paste0("Biplot of ", names(dat)[2])) +
    xlab(paste0("PC1 (", pc1, "%)")) +
    ylab(paste0("PC2 (", pc2, "%)")) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +

    geom_point( aes(size = dat[, 2])) +
    #geom_point(color = 'red', data = env ) +
    geom_segment(aes(x = rep(0, nrow(env)), y = rep(0, nrow(env)),
                     xend = env$PC1, yend = env$PC2,
                     color = 'environment'
    ), data = env,
    arrow = arrow(type="closed", length = unit(0.1, "inches"))) +
    theme(legend.position = 'none')

  if(!is.null(ranges)){
    gg = gg + coord_cartesian(xlim = ranges$x, ylim = ranges$y)
  }

  if(!is.null(distance)){
    gg = gg + geom_path(aes(x = x, y = y, color ='red'), data = df)
  }

  if (add_labels){
    gg = gg + geom_text_repel( aes(label = rownames(dat), color = factor(dat$type)))

  }
  gg
}
