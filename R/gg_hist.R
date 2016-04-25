
#' gg_hist
#'
#' Shows histogram of averages.
#'
#' @param model as from AMMI (agricolae library)
#' @param filtered a vector of filtered genotypes.
#' @import ggplot2
#' @export
gg_hist <- function(model, filtered = unique(row.names(model$biplot))){
  atable = model$biplot
  genotypes = filtered

  max_gt = 10
  if(length(genotypes) > max_gt) {
    genotypes = genotypes[1:max_gt]
  }

  #atable$Genotype = cbind(row.names(atable), atable)
  dat = atable[row.names(atable) %in% genotypes, ]

  #dat = dat %>% as.data.frame()
  #dat = atable[atable$Genotype %in% genotypes, ]

  if (nrow(dat) == 0)
    return()



  gg = ggplot(atable, aes(x = atable[, 2])) +
    ggtitle(paste0("Histogram for trait: ", names(atable)[2])) +
    xlab(paste0(names(atable)[2])) +
    geom_histogram(data = atable, fill = "red", alpha = 0.2, binwidth = 2) +
    geom_vline(aes(xintercept=mean(atable[, 2], na.rm=T)),   # Ignore NA values for mean
               color="red", linetype="dashed", size=1)
  if(nrow(dat) > 0){
    gg = gg + geom_histogram(data = dat,  aes(x = dat[, 2]),fill = "blue", alpha = 0.4,
                             binwidth = 2)
  }
  gg = gg + theme(legend.position = "right")
  gg
}

