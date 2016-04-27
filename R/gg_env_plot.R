#' gg_env_plot
#'
#' Shows performance of a trait or index for all varties separately in each environment.
#' Data for each environment are displayed as violin plots.
#'
#' One or more (up to 10) genotypes can be highlighted, that is they will be displayed in
#' differntly colored dots and are connected through lines which are also correspondingly
#' colored.
#'
#' @param atable a dataframe containing at least three columns: Genotype, Locality, [Trait]
#'    in this order.
#' @param trait the trait (or index) to be shown
#' @param genotypes a subset of up to 10 genotypes that will be highlighted through lines.
#' @import ggplot2
##@import magrittr
#'
#' @export
#'
#' @example inst/examples/ex_gg_env_plot.R
gg_env_plot <- function(atable,
                        genotypes = unique(as.character(atable$Genotype))[1:10],
                        trait = names(atable)[ncol(atable)]){
  stopifnot(all(c("Genotype", "Locality", trait) %in% names(atable)))

  max_gt = 10
  if(length(genotypes) > max_gt) {
    genotypes = genotypes[1:max_gt]
  }

  atable$Genotype = as.character(atable$Genotype)

  atable = atable %>% as.data.frame()
  dat = atable[atable$Genotype %in% genotypes, ]

  if (nrow(dat) == 0)
    return()

  gg = ggplot(data = atable, aes(factor(atable$Locality), atable[, trait])) +
    ggtitle(paste0("Performance over environments for trait: ", names(atable)[3])) +
    xlab("Locality") +
    ylab(trait) +
    geom_violin() +
  #gg = gg + with(dat, {
    geom_line(data = dat, aes(factor(dat$Locality), dat[, trait],
                              group = Genotype,
                              color = Genotype

    )) +
      geom_point(size=1.2, fill="white")
  #})
  gg
}
