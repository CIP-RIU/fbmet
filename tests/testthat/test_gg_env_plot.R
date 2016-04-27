context("gg_env_plot")

library(agricolae)
data(plrv)

library(fbmet)

# enable easy summaries
#library(magrittr)
library(dplyr)

atable <- plrv %>% group_by(Genotype, Locality) %>% summarise(Yield = mean(Yield))
at_Loc <- atable[, -c(2)]
at_Gen <- atable[, -c(1)]

test_that("gg_env_plot functin works", {
  expect_that(gg_env_plot(at_Loc), throws_error())
  expect_that(gg_env_plot(at_Gen), throws_error())
}
)
