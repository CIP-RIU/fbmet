# Get sample data
library(agricolae)
data(plrv)

library(fbmet)

# enable easy summaries
library(magrittr)
library(dplyr)

atable <- plrv %>% group_by(Genotype, Locality) %>% summarise(Yield = mean(Yield))

if(interactive()){
  gg_env_plot(atable)
}

