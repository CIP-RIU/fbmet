# Get sample data
library(agricolae)
data(plrv)

library(fbmet)

model<- with(plrv, AMMI(Locality, Genotype, Rep, Yield, console=FALSE))

if(interactive()){
  gg_biplot(model)
}

