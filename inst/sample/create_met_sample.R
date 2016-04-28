library(fbmet)

plrv =  loadRData((system.file("data/plrv.rda", package="agricolae")))

# invent some traits and randomly assign values to records; very simple
n = nrow(plrv)

asc_acid = rnorm(n, 20, 3.5)
saudpc = rpois(n, 5)
taste = rpois(n, 6)

met = cbind(plrv, asc_acid, saudpc, taste)

# now separate by env, take env column, save as distinct files named by env

env = unique(met$Locality)
for(i in 1:length(env)){
  dat = met[met$Locality == env[i], -c(2)]
  fn = paste0(as.character(env[i]), ".csv")
  fp = file.path('inst', 'sample', fn)
  write.csv(dat, file = fp, row.names = FALSE)
}

# Read back data

# Select multiple files
# later: filter by variables with less than 10% missing data
# later: filter by shared variables & shared germplasm
# Combine data by rbind
# Select between variable or Elston index
# if index: select vars for positive / negative
# Voila! Final table to be shared with

# Check graphs when no data!





