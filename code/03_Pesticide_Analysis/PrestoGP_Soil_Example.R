data(soil)
soil <- soil[!is.na(soil[,5]),] # remove rows with NA's
y <- soil[,4]                   # predict moisture content
X <- as.matrix(soil[,5:9])
locs <- as.matrix(soil[,1:2])

# Vecchia model
soil.vm <- new("VecchiaModel", n_neighbors = 10)
soil.vm <- prestogp_fit(soil.vm, y, X, locs)

# Full model
soil.fm <- new("FullModel")
soil.fm <- prestogp_fit(soil.fm, y, X, locs)

# Multivariate model
ym <- list()
ym[[1]] <- soil[1:100,5]             # predict two nitrogen concentration levels
ym[[2]] <- soil[,7]
Xm <- list()
Xm[[1]] <- Xm[[2]] <- as.matrix(soil[,c(4,6,8,9)])
Xm[[1]] <- Xm[[1]][1:100,]
locsm <- list()
locsm[[1]] <- locsm[[2]] <- locs
locsm[[1]] <- locsm[[1]][1:100,]
soil.mvm <-  new("MultivariateVecchiaModel", n_neighbors = 10)
soil.mvm <- prestogp_fit(soil.mvm, ym, Xm, locsm)

# Space/elevation model
data(soil250, package="geoR")
y2 <- soil250[,7]               # predict pH level
X2 <- as.matrix(soil250[,c(4:6,8:22)])
# columns 1+2 are location coordinates; column 3 is elevation
locs2 <- as.matrix(soil250[,1:3])

soil.vm2 <- new("VecchiaModel", n_neighbors = 10)
# fit separate scale parameters for location and elevation
soil.vm2 <- prestogp_fit(soil.vm2, y2, X2, locs2, scaling = c(1, 1, 2))
