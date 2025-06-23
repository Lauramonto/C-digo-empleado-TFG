library(ggplot2)
library(car)
library(nimble)
library(coda)


load("TFG/Data/covariables_segs.rda")
load("TFG/Data/cov_reducidas_scaled.rda")
load("TFG/Data/X_2014_2017.rda")
load("TFG/Data/BD.rda")

source("TFG/Models_codes/variable_selection_model.R")


#Constants creation

load("TFG/Data/NeighbourhoodMatrix.rda")

adj <- as.carAdjacency(W)

constants <- list(
  
  Nsegments = X$domain$lines$n,
  Ntypes = 6,
  n = BD$TOTAL_ACCIDENTES,
  Ncovariables = ncol(cov_reducidas_scaled),
  L = 11068,
  num = adj$num,
  W = adj$adj,
  weights = rep(1, length(adj$weights))
  
)

# Fit model

data <- list(Y = as.matrix(BD[,c("ALCANCE","ATROPELLO","CHOQUE","EMBESTIDA","RASCADA","SALIDA DE LA VIA")]), 
             covariables = as.matrix(cov_reducidas_scaled))

colnames(data$covariables) <- colnames(cov_reducidas_scaled) 


inits <- function() list(gamma = rep(0,constants$Ntypes),
                         pi = matrix(1/6,nrow = constants$Nsegments,ncol = constants$Ntypes),
                         sigma_u = rep(1,constants$Ntypes),
                         sigma_v = rep(1,constants$Ntypes),
                         u = matrix(0,nrow = constants$Nsegments,ncol = constants$Ntypes),
                         v = matrix(0,nrow = constants$Nsegments,ncol = constants$Ntypes),
                         beta = matrix(0, nrow = constants$Ntypes, ncol = constants$Ncovariables),
                         lambda = 1
)


mcmc.output <- nimbleMCMC(Variable_selection_model, data = data, inits = inits, constants = constants,
                          monitors = c( "gamma",
                                        "pi",
                                        "sigma_u",
                                        "sigma_v",
                                        "u",
                                        "v",
                                        "beta",
                                        "lambda"),
                          thin = 5, niter = 40000, nburnin = 10000, nchains = 2, summary = TRUE, WAIC = TRUE)


save(mcmc.output,file="TFG/Models/var_select_reduce_model.rda")