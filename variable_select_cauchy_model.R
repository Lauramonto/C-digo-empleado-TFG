Variable_select_cauchy_model<- nimbleCode({
  
  # efecto fijo (intercepto)
  for (j in 1:Ntypes){
    gamma[j] ~ dnorm(0,sd=100)
  }
  
  # efecto aleatorio
  for (i in 1:Nsegments){
    for (j in 1:Ntypes){
      v[i,j] ~ dnorm(0,sd=sigma_v[j])
    }
  }
  
  for (j in 1:Ntypes){
    sigma_v[j] ~ dgamma(0.001,0.001)
  }
  
  # efecto espacial
  for (j in 1:Ntypes){
    u[1:Nsegments,j] ~ dcar_normal(adj=W[1:L], weights=weights[1:L], num=num[1:Nsegments], tau=1/sigma_u[j]^2, zero_mean = 1)
    
  }
  
  for (j in 1:Ntypes){
    sigma_u[j] ~ T(dt(0, 1/25, 1), 0, )  # Half-Cauchy(0, 5)
  }
  
  
  # coeficientes de covariables lasso
  for (j in 1:Ntypes) {
    for (k in 1:Ncovariables) {
      beta[j,k] ~ ddexp(0, lambda) 
    }
  }
  
  lambda ~ dgamma(2, 2)  #media 1 varianza 0.5
  
  for (i in 1:Nsegments){
    for (j in 1:Ntypes){
      Y[i,j] ~ dbin(pi[i,j],n[i])
      logit(pi[i,j]) <- gamma[j] + u[i,j] + v[i,j] + inprod(covariables[i,], beta[j,])
    }
  }
  
})