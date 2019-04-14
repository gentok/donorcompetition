## Taken from http://thepoliticalmethodologist.com/2015/03/03/some-code-for-estimating-clustered-ses-in-mlogit-models/
## On Feb 1st 2016
# load in a function to create clustered standard errors for mlogit models
# initial code by Mahmood Ara: http://thetarzan.wordpress.com/2011/06/11/clustered-standard-errors-in-r/
# slightly modified for mlogit models by Justin Esarey on 3/3/2015

cl.mlogit   <- function(fm, cluster){
  
  # fm: a fitted mlogit model
  # cluster: a data vector with the cluster
  #          identity of each observation in fm
  
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- length(coefficients(fm))
  # edit 6/22/2015: change dfc
  # dfc <- (M/(M-1))*((N-1)/(N-K))
  dfc <- (M/(M-1))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat.=crossprod(uj)/N)
  coeftest(fm, vcovCL) }

#require(VGAMdata)
#data(vtinpat)
#vtinpat$hos.num <- as.numeric(vtinpat$hospital)
#vtinpat$age <- as.numeric(vtinpat$age.group)
#vtinpat.mlogit <- mlogit.data(vtinpat, choice = "admit", shape = "wide")
#vt.mod <- mlogit(admit ~ 0 | age + sex, data = vtinpat.mlogit)
#summary(vt.mod)

#cl.mlogit(vt.mod, vtinpat$hos.num)

cl.mlogit.vcov   <- function(fm, cluster){
  
  # fm: a fitted mlogit model
  # cluster: a data vector with the cluster
  #          identity of each observation in fm
  
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- length(coefficients(fm))
  # edit 6/22/2015: change dfc
  # dfc <- (M/(M-1))*((N-1)/(N-K))
  dfc <- (M/(M-1))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat.=crossprod(uj)/N)
  return(vcovCL) }
