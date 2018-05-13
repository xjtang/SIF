pheno_group <- function(doy, pheno){
  grp <- rep(2, length(doy))
  grp[doy < pheno[2]] <- 1
  grp[doy > pheno[3]] <- 3
  return(grp)
}


annual_sum <- function(years=c(2007, 2016)){
  years <- seq(years[1], years[2])
  r <- matrix(0, length(years), 6)
  for(i in 1:length(years)){
    gsr <- unlist(pheno_500m[pheno_500m[, 1] == years[i], c(3, 4)] + years[i] * 1000)
    r[i, 1] <- years[i]
    r[i, 2] <- gsr[2] - gsr[1]
    r[i, 3] <- sum(ems_daily[(ems_daily[, 1] >= gsr[1])&(ems_daily[, 1] <= gsr[2]), 2], na.rm=T)
    r[i, 4] <- r[i, 3] / r[i, 2]
    r[i, 5] <- mean(sif_daily[(sif_daily[, 1] >= gsr[1])&(sif_daily[, 1] <= gsr[2]), 2], na.rm=T, trim=5)
    r[i, 6] <- max(EVI_500m[(EVI_500m[, 1] >= gsr[1])&(EVI_500m[, 1] <= gsr[2]), 2], na.rm=T)
  }
  return(r)
}


remove_winter <- function(d, pheno, del=F){
  pheno[, 2] <- pheno[, 2] - 10 
  pheno[, 5] <- pheno[, 5] + 10 
  for(i in 1:nrow(pheno)){
    d[(d[,1]<(pheno[i,1]*1000+pheno[i,2]))&(d[,1]>(pheno[i,1]*1000)),-1] <- -9999
    d[(d[,1]>(pheno[i,1]*1000+pheno[i,5]))&(d[,1]<((pheno[i,1]+1)*1000)),-1] <- -9999
  }
  if(del){
    d <- d[d[, 2] != -9999, ]
  }else{
    d[d[, 2] == -9999, -1] <- 0
  }
  return(d)
}

