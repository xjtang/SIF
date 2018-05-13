ems_to_daily <- function(ems, var=13){
  # calculate doys
  doy <- sort(unique(ems[, 1] * 1000 + ems[, 3]))
  # initialize result
  ems_daily <- rep(0, length(doy))
  # sum daily data
  for(i in 1:length(doy)){
    year <- floor(doy[i] / 1000)
    day <- doy[i] - year * 1000
    ems_daily[i] = -sum(ems[(ems[, 1] == year)&(ems[, 3] == day), var])
  }
  return(cbind(doy, ems_daily))
}


ems_to_weekly <- function(ems, var=13, mtd='a'){
  # get available years and weeks 
  years <- sort(unique(ems[, 1]))
  weeks <- seq(1, 366, 7)
  # initialize output
  woy <- rep(1:52, length(years))
  ems_weekly <- rep(-9999, 52*length(years))
  # work through years
  for(i in 1:length(years)){
    woy[((i-1)*52+1):(i*52)] <- woy[((i-1)*52+1):(i*52)] + years[i] * 100
    ems_year <- ems[(ems[,1] == years[i]),]
    # work through weeks
    for(j in 1:51){
      if(sum((ems_year[,3] >= weeks[j]) & (ems_year[,3] < (weeks[j]+7))) > 0){
        if(mtd=='a'){
          ems_weekly[(i-1)*52+j] <- -mean(ems_year[(ems_year[,3]>=weeks[j])&(ems_year[,3]<(weeks[j]+7)), var], na.rm=T, trim=0.15)
        }else if(mtd=='max'){
          ems_weekly[(i-1)*52+j] <- max(ems_year[(ems_year[,3]>=weeks[j])&(ems_year[,3]<(weeks[j]+7)), var], na.rm=T)
        }else{
          ems_weekly[(i-1)*52+j] <- -median(ems_year[(ems_year[,3]>=weeks[j])&(ems_year[,3]<(weeks[j]+7)), var], na.rm=T)
        }
      }
    }
    # add last day (or two days) of year to data of last week
    if(sum((ems_year[,3]>=weeks[52])&(ems_year[,3]<366)) > 0){
      if(mtd=='a'){
        ems_weekly[(i-1)*52+j] <- -mean(ems_year[(ems_year[,3]>=weeks[52])&(ems_year[,3]<366), var], na.rm=T, trim=0.15)
      }else if(mtd=='max'){
        ems_weekly[(i-1)*52+j] <- max(ems_year[(ems_year[,3]>=weeks[52])&(ems_year[,3]<366), var], na.rm=T)
      }else{
        ems_weekly[(i-1)*52+j] <- -median(ems_year[(ems_year[,3]>=weeks[52])&(ems_year[,3]<366), var], na.rm=T)
      }
    }
  }
  ems_weekly <- cbind(woy, ems_weekly)
  return(ems_weekly[(ems_weekly[, 2]!=-9999), ])
}


ems_to_monthly <- function(ems, var=13, mtd='a'){
  # calculate moys
  moy <- sort(unique(ems[, 1] * 100 + ems[, 2]))
  # initialize result
  ems_monthly <- rep(0, length(moy))
  # sum monthly data
  for(i in 1:length(moy)){
    year <- floor(moy[i] / 100)
    month <- moy[i] - year * 100
    if(mtd=='a'){
      ems_monthly[i] = -mean(ems[(ems[, 1] == year)&(ems[, 2] == month), var], na.rm=T, trim=0.15)
    }else{
      ems_monthly[i] = -median(ems[(ems[, 1] == year)&(ems[, 2] == month), var], na.rm=T)
    }
  }
  return(cbind(moy, ems_monthly))
}


ems_to_yearly <- function(ems){
  # calculate moys
  years <- sort(unique(ems[, 1]))
  # initialize result
  ems_yearly <- rep(0, length(years))
  # sum yearly data
  for(i in 1:length(years)){
    ems_yearly[i] = -mean(ems[(ems[, 1] == years[i]), 13], rm.na=T, trim=0.15)
  }
  return(cbind(years, ems_yearly))
}


save_ems <- function(ems, des){
  write.table(ems, des, sep=',', row.names=F)
}


EVI_to_monthly <- function(evi){
  EVI2 <- c(1,1)
  years <- sort(unique(floor(evi[, 1] / 1000)))
  for(year in years){
    dim <- c(31,28,31,30,31,30,31,31,30,31,30,31)
    if(isleap(year)){
      dim[2] <- 29
    }
    for(i in 1:length(dim)){
      if(sum((evi[, 1] >= (year * 1000 + sum(dim[1:i]) - dim[i] + 1))&(evi[, 1] <= (year * 1000 + sum(dim[1:i])))) > 0){
        EVI2 <- rbind(EVI2, c((year * 100 + i), max(evi[evi[, 1] >= (year * 1000 + sum(dim[1:i]) - dim[i] + 1)&(evi[, 1] <= (year * 1000 + sum(dim[1:i]))), 2])))
      }
    }
  }
  return(EVI2[-1, ])
}


EVI_to_weekly <- function(evi){
  evi2 <- cbind(floor(evi[,1]/1000),evi[,2], evi[,1]-floor(evi[,1]/1000)*1000)
  return(ems_to_weekly(evi2, var=2, mtd='max'))
}


sif_QA <- function(sif, minnob=3, correct_date=T, date_block=7){
  if(correct_date){
    for(i in 1:nrow(sif)){
      sif[i,1] <- floor(sif[i,1]/1000)*100 + floor((sif[i,1]-floor(sif[i,1]/1000)*1000)/date_block) + 1
    }
  }
  sif <- sif[sif[,13]==0,]
  sif <- sif[sif[,5]>=0,]
  sif <- sif[sif[,10]>=minnob,]
  return(sif)
}


nbar_QA <- function(nbar, idx='EVI'){
  if(idx=='EVI'){
    nbar <- nbar[nbar[,10]==0, c(1,8)]
    nbar[,2] <- nbar[,2]/10000
  }else if(idx=='LSWI'){
    nbar <- nbar[nbar[,11]==0, c(1,9)]
    nbar[,2] <- nbar[,2]/10000
  }else{
    nbar <- nbar[nbar[,10]==0, c(1,9)]
    nbar[,2] <- nbar[,2]/10000
  }
  return(nbar)
}


pheno_pre <- function(pheno){
  diy <- c(366,365,365,365,366,365,365,365,366,365,365,365,366,365)
  pheno[, 1] <- floor(pheno[, 1] / 1000)
  for(i in 1:14){
    pheno[i:14,2:5] <- pheno[i:14,2:5] - diy[i]
  }
  fill = round(colMeans(pheno)[-1]+c(0,0,0,0))
  for(i in 1:4){
    pheno <- rbind(pheno, c((2014 + i) ,fill))
  }
  return(pheno)
}


gosat_pre <- function(gsif){
  gsif <- gsif[gsif[, 'CLFLG'] == 0, ]
  gsif <- gsif[ceiling(gsif[, 'Longitude']) == -72, ]
  gsif <- cbind(doy = 0, gsif)
  for(i in 1:nrow(gsif)){
    gsif[i, 1] <- date2doy(as.integer(substr(gsif[i,2],1,4)), as.integer(substr(gsif[i,2],6,7)), as.integer(substr(gsif[i,2],9,10)))
  }
  gsif[, 'SIF'] <- gsif[, 'SIF'] * ((10^-9*1000*10^7)/(10^-4*760*760))
  return(gsif)
}

