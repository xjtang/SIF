plot_ts <- function(ts, file, des=plotPath, title='HF', y_name='GPP', y_limit=0, pheno='NA', x_limit=c(2007,2017)){
  png(file=paste(des, file, sep=''), width=2000, height=1000, pointsize=20)
  if(min(y_limit==0)){
    y_margin <- (max(ts[,2]) - min(ts[,2]))*0.2
    y_limit <- c(min(ts[,2])-y_margin, max(ts[,2])+y_margin)
  }
  plot(0,-1,main=title,ylab=y_name,xlab='Date',xlim=x_limit, ylim=y_limit,lwd=8,bty='n')
  box(col='black',lwd=1)
  
  x <- floor(ts[,1]/1000)+(ts[,1]-floor(ts[,1]/1000)*1000)/(ifelse(isleap(floor(ts[,1])/1000),366,365))
  y <- ts[,2]
  
  fit <- spline(x, y)
  
  lines(fit, col='blue', lwd=1)
  points(x, y, col='blue', pch=1, cex=pcex)
  
  if(max(pheno!='NA')){
    for(i in 1:nrow(pheno)){
      abline(v=(pheno[i,1]+pheno[i,2]/365.25),col='red',lwd=1)
      abline(v=(pheno[i,1]+pheno[i,3]/365.25),col='grey',lwd=1)
      abline(v=(pheno[i,1]+pheno[i,4]/365.25),col='grey',lwd=1)
      abline(v=(pheno[i,1]+pheno[i,5]/365.25),col='red',lwd=1)
    }
  }
  
  dev.off()
  return(0)
}


plot_2ts <- function(ts1, ts2, file, des=plotPath, title, y_name=c('GPP','SIF'), y_limit_1=0, y_limit_2=0, pheno='NA', x_limit=c(2007,2017), matchdata=F){
  png(file=paste(des, file, sep=''), width=2000, height=1000, pointsize=20)
  if(min(y_limit_1==0)){
    y_margin_1 <- (max(ts1[, 2]) - min(ts1[, 2]))*0.2
    y_limit_1 <- c(min(ts1[, 2]) - y_margin_1, max(ts1[, 2]) + y_margin_1)
  }
  if(min(y_limit_2==0)){
    y_margin_2 <- (max(ts2[, 2]) - min(ts2[, 2]))*0.2
    y_limit_2 <- c(min(ts2[, 2]) - y_margin_2, max(ts2[, 2]) + y_margin_2)
  }
  
  par1 <- par(mar=c(5.1, 4.1, 4.1, 5.1))
  
  plot(0,-1,main=title,ylab=y_name[1],xlab='Date',xlim=x_limit, ylim=y_limit_1,lwd=8,bty='n')
  box(col='black',lwd=1)
  if(matchdata){
    ts1 <- match_data(ts1, ts2)
    ts2 <- match_data(ts2, ts1)
  }
  
  x1 <- floor(ts1[,1]/1000)+(ts1[,1]-floor(ts1[,1]/1000)*1000)/(ifelse(isleap(floor(ts1[,1])/1000),366,365))
  y1 <- ts1[,2]
  
  x2 <- floor(ts2[,1]/1000)+(ts2[,1]-floor(ts2[,1]/1000)*1000)/(ifelse(isleap(floor(ts2[,1])/1000),366,365))
  y2 <- ts2[,2]
  
  fit1 <- spline(x1, y1)
  fit2 <- spline(x2, y2)
  
  points(x1, y1, col='blue', pch=1, cex=pcex)
  lines(fit1, col='blue', lwd=2)
  
  if(max(pheno!='NA')){
    for(i in 1:nrow(pheno)){
      abline(v=(pheno[i,1]+pheno[i,2]/365.25),col='red',lwd=1)
      abline(v=(pheno[i,1]+pheno[i,3]/365.25),col='grey',lwd=1)
      abline(v=(pheno[i,1]+pheno[i,4]/365.25),col='grey',lwd=1)
      abline(v=(pheno[i,1]+pheno[i,5]/365.25),col='red',lwd=1)
    }
  }
  
  par(new=T)
  plot(0, -1, axes=F, ylab=NA, xlab=NA, xlim=x_limit, ylim=y_limit_2, lwd=8, bty='n')
  points(x2, y2, col='green4', pch=4, cex=pcex)
  lines(fit2, col='green4', lwd=2)
  axis(side = 4)
  mtext(side = 4, line = 3, y_name[2])
  
  par(par1)
  dev.off()
  return(0)
}


plot_ts_on_ts <- function(ts1, ts2, file, des=plotPath, title, axis_name=c('SIF','SWR'), y_limit=0, x_limit=0, t_limit=0, pheno=0){
  png(file=paste(des, file, sep=''), width=1500, height=1500, pointsize=20)
  if(min(y_limit==0)){
    y_margin <- (max(ts1[, 2]) - min(ts1[, 2]))*0.2
    y_limit <- c(min(ts1[, 2]) - y_margin, max(ts1[, 2]) + y_margin)
  }
  if(min(x_limit==0)){
    x_margin <- (max(ts2[, 2]) - min(ts2[, 2]))*0.2
    x_limit <- c(min(ts2[, 2]) - x_margin, max(ts2[, 2]) + x_margin)
  }
  
  if(!min(t_limit==0)){
    ts1 <- ts1[ts1[, 1] >= (t_limit[1] * 1000), ]
    ts1 <- ts1[ts1[, 1] <= (t_limit[2] * 1000), ]
    ts2 <- ts2[ts2[, 1] >= (t_limit[1] * 1000), ]
    ts2 <- ts2[ts2[, 1] <= (t_limit[2] * 1000), ]
  }
  
  ts1 <- remove_winter(ts1, pheno_500m, T)
  ts2 <- remove_winter(ts2, pheno_500m, T)
  
  ts1 <- match_data(ts1, ts2)
  ts2 <- match_data(ts2, ts1)
  
  x1 <- ts1[, 1] - floor(ts1[, 1] / 1000) * 1000
  y1 <- ts1[, 2]
  
  x2 <- ts2[, 1] - floor(ts2[, 1] / 1000) * 1000
  y2 <- ts2[, 2]
  
  layout(t(1:2),widths=c(6,1))
  par(mar=c(4,4,1,0.5))
  plot(-9999,-9999,main=title,ylab=axis_name[1],xlab=axis_name[2],xlim=x_limit,ylim=y_limit,lwd=8,bty='n')
  box(col='black',lwd=1)
  
  if(min(pheno==0)){
    doy_range <- c(95, 320)
    cr <- rev(rainbow((doy_range[2] - doy_range[1] + 1), end = 4 / 6))
    x1[x1 < doy_range[1]] <- doy_range[1]
    x1[x1 > doy_range[2]] <- doy_range[2]
    points(y1, y2, col=cr[(x1 - doy_range[1] + 1)], pch=16, cex=pcex)
    
    par(mar=c(20,5,20,2.5))
    image(y=doy_range[1]:doy_range[2],z=t(doy_range[1]:doy_range[2]), ylab='', col=cr, axes=F, main='', cex.main=.8)
    axis(4, cex.axis=0.8, mgp=c(0,.5,0))
  }else{
    grp <- pheno_group(x1, pheno)
    points(y1[grp==1], y2[grp==1], col='blue', pch=16, cex=pcex)
    points(y1[grp==2], y2[grp==2], col='green4', pch=16, cex=pcex)
    points(y1[grp==3], y2[grp==3], col='red', pch=16, cex=pcex)
    par(mar=c(0,2,0,0))
    plot.new()
    legend(0, 0.5, c('Start', 'Peak', 'End'), col=c('blue', 'green4', 'red'), pch=16, cex=pcex)
  }
  
  dev.off()
  return(0)
}


plot_3ts <- function(ts1, ts2, ts3, file, des=plotPath, axis_name=c('SIF','SWR'), y_limit=0, x_limit=0, cr_range=0, t_limit=0){
  if(min(y_limit==0)){
    y_margin <- (max(ts1[, 2]) - min(ts1[, 2]))*0.2
    y_limit <- c(min(ts1[, 2]) - y_margin, max(ts1[, 2]) + y_margin)
  }
  if(min(x_limit==0)){
    x_margin <- (max(ts2[, 2]) - min(ts2[, 2]))*0.2
    x_limit <- c(min(ts2[, 2]) - x_margin, max(ts2[, 2]) + x_margin)
  }
  
  if(!min(t_limit==0)){
    ts1 <- ts1[ts1[, 1] >= (t_limit[1] * 1000), ]
    ts1 <- ts1[ts1[, 1] <= (t_limit[2] * 1000), ]
    ts2 <- ts2[ts2[, 1] >= (t_limit[1] * 1000), ]
    ts2 <- ts2[ts2[, 1] <= (t_limit[2] * 1000), ]
  }
  
  ts1 <- remove_winter(ts1, pheno_500m, T)
  ts2 <- remove_winter(ts2, pheno_500m, T)
  
  ts1 <- match_data(ts1, ts2)
  if(!min(ts3==0)){
    ts1 <- match_data(ts1, ts3)
    ts3 <- match_data(ts3, ts1)
    z1 <- ts3[, 2]
    z1[z1<cr_range[1]] <- cr_range[1]
    z1[z1>cr_range[2]] <- cr_range[2]
  }else{
    z1 <- rep(cr_range[1], nrow(ts1))
  }
  ts2 <- match_data(ts2, ts1)
  
  x_year <- floor(ts1[, 1] / 1000)
  years <- sort(unique(x_year))
  
  grp_name <- c('Early', 'Mid', 'Late')
  cr <- rev(rainbow((cr_range[2] - cr_range[1] + 1), end = 4 / 6))
  
  for(i in 1:length(grp_name)){
    png(file=paste(des, grp_name[i], '_', file, sep=''), width=1500, height=1500, pointsize=20)
    layout(t(1:2), widths=c(6, 1))
    par(mar=c(4, 4, 1, 0.5))
    plot(-9999, -9999, main=grp_name[i], ylab=axis_name[1], xlab=axis_name[2], xlim=x_limit, ylim=y_limit, lwd=8, bty='n')
    box(col='black',lwd=1)
    for(year in years){
      y1 <- ts1[x_year == year, 2]
      y2 <- ts2[x_year == year, 2]
      x1 <- ts1[x_year == year, 1] - floor(ts1[x_year == year, 1] / 1000) * 1000
      z2 <- z1[x_year == year]
      grp <- pheno_group(x1, unlist(pheno_500m[pheno_500m[, 1] == year, -1]))
      points(y1[grp==i], y2[grp==i], col=cr[(z2[grp==i] - round(cr_range[1]) + 1)], pch=16, cex=pcex)
    }
    par(mar=c(20,5,20,2.5))
    image(y=cr_range[1]:cr_range[2], z=t(cr_range[1]:cr_range[2]), ylab='', col=cr, axes=F, main='', cex.main=.8)
    axis(4, cex.axis=0.8, mgp=c(0, .5, 0))
    dev.off()
  }
  
  return(0)
}


plot_scatter <- function(x, y, z, file, des=plotPath, title='SIF vs. GPP', axis_name=c('SIF','GPP'), y_limit=0, x_limit=0){
  cr_range <- c(min(z), max(z))
  cr <- rev(rainbow((cr_range[2] - cr_range[1] + 1), end = 4 / 6))
  png(file=paste(des, file, sep=''), width=1500, height=1500, pointsize=20)
  layout(t(1:2), widths=c(6, 1))
  par(mar=c(4, 4, 1, 0.5))
  plot(-9999, -9999, main=title, ylab=axis_name[1], xlab=axis_name[2], xlim=x_limit, ylim=y_limit, lwd=8, bty='n')
  box(col='black', lwd=1)
  points(x, y, col=cr[round(z - cr_range[1] + 1)], pch=16, cex=pcex)
  par(mar=c(20,5,20,2.5))
  image(y=cr_range[1]:cr_range[2], z=t(cr_range[1]:cr_range[2]), ylab='', col=cr, axes=F, main='', cex.main=.8)
  axis(4, cex.axis=0.8, mgp=c(0, .5, 0))
  dev.off()
  return(0)
}


plot_rad <- function(des, x_limit=c(2016, 2018)){
  y_name <- 'SWR (W/m2)'
  plot_ts(rad_daily[, c(1, 3)], 'SWR_Daily.png', des, 'GOES Short Wave Radiation', y_name, rad_range, pheno_500m, x_limit)
  return(0)
}


plot_gsif <- function(des, x_limit=c(2016, 2018)){
  y_name <- 'GOSAT SIF (mW/m2/sr/nm)'
  plot_ts(gsif[, c(1, 17)], 'GOSAT_SIF.png', des, 'GOSAT SIF', y_name, SIF_range, pheno_500m, x_limit)
  return(0)
}


plot_nbar <- function(des, x_limit=c(2007, 2017)){
  plot_ts(EVI_500m, 'EVI_500m.png', des, 'MODIS EVI 500m', 'EVI', EVI_range, pheno_500m, x_limit)
  plot_ts(LSWI_500m, 'LSWI_500m.png', des, 'MODIS LSWI 500m', 'LSWI', LSWI_range, pheno_500m, x_limit)
  plot_ts(EVI_CMG, 'EVI_CMG.png', des, 'MODIS EVI CMG', 'EVI', EVI_range, pheno_500m, x_limit)
  plot_ts(LSWI_CMG, 'LSWI_CMG.png', des, 'MODIS LSWI CMG', 'LSWI', LSWI_range, pheno_500m, x_limit)
  return(0)
}


plot_sif <- function(des, x_limit=c(2007, 2017)){
  y_name1 <- 'GOME-2 SIF (mW/m2/sr/nm)'
  y_name2 <- 'GOME-2 SIF Par Normalized (mW/m2/sr/nm)'
  plot_ts(sif_daily[,c(1,2)], 'SIF_Daily.png', des, 'GOME-2 SIF Daily', y_name1, SIF_range, pheno_500m, x_limit)
  plot_ts(sif_weekly[,c(1,2)], 'SIF_Weekly.png', des, 'GOME-2 SIF Weekly', y_name1, SIF_range, pheno_500m, x_limit)
  plot_ts(sif_monthly[,c(1,2)], 'SIF_Monthly.png', des, 'GOME-2 SIF Monthly', y_name1, SIF_range, pheno_500m, x_limit)
  plot_ts(sif_daily[,c(1,5)], 'SIF_PAR_Daily.png', des, 'GOME-2 SIF PAR Normalized Daily', y_name2, SIF_range, pheno_500m, x_limit)
  plot_ts(sif_weekly[,c(1,5)], 'SIF_PAR_Weekly.png', des, 'GOME-2 SIF PAR Normalized Weekly', y_name2, SIF_range, pheno_500m, x_limit)
  plot_ts(sif_monthly[,c(1,5)], 'SIF_PAR_Monthly.png', des, 'GOME-2 SIF PAR Normalized Monthly', y_name2, SIF_range, pheno_500m, x_limit)
  return(0)
}


plot_ems <- function(des, x_limit=c(2007, 2017)){
  y_name <- 'GPP (umol/m2/s)'
  plot_ts(ems_daily, 'GPP_Daily.png', des, 'HF EMS Tower GPP', y_name, GPP_range, pheno_500m, x_limit)
  plot_ts(ems_weekly, 'GPP_Weekly.png', des, 'HF EMS Tower GPP', y_name, GPP_range, pheno_500m, x_limit)
  plot_ts(ems_monthly, 'GPP_Monthly.png', des, 'HF EMS Tower GPP', y_name, GPP_range, pheno_500m, x_limit)
  plot_ts(ems_yearly, 'GPP_Yearly.png', des, 'HF EMS Tower GPP', y_name, GPP_range, pheno_500m, x_limit)
  return(0)
}

