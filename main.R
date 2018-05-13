wd <- '/Users/xjtang/Applications/GitHub/SIF/'
dd <- '/Users/xjtang/Applications/Dropbox/'
setwd(wd)
source('const.R')
source('process.R')
source('io.R')
source('visual.R')
source('model.R')
source('utility.R')


read_data <- function(){
  if(!exists('ems')){
    ems1 <<- read.table(paste(dataPath, '/HF_2016_filled', sep=''), header=T, sep='\t', stringsAsFactors=F, skip=1)
    ems2 <<- read.table(paste(dataPath, '/HF_9215_filled', sep=''), header=T, sep='\t', stringsAsFactors=F)
    ems <<- rbind(ems1, ems2)
    ems <<- ems[ems[,4]==9,]
  }
  ems_daily <<- read.table(paste(dataPath, '/HF_daily_sum.csv', sep=''), header=T, sep=',', stringsAsFactors=F)
  ems_weekly <<- read.table(paste(dataPath, '/HF_weekly_sum.csv', sep=''), header=T, sep=',', stringsAsFactors=F)
  ems_monthly <<- read.table(paste(dataPath, '/HF_monthly_sum.csv', sep=''), header=T, sep=',', stringsAsFactors=F)
  ems_yearly <<- read.table(paste(dataPath, '/HF_yearly_sum.csv', sep=''), header=T, sep=',', stringsAsFactors=F)
  ems_weekly[, 1] <<- sapply(ems_weekly[, 1], woy2doy)
  ems_monthly[, 1] <<- sapply(ems_monthly[, 1], moy2doy)
  ems_yearly[, 1] <<- ems_yearly[, 1] * 1000 + 1
  
  par_weekly <<- read.table(paste(dataPath, '/HF_PAR_weekly_median.csv', sep=''), header=T, sep=',', stringsAsFactors=F)
  ems_weekly2 <<- read.table(paste(dataPath, '/HF_weekly_median.csv', sep=''), header=T, sep=',', stringsAsFactors=F)
  ems_monthly2 <<- read.table(paste(dataPath, '/HF_monthly_median.csv', sep=''), header=T, sep=',', stringsAsFactors=F)
  ems_weekly2[, 1] <<- sapply(ems_weekly2[, 1], woy2doy)
  ems_monthly2[, 1] <<- sapply(ems_monthly2[, 1], moy2doy)
  par_weekly[, 1] <<- sapply(par_weekly[, 1], woy2doy)
  par_weekly[, 2] <<- -par_weekly[, 2]
  
  sif_daily <<- read.table(paste(sifPath, '/SIF_Daily.csv', sep=''), header=F, sep=',', stringsAsFactors=F)
  sif_weekly <<- read.table(paste(sifPath, '/SIF_Weekly.csv', sep=''), header=F, sep=',', stringsAsFactors=F)
  sif_monthly <<- read.table(paste(sifPath, '/SIF_Monthly.csv', sep=''), header=F, sep=',', stringsAsFactors=F)
  sif_daily <<- sif_QA(sif_daily, 1, F)
  sif_weekly <<- sif_QA(sif_weekly, 3, T, 7)
  sif_monthly <<- sif_QA(sif_monthly, 5, T, 30)
  sif_daily[, c(2, 5)] <<- sif_daily[, c(2, 5)] / 1000
  sif_weekly[, c(2, 5)] <<- sif_weekly[, c(2, 5)] / 1000
  sif_monthly[, c(2, 5)] <<- sif_monthly[, c(2, 5)] / 1000
  sif_weekly[, 1] <<- sapply(sif_weekly[, 1], woy2doy)
  sif_monthly[, 1] <<- sapply(sif_monthly[, 1], moy2doy)
  
  gsif <<- read.table(paste(sifPath, '/gosat_sif.txt', sep=''), header=T, skip=10, sep='', stringsAsFactors=F)
  gsif <<- gosat_pre(gsif)
  
  nbar_500m <<- read.table(paste(nbarPath, '/HF_EMS_NBAR.csv', sep=''), header=F, sep=',', stringsAsFactors=F)
  nbar_cmg <<- read.table(paste(nbarPath, '/NBAR_CMG.csv', sep=''), header=F, sep=',', stringsAsFactors=F)
  EVI_500m <<- nbar_QA(nbar_500m, 'EVI')
  LSWI_500m <<- nbar_QA(nbar_500m, 'LSWI')
  EVI_CMG <<- nbar_QA(nbar_cmg, 'EVI')
  LSWI_CMG <<- nbar_QA(nbar_cmg, 'CMG')
  EVI_500m_weekly <<- as.data.frame(EVI_to_weekly(EVI_500m))
  EVI_500m_weekly[, 1] <<- sapply(EVI_500m_weekly[, 1], woy2doy)
  EVI_500m_monthly <<- as.data.frame(EVI_to_monthly(EVI_500m))
  EVI_500m_monthly[, 1] <<- sapply(EVI_500m_monthly[, 1], moy2doy)
  
  pheno_500m <<- read.table(phenoPath, header=F, sep=',', stringsAsFactors=F)
  pheno_500m <<- pheno_pre(pheno_500m)
  
  rad_daily <<- read.table(radPath, header=F, sep=',', stringsAsFactors=F)
  rad_daily <<- rad_daily[rad_daily[, 4] > 3, ]
  rad_daily[, 3] <<- rad_daily[, 3] / 10
  
  ems_daily <<- remove_winter(ems_daily, pheno_500m)
  ems_weekly <<- remove_winter(ems_weekly, pheno_500m)
  ems_monthly <<- remove_winter(ems_monthly, pheno_500m)
  sif_daily <<- remove_winter(sif_daily, pheno_500m)
  sif_weekly <<- remove_winter(sif_weekly, pheno_500m)
  sif_monthly <<- remove_winter(sif_monthly, pheno_500m)
  
  asum <<- annual_sum()
  
  return(0)
}


sum_ems <- function(ems, des){
  save_ems(ems_to_daily(ems), paste(des, '/HF_daily_sum.csv', sep=''))
  save_ems(ems_to_weekly(ems), paste(des, '/HF_weekly_sum.csv', sep=''))
  save_ems(ems_to_weekly(ems, mtd='m'), paste(des, '/HF_weekly_median.csv', sep=''))
  save_ems(ems_to_monthly(ems, mtd='m'), paste(des, '/HF_monthly_median.csv', sep=''))
  save_ems(ems_to_weekly(ems, var=29, mtd='m'), paste(des, '/HF_PAR_weekly_median.csv', sep=''))
  save_ems(ems_to_monthly(ems), paste(des, '/HF_monthly_sum.csv', sep=''))
  save_ems(ems_to_yearly(ems), paste(des, '/HF_yearly_sum.csv', sep=''))
  return(0)
}


plot_all <- function(des, x_limit=c(2007, 2017)){
  plot_nbar(des, x_limit)
  plot_sif(des, x_limit)
  plot_ems(des, x_limit)
  plot_rad(des)
  plot_gsif(des)
  return(0)
}


plot_all_2 <- function(des, x_limit=c(2007, 2017)){
  plot_2ts(sif_daily[,c(1,2)], ems_daily, 'SIF_GPP_Daily.png', des, 'GOME-2 SIF vs. GPP (Daily)', c('GOME-2 SIF','GPP'), SIF_range, GPP_range, pheno_500m, x_limit, T)
  plot_2ts(sif_weekly[,c(1,2)], ems_weekly, 'SIF_GPP_Weekly.png', des, 'GOME-2 SIF vs. GPP (Weekly)', c('GOME-2 SIF','GPP'), SIF_range, GPP_range, pheno_500m, x_limit, F)
  plot_2ts(sif_monthly[,c(1,2)], ems_monthly, 'SIF_GPP_Monthly.png', des, 'GOME-2 SIF vs. GPP (Monthly)', c('GOME-2 SIF','GPP'), SIF_range, GPP_range, pheno_500m, x_limit, F)
  
  plot_2ts(sif_daily[,c(1,2)], sif_daily[,c(1,5)], 'SIF_SIFP_Daily.png', des, 'GOME-2 SIF vs. GOME-2 SIF PAR Normalized (Daily)', c('GOME-2 SIF','GOME-2 SIF PAR Normalized'), SIF_range, SIF_range, pheno_500m, x_limit, F)
  plot_2ts(sif_weekly[,c(1,2)], sif_weekly[,c(1,5)], 'SIF_SIFP_Weekly.png', des, 'GOME-2 SIF vs. GOME-2 SIF PAR Normalized (Weekly)', c('GOME-2 SIF','GOME-2 SIF PAR Normalized'), SIF_range, SIF_range, pheno_500m, x_limit, F)
  plot_2ts(sif_monthly[,c(1,2)], sif_monthly[,c(1,5)], 'SIF_SIFP_Monthly.png', des, 'GOME-2 SIF vs. GOME-2 SIF PAR Normalized (Monthly)', c('GOME-2 SIF','GOME-2 SIF PAR Normalized'), SIF_range, SIF_range, pheno_500m, x_limit, F)
  
  plot_2ts(sif_daily[,c(1,5)], ems_daily, 'SIFP_GPP_Daily.png', des, 'GOME-2 SIF PAR Normalized vs. GPP (Daily)', c('GOME-2 SIF PAR Normalized','GPP'), SIF_range, GPP_range, pheno_500m, x_limit, T)
  plot_2ts(sif_weekly[,c(1,5)], ems_weekly, 'SIFP_GPP_Weekly.png', des, 'GOME-2 SIF PAR Normalized vs. GPP (Weekly)', c('GOME-2 SIF PAR Normalized','GPP'), SIF_range, GPP_range, pheno_500m, x_limit, F)
  plot_2ts(sif_monthly[,c(1,5)], ems_monthly, 'SIFP_GPP_Monthly.png', des, 'GOME-2 SIF PAR Normalized vs. GPP (Monthly)', c('GOME-2 SIF PAR Normalized','GPP'), SIF_range, GPP_range, pheno_500m, x_limit, F)
  
  plot_2ts(sif_daily[,c(1,2)], gsif[,c(1,17)], 'SIF_GSIF_Daily_Match.png', des, 'GOME-2 SIF vs. GOSAT SIF (Daily)', c('GOME-2 SIF','GOSAT SIF'), SIF_range, SIF_range, pheno_500m, c(2016, 2018), T)
  plot_2ts(sif_daily[,c(1,2)], gsif[,c(1,17)], 'SIF_GSIF_Daily.png', des, 'GOME-2 SIF vs. GOSAT SIF (Daily)', c('GOME-2 SIF','GOSAT SIF'), SIF_range, SIF_range, pheno_500m, c(2016, 2018), F)
  plot_2ts(sif_weekly[,c(1,2)], gsif[,c(1,17)], 'SIF_GSIF_Weekly.png', des, 'GOME-2 SIF (Weekly) vs. GOSAT SIF (Daily)', c('GOME-2 SIF (Weekly)','GOSAT SIF'), SIF_range, SIF_range, pheno_500m, c(2016, 2018), F)
  plot_2ts(sif_monthly[,c(1,2)], gsif[,c(1,17)], 'SIF_GSIF_Monthly.png', des, 'GOME-2 SIF (Monthly) vs. GOSAT SIF (Daily)', c('GOME-2 SIF (Monthly)','GOSAT SIF'), SIF_range, SIF_range, pheno_500m, c(2016, 2018), F)
  
  plot_2ts(gsif[,c(1,17)], ems_daily, 'GSIF_GPP_Daily.png', des, 'GOSAT SIF vs. GPP (Daily)', c('GOSAT-2 SIF','GPP'), SIF_range, GPP_range, pheno_500m, c(2016, 2018), F)
  plot_2ts(gsif[,c(1,17)], ems_daily, 'GSIF_GPP_Daily_Match.png', des, 'GOSAT SIF vs. GPP (Daily)', c('GOSAT-2 SIF','GPP'), SIF_range, GPP_range, pheno_500m, c(2016, 2018),T)
  plot_2ts(gsif[,c(1,17)], ems_weekly, 'GSIF_GPP_Weekly.png', des, 'GOSAT SIF vs. GPP (Weekly)', c('GOSAT-2 SIF','GPP'), SIF_range, GPP_range, pheno_500m, c(2016, 2018), F)
  
  plot_2ts(EVI_500m, EVI_CMG, 'EVI500_EVICMG_Daily.png', des, 'EVI 500m vs. EVI CMG (Daily)', c('EVI 500m','EVI CMG'), EVI_range, EVI_range, pheno_500m, x_limit, F)
  
  plot_2ts(EVI_500m, ems_daily, 'EVI500_GPP_Daily.png', des, 'EVI 500m vs. GPP (Daily)', c('EVI 500m','GPP'), EVI_range, GPP_range, pheno_500m, x_limit, F)
  plot_2ts(EVI_500m, ems_weekly, 'EVI500_GPP_Weekly.png', des, 'EVI 500m vs. GPP (Weekly)', c('EVI 500m','GPP'), EVI_range, GPP_range, pheno_500m, x_limit, F)
  plot_2ts(EVI_500m, ems_monthly, 'EVI500_GPP_Monthly.png', des, 'EVI 500m vs. GPP (Monthly)', c('EVI 500m','GPP'), EVI_range, GPP_range, pheno_500m, x_limit, F)
  
  plot_2ts(LSWI_500m, ems_daily, 'LSWI500_GPP_Daily.png', des, 'LSWI 500m vs. GPP (Daily)', c('LSWI 500m','GPP'), LSWI_range, GPP_range, pheno_500m, x_limit, F)
  plot_2ts(LSWI_500m, ems_weekly, 'LSWI500_GPP_Weekly.png', des, 'LSWI 500m vs. GPP (Weekly)', c('LSWI 500m','GPP'), LSWI_range, GPP_range, pheno_500m, x_limit, F)
  plot_2ts(LSWI_500m, ems_monthly, 'LSWI500_GPP_Monthly.png', des, 'LSWI 500m vs. GPP (Monthly)', c('LSWI 500m','GPP'), LSWI_range, GPP_range, pheno_500m, x_limit, F)
  
  plot_2ts(EVI_CMG, sif_daily[,c(1,5)], 'EVICMG_SIF_Daily.png', des, 'EVI CMG vs. GOME-2 SIF (Daily)', c('EVI CMG','GOME-2 SIF'), EVI_range, SIF_range, pheno_500m, x_limit, F)
  plot_2ts(EVI_CMG, sif_weekly[,c(1,5)], 'EVICMG_SIF_Weekly.png', des, 'EVI CMG vs. GOME-2 SIF (Weekly)', c('EVI CMG','GOME-2 SIF'), EVI_range, SIF_range, pheno_500m, x_limit, F)
  plot_2ts(EVI_CMG, sif_monthly[,c(1,5)], 'EVICMG_SIF_Monthly.png', des, 'EVI CMG vs. GOME-2 SIF (Monthly)', c('EVI CMG','GOME-2 SIF'), EVI_range, SIF_range, pheno_500m, x_limit, F)
  
  plot_2ts(EVI_500m, sif_daily[,c(1,5)], 'EVI500_SIF_Daily.png', des, 'EVI 500m vs. GOME-2 SIF (Daily)', c('EVI 500m','GOME-2 SIF'), EVI_range, SIF_range, pheno_500m, x_limit, F)
  plot_2ts(EVI_500m, sif_weekly[,c(1,5)], 'EVI500_SIF_Weekly.png', des, 'EVI 500m vs. GOME-2 SIF (Weekly)', c('EVI 500m','GOME-2 SIF'), EVI_range, SIF_range, pheno_500m, x_limit, F)
  plot_2ts(EVI_500m, sif_monthly[,c(1,5)], 'EVI500_SIF_Monthly.png', des, 'EVI 500m vs. GOME-2 SIF (Monthly)', c('EVI 500m','GOME-2 SIF'), EVI_range, SIF_range, pheno_500m, x_limit, F)
  
  plot_2ts(LSWI_500m, sif_daily[,c(1,5)], 'LSWI500_SIF_Daily.png', des, 'LSWI 500m vs. GOME-2 SIF (Daily)', c('LSWI 500m','GOME-2 SIF'), LSWI_range, SIF_range, pheno_500m, x_limit, F)
  plot_2ts(LSWI_500m, sif_weekly[,c(1,5)], 'LSWI500_SIF_Weekly.png', des, 'LSWI 500m vs. GOME-2 SIF (Weekly)', c('LSWI 500m','GOME-2 SIF'), LSWI_range, SIF_range, pheno_500m, x_limit, F)
  plot_2ts(LSWI_500m, sif_monthly[,c(1,5)], 'LSWI500_SIF_Monthly.png', des, 'LSWI 500m vs. GOME-2 SIF (Monthly)', c('LSWI 500m','GOME-2 SIF'), LSWI_range, SIF_range, pheno_500m, x_limit, F)
  
  return(0)
}


plot_all_3 <- function(des, t_limit=c(2016, 2017), pheno=0){
  plot_ts_on_ts(rad_daily[,c(1,3)], ems_daily, 'GPP_Rad_doy.png', des, 'GPP vs. SWR', c('GPP','SWR'), GPP_range, rad_range, t_limit, 0)
  plot_ts_on_ts(rad_daily[,c(1,3)], ems_daily, 'GPP_Rad_pheno.png', des, 'GPP vs. SWR', c('GPP','SWR'), GPP_range, rad_range, t_limit, unlist(pheno_500m[16,c(2,3,4,5)]))
  
  plot_ts_on_ts(rad_daily[,c(1,3)], sif_daily[,c(1,2)], 'SIF_Rad_doy.png', des, 'GOME-2 SIF vs. SWR', c('GOME-2 SIF','SWR'), SIF_range, rad_range, t_limit, 0)
  plot_ts_on_ts(rad_daily[,c(1,3)], sif_daily[,c(1,2)], 'SIF_Rad_pheno.png', des, 'GOME-2 SIF vs. SWR', c('GOME-2 SIF','SWR'), SIF_range, rad_range, t_limit, unlist(pheno_500m[16,c(2,3,4,5)]))
  
  plot_ts_on_ts(rad_daily[,c(1,3)], sif_daily[,c(1,5)], 'SIFP_Rad_doy.png', des, 'GOME-2 SIF PAR Normalized vs. SWR', c('GOME-2 SIF PAR Normalized','SWR'), SIF_range, rad_range, t_limit, 0)
  plot_ts_on_ts(rad_daily[,c(1,3)], sif_daily[,c(1,5)], 'SIFP_Rad_pheno.png', des, 'GOME-2 SIF PAR Normalized vs. SWR', c('GOME-2 SIF PAR Normalized','SWR'), SIF_range, rad_range, t_limit, unlist(pheno_500m[16,c(2,3,4,5)]))
  
  plot_ts_on_ts(rad_daily[,c(1,3)], gsif[,c(1,17)], 'GSIF_Rad_doy.png', des, 'GOSAT SIF vs. SWR', c('GOSAT SIF','SWR'), SIF_range, rad_range, t_limit, 0)
  plot_ts_on_ts(rad_daily[,c(1,3)], gsif[,c(1,17)], 'GSIF_Rad_pheno.png', des, 'GOSAT SIF vs. SWR', c('GOSAT SIF','SWR'), SIF_range, rad_range, t_limit, unlist(pheno_500m[16,c(2,3,4,5)]))
  
  plot_ts_on_ts(rad_daily[,c(1,3)], EVI_500m, 'EVI500_Rad_doy.png', des, 'EVI 500m vs. SWR', c('EVI 500m','SWR'), EVI_range, rad_range, t_limit, 0)
  plot_ts_on_ts(rad_daily[,c(1,3)], EVI_500m, 'EVI500_Rad_pheno.png', des, 'EVI 500m vs. SWR', c('EVI 500m','SWR'), EVI_range, rad_range, t_limit, unlist(pheno_500m[16,c(2,3,4,5)]))
  
  plot_ts_on_ts(sif_daily[,c(1,2)], ems_daily, 'GPP_SIF_doy.png', des, 'GPP vs. GOME-2 SIF', c('GOME-2 SIF','SWR'), GPP_range, SIF_range, t_limit, 0)
  plot_ts_on_ts(sif_daily[,c(1,2)], ems_daily, 'GPP_SIF_pheno.png', des, 'GPP vs. GOME-2 SIF', c('GOME-2 SIF','SWR'), GPP_range, SIF_range, t_limit, unlist(pheno_500m[16,c(2,3,4,5)]))
  
  plot_ts_on_ts(sif_daily[,c(1,2)], ems_daily, 'GPP_SIF_all_doy.png', des, 'GPP vs. GOME-2 SIF', c('GOME-2 SIF','SWR'), GPP_range, SIF_range, 0, 0)
  plot_ts_on_ts(sif_daily[,c(1,2)], ems_daily, 'GPP_SIF_all_pheno.png', des, 'GPP vs. GOME-2 SIF', c('GOME-2 SIF','SWR'), GPP_range, SIF_range, 0, unlist(pheno_500m[16,c(2,3,4,5)]))
  
  plot_ts_on_ts(sif_daily[,c(1,5)], sif_daily[,c(1,2)], 'SIF_SIFP_all_doy.png', des, 'GOME-2 SIF vs. GOME-2 SIF PAR Normalized', c('GOME-2 SIF','GOME-2 SIF PAR Normalized'), SIF_range, SIF_range, 0, 0)
  
  plot_ts_on_ts(gsif[,c(1,17)], sif_daily[,c(1,2)], 'SIF_GSIF_doy_all.png', des, 'GOME-2 SIF vs. GOSAT SIF', c('GOME-2 SIF','GOSAT SIF'), SIF_range, SIF_range, 0, 0)
  
  plot_ts_on_ts(EVI_CMG, EVI_500m, 'EVI500_EVICMG_all_doy.png', des, 'EVI 500m vs. EVI CMG', c('EVI 500m','EVI CMG'), EVI_range, EVI_range, 0, 0)
  
  return(0)
}


plot_all_4 <- function(des, t_limit=c(2016, 2017), pheno=0){
  plot_3ts(sif_daily[, c(1, 2)], ems_daily, rad_daily[, c(1, 3)], 'GPP_SIF_RAD.png', des=plotPath, axis_name=c('GPP','SIF'), GPP_range, SIF_range, rad_range, t_limit=0)
  EVI <- match_data(EVI_500m, sif_daily[, c(1, 2)])
  plot_3ts(EVI, ems_daily, rad_daily[, c(1, 3)], 'GPP_EVI_RAD.png', des=plotPath, axis_name=c('GPP','EVI'), GPP_range, EVI_range, rad_range, t_limit=0)
  
  col_by_years <- cbind(ems_daily[, 1], floor(ems_daily[, 1]/1000))
  plot_3ts(sif_daily[, c(1, 2)], ems_daily, col_by_years, 'GPP_SIF_all.png', des=plotPath, axis_name=c('GPP','SIF'), GPP_range, SIF_range, c(2007, 2017), t_limit=0)
  plot_3ts(EVI, ems_daily, col_by_years, 'GPP_EVI_all.png', des=plotPath, axis_name=c('GPP','EVI'), GPP_range, EVI_range, c(2007, 2017), t_limit=0)
  
  EVI2 <- match_data(EVI_500m_weekly, sif_weekly[, c(1, 2)])
  SIF <- match_data(sif_weekly[, c(1, 2)], EVI_500m_weekly)
  plot_3ts(SIF, ems_weekly2, par_weekly, 'GPP_SIF_PAR_weekly.png', des=plotPath, axis_name=c('GPP Weekly Median','SIF Weekly Mean'), GPP_range, SIF_range, par_range, t_limit=0)
  plot_3ts(EVI2, ems_weekly2, par_weekly, 'GPP_EVI_PAR_weekly.png', des=plotPath, axis_name=c('GPP Weekly Median','EVI Weekly Max'), GPP_range, EVI_range, par_range, t_limit=0)
  
  EVI2 <- match_data(EVI_500m_monthly, sif_monthly[, c(1, 2)])
  SIF <- match_data(sif_monthly[, c(1, 2)], EVI_500m_monthly)
  plot_3ts(SIF, ems_monthly2, col_by_years, 'GPP_SIF_PAR_monthly.png', des=plotPath, axis_name=c('GPP Monthly Median','SIF Monthly product'), GPP_range, SIF_range, c(2007, 2017), t_limit=0)
  plot_3ts(EVI2, ems_monthly2, col_by_years, 'GPP_EVI_PAR_monthly.png', des=plotPath, axis_name=c('GPP Monthly Median','EVI Monthly Max'), GPP_range, EVI_range, c(2007, 2017), t_limit=0)
  
  return(0)
}


plot_all_5 <- function(des){
  plot_scatter(asum[, 4], asum[, 5], asum[, 2], 'GPP_SIF_annual.png', des, title='SIF vs. GPP (color by peak length)', axis_name=c('GOME-2 SIF (annual peak season median)','GPP (annual peak season sum normalized by length)'), x_limit=c(20, 35), y_limit=c(1.5, 2.5))
  plot_scatter(asum[, 4], asum[, 6], asum[, 2], 'GPP_EVI_annual.png', des, title='EVI vs. GPP (color by peak length)', axis_name=c('MODIS 500m EVI (annual peak season max)','GPP (annual peak season sum normalized by length)'), x_limit=c(20, 35), y_limit=c(0.6, 0.8))
  return(0)
}

