# data path
dataPath <- paste(dd, 'SIF/data/ems', sep='')
plotPath <- paste(dd, 'SIF/plots/', sep='')
sifPath <- paste(dd, 'SIF/data/sif', sep='')
nbarPath <- paste(dd, 'SIF/data/nbar', sep='')
phenoPath <- paste(dd, 'SIF/data/pheno/HF_Pheno.csv', sep='')
radPath <- paste(dd, 'SIF/data/rad/GOES_SWR.csv', sep='')

# data value range
EVI_range <- c(0.2, 0.8)
LSWI_range <- c(0.3, 0.8)
GPP_range <- c(-5, 50)
SIF_range <- c(-0.5, 4.5)
GSIF_range <- c(-5,20)
rad_range <- c(0, 1000)
par_range <- c(500, 1500)

# parameters
pcex <- 1.6

