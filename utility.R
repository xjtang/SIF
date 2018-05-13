isleap <- function(year){
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}


moy2doy <- function(moy){
  year <- floor(moy/100)
  month <- moy - floor(moy/100)*100
  dim <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  if(isleap(year)){
    dim[2] <- 29
  }
  return(sum(dim[1:month])-floor(dim[month]/2)+year*1000)
}


woy2doy <- function(woy){
  year <- floor(woy/100)
  week <- woy - floor(woy/100)*100
  return(week*7-3+year*1000)
}


match_data <- function(a, b){
  for(i in 1:nrow(a)){
    if(sum(b[, 1] == a[i, 1]) == 0){
      a[i, 1] <- -9999
    }
  }
  a <- a[a[, 1] != -9999, ]
  return(a)
}


date2doy <- function(y, m, d){
  if(isleap(y)){
    dom <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  }else{
    dom <- c(31,29,31,30,31,30,31,31,30,31,30,31)
  }
  doy <- y * 1000 + sum(dom[1:m]) - dom[m] + d
  return(doy)
}

