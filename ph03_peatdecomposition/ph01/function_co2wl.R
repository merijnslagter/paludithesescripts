co2wt <- function(wl){
  copy <- wl
  for (i in 1:length(wl)){
    funmid <- 8.14
    funhighsteady <- 16
    funlow <- function(x){- x / 2.456}
    funhigh <- function(x){10 + x * 0.2}
    testfun <- function(rastercell){
      if (rastercell < -20){
	co2 <- funlow(rastercell)}
      if (rastercell >= -20 & rastercell < 0){
	co2 <- funmid}
      if (rastercell >= 0){
	co2 <- funhigh(rastercell)}
      if (rastercell > 25){
	co2 <- funhighsteady}
	return(co2)}
    ifelse (is.na(wl[i]), co2 <- NA, co2 <- testfun(wl[i]))
    copy[i] <- co2}
    return(copy)
    }
    
