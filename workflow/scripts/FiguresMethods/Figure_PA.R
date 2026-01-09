
################################################################################
#Function
################################################################################

plot_P_PA<-function(data, period, PA_N=NULL){
  
  load("~/Internship UNIL/PCA_ax1-2.RData")
  rastclim<-rast(climate_data) 
  
  plot(rastclim[[1]],col='grey',legend=F, main=paste0("Presences and Pseudoabsences ", period))
  points(data$decimallongitude[data$Occurrence==1 ],
         data$decimallatitude[data$Occurrence==1 ],pch='+',col='red')
  
  if( is.null(PA_N)){
    points(data$decimallongitude[data$Occurrence==0 ],
             data$decimallatitude[data$Occurrence==0 ],  pch=19, cex=0.3) 
  }
  
 else{
   points(data$decimallongitude[data$Occurrence==0 ],
          data$decimallatitude[data$Occurrence==0 ],col=data$PA_N,pch=19, cex=0.3)
   }
 
  legend("left",bty= "n", cex=0.7, legend=c("Presences (P)", "Pseudoabsences (PA)"), 
         col=c("red", "black"), pch = c("+",19))
  
  
}

################################################################################
#Code
################################################################################
occ_PA <- read.csv(occurrenes_PA)

p<-plot_P_PA(occ_PA[occ_PA$S == "S1", ], "1885-1970", PA_N = TRUE)

for (i in 1:10) {
  
  plot_P_PA(occ_PA[occ_PA$S == "S1" & occ_PA$PA_N %in% c(NA, i), ], paste0("1885-1970; PA_N=", i))
  plot_P_PA(occ_PA[occ_PA$S == "S2" & occ_PA$PA_N %in% c(NA, i), ], paste0("1970-2025; PA_N=", i))
  
}



