predictemp<-function(data, ahead=(12*5), depth=NULL){

  #PRE-RUNING
  #CHECKING INPUT &
  #PARTICULAR REQUEST VS. FULL REQUEST
  #----------------------------------------------
  if(is.null(depth)){
    depthIN=c(seq(1,4))}
  else{
    if(depth==0 || depth==1){
      depthIN=1
    } else if(depth==-20 || depth==2) {
      depthIN=2
    } else if(depth==-50 || depth==3) {
      depthIN=3
    } else if(depth==-80 || depth==4) {
      depthIN=4
    } else {warning("¡¡INADEQUATE INPUT!!\n ------------------------------------------\n <Depth> wrongly indicated. \n------------------------------------------\n Admited values:\n[1-4]\n 1 ||   0:   0m \n 2 || -20: -20m \n 3 || -50: -50m \n 4 || -80: -80m \n <<NULL>>: All from above\n ------------------------------------------\n\n")
      break}
  }

  #PREPARING TIME-SERIES STRUCTURE
  ##################################
  timemonth<-c()
  i<-0
  for (y in seq(2000,2017)){
    for (m in c("Jan","Feb","Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec")){
      i=i+1
      timemonth[i]<-paste(y,m,sep="")
    }}
  rm(i)

  dataTemp<-array(dim=c(12*18,4),
                  dimnames=list(Month=timemonth,
                                Depth_m=c(0,-20,-50,-80)))

  for (y in seq(2000-1999,2017-1999)){
    dataTemp[(1+12*(y-1)):(12+12*(y-1)),1:4]<-data[1:12,,y]
  }

  #PREDICTING DEMANDED VALUES
  ##################################
  install.packages("forecast")
  library(forecast)

  par(mfrow=c(2,2), mar=c(5,4,4,5))
  for (d in depthIN){
    plot(forecast::forecast(forecast::auto.arima(ts(dataTemp[,d], frequency = 12, start = c(2000, 1))),ahead,level=95),
         main=paste("Forecast for",ahead,"months ahead \n at", "depth level", d),
         ylab="Temperature")
  }
}
