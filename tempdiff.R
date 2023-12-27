tempdiff<-function(data, pre_data=NULL, year=NULL, month=NULL, depth=NULL, pre_period=FALSE, printing=FALSE, plotting=TRUE, info=TRUE){

  #PRE-RUNING
  #CHECKING INPUT &
  #PARTICULAR REQUEST VS. FULL REQUEST
  #----------------------------------------------

  #Particular Year vs. All Years
  if(is.null(year)){
    yearIN=c(seq(from=2000-1999,to=2017-1999))}
  else if ((year>=2000) & (year<=2017)){
    yearIN=year-1999}
  else {warning("¡¡INADEQUATE INPUT!!\n------------------------------------------\n <Year> not admitted.\n------------------------------------------\n \n Admited values:\n #[2000-2017]\n #<<NULL>>: All from above\n ------------------------------------------\n\n")
    break}

  #Particular Depth vs. All Depths
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

  ############################
  #EJERCICIO 3A
  ############################
  if (pre_period==FALSE){

    #Particular Month vs. All Months
    if(is.null(month)){
      monthIN=c(seq(from=1,to=11))}
    else if (month>=1 & month<=11) {
      monthIN=month}
    else {warning("¡¡INADEQUATE INPUT!! \n------------------------------------------\n <Month> wrongly indicated. \n------------------------------------------\n Admited values (number indicates initial month):\n[1-11]\n 1: February-January \n 2: March-February\n          ...       \n 10: November-October\n 11: December-November\n <<NULL>>: All from above\n ------------------------------------------\n\n")
      break}


    if(printing==TRUE){

      #Compute difference month by month
      for (y in yearIN) {
        for (d in depthIN){
          for (m in monthIN){
            cat("Difference between months(", m+1 ,"-", m,") in year", y+1999 ,"for depth level",d,":","\n",
                data[m+1,d,y]-data[m,d,y], "[",data[m+1,d,y],"-",data[m,d,y],"]\n")
          }}}
    }

    A<-array(dim=c(11,4,18),
             dimnames=list(MonthDiff=c(seq(1,11)),
                           Depth_m=c(0,-20,-50,-80),
                           Year=c(seq(2000,2017))))

    for (y in yearIN) {
      for (d in depthIN){
        for (m in monthIN){
          A[m,d,y]<-data1[m+1,d,y]-data1[m,d,y]
        }}}

    Aout<-A[monthIN,depthIN,yearIN]

    if (info==TRUE){
      Info=list("DIFFERENCE IN MEAN SEA TEMPERATURE",
                list("Depth CODE","1:   0m", "2: -20m", "3: -50m", "4: -80m"),
                list("MonthDiff CODE","1: February-January","2: March-February","...","10: November-October", "11: December-November", ">>Code indicates initial month<<"))
      result3A=list(info=Info,results=Aout)
    } else {
      result3A=Aout
    }

    #Graphics

    if(plotting==TRUE){
      for (y in yearIN) {
        for(d in depthIN){
          barplot(A[monthIN,d,y],
                  col="lightblue",
                  ylim=c(-5,5),
                  main="Sea Temperature Difference (ºC) between months",
                  sub=paste("At depth", d, "for the year", y+1999),
                  ylab="Temperature Diff (ºC)")

          abline(h=mean(A[,d,]), #Difference average at this depth for all the years studied
                 col="red")

        }}}

    return(result3A)

  }
  ############################
  #EJERCICIO 3B
  ############################
  else{
    if (is.null(pre_data)){
      warning("¡¡INADEQUATE INPUT!! \n------------------------------------------\n <pre_data> not indicated. \n------------------------------------------\n Please, provide the data to which <data> will be compared to.\n------------------------------------------\n\n")
    }
    else{

      #Particular Month vs. All Months
      if(is.null(month)){
        monthIN=c(seq(from=1,to=13))}
      else if (month>=1 & month<=13) {
        monthIN=month}
      else {warning("¡¡INADEQUATE INPUT!! \n------------------------------------------\n <Month> wrongly indicated. \n------------------------------------------\n Admited values:\n[1-13]\n 1: January \n 2: February\n          ...       \n 11: November\n 12: December\n 13: Year Average\n <<NULL>>: All from above\n ------------------------------------------\n\n")
        break}


      if(printing==TRUE){

        #Compute difference month by month
        for (y in yearIN) {
          for (d in depthIN){
            for (m in monthIN){
              cat("Difference between the month number", m ," in year", y+1999 ,"and previous period","for depth level",d,":","\n",
                  data[m,d,y]-pre_data[m,d,y], "[",data[m,d,y],"-",pre_data[m,d,y],"]\n")
            }}}
      }

      B<-array(dim=c(13,4,18),
               dimnames=list(MonthDiff=c(seq(1,13)),
                             Depth_m=c(0,-20,-50,-80),
                             Year=c(seq(2000,2017))))

      for (y in yearIN) {
        for (d in depthIN){
          for (m in monthIN){
            B[m,d,y]<-data[m,d,y]-pre_data[m,d,y]
          }}}

      Bout<-B[monthIN,depthIN,yearIN]



      if (info==TRUE){
        InfoB=list("DIFFERENCE IN MEAN SEA TEMPERATURE",
                   list("Depth CODE","1:   0m", "2: -20m", "3: -50m", "4: -80m"),
                   list("Month CODE","1: January", "2: February","...","11: November", "12: December", "13: Year Average"))
        result3B=list(info=InfoB,results=Bout)
      } else {
        result3B=Bout
      }

      #Graphics

      if(plotting==TRUE){
        for (y in yearIN) {
          for(d in depthIN){
            barplot(B[monthIN,d,y],
                    col=c(rep("lightblue",12), "magenta"),
                    ylim=c(-5,5),
                    main="Sea Temperature Difference (ºC) with previous period",
                    sub=paste("At depth", d, "for the year", y+1999),
                    ylab="Temperature Diff (ºC)")

            abline(h=mean(B[1:12,d,]), #Difference average at this depth for all the years studied
                   col="red")

          }}}

      return(result3B)

    }
  }}
