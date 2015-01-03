pollutantmean <- function(directory, pollutant, id = 1:332) {
  file<-paste(directory,"/",formatC(id,width=3,flag="0"),".csv",sep = "")
  data<-lapply(file,FUN = read.csv)
  final_data<-do.call(rbind,data)
  if(pollutant=="sulfate"){
    mean<-mean(final_data$sulfate,na.rm = T)
  }
  if(pollutant=="nitrate"){
    mean<-mean(final_data$nitrate,na.rm = T)
  }
  mean<-round(mean,digits = 4)
  return(mean)
  }

complete <- function(directory, id = 1:332) {
  file<-paste(directory,"/",formatC(id,width=3,flag="0"),".csv",sep = "")
  data<-lapply(file,FUN = read.csv)
  final_data<-do.call(rbind,data)
  final_data<-na.omit(final_data)
  names(final_data)<-tolower(names(final_data))
  a<-table(final_data$id)
  complete_cases<-data.frame(a)
  colnames(complete_cases)<-c("id","nobs")
  return(complete_cases)
}

corr <- function(directory, threshold = 0) {
  id<-1:332
  file<-paste(directory,"/",formatC(id,width=3,flag="0"),".csv",sep = "")
  data<-lapply(file,FUN = read.csv)
  final_data<-do.call(rbind,data)
  final_data<-na.omit(final_data)
  names(final_data)<-tolower(names(final_data))
  a<-table(final_data$id)
  case<-data.frame(a)
  greater<-threshold<case[,2]
  id_greater_than_threshold<-which(greater=="TRUE",arr.ind = TRUE)
  if(length(id_greater_than_threshold)==0){
    return (0)
  }
    correlation<-c()
    for(i in id_greater_than_threshold){
      data<-final_data[final_data$id==i,]
      correlation<-c(correlation,cor(data$sulfate,data$nitrate))
                                      }
  return(correlation)
  }
  