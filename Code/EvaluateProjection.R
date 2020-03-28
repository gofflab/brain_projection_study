findStructureAnnotation<-function(x,group){
  
  str.list<-c("D","M","MH","Pall","PH","PHy","PMH","POTel","PPH","SPall","TelR","THy")

  cor.list<-list("D"=x[which(group==str.list[1]),1],"M"=x[which(group==str.list[2]),1],"MH"=x[which(group==str.list[3]),1], "Pall"=x[which(group==str.list[4]),1], "PH"=x[which(group==str.list[5]),1], "PHy"=x[which(group==str.list[6]),1],
"PMH"=x[which(group==str.list[7]),1],"POTel"=x[which(group==str.list[8]),1],"PPH"=x[which(group==str.list[9]),1],"SPall"=x[which(group==str.list[10]),1],"TelR"=x[which(group==str.list[11]),1],"THy"=x[which(group==str.list[12]),1])
 
  mean<-c(mean(cor.list$D,na.rm=T),mean(cor.list$M,na.rm=T),mean(cor.list$MH,na.rm=T),mean(cor.list$Pall,na.rm=T),
    mean(cor.list$PH,na.rm=T),mean(cor.list$PHy,na.rm=T),mean(cor.list$PMH,na.rm=T),mean(cor.list$POTel,na.rm=T),
    mean(cor.list$PPH,na.rm=T),mean(cor.list$SPall,na.rm=T),mean(cor.list$TelR,na.rm=T),mean(cor.list$THy,na.rm=T))
 
  sort<-sort(mean,decreasing = TRUE, na.last=NA)
  
  max1<-which(mean==sort[1])
 
  max2<-which(mean==sort[2])

#if you want to inlcude TelR, please unmark the following lines
  if(str.list[max1] == "TelR"){
    return(str.list[max2])
  }else{
    return(str.list[max1])
  }
  
#  k<-wilcox.test(c(as.numeric(unlist(cor.list[max1])),as.numeric(unlist(cor.list[max2]))),c(rep(1,length(as.numeric(unlist(cor.list[max1])))),rep(2,length(as.numeric(unlist(cor.list[max2]))))))
  
#  if(k$p.value<0.05){
#   return(str.list[max1])
# }else{
#    return("Can't distinguish") 
# }
}

## x: projectR$projection
## y: NNMF$H
EvaluateProjection<-function(x,y,group,allzero){
  
  if(is.factor(group)){
    group<-as.character(group)
  }
  if(length(allzero)!=0){
  group<-group[-allzero]
  }
  
  if(length(allzero)!=0){
    y<-y[,-allzero]
  }
  
  eval<-c()
  
  for(i in 1:ncol(x)){
    
    cellname<-colnames(x)
    
    
    cor<-cor(y,x[,i])
    
    s<-findStructureAnnotation(cor,group)
    
    eval<-c(eval,s)
  }
  return(eval)
}


