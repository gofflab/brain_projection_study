Distinguish2Dor3D<-function(pattern, row = TRUE, z, k = 9 ){
  
 if(row == FALSE){
   pattern <- t(pattern)
 }

## calculated mean for each slice
  m <- matrix(, nrow = dim(pattern)[1], ncol = 0)
  
  i<-1
  for(g in unique(z)){
    m<-cbind(m, apply(pattern[,which(z == g)], 1, mean))
    i = i+1
  }

  
 result<-c()
 
 for(p in 1:dim(m)[1]){
   #get the 0.25 and 0.75 qauntile  
   x <- quantile(m[p,])[c(2,4)]
  
   #calculate the critical value for numeric outlier
   c <- (x[2]-x[1])*k
  
   #find outlier, if outlier is found, the pattern is 2D, assgin 2 to result. if not found, pattern is 3D, assign 3 to result.
   if ( max(m[p,]) > c){
     result<- c(result,2)
   }else{
    result<-c(result,3)
   }
 }
 return(which(result == 3))
}
