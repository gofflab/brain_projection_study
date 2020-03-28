imputationEnergy<-function(mat, coordination ,maxdistance){
  
  library(fractal)
  neighbor = findNeighbors(coordination, max.distance = maxdistance )
  
  mat[mat == -1]<-NA
  
  for(gene in 1:dim(mat)[1]){
    for (voxel in  1:dim(mat)[2]){
      if(is.na(mat[gene,voxel])){
        mat[gene,voxel]<-mean(mat[gene, neighbor$neighbor[which(neighbor$original == voxel)]], na.rm = TRUE)
      }
    }
  }
  mat[is.na(mat)]<--1
  return(mat)
}





