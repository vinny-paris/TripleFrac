alias_design <- function(inc_mat, l, trees){
  
  x <- inc_mat
  
  #get needed dim of l and inc_mat
  n <- ifelse(is.vector(l) == TRUE, dim(as.data.frame(l))[1], dim(as.data.frame(l))[2])
  x_length <- dim(x)[1]
  
  
  
  for(i in 1:n){
    
    inner_holding <- NULL
    
    for(j in 1:x_length){
      x[j,] %*% l[i,] %% 3 - trees[i] 
      
      
      
      
      
    }
  }
  
  
  
  
  
  
  
  
  
}