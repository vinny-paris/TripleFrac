#Gives all possible equations for how a factorial experiment at 3 levels may be split upon.

alias_design <- function(x){

  
  books <- NULL
  for(i in 1:x){
    
    col <- c(kronecker(c(rep(c(0,1,2), alias_dim_calc(x-i)-1), 1), rep(1, 3^(i - 1))), rep(0, (3^(i-1) + 1)/2 - 1))
    
    books <- cbind(col, books)
    
    
    
  }
    

  return(books)
}
  
  
  
  
  
  
  
  
  
