alias_design <- function(x){
  

  #back col
  back <- c(rep(c(0,1,2), alias_dim_calc(x-1)-1), 1)
  
  
  second_back <- c(kronecker(c(rep(c(0,1,2), alias_dim_calc(x-2)-1), 1), rep(1, 3)), 0)
  
  
  third_back <- c(kronecker(c(rep(c(0,1,2), alias_dim_calc(x-3)-1), 1), rep(1,9)), rep(0, 3), 0)
  
  
  fourth_back <- c(kronecker(c(rep(c(0,1,2), alias_dim_calc(x-4)-1), 1), rep(1,27)), rep(0, 12), 0)
  
  fifth_back <- c(kronecker(c(rep(c(0,1,2), alias_dim_calc(x-5)-1), 1), rep(1,81)), rep(0, 39), 0)
  
  
  books <- NULL
  for(i in 1:x){
    
    col <- c(kronecker(c(rep(c(0,1,2), alias_dim_calc(x-i)-1), 1), rep(1, 3^(i - 1))), rep(0, (3^(i-1) + 1)/2 - 1))
    
    books <- cbind(col, books)
    
    
    
  }
    
  back <- c(rep(c(0,1,2), alias_dim_calc(x-1)-1), 1)
  
  books <- cbind(books, back)
  
}
  
  
  
  
  
  
  
  
  
