

opt_rotation <- function(design){
  
  x <- design
  
  
  #produce psuedo-design matrix
  holding <- NULL
  
  num_of_vars <- dim(x)[2]
  
  possible_equations <- alias_design(num_of_vars)
  
  eqs_length <- dim(possible_equations)[1]
  
  for(i in 1:eqs_length){
    e <- possible_equations[i,]
    k <- uniqueN((x %*% e) %% 3)
    if(k == 1) {
      
      e <- t(as.matrix(e))
      holding <- rbind(holding, e)
    }
  }
  
  
  #Collect g^3
  idicate <- holding^2 %% 3
  correct_rows <- rowSums(idicate) == 3
  g3 <- holding[correct_rows, ]
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  x <- g3
  
  #produce psuedo-design matrix
  holding <- NULL
  
  num_of_vars <- dim(x)[2]
  
  possible_equations <- alias_design(num_of_vars)
  
  eqs_length <- dim(possible_equations)[1]
  
  for(i in 1:eqs_length){
    e <- possible_equations[i,]
    k <- uniqueN((x %*% e) %% 3)
    if(k != 3) {
      
      e <- t(as.matrix(e))
      holding <- rbind(holding, e)
    }
  }
  
  
  
  if(class(g3) == "vector"){
  outs <- holding %*% g3 %% 3
  
  if(sum(outs != 0 )) {sol <-  min(which(outs != 0))}
  
  sol <- as.data.frame(sol)
  
  colnames(sol) <- "Resolution IV Obtainable"
  
  return(sol)
  }
  
  if(class(g3) == "matrix"){
    
    
    outs <- (holding %*% t(g3)) %% 3
    
   
    
    ri_num <- rowSums(outs != 0)
    
    sol <- as.data.frame(holding[min(which(ri_num == max(ri_num))),])
    
    theo_max <- dim(g3)[1]
    
    if(max(ri_num) == theo_max){ colnames(sol) <- "Resolution IV Obtainable"} else {colnames(sol) <- "Minimum Aberration Achieved"}

    
  }
  
 
  
  
  
  return(sol)
  
  
}
