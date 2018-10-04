#returns what fraction is the matix is split along
#be warned that it only pays attention to fully aliased situations
#so if only 2 of three levels appear the function will not flag them
#you have been warned. 

what_frac <- function(x){
  
  holding <- NULL
  
  num_of_vars <- dim(x)[2]
  
  possible_equations <- alias_design(num_of_vars)
  
  eqs_length <- dim(possible_equations)[1]
  
  for(i in 1:eqs_length){
    e <- possible_equations[i,]
    k <- uniqueN((x %*% e) %% 3)
    if(k == 1) {
   
   e <- (as.matrix(e))
   ll <- data.frame(c(1:num_of_vars), (e + 1), row.names = NULL)
   nam <- paste(latin[as.matrix(ll)], collapse = "")
   nam <- substr(nam, 1, nchar(nam) -1)
   holding <- rbind(holding, nam)
    }
  }
  
  rownames(holding) <- NULL
  colnames(holding) <- "Aliased with Fraction"
  
  return(holding)
}

