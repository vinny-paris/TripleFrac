# x is the var identifiers you want
# l is the single equation you want to generate the fraction from
# tree is (a number and) the particular branch of the trianary tree you want (i.e. 1*x + 2*y + 0*z mod 3 = tree)

part <- function(x, l, tree){
  
  #create the correct coef.'s as l_* 
  n_long <- length(l)
  
  if(length(x) != n_long) stop("Number of variables does not match number of coefs; please correct!")
  
  needed <- (l != 0)
  n <- sum(needed)
  l <- l[needed]
  x_short <- x[needed]
  x_useless <- x[!needed]
  
  holding <- NULL
  
  for (j in 1:n){
    y <- kronecker(rep(c(0,1,2), 3^(j-1)), rep(1, 3^(n-j)))
    holding <- cbind(holding, y)
  }
  
  colnames(holding) <- x_short
  
  
  
  goal <- function(vec, l, tree){
    
    sol <- sum(vec * l) %% 3 - tree 
    
    ifelse(sol == 0, TRUE, FALSE)
  }
  
  
  branches <- apply(holding, 1, goal, l = l, tree = tree)
  
  cut <- list(holding[branches, ], x_useless)
  
  names(cut) <- c("Short Design", "Expansion Variables")
  
  
  return(cut)
}


  