# x is the var identifiers you want
# l is a matrix of equations you want to generate the fraction from, filled in by row
# tree is (a vector and) the particular branch of the trianary tree you want (i.e. 1*x + 2*y + 0*z mod 3 = tree)

part <- function(x, l, trees){
  
  #create the correct coef.'s as l_* 
  n <- ifelse(is.vector(l) == TRUE, dim(as.data.frame(l))[1], dim(as.data.frame(l))[2])
  
  if(length(x) != n) stop("Number of variables does not match number of coefs; please correct!")
  

  
  needed <- colSums(abs(l)) != 0
  n <- sum(needed)
  l <- l[, needed]
  x_short <- x[needed]
  x_useless <- x[!needed]
  holding <- NULL
  
  for (j in 1:n){
    y <- kronecker(rep(c(0,1,2), 3^(j-1)), rep(1, 3^(n-j)))
    holding <- cbind(holding, y)
  }
  
  colnames(holding) <- x_short
  
  
  
  goal <- function(vec, l, trees){
    
    sol <- rowSums(l %*% (vec)) %% 3 - trees 
    
    ifelse(sum(abs(sol)) == 0, TRUE, FALSE)
  }
  
  
  branches <- apply(holding, 1, goal, l = l, trees = trees)
  
  cut <- list(holding[branches, ], x_useless)
  
  names(cut) <- c("Short Design", "Expansion Variables")
  
  
  return(cut)
}


#Examples


x = c('dog', 'cat', 'bird', 'fish')
l = matrix(c(2, 1,  1, 1, 1, 0, 0, 1), nrow = 2, byrow = TRUE)
trees <- c(1,2)
part(x, l, trees)

g <- part(x, l, trees)[[1]]
n <- part(x, l, trees)
what_frac(g)

triple_fold(g, c(1,1,1,1))
triple_fold(g, c(1,2,1,1))[[1]]
triple_fold(g, c(1,2,2,1))[[1]]
triple_fold(g, c(1,1,2,1))[[1]]
triple_fold(g, c(2,2,1,1))[[1]]
triple_fold(g, c(1,0,0,0))[[1]]
triple_fold(g, c(1,2,0,0))[[1]]










x <- c("a", "b", "c", "d")
l <- matrix(c(1,2,0,1,0,1,2,0), nrow = 2, byrow = TRUE)
trees <- c(1,1)
h <- part(x, l, trees)[[1]]
part(x, l, trees)
what_frac(h)
triple_fold(h, c(1,0,0,0))[[1]]
triple_fold(h, c(0,1,1,0))[[1]]
triple_fold(h, c(0,1,2,0))[[1]]



x <- c("a", "b", "c", "d", "e", "f")
l <- matrix(c(1,2,0,1,1,1, 0,1,1,2,0,0, 1,1,1,2,2,0), nrow = 3, byrow = TRUE)
trees <- c(0,0,0)
m <- part(x, l, trees)[[1]]
part(x, l, trees)
what_frac(m)
triple_fold(m, c(1,0,0,0,0,0))[[1]]
triple_fold(m, c(1,1,0,1,0,0))[[1]]
triple_fold(m, c(1,1,0,2,0,0))[[1]]


