#' Finds an Optimal Rotation Vector
#' 
#' This function will produce an optimal rotation vector for some design. It will produce Resolution IV if possible and if not will find a minimal aberration. If multiple minimal aberration designs exist it will randomly choose one and ignore any "desirability" between factors.
#' 
#' @export
#' 
#' @param design This needs to be a coded design matrix using (0,1,2) where each row corresponds to one treatment. 
#' 
#' @value It will produce a list
#' \item{Rotation Vector: The first element of the list will be the optimum rotation vector and a small message whether Resoluiton IV is obtainable or if this is minimal abberration}
#' \item{psuedo-design matrix: This is the fake design matrix which is the coded effects of length 3 that are aliased with the intercept in the original design. This will not neccessarly be a regular fractional factorial design (hence psuedo)}
#' 
#' @examples
#' \dontrun{
#' x <- c("a", "b", "c", "d", "e", "f")
l <- matrix(c(1,2,0,1,1,1, 0,1,1,2,0,0, 1,1,1,2,2,0), nrow = 3, byrow = TRUE)
trees <- c(0,0,0)
m <- part(x, l, trees)[[1]]
head(m)
what_frac(m)
opt_rotation(m)
#' }









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
    
    sol <- as.vector(as.vector(holding[min(which(ri_num == max(ri_num))),]))
    
    colnames(g3) <- NULL
    
    result <- list(sol, g3)
    
    names(result)[[2]] <- "psuedo-design matrix"
    
    theo_max <- dim(g3)[1]
    
    if(max(ri_num) == theo_max){ names(result)[1]<- "Resolution IV Obtainable"} else {names(result)[1] <- "Minimum Aberration Achieved"}

    
  }
  
 
  
  
  
  return(result)
  
  
}
