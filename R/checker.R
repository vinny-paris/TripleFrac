#' Test for optimal rotation vectors
#' 
#'  For some set of generators, this function will show what the corresponding effect the rotation vector will have on it. Most importantly if the effects level come out to be 0 then it is rotationally tolerant.
#'  
#' @export
#'   
#' @param  g A matrix. This is the coded psuedo-design matrix with each element being 0, 1, or 2. Each row corresponds to one generator.
#' @param rotation_vector This is the rotation vector that is to be evaluated. 
#' 
#' @return A List. 
#' \item{rotation vector} This is the vector you put in from the parameters

checker <- function(g, rotation_vector){
  q <- g
  q %*% x -> l
  l <- l %% 3
  
  
  
  num_of_vars <- dim(q)[2]
  holding <- NULL
  
  for(i in 1:dim(q)[1]){
    e <- q[i,]
  s <- data.frame(c(1:num_of_vars), e + 1, row.names = NULL)
  j <- paste(latin[as.matrix(s)], collapse = "")
  j <- substr(j, 1, nchar(j) -1)
  holding <- rbind(holding, j)
  }
  
  h <- suppressWarnings(data.frame(holding, l))
  names(h) <- c('levels', 'origin')
  
  h <- list(x, h)
  
  names(h) <- c('rotation vector', 'output')
  return(h)

  
}









q <- c(1, 2, 1, 0, 0, 0,
       0, 0, 1, 0, 2, 2,
       0, 0, 1, 1, 0, 1,
       0, 0, 1, 2, 1, 1,
       0, 0, 0, 1, 1, 2)
q <- matrix(q, nrow = 5, byrow = TRUE)
q
prace(q, c(2,1,1,0,1,1))

