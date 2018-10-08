tripling <- function(x, l){
  f <- dim(x)[2]
  n <- dim(x)[1]
  l_n <- dim(l)[1]
  
  holding <- NULL
  for (j in 1:f){
    y <- kronecker(rep(c(0,1,2), 3^(j-1)), rep(1, 3^(f-j)))
    holding <- cbind(holding, y)
  }
  
  m <- dim(holding)[1]
  
  qq <- (l %*% t(holding)) %% 3
  
  
    q <- (l %*% t(x)) %% 3
   
  jig <- dim(q)[2]
  
  h1 <- NULL
  h2 <- NULL
  h3 <- NULL
  
  for(i in 1:jig){
    ali <- (1:m)[colSums(q[,i] == qq) == l_n]
    g1 <- holding[ali[1], ]
    g2 <- holding[ali[2], ]
    g3 <- holding[ali[3], ]
    h1 <- rbind(h1, g1)
    h2 <- rbind(h2, g2)
    h3 <- rbind(h3, g3)
  }
  
  helper <- list(x, h1, h2, h3, rbind(h1, h2, h3))
  
  names(helper) <- c('original', 'D1', 'D2', 'D3', 'new_matrix')
  
  return(helper)
  

  
  
  }
}



tripling(x, l)

