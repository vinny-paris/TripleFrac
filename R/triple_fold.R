#This function is the "meat" of the program. It will expand a design matrix x using a 
#vector specficed as expansion. The expansion variable should be a vector of length 
# f, for number of factors. All entries should be 0,1,2 depending on what youd like the replacement to be

triple_fold <- function(x, expansion){
   
   x1 <- t((t(x) + expansion) %% 3)
   
   x2 <- t((t(x) + 2*expansion) %% 3)
   
   
   y <- rbind(x, x1, x2)
   
   
   aliased <- what_frac(y)
   colnames(aliased) <- NULL
   
   holding <- list(aliased, y)
   
   names(holding) <- c('Aliased_with_Fraction', 'Design_Matrix')
   
   return(holding)
   
 }
 
