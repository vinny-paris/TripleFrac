#' Generating a New Design Matrix
#' 
#' Create the new design matrix from a triple foldover techinque using a rotation vector
#'
#' @export
#' 
#' @param design_matrix The original design matrix that you would like expanded upon, must come in coded with 0,1,2's
#' @param rotation_vector The rotation vector with the same length as the column width of the design matirx. Entries may be 0, 1, or 2.
#' 
#' @examples
#' \dontrun{
#' design <- matrix(c(1,2,1,0,1,1,1,2,2), byrow = TRUE, nrow = 3)
#' x <- c(1, 2, 0)
#' triple_fold(design, x)
#' }


triple_fold <- function(design_matrix, rotation_vector){
   expansion <- rotation_vector
   x <- design_matrix
   
   x1 <- t((t(x) + expansion) %% 3)
   
   x2 <- t((t(x) + 2*expansion) %% 3)
   
   
   y <- rbind(x, x1, x2)
   
   
   aliased <- what_frac(y)
   colnames(aliased) <- NULL
   
   holding <- list(aliased, y)
   
   names(holding) <- c('Aliased_with_Fraction', 'Design_Matrix')
   
   return(holding)
   
 }
 
