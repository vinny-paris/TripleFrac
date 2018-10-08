x <- c("a", "b", "c", "d")
l <- matrix(c(1,2,0,1,0,1,2,0), nrow = 2, byrow = TRUE)
trees <- c(1,1)
h <- part(x, l, trees)[[1]]

l <- matrix(c(1,2,0,0,0,1,2,0,0,0,1,1), nrow = 3, byrow = TRUE)


what_frac(h)

what_frac(tripling(h,l)[[5]])

