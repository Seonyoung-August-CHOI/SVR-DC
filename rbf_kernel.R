rbf_kernel = function(x1, x2 = NULL, q = 1 / ncol(x1)) {
  if (is.null(x2)) x2 = x1
  norm1 = drop(rowSums(x1^2))  
  norm2 = drop(rowSums(x2^2))  
  d2 = outer(norm1, norm2, "+") - 2 * x1 %*% t(x2)
  exp(-q * d2)
}