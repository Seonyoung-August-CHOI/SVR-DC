find_lambda = function(target, fx, x) {
  scale_T = fx
  T_fixed = rweibull(n, shape = 2, scale = scale_T)
  
  obj = function(lambda) {
    C = runif(n, min = exp(lambda), max = exp(lambda + 2)) 
    mean(T_fixed > C) - target
  }
  
  uniroot(obj, c(-10, 10))$root
}