kde_gc = function(y, d, sfit) {
  hn = 1.06 * sd(y[d == 1]) * length(y[d == 1])^(-1 / 5)
  hn = max(hn, 1e-2) 
  
  dG = diff(c(0, 1 - sfit$surv))
  time = sfit$time
  
  gc_vals = sapply(y, function(t) {
    sum(dnorm(t - time, sd = hn) * dG)
  })
  
  return(pmax(gc_vals, 1e-3))
}