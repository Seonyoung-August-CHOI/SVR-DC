cindex_manual = function(pred, time, status) {
  n = length(time)
  concordant = 0
  comparable = 0
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (status[i] == 1 && time[i] < time[j]) {
        comparable = comparable + 1
        if (pred[i] > pred[j]) concordant = concordant + 1
      } else if (status[j] == 1 && time[j] < time[i]) {
        comparable = comparable + 1
        if (pred[j] > pred[i]) concordant = concordant + 1
      }
    }
  }
  
  if (comparable == 0) return(NA)
  return(concordant / comparable)
}