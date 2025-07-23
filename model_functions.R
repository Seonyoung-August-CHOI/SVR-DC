svcr_ipop = function(K, y, d, C, eps) {
  n = length(y)
  H = rbind(cbind(K, -K), cbind(-K, K)) + diag(2 * n) * 1e-3
  cvec = c(eps - y, eps + y)
  A = matrix(0, 1, 2 * n); A[1, 1:n] = 1; A[1, (n+1):(2*n)] = -1
  l = rep(0, 2 * n); u = rep(C, 2 * n); u[n + (d == 0)] = 0
  sol = ipop(cvec, H, A, 0, l, u, 0)
  sol@primal[1:n] - sol@primal[(n+1):(2*n)]
}

ipcw_km_ipop = function(K, y, d, sc, C, eps) {
  n = length(y)
  w = d / pmax(sc, 1e-3)
  H = rbind(cbind(K, -K), cbind(-K, K)) + diag(2 * n) * 1e-3
  cvec = c(eps - y, eps + y)
  A = matrix(0, 1, 2 * n); A[1, 1:n] = 1; A[1, (n+1):(2*n)] = -1
  l = rep(0, 2 * n); u = rep(C, 2 * n) * w
  sol = ipop(cvec, H, A, 0, l, u, 0)
  sol@primal[1:n] - sol@primal[(n+1):(2*n)]
}

ipcw_ph_ipop = ipcw_km_ipop

wcsvr_km_ipop = function(K, y, d, sc, gc, C, eps) {
  n = length(y)
  w1 = d / pmax(sc, 1e-3)
  w2 = (1 - d) / pmax(gc, 1e-3)
  H = matrix(0, 3*n, 3*n)
  H[1:n, 1:n] = diag(d) %*% K %*% diag(d)
  H[(n+1):(2*n), (n+1):(2*n)] = H[1:n, 1:n]
  H[(2*n+1):(3*n), (2*n+1):(3*n)] = diag(1 - d) %*% K %*% diag(1 - d)
  H = H + diag(3 * n) * 1e-3
  cvec = c(-d*(y - eps), -d*(-y - eps), -(1 - d)*(y + eps))
  A = matrix(0, 1, 3 * n); A[1, 1:n] = d; A[1, (n+1):(2*n)] = -d; A[1, (2*n+1):(3*n)] = 1 - d
  l = rep(0, 3 * n); u = c(C * w1, C * w1, C * w2)
  sol = ipop(cvec, H, A, 0, l, u, 0)
  a = sol@primal[1:n]; a_star = sol@primal[(n+1):(2*n)]; g = sol@primal[(2*n+1):(3*n)]
  d * (a - a_star) + (1 - d) * g
}

wcsvr_ph_ipop = wcsvr_km_ipop