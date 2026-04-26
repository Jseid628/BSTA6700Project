library(osqp)

#' Solve the synth QP directly
#' @param X1 Target vector
#' @param X0 Matrix of control outcomes
#' @noRd
synth_qp <- function(X1, X0) {
  
  Pmat <- X0 %*% t(X0)
  qvec <- - t(X1) %*% t(X0)
  
  n0 <- nrow(X0)
  A <- rbind(rep(1, n0), diag(n0))
  l <- c(1, numeric(n0))
  u <- c(1, rep(1, n0))
  
  settings = osqp::osqpSettings(verbose = FALSE,
                                eps_rel = 1e-8,
                                eps_abs = 1e-8)
  sol <- osqp::solve_osqp(P = Pmat, q = qvec,
                          A = A, l = l, u = u, 
                          pars = settings)
  
  return(sol$x)
}