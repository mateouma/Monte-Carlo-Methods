original_pi <- function(n, m) {
  pi_vals <- vapply(numeric(m), function(k){
    x <- runif(n)
    y <- runif(n)
    mean(x^2 + y^2 <= 1) * 4
  }, 0)
  list(pi = pi_vals, variance = var(pi_vals))
}
set.seed(1)
original_pi(10000, 100)

conditioning_pi <- function(n, m) {
  pi_vals <- vapply(numeric(m), function(k) {
    x <- runif(n)
    mean(sqrt(1 - x^2)) * 4
  }, 0)
  list(pi = pi_vals, variance = var(pi_vals))
}
set.seed(1)
conditioning_pi(10000, 100)

antithetic_pi <- function(n, m) {
  pi_vals <- vapply(numeric(m), function(k) {
    x <- runif(n)
    mean((sqrt(1 - x^2) + sqrt(1 - (1 - x)^2)) / 2) * 4
  }, 0)
  list(pi = pi_vals, variance = var(pi_vals))
}
set.seed(1)
antithetic_pi(10000, 100)

control_variate_pi <- function(n, m) {
  pi_vals <- vapply(numeric(m), function(k) {
    x <- runif(n)
    (mean(sqrt(1 - x^2) - (1 - x)) + 0.5) * 4
  }, 0)
  list(pi = pi_vals, variance = var(pi_vals))
}
set.seed(1)
control_variate_pi(10000, 100)

stratified_pi <- function(n, m) {
  l_bounds <- seq(0, 0.9, 0.1)
  u_bounds <- seq(0.1, 1.0, 0.1)
  
  pi_vals <- vapply(numeric(m), function(k) {
    x <- list()
    for (i in seq_len(10)) {
      to_add <- runif(n / 10, l_bounds[i], u_bounds[i])
      x[[i]] <- to_add
    }
    mean(sqrt(1 - unlist(x)^2)) * 4
  }, 0)
  list(pi = pi_vals, variance = var(pi_vals))
}
set.seed(1)
stratified_pi(10000, 100)

importance_sampling <- function(n, c) {
  x <- rnorm(n, c)
  w <- dnorm(x) / dnorm(x, mean = c)
  mean((x > c) * w) # I_hat
}
set.seed(1)
importance_sampling(10000, 10)
