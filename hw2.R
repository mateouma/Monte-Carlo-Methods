gen_unif_num <- function(a = 7^5, b = 0, m = (2^31 - 1), run.length = 1, seed) {
  X <- numeric(run.length)
  X[1] <- seed
  t <- 2
  run.length <- run.length - 1
  while(run.length > 0){
    X[t] <- (a * X[t-1]) %% m
    t <- t + 1
    run.length <- run.length - 1
  }
  U = X / m
  list(U = U, X = X)
}

# problem 1

polar_method_normal <- function(n) {
  theta_vals <- 2 * pi * runif(n = n)
  r_vals <- sqrt(-2 * log(runif(n = n)))
  list(x = r_vals * cos(theta_vals),
       y = r_vals * sin(theta_vals))
}

norm_rand_nums <- polar_method_normal(10000)
hist(norm_rand_nums$x, breaks = 50, xlab = "X", probability = TRUE, main =
       "Histogram of Numbers from Normal Distribution using Polar Method")
x <- seq(-6, 6, length=1000)
curve(dnorm(x), col = "blue", add = TRUE)

# problem 2

accept_reject_normal <- function(n) {
  M <- sqrt(2 / pi) * exp(0.5)
  Z <- numeric(n)
  S <- ifelse(runif(n) >= 0.5, 1, -1)
  
  i <- 0
  tot_iter <- 0
  while (Z[n] == 0) {
    X <- -log(runif(1))
    f_X <- sqrt(2 / pi) * exp(-X^2 / 2)
    Mg_X <- M * exp(-X)
    
    if (runif(1) <= (f_X / Mg_X)) {
      Z[i] <- S[i] * X
      i <- i + 1
    }
    tot_iter <- tot_iter + 1
  }
  list(z = Z, emp_accept_rate = 10000 / tot_iter)
}

norm_rand_nums <- accept_reject_normal(10000)
hist(norm_rand_nums$z, breaks = 50, xlab = "X", probability = TRUE, main =
       "Histogram of Numbers from Normal Distribution using Acceptance-Rejection Method")
curve(dnorm(x), col = "blue", add = TRUE)

print(norm_rand_nums$emp_accept_rate)
print(1 / (sqrt(2 / pi) * exp(0.5)))
