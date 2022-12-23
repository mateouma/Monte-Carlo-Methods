# Problem 1

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

unif_num <- gen_unif_num(run.length = 10000, seed = 1)

hist(unif_num$U, xlab = "U", probability = TRUE, main = "Histogram of U")

plot(unif_num$X[seq(2,1000,2)], unif_num$X[seq(3,1001,2)], xlab = "X", main = "Scatterplot of (X2t, X2t+1)")

# Problem 2

fair_coins <- function(n, seed) {
  U <- gen_unif_num(run.length = n, seed = seed)$U
  U >= 0.5 # returns TRUE if heads, FALSE if tails
}

roll_fair_die <- function(n, seed) {
  U <- gen_unif_num(run.length = n, seed = seed)$U
  intervals <- seq(0, 1, length.out = 7)
  vapply(U, function(u){
    die <- 0
    for (i in seq_len(6)) {
      if (u >= intervals[i] & u < intervals[i + 1]) {
        die <- i
        break
      }
    }
    die
  }, 0)
}

roll_fair_die2 <- function(n, seed) {
  U <- gen_unif_num(run.length = 100, seed = seed)$U
  U <- tail(U, n) # last n elements of U since seed makes first elements small
  intervals <- seq(0, 1, length.out = 7)
  vapply(U, function(u){
    die <- 0
    for (i in seq_len(6)) {
      if (u >= intervals[i] & u < intervals[i + 1]) {
        die <- i
        break
      }
    }
    die
  }, 0)
}

head_freqs <- numeric(10000)
for (i in seq_along(head_freqs)) {
  flips <- fair_coins(n = 100, seed = i) # use index as seed
  head_freqs[i] <- mean(flips)
}

hist(head_freqs, breaks = 30, xlab = "Frequency", probability = TRUE, main =
       "Head Frequency of Fair Coin")

die_avgs_list <- list()
for (n in seq_len(20)) {
  die_avgs <- numeric(10000)
  for (i in seq_len(10000))
    die_avgs[i] <- mean(roll_fair_die(n = n, seed = i))
  die_avgs_list[[n]] <- die_avgs
}

par(mfrow = c(2, 4))
for (n in seq_along(die_avgs_list)) {
  hist(die_avgs_list[[n]], xlab = "Average", probability =
         TRUE, main = paste("Average of ", n, " die rolls"))
}

die_avgs_list2 <- list()
for (n in seq_len(20)) {
  die_avgs <- numeric(10000)
  for (i in seq_len(10000))
    die_avgs[i] <- mean(roll_fair_die2(n = n, seed = i))
  die_avgs_list2[[n]] <- die_avgs
}

par(mfrow = c(2, 4))
for (n in seq_along(die_avgs_list2)) {
  hist(die_avgs_list2[[n]], xlab = "Average", probability =
         TRUE, main = paste("Average of ", n, " die rolls"))
}

# Problem 3

gen_exp_num <- function(lambda, n, seed) {
  U <- gen_unif_num(run.length = n, seed = seed)$U
  vapply(U, function(u){
    (-log(1 - u)) / lambda
  }, 0)
}

par(mfrow = c(2, 2))
vapply(c(1, 5, 10, 50), function(lambda){
  exp_nums <- gen_exp_num(lambda = lambda, n = 10000, seed = 1)
  hist(exp_nums, breaks = 30, xlab = "X", probability = TRUE, main = 
         paste("Histogram of exp(", lambda, ")"))
  
  x = seq(0, 40, length= 1000)
  curve(dexp(x, lambda), col = "blue", add = TRUE)
  0
}, 0)

# Problem 4

f_inv <- function(u) {
  (10 / log(41)) * log(1 - (log(-log(u)/log(2)) / log(22)))
}

gen_normal_num <- function(n, seed) {
  U <- gen_unif_num(run.length = n, seed = seed)$U
  vapply(U, function(u){
    ifelse(u >= 0.5, f_inv(u), -1 * f_inv(1 - u))
  }, 0)
}

norm_rand_nums <- gen_normal_num(n = 10000, seed = 1)
hist(norm_rand_nums, breaks = 50, xlab = "X", probability = TRUE, main =
       "Histogram of Numbers from Normal Distribution")
x <- seq(-6, 6, length=1000)
curve(dnorm(x), col = "blue", add = TRUE)






