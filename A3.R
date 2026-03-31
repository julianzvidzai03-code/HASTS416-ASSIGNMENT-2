# ==============================================================================
# MARKOV CHAIN SIMULATION & ANALYSIS (Question A3)
# ==============================================================================

# --- Setup & Parameters ---
matrix_power <- function(M, n) {
  result <- diag(nrow(M))
  for (i in 1:n) {
    result <- result %*% M
  }
  return(result)
}

# transition matrices
# 1 PM to 4 PM matrix
P1 <- matrix(c(
  0.4, 0.4, 0.2,
  0.3, 0.4, 0.3,  
  0.0, 0.1, 0.9
), nrow = 3, byrow = TRUE)

# 4 PM to 6 PM matrix
P2 <- matrix(c(
  0.1, 0.5, 0.4,
  0.1, 0.3, 0.6,
  0.0, 0.1, 0.9
), nrow = 3, byrow = TRUE)

# Initial state distribution at 1 PM (starts as 'light' = state 1)
pi_0 <- c(1, 0, 0)


# --- PART A:6 PM outcome ---

# 1 PM to 4 PM = 3 hours = 9 steps (of 20-minute intervals)
# 4 PM to 6 PM = 2 hours = 6 steps (of 20-minute intervals)
pi_4pm <- pi_0 %*% matrix_power(P1, 9)
pi_6pm <- pi_4pm %*% matrix_power(P2, 6)

cat("--- Part (a) Results ---\n")
cat("State distribution at 6 PM (Analytical):\n")
print(pi_6pm)
cat("\n")


# --- PART B: simulation and distribution evolution plot---

set.seed(42)  
n_trajectories <- 10000
steps <- 15

state_matrix <- matrix(NA, nrow = n_trajectories, ncol = steps + 1)
state_matrix[, 1] <- 1  # All start in state 1 (Light)

for (i in 2:10) {
  for (s in 1:n_trajectories) {
    curr_state <- state_matrix[s, i - 1]
    state_matrix[s, i] <- sample(1:3, 1, prob = P1[curr_state, ])
  }
}

for (i in 11:16) {
  for (s in 1:n_trajectories) {
    curr_state <- state_matrix[s, i - 1]
    state_matrix[s, i] <- sample(1:3, 1, prob = P2[curr_state, ])
  }
}

# Calculate proportions of trajectories in each state at each time step
proportions <- matrix(0, nrow = steps + 1, ncol = 3)
for (i in 1:(steps + 1)) {
  counts <- table(factor(state_matrix[, i], levels = 1:3))
  proportions[i, ] <- counts / n_trajectories
}

cat("--- Part (b) Results ---\n")
cat("State distribution at 6 PM (Simulated):\n")
print(proportions[16, ])

# Plotting Distribution Evolution Plot
plot(0:15, proportions[, 1], type = "o", col = "blue", pch = 16, ylim = c(0, 1),
     xlab = "Time Step (20-min intervals from 1 PM)", 
     ylab = "Proportion of Trajectories", 
     main = "Distribution Evolution Plot")
lines(0:15, proportions[, 2], type = "o", col = "orange", pch = 15)
lines(0:15, proportions[, 3], type = "o", col = "darkgreen", pch = 17)

abline(v = 9, col = "red", lty = 2, lwd = 2) 

legend("topleft", legend = c("Light", "Heavy", "Jammed", "4 PM Switch"),
       col = c("blue", "orange", "darkgreen", "red"), 
       lty = c(1, 1, 1, 2), pch = c(16, 15, 17, NA), lwd = c(1, 1, 1, 2))
grid()