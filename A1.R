# ==========================================
# QUESTION A1: COMPLETE ANALYSIS
# ==========================================
library(markovchain)
library(igraph)

# --- SETUP: Define States and Matrix ---
states_A1 <- c("S1", "S2", "S3", "S4", "S5")
P_matrix_A1 <- matrix(c(
  1.0, 0.0, 0.0, 0.0, 0.0, # S1 is absorbing
  0.5, 0.0, 0.0, 0.0, 0.5, # S2
  0.2, 0.0, 0.0, 0.0, 0.8, # S3
  0.0, 0.0, 1.0, 0.0, 0.0, # S4
  0.0, 0.0, 0.0, 1.0, 0.0  # S5
), nrow = 5, byrow = TRUE)

mc_A1 <- new("markovchain", states = states_A1, byrow = TRUE, 
             transitionMatrix = P_matrix_A1, name = "Question_A1")

# --- PART (a)
plot(mc_A1, 
     package = "igraph",
     vertex.size = 35, 
     vertex.color = c("lightgreen", "lightblue", "lightblue", "lightblue", "lightblue"), 
     vertex.label.color = "black", 
     vertex.label.font = 2,
     edge.arrow.size = 0.6,
     edge.curved = 0.2,
     main = "A1: Markov Chain Diagram")

cat("--- A1: State Classification ---\n")
print(summary(mc_A1))


# --- PART (b): Simulations ---
cat("\n--- A1: 3 Trajectories (15 steps) ---\n")
set.seed(42) # For consistent results
for(i in 1:3) {
  traj <- rmarkovchain(n = 15, object = mc_A1, t0 = sample(states_A1, 1))
  cat("Trajectory", i, ":", traj, "\n")
}


# --- PART (c): Steady-State ---
cat("\n--- A1: Steady-State Probabilities ---\n")
print(steadyStates(mc_A1))


# --- PART (d): Unconditional Probabilities (n=50) ---
# Starting with equal probability (0.2) for each state
initial_dist <- c(0.2, 0.2, 0.2, 0.2, 0.2)
n_steps <- 50

# Calculate probabilities for steps 0 through 50
prob_history <- matrix(NA, nrow = n_steps + 1, ncol = 5)
prob_history[1, ] <- initial_dist

for(i in 1:n_steps) {
  prob_history[i+1, ] <- initial_dist * (mc_A1 ^ i)
}

# Plotting the results
matplot(0:n_steps, prob_history, type = "l", lty = 1, lwd = 3, 
        col = c("green3", "red", "blue", "orange", "purple"),
        xlab = "Time Step (n)", ylab = "Probability",
        main = "A1: Probability Convergence (n=50)")

legend("right", legend = states_A1, 
       col = c("green3", "red", "blue", "orange", "purple"), 
       lty = 1, lwd = 3)