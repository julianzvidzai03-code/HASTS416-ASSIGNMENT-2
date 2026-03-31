# ==========================================
# QUESTION A2: 7-STATE MARKOV CHAIN ANALYSIS
# ==========================================
library(markovchain)
library(igraph)

# --- 1. DEFINE THE MATRIX  ---
states_A2 <- c("S1", "S2", "S3", "S4", "S5", "S6", "S7")

P_matrix_A2 <- matrix(c(
  0,   1,   0,   0,   0,   0,   0,   # Row 1 (S1)
  1,   0,   0,   0,   0,   0,   0,   # Row 2 (S2)
  0,   0,   0,   0.4, 0.2, 0.2, 0.2, # Row 3 (S3)
  0,   0,   0,   0,   0.2, 0.4, 0.4, # Row 4 (S4)
  0.3, 0,   0,   0.1, 0.3, 0.1, 0.2, # Row 5 (S5)
  0,   0,   0,   0.2, 0.2, 0.3, 0.3, # Row 6 (S6)
  0,   0,   0,   0.5, 0.2, 0.2, 0.1  # Row 7 (S7)
), nrow = 7, byrow = TRUE)

mc_A2 <- new("markovchain", states = states_A2, byrow = TRUE, transitionMatrix = P_matrix_A2)

# ---  PART (a)
full_layout <- matrix(c(
  -8,  3,   # S1
  -8, -3,   # S2
  0,  5,   # S3 
  5,  2,   # S4
  -2,  1,   # S5 
  -2, -3,   # S6 
  4, -3    # S7
), ncol = 2, byrow = TRUE)

plot(mc_A2, package = "igraph", layout = full_layout,
     vertex.size = 35, vertex.color = c("salmon", "salmon", rep("gold", 5)),
     edge.arrow.size = 0.5, edge.curved = 0.3, main = "A2: 7-State Transition Diagram")

# --- 3. PART (b): ---
cat("\n--- PART B: CLASSIFICATION ---\n")
summary(mc_A2)
# Calculation of Period for the recurrent class {S1, S2}
period(mc_A2)

# --- 4. PART (c): TWO TRAJECTORIES ---
traj1 <- rmarkovchain(n = 20, object = mc, t0 = start1)
traj2 <- rmarkovchain(n = 20, object = mc, t0 = start2)

t1_num <- c(as.numeric(start1), as.numeric(traj1))
t2_num <- c(as.numeric(start2), as.numeric(traj2))


plot(0:20, t1_num, type = "b", col = "blue", pch = 16, ylim = c(1, 7),
     xlab = "Step / Time", ylab = "State", main = "Simulated Trajectories")
lines(0:20, t2_num, type = "b", col = "red", pch = 17)
legend("topright", legend = c("Trajectory 1", "Trajectory 2"), 
       col = c("blue", "red"), pch = c(16, 17), lty = 1)

# --- 5. PART (d): LIMITING PROBABILITIES (n=50) ---
# Calculate the stationary distribution
steadyStates(mc)