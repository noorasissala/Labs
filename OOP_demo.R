library(R6)

Agent <- R6Class("Agent",
                 public = list(
                   state = character(),
                   initialize = function(state = "healthy") {
                     self$state <- state
                   },
                   update_state = function(sick_prob, recovery_prob, death_prob) {
                     if (self$state == "healthy" && runif(1) <= sick_prob) {
                       self$state <- "sick"
                     } else if (self$state == "sick") {
                       fate <- runif(1)
                       if (fate <= recovery_prob) {
                         self$state <- "immune"
                       } else if (fate > recovery_prob && fate <= (recovery_prob + death_prob)) {
                         self$state <- "dead"
                       } else if (fate > recovery_prob + death_prob) {
                         self$state == "sick"                   # Explicitly written for teaching purposes only
                       }
                     }
                     # Immune and dead states do not change
                   }
                 )
)

World <- R6Class("World",
                 public = list(
                   size = NULL,
                   world = NULL,
                   
                   initialize = function(size) {
                     self$size <- size
                     self$world <- matrix(vector("list", size * size), nrow = size, ncol = size)
                     for (i in 1:size) {
                       for (j in 1:size) {
                         self$world[[i, j]] <- list()
                       }
                     }
                   },
                   
                   add_agent = function(row, col, agent) {
                     self$world[[row, col]] <- c(self$world[[row, col]], list(agent))
                   },
                   
                   move_agents = function() {
                     new_world <- matrix(vector("list", self$size * self$size), nrow = self$size, ncol = self$size)
                     for (i in 1:self$size) {
                       for (j in 1:self$size) {
                         agents <- self$world[[i, j]]
                         for (agent in agents) {
                           if (agent$state != "dead") {
                             move <- rnorm(2, mean = 0, sd = 1)
                             new_row <- min(max(1, round(i + move[1])), self$size)
                             new_col <- min(max(1, round(j + move[2])), self$size)
                             new_world[[new_row, new_col]] <- c(new_world[[new_row, new_col]], list(agent))
                           } else {
                             new_world[[i, j]] <- c(new_world[[i, j]], list(agent))
                           }
                         }
                       }
                     }
                     self$world <- new_world
                   },
                   
                   update_states = function(beta, recovery_prob, death_prob) {
                     for (i in 1:self$size) {
                       for (j in 1:self$size) {
                         agents <- self$world[[i, j]]
                         num_sick <- sum(vapply(agents, function(agent) agent$state == "sick", logical(1)))
                         # Now, probability of becoming sick is naively proportional to percentage of 
                         # already sick individuals in a cell
                         sick_prob <- 1 - (1 - beta) ^ num_sick
                         for (agent in agents) {
                           agent$update_state(sick_prob, recovery_prob, death_prob)
                         }
                       }
                     }
                   },
                   
                   get_counts = function() {
                     counts <- data.frame(
                       row = integer(),
                       col = integer(),
                       healthy = integer(),
                       immune = integer(),
                       sick = integer(),
                       dead = integer()
                     )
                     for (i in 1:self$size) {
                       for (j in 1:self$size) {
                         agents <- self$world[[i, j]]
                         counts <- rbind(counts, data.frame(
                           row = i,
                           col = j,
                           healthy = sum(vapply(agents, function(agent) agent$state == "healthy", logical(1))),
                           immune = sum(vapply(agents, function(agent) agent$state == "immune", logical(1))),
                           sick = sum(vapply(agents, function(agent) agent$state == "sick", logical(1))),
                           dead = sum(vapply(agents, function(agent) agent$state == "dead", logical(1)))
                         ))
                       }
                     }
                     return(counts)
                   }
                 )
                 )
                 
                 
library(ggplot2)
library(gganimate)

run_simulation <- function(size, num_steps, initial_immune, initial_sick, beta, recovery_prob, death_prob) {
  world <- World$new(size)
  
  # Initialize agents
  if (initial_healthy > 0) {
    for (i in 1:initial_healthy) {
      row <- sample(1:size, 1)
      col <- sample(1:size, 1)
      world$add_agent(row, col, Agent$new())
    }
    }
  
  # Introduce immune and sick agents
  if (initial_immune > 0) {
    for (i in 1:initial_immune) {
      row <- sample(1:size, 1)
      col <- sample(1:size, 1)
      world$add_agent(row, col, Agent$new(state = "immune"))
    }
    }
  
  if (initial_sick > 0) {
    for (i in 1:initial_sick) {
      row <- sample(1:size, 1)
      col <- sample(1:size, 1)
      world$add_agent(row, col, Agent$new(state = "sick"))
      }
}

results <- list()

# Simulate one generation
for (step in 1:num_steps) {
world$update_states(beta, recovery_prob, death_prob)
world$move_agents()
counts <- world$get_counts()
counts$step <- step
results[[step]] <- counts
}

results <- do.call(rbind, results)

return(results)
}

visualize_simulation <- function(results) {
p <- ggplot(results, aes(x = col, y = row)) +
geom_tile(aes(fill = sick)) +
scale_fill_gradient(low = "white", high = "red") +
theme_minimal() +
labs(title = "Disease Spread Simulation", x = "X", y = "Y", fill = "Sick Individuals") +
transition_states(step, transition_length = 2, state_length = 1) +
ease_aes('linear') +
ggtitle('Disease Spread Simulation - Generation {closest_state}') +
theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

animate(p, nframes = length(unique(results$step)), fps = 2, renderer = gifski_renderer())
}



# Parameters
size <- 20                    # World size
num_steps <- 50               # Number of generations
initial_healthy <- 998
initial_immune <- 0
initial_sick <- 2
beta <- 0.5                  # Transmission probability
recovery_prob <- 0.1
death_prob <- 0.05

# Run the simulation
results <- run_simulation(size, num_steps, initial_immune, initial_sick, beta, recovery_prob, death_prob)

library(tidyverse)
results |> group_by(step) |> 
summarise(n_sick = sum(sick), n_healthy = sum(healthy), n_immune = sum(immune), n_dead = sum(dead)) |> 
pivot_longer(starts_with("n_"), names_to = 'measure') |> 
ggplot(aes(x = step, y = value, col=measure)) + geom_line() + theme_minimal()

# Visualize the results
visualize_simulation(results)

                 
