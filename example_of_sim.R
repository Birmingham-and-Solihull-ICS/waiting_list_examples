#remotes::install_github("Birmingham-and-Solihull-ICS/BSOLwaitinglist")

library(BSOLwaitinglist)
library(NHSRwaitinglist)

# Let's assume:
# imagine we have a list of 350, with a weekly
# Weekly referrals 55
# Weekly capacity 60
# Slow reduction in queue

wl_size <- 350
referrals <- 55
capacity <- 60


# Wrong way of setting up waiting list, specificlly mentioned in Rich & Luke's paper.
current_wl <- data.frame(
  Referral = rep(as.Date('2025-03-31')
                 , wl_size),
  Removal = rep(as.Date(NA), wl_size)
)


# Simulate for 3 months, using current_wl as starter
# There's a few stray _ on the end of names, should pull them out.
sim <- wl_simulator_cpp(
  start_date_ = as.Date('2025-04-01'),
  end_date_ = as.Date('2025-06-30'),
  demand = referrals,
  capacity = capacity, # project last point forward
  waiting_list_ = current_wl
)


# Queue size function from NHSRwaiting list
queue1 <- wl_queue_size(sim)

# Basic plot of queue
ggplot(queue1, aes(x=dates, y = queue_size))+
  geom_line(col = "#00204DFF") +
  theme_minimal()



############################################
# Same thing applied in as monte carlo x 20
############################################

# Function to run multiple times.
mc_func <- function(){
  wl_list <-
    wl_simulator_cpp(
    start_date_ = as.Date('2025-04-01'),
    end_date_ = as.Date('2025-06-30'),
    demand = referrals,
    capacity = capacity, # project last point forward
    waiting_list_ = current_wl
  )

  wl_queue_size(wl_list)
}



# Nice parallelism in R using future
library(future.apply)
library(parallel)

cl <- future::makeClusterPSOCK(workers = 5)

clusterEvalQ(cl, {
  library(BSOLwaitinglist)

})
clusterExport(cl, "mc_func")
plan(cluster, workers = cl)


# return list of 50 simulation outputs
mc_out <- future_replicate(50, mc_func(), simplify = FALSE)


# Stop cluster and set back to sequential execution
parallel::stopCluster(cl)
gc()

plan(sequential)


# bind lists together in single data.frame plus id per run, then calculate average
all_results <-
  bind_rows(mc_out, .id = "index") %>%
  mutate(index = as.integer(index))


# aggregate, should probably do this in tidyverse, but didn't....
mc_agg <-
  aggregate(
    queue_size ~ dates
    , data = all_results
    , FUN = \(x) {
      c(mean_q = mean(x),
        median_q = median(x),
        lower_95CI = mean(x) -  (1.96 * (sd(x) / sqrt(length(x)))),
        upper_95CI = mean(x) +  (1.96 * (sd(x) / sqrt(length(x)))),
        q_25 = quantile(x, .025, names = FALSE),
        q_75 = quantile(x, .975, names = FALSE))
    }
  )

# Rename as aggregate funciton is a bit odd with names
mc_agg <- data.frame(dates = as.Date(mc_agg$dates), unlist(mc_agg$queue_size))


# plot runs
# Red is mean and 95% point-wise confidence interval for mean queue per day.
ggplot(all_results, aes(x = dates, y = queue_size)) +
  geom_line(aes(group = index), colour = "grey", alpha = 0.5) +
  geom_line(aes(y = mean_q), colour = "red", data = mc_agg) +
  geom_ribbon(aes(y = mean_q, ymin = lower_95CI, ymax = upper_95CI),
                fill = "red", alpha = 0.2, data = mc_agg) +
  theme_minimal()






# Thoughts for functions:
#
# Change time-base: it's currently in weeks, and I've not changed it, but should be an option I think
# Add a renage percentage or value.
# Add the approach to warm-up the starting waiting list distribution.  It would be required to take a waiting list input:
#   (2 column data.frame: Date added, Date removed), but all
# There some slightly odd behaviour at the start of each period, might be due to the warm up, and this make it tricky
# when iterating over different periods with changing demand and capacity, as the piece-wise sections don't always line up.
# We should consider whether we want the whole queue out each time, or just the 'current waiting list' function output.