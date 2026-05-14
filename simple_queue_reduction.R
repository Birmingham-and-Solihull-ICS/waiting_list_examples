library(ggplot2)
library(scales)
library(BSOLwaitinglist)
library(NHSRwaitinglist)
library(BSOLutils)

q <- 1300
d <- 130
t <- 18
weeks <- 52

compliance <- 1 - exp(-(d * t) / q)


target_q <- calc_target_queue_size(d,t, qexp(0.92))

rel <- calc_relief_capacity(d,
                            q,
                            target_q,
                            weeks-1)

dt <-
    data.frame(
        weeks = seq(weeks),
        demand = d,
        capacity = rel,
        queue = q
    )


for (i in 2:nrow(dt)) {
    dt[i,]$queue <-  dt[i-1,]$queue + dt[i,]$demand - dt[i,]$capacity
}

#weeks <- 52 / 12

ggplot(dt, aes(x = weeks, y = queue)) +
    geom_line(colour = icb_theme_cols("cluster_lightblue"), size = 1) +
    geom_hline(yintercept = dt[1,]$queue, colour = icb_theme_cols("cluster_orange"),
               linetype= "dashed", linewidth = 1) +
    geom_hline(yintercept = dt[1,]$queue, colour = icb_theme_cols("cluster_orange"),
               linetype= "dashed", linewidth = 1) +
    geom_hline(yintercept = target_q, colour = icb_theme_cols("cluster_green1"),
               linetype= "dashed", linewidth = 1) +
    #scale_x_continuous(limits = c(0, weeks), expand = expansion(0,0.01)) +
    #scale_y_continuous(limits = c(0, weeks), expand = expansion(0,0.01)) +
    annotate("text", label = paste0("Target queue size = ", round(target_q) )
             , y = target_q , x = 1
             , colour = icb_theme_cols("cluster_green1"), vjust = -0.5,
             hjust = 0) +
    annotate("text", label = paste0("Current queue size = ", round(q) )
             , y = q , x = weeks
             , colour = icb_theme_cols("cluster_orange"), vjust = 1.3,
             hjust = 1) +

    scale_y_continuous(labels = comma) +
    labs(
        title = paste0(
            "Factor for ", percent(compliance), "% compliance to waiting list target"
        ),
        subtitle = "Exponential distribution used for queue, assuming 'perfect world'",
        x = "Time in Weeks",
        y = "Queue Size"
    ) +
    theme_icb()





