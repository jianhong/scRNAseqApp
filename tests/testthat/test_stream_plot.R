test_that("stream plot works not correct", {
    set.seed(42)
    n <- 200
    dt <- data.table(
        time   = rep(seq.int(n), times = 8),
        value  = c(rpois(n, 1), rpois(n, 2),
                   rpois(n, 3), rpois(n, 4),
                   rpois(n, 1), rpois(n, 2),
                   rpois(n, 3), rpois(n, 4)),
        group  = rep(rep(c("A", "B", "C", "D"), each = n), 2),
        groupY = rep(c("gp1", "gp2"), each = n * 4)
    )
    
    p <- stream_plot(dt, x = "time", y = "value",
                     group = "group", groupY = "groupY",
                     n_grid = 20, type = "proportional")
    
    dat <- p$data
    
    # expected proportions from lambda
    expected <- c(A = 0.1, B = 0.2, C = 0.3, D = 0.4)
    
    # compute mean smoothed proportion per group (height = ymax - ymin)
    obs <- dat[, list(obs_prop = mean(.SD$ymax - .SD$ymin)),
               by = c("groupY", "group")]
    
    # check within tolerance (loess won't be exact, allow ~5%)
    obs[, "exp_prop" := expected[as.character(.SD$group)]]
    obs[, "diff" := abs(.SD$obs_prop - .SD$exp_prop)]
    
    expect_true(all(obs$diff < 0.05))
})
