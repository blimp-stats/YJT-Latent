## Analyze Results

# Set wd to script
fdir::set()

# Read one replication
read_rep <- function(i) {
    tryCatch(
        paste0('results/rep', i, '.rds') |> readRDS(),
        error = \(e) {list(
            upi = data.frame(est = rep(NA, 3)),
            nor = data.frame(est = rep(NA, 4)),
            yjt = data.frame(est = rep(NA, 4)),
            yjtcor = data.frame(est = rep(NA, 4))
        )}
    ) |> suppressWarnings()
}

# Summarize
summarize <- function(r) {
    pop <- c(0.5, 0.5, 0.2)
    estimates <- r |> rowMeans(dims = 2, na.rm = T)
    mse <-  r |> apply(2, \(.) rowMeans((. - pop)^2, na.rm = T))
    list(
        n_rep = r |> is.na() |> apply(3, any) |> sapply(isFALSE) |> sum(),
        estimate = estimates,
        pbias = (estimates - pop ) / pop,
        mse = mse,
        mse_ratio = cbind(
            `upi/yjt` = mse[,'upi'] / mse[,'yjt'],
            `nor/yjt` = mse[,'nor'] / mse[,'yjt'],
            `cor/yjt` = mse[,'yjtcor'] / mse[,'yjt']
        )
    )
}

# Number of replications
n_rep <- 1000

# Read and summarize
n_rep |> seq_len() |> lapply(read_rep) |>
    sapply(with, simplify = 'array', {
        cbind(
            upi = upi[,'est'],
            nor = nor[2:4, 1],
            yjt = yjt[2:4, 1],
            yjtcor = yjtcor[2:4, 1]
        )
    }) |> summarize() -> results

# Print out results
results |> lapply(round, digits = 4)

