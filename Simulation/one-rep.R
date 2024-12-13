library(rblimp)
library(lavaan)

one_rep <- function(seed) {
    
    ## Generate Data
    # Set sample size
    N <- 1000
    n_items <- seq_len(5)
    
    # Set seed
    set.seed(seed)
    
    # Centering function
    center <- \(x) x - mean(x)
    
    # Generate data
    rnorm(N, 0, sqrt(0.22)) |> mdmb::yj_antitrafo(1.5) |> center() -> enjoy
    rnorm(N, 0.4 * enjoy, sqrt(0.4)) |> mdmb::yj_antitrafo(1.5) |> center()  -> self
    rnorm(N, 0.5 * enjoy + 0.5 * self + 0.2 * self * enjoy, sqrt(0.4)) |> 
        center()  |> mdmb::yj_antitrafo(1.5) -> asp
    
    enjoy_items <- sapply(n_items, \(.) {
        rnorm(N, enjoy, sqrt(0.5))
    })
    
    self_items <- sapply(n_items, \(.) {
        rnorm(N, self, sqrt(0.5))
    })
    
    asp_items <- sapply(n_items, \(.) {
        rnorm(N, asp, sqrt(0.5))
    })
    
    
    dataset <- as.data.frame(
        cbind(
            enjoy_items,
            self_items,
            asp_items
        )
    )
    names(dataset) <- c(
        paste0('enjoy', n_items),
        paste0('self', n_items),
        paste0('asp', n_items)
    )
    
    
    mdl2 <- rblimp(
        c(
            focal ='yjt(asp) ~ enjoy self enjoy*self',
            other = 'enjoy -> enjoy1:enjoy5', 
            'self -> self1:self5',
            'asp -> asp1:asp5',
            'yjt(self) ~ enjoy',
            'yjt(enjoy) ~ 0'
        ),
        dataset,
        seed = seed,
        burn = 10000,
        iter = 10000,
        latent = ~ self + asp + enjoy,
        print_output = F
    )
    
    mdl3 <- rblimp(
        c(
            focal ='yjt(asp) ~ enjoy self enjoy*self',
            other = 'enjoy -> enjoy1:enjoy5', 
            'self -> self1:self5',
            'asp -> asp1:asp5',
            'yjt(self) ~ 0',
            'yjt(enjoy) ~ 0',
            'self ~~ enjoy'
        ),
        dataset,
        seed = seed,
        burn = 10000,
        iter = 10000,
        latent = ~ self + asp + enjoy,
        print_output = F
    )
    
    mdl1 <- rblimp(
        c(
            focal ='asp ~ enjoy self enjoy*self',
            other = 'enjoy -> enjoy1:enjoy5', 
            'self -> self1:self5',
            'asp -> asp1:asp5',
            'self ~~ enjoy'
        ),
        dataset,
        seed = seed,
        burn = 10000,
        iter = 10000,
        latent = ~ self + asp + enjoy,
        print_output = F
    )
    
    # Create data set
    dataProd <- dataset |>
        semTools::indProd(
            paste0('enjoy', n_items),
            paste0('self', n_items),
            match = T, meanC = T
        )
    # Fit Lavaan model
    lav_model <- c(
        'Enjoy         =~ enjoy1 + enjoy2 + enjoy3 + enjoy4 + enjoy5',
        'SelfConcept   =~ self1 + self2 + self3 + self4 + self5',
        'Aspirations   =~ asp1 + asp2 + asp3 + asp4 + asp5',
        'Interact      =~ enjoy1.self1 + enjoy2.self2 + enjoy3.self3 + enjoy4.self4 + enjoy5.self5',
        'Aspirations    ~ Enjoy + SelfConcept + Interact'
    ) |> sem(dataProd, estimator = "MLR") -> ext_m
    
    # Summarize output
    # summary(ext.m, fit.measures = TRUE)
    list(
        nor = summary(mdl1)[1:4,],
        yjt = summary(mdl2)[1:5,],
        yjtcor = summary(mdl3)[1:5,],
        upi = lavaan::partable(ext_m)[21:23, c('lhs', 'op', 'rhs','est', 'se')]
    )
}

# Run 
Sys.getenv('SEED') |> as.numeric() |> one_rep() |> 
    saveRDS(paste0('results/rep', Sys.getenv('REP'), '.rds'))
