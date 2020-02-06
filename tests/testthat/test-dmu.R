
p <- runif(1000L)

testthat::test_that(
  "Derivative functions for link are equal",{
    testthat::expect_equal(
      dmudeta_probit(p),
      make.link("probit")$mu.eta(p)
    )
    
    testthat::expect_equal(
      dmudeta_logit(p),
      make.link("logit")$mu.eta(p)
    )
  }
)



# microbenchmark::microbenchmark(
#   dmudeta_probit(p),
#   make.link("probit")$mu.eta(p)
# )
# microbenchmark::microbenchmark(
#   dmudeta_logit(p),
#   make.link("logit")$mu.eta(p)
# )