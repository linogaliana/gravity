
p <- runif(1000L)

testthat::test_that(
  "Derivative functions for link are equal",{
    testthat::expect_equal(
      dmudeta_probit(p),
      make.link("probit")$mu.eta(p)
    )
  }
)
