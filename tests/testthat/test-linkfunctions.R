testthat::context("Inverse logit and probit links well implemented")


# LOGIT LINKS ---------------------------

x <- runif(1000L)

testthat::test_that(
  "R and Rcpp return the same value",{
    testthat::expect_equal(
      logit(x),
      make.link("logit")$linkfun(x)
    )
  }
)


  
# PROBIT LINK ---------------------------

x <- runif(1000L)

testthat::test_that(
  "R and Rcpp return the same value",{
    testthat::expect_equal(
      probit(x),
      make.link("probit")$linkfun(x)
    )
  }
)



# INVERSE LOGIT LINKS -------------------

x <- rnorm(1000L)

testthat::test_that(
  paste0("R and Rcpp return the same value",
         "when using value inside machine precision"
  ), {
    testthat::expect_equal(
      invlogit(x),
      make.link("logit")$linkinv(x)
    )
  }
)

testthat::test_that(
  paste0("R and Rcpp return only positive values"
  ), {
    testthat::expect_equal(
      sum(invlogit(x)>0), length(x)
    )
  }
)


x <- .Machine$double.eps/2
testthat::test_that(
  paste0("R and Rcpp return the same value",
         "when using value outside machine precision"
  ), {
    testthat::expect_equal(
      invlogit(x),
      make.link("logit")$linkinv(x)
    )
  }
)

testthat::test_that(
  "Rcpp version works for numerical value whatever the type",
  {
    testthat::expect_length(invlogit(rep(0.2, 100L)), 100)
    testthat::expect_length(invlogit(0.2), 1)
    testthat::expect_length(invlogit(1L), 1)
  }
)

testthat::test_that(
  "Rcpp version returns error for non numeric values",
  {
    testthat::expect_error(invlogit("a"),
                           class = "Rcpp::not_compatible")
    testthat::expect_equal(invlogit(TRUE),
                           invlogit(1L)) #Bool are converted to 0/1
    testthat::expect_equal(invlogit(FALSE),
                           invlogit(0L)) #Bool are converted to 0/1
  }
)



# INVERSE PROBIT LINKS ------------------

x <- rnorm(1000L)



testthat::test_that(
  paste0("R and Rcpp return only positive values"
  ), {
    testthat::expect_equal(
      sum(invprobit(x)>0), length(x)
    )
  }
)


testthat::test_that(
  paste0("R and Rcpp return the same value",
  "when using value inside machine precision"
  ), {
    testthat::expect_equal(
      invprobit(x),
      make.link("probit")$linkinv(x)
    )
  }
)

x <- .Machine$double.eps/2
testthat::test_that(
  paste0("R and Rcpp return the same value",
         "when using value outside machine precision"
  ), {
    testthat::expect_equal(
      invprobit(x),
      make.link("probit")$linkinv(x)
    )
  }
)

testthat::test_that(
 "Rcpp version works for numerical value whatever the type",
 {
   testthat::expect_length(invprobit(rep(0.2, 100L)), 100)
   testthat::expect_length(invprobit(0.2), 1)
   testthat::expect_length(invprobit(1L), 1)
 }
)

testthat::test_that(
  "Rcpp version returns error for non numeric values",
  {
    testthat::expect_error(invprobit("a"),
                           class = "Rcpp::not_compatible")
    testthat::expect_equal(invprobit(TRUE),
                           invprobit(1L)) #Bool are converted to 0/1
    testthat::expect_equal(invprobit(FALSE),
                           invprobit(0L)) #Bool are converted to 0/1
  }
)


# TESTS DE VITESSE ------------------

# x <- runif(1e6)
# 
# microbenchmark::microbenchmark(
#   invprobit(x),
#   make.link("probit")$linkinv(x),
#   times = 50L
# )
# 
# microbenchmark::microbenchmark(
#   invlogit(x),
#   make.link("logit")$linkinv(x),
#   times = 50L
# )

# x <- runif(1e6)

# microbenchmark::microbenchmark(
#   probit(x),
#   make.link("probit")$linkfun(x),
#   times = 50L
# )
# 
# microbenchmark::microbenchmark(
#   logit(x),
#   make.link("logit")$linkfun(x),
#   times = 50L
# )

