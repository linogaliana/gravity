context("BIC formula is correct")

fit_glm  <- MASS::glm.nb(Days ~ Sex + Age,
                                  data = MASS::quine)
fit_glm2  <- gravity::fastglm.nb(Days ~ Sex + Age,
                                       data = MASS::quine)
fit_zeroinfl  <- gravity::fastzeroinfl(Days ~ Sex + Age,
                              data = MASS::quine,
                              dist = "poisson")
fit_zeroinfl2  <- gravity::fastzeroinfl(Days ~ Sex + Age,
                                       data = MASS::quine,
                                       dist = "negbin")

test_that("Same BIC than MASS for glm.nb objects", {
  expect_equal(BIC(fit_glm), BIC(fit_glm2))
})

test_that("BIC correct for fastzeroinfl poisson", {
  expect_equal(BIC(fit_zeroinfl),  -2*fit_zeroinfl$loglik +
                 (length(fit_zeroinfl$coefficients$count) + length(fit_zeroinfl$coefficients$zero))*log(fit_zeroinfl$n))
})

test_that("BIC correct for fastzeroinfl negbinomial", {
  expect_equal(BIC(fit_zeroinfl2),  -2*fit_zeroinfl2$loglik +
                 (length(fit_zeroinfl2$coefficients$count) + length(fit_zeroinfl2$coefficients$zero)+1)*log(fit_zeroinfl2$n))
})
