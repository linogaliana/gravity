testthat::context("strip methods reduce memory requirements")


# GLM OBJECT -----------------------------------

utils::data(anorexia, package = "MASS")

anorex.1 <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
                family = gaussian, data = anorexia)

obj_strip <- gravity::strip(anorex.1)


testthat::test_that("[glm] less memory used by stripped object", {
  testthat::expect_true(object.size(obj_strip)<object.size(anorex.1))
})

testthat::test_that("[glm] stripped object has new class", {
  testthat::expect_equal(class(obj_strip), c(class(anorex.1),"light.glm"))
})

model_anorex <- summary(anorex.1)
model_anorex_strip <- gravity::strip(model_anorex)


testthat::test_that("[summary.glm] less memory used by stripped object", {
  testthat::expect_true(object.size(model_anorex_strip)<object.size(model_anorex))
})

testthat::test_that("[summary.glm] stripped object has new class", {
  testthat::expect_equal(class(model_anorex_strip), c(class(model_anorex),"light.summary.glm"))
})


# PSCL::ZEROINFL OBJECT -----------------------------

data("bioChemists", package = "pscl")


fm_zinb <- pscl::zeroinfl(art ~ . | 1, data = bioChemists, dist = "negbin")


testthat::test_that("[pscl::zeroinfl] less memory used by stripped object", {
  testthat::expect_true(object.size(gravity::strip(fm_zinb))<object.size(fm_zinb))
})

testthat::test_that("[pscl::zeroinfl] stripped object has new class", {
  testthat::expect_equal(class(gravity::strip(fm_zinb)), c(class(fm_zinb),"light.zeroinfl"))
})

model_zeroinfl <- summary(fm_zinb)

testthat::test_that("[pscl::zeroinfl] less memory used by stripped object", {
  testthat::expect_true(object.size(gravity::strip(model_zeroinfl))<object.size(model_zeroinfl))
})
testthat::test_that("[pscl::summary.zeroinfl] stripped object has new class", {
  testthat::expect_equal(class(gravity::strip(model_zeroinfl)), c(class(model_zeroinfl),"light.summary.zeroinfl"))
})


# GRAVITY::FASTZEROINFL OBJECT --------------------

fm_zinb <- gravity::fastzeroinfl(art ~ . | 1, data = bioChemists, dist = "negbin")

testthat::test_that("[gravity::fastzeroinfl] less memory used by stripped object", {
  testthat::expect_true(object.size(gravity::strip(fm_zinb))<object.size(fm_zinb))
})
testthat::test_that("[pscl::zeroinfl] stripped object has new class", {
  testthat::expect_equal(class(gravity::strip(fm_zinb)), c(class(fm_zinb),"light.zeroinfl"))
})

model_zeroinfl <- summary(fm_zinb)

test_that("[gravity::fastzeroinfl] less memory used by stripped object", {
  expect_true(object.size(gravity::strip(model_zeroinfl))<object.size(model_zeroinfl))
})
testthat::test_that("[pscl::summary.zeroinfl] stripped object has new class", {
  testthat::expect_equal(class(gravity::strip(model_zeroinfl)), c(class(model_zeroinfl),"light.summary.zeroinfl"))
})



# TRIM GLM ----------------------

utils::data(anorexia, package = "MASS")

anorex.1 <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
                family = gaussian, data = anorexia)


# gravity::trim_big_object(model_fat)


