context("strip methods reduce memory requirements")


# GLM OBJECT -----------------------------------

utils::data(anorexia, package = "MASS")

anorex.1 <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
                family = gaussian, data = anorexia)

obj_strip <- gravity::strip(anorex.1)


test_that("[glm] less memory used by stripped object", {
  expect_true(object.size(obj_strip)<object.size(anorex.1))
})


model_anorex <- summary(anorex.1)
model_anorex_strip <- gravity::strip(model_anorex)


test_that("[summary.glm] less memory used by stripped object", {
  expect_true(object.size(model_anorex_strip)<object.size(model_anorex))
})


# PSCL::ZEROINFL OBJECT -----------------------------

data("bioChemists", package = "pscl")


fm_zinb <- pscl::zeroinfl(art ~ . | 1, data = bioChemists, dist = "negbin")


test_that("[pscl::zeroinfl] less memory used by stripped object", {
  expect_true(object.size(gravity::strip(fm_zinb))<object.size(fm_zinb))
})

model_zeroinfl <- summary(fm_zinb)

test_that("[pscl::zeroinfl] less memory used by stripped object", {
  expect_true(object.size(gravity::strip(model_zeroinfl))<object.size(model_zeroinfl))
})


# GRAVITY::FASTZEROINFL OBJECT --------------------

fm_zinb <- gravity::fastzeroinfl(art ~ . | 1, data = bioChemists, dist = "negbin")

test_that("[gravity::fastzeroinfl] less memory used by stripped object", {
  expect_true(object.size(gravity::strip(fm_zinb))<object.size(fm_zinb))
})

model_zeroinfl <- summary(fm_zinb)

test_that("[gravity::fastzeroinfl] less memory used by stripped object", {
  expect_true(object.size(gravity::strip(model_zeroinfl))<object.size(model_zeroinfl))
})
