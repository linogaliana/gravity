## code to prepare `speed` dataset goes here

sizes <- c(1e3, 1e4, 1e5, 5e5, 1e6, 3e6, 5e6, 8e6, 1e7, 3e7, 5e7, 8e7)

test_speed <- lapply(sizes, function(n) ({
  as.numeric(
    system.time(
      pscl::zeroinfl(formula = y2 ~ x1 + x2,
                     offset = off,
                     control = pscl::zeroinfl.control(reltol = 1e-2),
                     data = df[1:n,])
    )['elapsed']
  )}))

test_speed2 <- lapply(sizes, function(n) ({
  as.numeric(
    system.time(
      gravity::fastzeroinfl(formula = y2 ~ x1 + x2,
                            offset = off,
                            control = pscl::zeroinfl.control(reltol = 1e-2),
                            data = df[1:n,])
    )['elapsed']
  )}))

vitesse_zeroinfl <- data.frame(
  size = sizes,
  pscl = as.numeric(test_speed),
  gravity = as.numeric(test_speed2)
)


usethis::use_data("speed")
