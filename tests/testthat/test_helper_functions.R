context("Helper Functions")

test_that("vech and devech", {
  M <- matrix(c(11, 12, 13, 14,
                12, 22, 23, 24,
                13, 23, 33, 34,
                14, 24, 34, 44), 4, 4, byrow = TRUE)
  v <- drop(vech(M))
  expect_equal(v, c(11:14, 22:24, 33:34, 44))
  expect_that(vech(matrix(1:6, 3, 2)), throws_error())
  expect_equal(devech(v, 4), M)
  expect_that(devech(v, 3), throws_error())
})

test_that("draw_wishart matches rwish from MCMCpack", {
  M <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
  M_inv <- matrix(c(4/3, -2/3, -2/3, 4/3), 2, 2)
  m <- c(-3, 3)
  set.seed(342)
  sims_rwish <- replicate(100, MCMCpack::rwish(5, M))
  set.seed(342)
  sims_draw_wishart <- replicate(100, draw_wishart(5, M))
  expect_equal(sims_rwish, sims_draw_wishart)
})

test_that("density_normal matches dmvnorm from mvtnorm", {
  M <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
  M_inv <- matrix(c(4/3, -2/3, -2/3, 4/3), 2, 2)
  m <- c(-3, 3)
  x <- cbind(c(1, 1), c(-1, 1), c(-1, -1))
  f_mine <- density_normal(x, mu = m, Sigma_inv = M_inv)
  f_dmvnorm <- mvtnorm::dmvnorm(t(x), mean = m, sigma = M)
  expect_equal(drop(f_mine), f_dmvnorm)
  logf_mine <- density_normal(x, mu = m, Sigma_inv = M_inv, logret = TRUE)
  logf_dmvnorm <- mvtnorm::dmvnorm(t(x), mean = m, sigma = M, log = TRUE)
  expect_equal(drop(logf_mine), logf_dmvnorm)
})

test_that("density_wishart matches dwish from MCMCpack", {
  M_inv <- matrix(c(4/3, -2/3, -2/3, 4/3), 2, 2)
  f_mine <- density_wishart(3 * M_inv, 5, M_inv)
  f_MCMCpack <- MCMCpack::dwish(3 * M_inv, 5, M_inv)
  expect_equal(f_mine, f_MCMCpack)
  logf_mine <- density_wishart(3 * M_inv, 5, M_inv, logret = TRUE)
  all.equal(logf_mine, log(f_MCMCpack))
})
