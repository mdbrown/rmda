
test_that("calculate.nb", {

  d = rbinom(25, size = 1, prob = .3)
  y = runif(25)
  ck = seq(0,1, by = .05)

  nb <- calculate.nb(d = d, y = y, ck = ck)

  expect_that( nrow(nb), equals(length(ck)) )
  expect_that(order(nb$threshold), is_identical_to(1:nrow(nb)))
})

