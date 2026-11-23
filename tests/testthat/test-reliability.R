test_that("reliability compares the error with the spread between sites", {
  x <- data.frame(a = c(0, 10, 20, 30), a_se = 5, b = c(1, 2, 3, 4), b_lo95 = c(1, 2, 3, 4) - stats::qnorm(0.975), b_hi95 = c(1, 2, 3, 4) + stats::qnorm(0.975))
  r <- attribute_reliability(x)
  expect_equal(r$reliability[r$variable == "a"], 1 - 25 / stats::var(x$a))
  expect_equal(r$rms_se[r$variable == "b"], 1, tolerance = 1e-6)
  expect_equal(r$se_from, c("interval", "se"))
  expect_error(attribute_reliability(x, "nope"), "No _se")
})
