test_that("factor smooth plots accept axis limits", {
  set.seed(1)
  dat <- data.frame(x = runif(150), fac = factor(rep(1:3, each = 50)))
  dat$y <- sin(2 * pi * dat$x) + rnorm(150, sd = 0.1)
  b <- getViz(gam(y ~ s(x, fac, bs = "fs", k = 5), data = dat))

  expect_warning(
    p <- plot(sm(b, 1)) + l_fitLine() +
      ylim(-2, 2) + xlim(0, 1),
    NA
  )
  expect_equal(p$ggObj$scales$get_scales("x")$limits, c(0, 1))
  expect_equal(p$ggObj$scales$get_scales("y")$limits, c(-2, 2))
  expect_warning(g <- ggplot2::ggplotGrob(p$ggObj), NA)
  expect_s3_class(g, "gtable")
})

test_that("smooth plots accept guides after raster and contour layers", {
  set.seed(2)
  dat <- data.frame(x = runif(150), z = runif(150))
  dat$y <- sin(2 * pi * dat$x) + dat$z + rnorm(150, sd = 0.1)
  b <- getViz(gam(y ~ s(x, z), data = dat))

  expect_warning(
    p <- plot(sm(b, 1)) + l_fitRaster() + l_fitContour() +
      labs(title = NULL) + guides(fill = "none"),
    NA
  )
  expect_warning(g <- ggplot2::ggplotGrob(p$ggObj), NA)
  expect_s3_class(g, "gtable")
  expect_false(any(grepl("^guide-box", g$layout$name) &
                     !vapply(g$grobs, inherits, logical(1), "zeroGrob")))
})

test_that("multi-plot additions apply scales and guides to every plot", {
  set.seed(3)
  dat <- data.frame(x = runif(150), z = runif(150))
  dat$y <- sin(2 * pi * dat$x) + dat$z + rnorm(150, sd = 0.1)
  b <- getViz(gam(y ~ s(x) + s(z), data = dat))

  expect_warning(
    p <- plot(b) + l_fitLine() + xlim(0, 1) + guides(colour = "none"),
    NA
  )
  expect_length(p$plots, 2)
  for (panel in p$plots) {
    expect_equal(panel$ggObj$scales$get_scales("x")$limits, c(0, 1))
    expect_warning(g <- ggplot2::ggplotGrob(panel$ggObj), NA)
    expect_s3_class(g, "gtable")
  }
})

test_that("ordinary ggplots still accept components after loading mgcViz", {
  expect_warning(
    p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
      geom_point() + xlim(0, 6) + guides(colour = "none") + theme_bw(),
    NA
  )
  expect_equal(p$scales$get_scales("x")$limits, c(0, 6))
  expect_warning(g <- ggplot2::ggplotGrob(p), NA)
  expect_s3_class(g, "gtable")
})
