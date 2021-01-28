# Testing Bpartition()


test_that("returning a list",
    {
        expect_true(is.list(Bpartition(iris)))
        expect_true(is.list(Bpartition(cars, .5)))
    }
)


test_that("ensure df is matrix or data.frame (error)",
    {
        expect_error(Bpartition(as.table(Titanic), .9))
        expect_error(Bpartition(c(1, 2, 3, 4), .8))
        expect_error(Bpartition(1, .8))
        expect_error(Bpartition(c(),))
    }
)


test_that("ensure 0 <= p <= 1 (error)",
    {
        expect_error(Bpartition(iris, p = -0.5))
        expect_error(Bpartition(cars, p = -9999999))
        expect_error(Bpartition(cars, p = 1.01))
        expect_error(Bpartition(iris, p = 10))
    }
)


test_that("warn if p == 1",
    {
        expect_warning(Bpartition(iris, p = 1))
        expect_warning(Bpartition(cars, p = 1))
    }
)


test_that("warn if p == 0",
          {
              expect_warning(Bpartition(iris, p = 0))
              expect_warning(Bpartition(cars, p = 0))
          }
)



elips <- function(...)
{
    a <- list(...)
    return(a)
}
