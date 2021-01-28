emptyTune <- new_tune()
abcTune <- set_param_names(emptyTune, c("a", "b", "c"))


# Testing new_tune()
test_that("new_tune() sets proper class",
          expect_equal(class(emptyTune), "tune")
          )

test_that("new_tune() sets 3 slots",
          expect_equal(length(emptyTune), 3)
          )

test_that("new_tune() has proper slot names",
          expect_equal(names(emptyTune), c("grid", "currentTune", "bestTune"))
          )

test_that("newTune() sets all data.frames to NA",
                expect_true(all(is.na(emptyTune[[]][])))
          )

test_that("new_tune() sets all data.frame cols to 4",
            {
                expect_equal(ncol(emptyTune[[1]]), 4)
                expect_equal(ncol(emptyTune[[2]]), 4)
                expect_equal(ncol(emptyTune[[3]]), 4)
            }
          )

test_that("new_tune() sets all data.frame rows to 1",
          {
              expect_equal(nrow(emptyTune[[1]]), 1)
              expect_equal(nrow(emptyTune[[2]]), 1)
              expect_equal(nrow(emptyTune[[3]]), 1)
          }
)

test_that("newTune() has all identical slots",
            {
                expect_identical(emptyTune[[1]], emptyTune[[2]])
                expect_identical(emptyTune[[2]], emptyTune[[3]])
            }
          )


#testing set_param_names()

abcTune <- set_param_names(emptyTune, c("a", "b", "c"))

test_that("set_param_names() sets all data.frame colnames",
{
    expect_identical(colnames(abcTune[[1]][1:3]), c("a", "b", "c"))
    expect_identical(colnames(abcTune[[2]][1:3]), c("a", "b", "c"))
    expect_identical(colnames(abcTune[[3]][1:3]), c("a", "b", "c"))
}
)


test_that("set_param_names() maintains class",
          expect_equal(class(emptyTune), class(abcTune))
          )


# testing clone_bottom_row
abcCloned <- clone_bottom_row(abcTune)

test_that("clone_bottom_row() adds a bottom row",
          expect_equal(nrow(abcTune$grid) + 1, nrow(abcCloned$grid))
          )


# testing reTune()
filledTune <- tuneKproto()
reTuned <- reTune(filledTune, k = 99, p = 10, featWeights = NA)

test_that("reTune() sets proper parameters in $currentTune",
              {
                  expect_equal(reTuned$currentTune$k, 99)
                  expect_equal(reTuned$currentTune$p, 10)
                  expect_equal(reTuned$currentTune$featWeights, NA)
              }
         )
