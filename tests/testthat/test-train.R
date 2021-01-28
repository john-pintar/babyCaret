# testing methodMatchTune()
test_that("methodMatchTune errors when tune obj is mismatched",
          {
              expect_error(methodMatchTune("knn", tuneTree()))
              expect_error(methodMatchTune("apriori", tuneKproto()))
          })


missing <- cars[1, 1] <- NA
suppressWarnings(notMissing <- noNA(missing))
#testing noNA()

test_that("noNA() warns for missing data",
          {
              expect_warning(noNA(missing))
          })

test_that("noNA() removes missing data",
          {
              expect_false(anyNA(notMissing))
          })


# testing train() warnings
test_that("train() warns when no target is provided for class/regress",
          {
              expect_warning(train(iris, "knn", tuneKnn()))
              expect_warning(train(cars, "tree", tuneTree()))
          })

test_that("train() warns when a target is provided for rules/clust",
           {
               # This is failing because of the bug described in TODO.txt
               expect_warning(train(cars$speed, cars, "kproto", tuneKproto()))
           })

test_that("train() warns when a formula is provided for rules/clust",
          {
              expect_warning(train(cars$speed ~ cars$dist, cars, "apriori", tuneApriori()))
          }
          )


# testing train() errors
test_that("train() warns when user-method DNE",
          {
              expect_error(train(iris$Sepal.Length, iris, "check", trainKnn()))
              expect_error(train(iris$Species ~., iris, "KNN", trainKnn()))
          })

test_that("train() errors when data argument is character type",
          {
              expect_error(train(cars$speed, "knn", tuneKnn()))
              expect_error(train(iris$Petal.Length, data = "hello", tuneTree()))
          })

