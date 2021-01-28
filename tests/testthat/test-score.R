# testing misClass()
tf1 <- c(T, T, F, F, T)
tf2 <- c(T, F, F, T, F)

fact1 <- as.factor(c(1, 2, 3, 1, 1, 2, 3, 1, 3, 2))
fact2 <- as.factor(c(3, 2, 1, 1, 2, 3, 4, 2, 3, 1))

num1 <- c(1, 2, 2, 1, 2, 1)
num2 <- c(2, 2, 2, 2, 2, 2)

test_that("misClass() calculates proper error on simple examples",
          {
              expect_equal(as.numeric(misClass(tf1, tf2)), .6)
              expect_equal(as.numeric(misClass(fact1, fact2)), .7)
              expect_equal(as.numeric(misClass(num1, num2)), .5)
          })

test_that("misClass() sets proper class and inheritance",
          {
              expect_equal(class(misClass(tf1, tf2)), c("MisClass", "numeric"))
          })


# testing mape()
num3 <- as.numeric(fact1)
num4 <- as.numeric(fact2)
num5 <- c(6.4, 6, 5.8, 4.2, 5.6)
num6 <- c(5.4, 6.8, 4.2, 7.8, 5.6)

test_that("mape() calculates error properly on simple examples",
          {
              expect_lt(as.numeric(mape(num1, num2) - .25), .001)
              expect_gt(as.numeric(mape(num1, num2) - .25), -.001)
              expect_lt(as.numeric(mape(num3, num4) - 0.525), .001)
              expect_gt(as.numeric(mape(num3, num4) - 0.525), -.001)
              expect_lt(as.numeric(mape(num5, num6) - 0.229), .001)
              expect_gt(as.numeric(mape(num5, num6) - 0.229), -.001)
          })

test_that("mape() sets proper class and inheritance",
          {
              expect_equal(class(mape(num5, num6)), c("Mape", "numeric"))
          })


### these tests take a lot of time. models are getting trained, but this seems like
### its taking a long time here
# testing test()
test_that("test() does not accept rule or cluster models",
          {
             expect_error(score(train(iris, "kproto", tuneKproto()), iris))
             suppressWarnings((expect_error(score(train(cars, "apriori", tuneApriori()), iris))))
          })
