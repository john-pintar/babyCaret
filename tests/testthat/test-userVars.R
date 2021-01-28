# Initializing inputs
myData <- datasets::iris
myTarget <- datasets::iris$Petal.Length
myFormula <- myData[,5] ~ myData[,2]

# Initializing objects
emptyVars <- new_userVars()
defMod <- model_from_default(myData, myTarget)
xVars <- userVars(myTarget, myData)
formVars <- userVars(myFormula, myData)


# Testing new_userVars()
test_that("new_userVars() sets proper class",
            expect_equal(class(emptyVars), "userVars")
         )

test_that("new_userVars() sets 6 slots",
            expect_length(emptyVars, 6)
          )

test_that("new_userVars() sets proper names",
            expect_equal(names(emptyVars), c("data", "dvName", "dv", "modelFrame", "standardFrame", "formula"))
          )


# Testing model_from_default()
test_that("target becomes 1st column",
            expect_equal(myTarget, defMod[,1])
          )

test_that("model frame hasn't gained a column",
          expect_equal(ncol(myData), ncol(defMod))
          )

test_that("original names are maintained",
            expect_true(setequal(names(myData), names(defMod)))
          )


irisStand <- model_to_standard(iris)
carsStand <- model_to_standard(cars)
#testing model_to_standard

test_that("model_to_standard flips proper cols",
          {
              expect_identical(iris[,1], irisStand[,ncol(irisStand)])
              expect_identical(cars[,1], carsStand[,ncol(carsStand)])
          })

test_that("model_to_standard flips col names",
          {
              expect_identical(colnames(iris)[1], colnames(irisStand)[ncol(irisStand)])
              expect_identical(colnames(cars)[1], colnames(carsStand)[ncol(carsStand)])
          })


# Testing userVars()
test_that("userVars() maintains proper class",
          {
            expect_equal(class(xVars), "userVars")
            expect_equal(class(formVars), "userVars")
          }
)

test_that("userVars() maintains 4 slots",
          {
            expect_length(xVars, 6)
            expect_length(formVars, 6)
         }
)

test_that("userVars() maintains proper names",
          {
             expect_equal(names(xVars), c("data", "dvName", "dv", "modelFrame", "standardFrame", "formula"))
             expect_equal(names(formVars), c("data", "dvName", "dv", "modelFrame", "standardFrame", "formula"))
          }
)

test_that("userVars() fills all slots",
          {
                expect_false(anyNA(xVars))
                expect_false(anyNA(xVars))
          }

)

test_that("userVars fills wilth correct datatypes",
          {
              expect_equal(unname(unlist(lapply(xVars, class))), c("data.frame", "character", "numeric", "data.frame", "data.frame", "formula"))
              expect_equal(unname(unlist(lapply(formVars, class))), c("data.frame", "character", "factor", "data.frame", "data.frame", "formula"))
          }
          )
