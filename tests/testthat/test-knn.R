## Testing knn features

#########
# knn.R #
#########


knnInit1 <- init_knn(iris, 5, 0, NA )
knnInit2 <- init_knn(cars, 3, 2, c(1))


test_that("init_knn predictor matrix is numeric",
          {
              expect_true(is.numeric(knnInit1$predictors))
              expect_true(is.numeric(knnInit2$predictors))
          })

test_that("init_knn sets NA featWeights to all 1", {
    expect_true(all(knnInit1$featWeights == 1))
})

test_that("init_knn sets NA featWeights to proper length", {
    expect_equal(length(knnInit1$featWeights), ncol(knnInit1$predictors))
})

test_that("init_knn deconverts featWeights (string to numeric)",{
    expect_equal(class(knnInit2$featWeights), "numeric")
})

test_that("init_knn's predictor matrix does not include the target", {
    expect_true(all(apply(knnInit1$predictors,2 ,
                          function(x) !identical(x, knnInit1$target))))
    expect_true(all(apply(knnInit2$predictors,2 ,
                          function(x) !identical(x, knnInit2$target))))
})

test_that("init_knn sets myLevels to 0 for regression", {
    expect_equal(knnInit1$myLevels, 0)
    expect_equal(knnInit2$myLevels, 0)
})

catFrame <- iris[c("Species", "Sepal.Width", "Petal.Width")]
knnInit3 <- init_knn(catFrame, 7, 1.5, NA)
test_that("init_knn sets proper levels for classification",{
    expect_equal(knnInit3$myLevels, 3)
})


###########
# knn.cpp #
###########



# testing makeDist

mat1 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 4, 3, 2, 1, 1, 1, 2, 2), 4, 4)
mat2 <- t(mat1)
mat2[1, 4] <- 1
mat2[2, 4] <- 2
nums <- c(T, T, T, F)

result <- matrix(c(4, 9, 7, 7, 5, 8, 6, 8, 9, 8, 6, 8, 12, 9, 7, 11), 4, 4, byrow = TRUE)


test_that("cpp_makeDist creates correct unweighted manhattan matrix", {
    expect_identical(cpp_makeDist(nums, mat1, mat2, T, c(1, 1, 1, 1)), result)
})


# expecting equal to a rounded determinant of the correct matrix
test_that("cpp_makeDist creates the correct unweighted euclidean matrix", {
    expect_equal(round(det(cpp_makeDist(nums, mat1, mat2, F, c(1,1,1,1))),
                       digits = 2), -1.38)
})

# expecting determinant value for
test_that("cpp_makeDist creates the correct weighted manhattan matrix", {
    expect_equal(det(cpp_makeDist(nums, mat1, mat2, T, c(2, 2, 2, 2))), -512)
})

# det value again
test_that("cpp_makeDist created the correct weighted euclidean matrix", {
    expect_equal(round(det(cpp_makeDist(nums, mat1, mat2, F, c(2, 2, 2, 2))),
                       digits = 2 ), -8.10)
})


#testing mergeSortCompanion
a <- c(4, 2, 5, 1)
b <- c(1, 2, 3, 4)
mergeSortCompanion(a, b, 0, 3)

one <- 1
two <- 2
mergeSortCompanion(one, two, 0, 0)

test_that("mergeSortCompanion sorts the main (sorting by) vector properly", {
    expect_equal(a, c(1, 2, 4, 5))
})

test_that("mergeSortCompanion sorts the companion array according to the main array", {
    expect_equal(b, c(4, 2, 1, 3))
})

test_that("mergeSortCompanion can handle scalars", {
    expect_equal(one, 1)
    expect_equal(two, 2)
})


a <- c(2, 32, 99, 22, 55, 61)
b <- c(1, 2, 3, 4, 5, 6)
mergeCompanion(a, b, 0, 2, 3, 5)


test_that("mergeCompanion merges", {
    expect_equal(a, c(2, 22, 32, 55, 61, 99))
    expect_equal(b, c(1, 4, 2, 5, 6, 3))
})


# Testing voting

test_that("voting chooses majority class in binary classification when p = 0", {
              expect_equal(voting(c(1, 1, 2, 2, 2), c(.5, .4, .3, 7, 2.4), 2, 0), 2)
              expect_equal(voting(c(1, 1, 1, 2, 2), c(.5, .4, .3, 7, 2.4), 2, 0), 1)
})

test_that("voting results for binary class are stable when levels arg is higher than levels present",{
    expect_equal(voting(c(1, 1, 2, 2, 2), c(.5, .4, .3, 7, 2.4),levels = 30, p = 0), 2)
    expect_equal(voting(c(1, 1, 1, 2, 2), c(.5, .4, .3, 7, 2.4),levels = 70, p = 0), 1)
})

test_that("voting changes results to closest when p is sufficently large", {
    expect_equal(voting(c(1, 1, 2, 2, 2), c(.2, .4, .3, 7, 2.4), 2, 20), 1)
    expect_equal(voting(c(2, 1, 1, 1, 2), c(.2, .9, 1, 7, 2.4), 2, 2), 2)
})

test_that("voting works for 10 unique labels and p works with miniscule advantage", {
    expect_equal(voting(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                 c(1, 1, 1, 1, 1, 1, 1, .99, 1, 1), levels = 10, p = .001), 8)
})


# Testing idw

test_that("idw returns mean when p = 0", {
    expect_equal(idw(c(1, 2, 3, 4, 5), c(2, 3, 1, 7, 22), p = 0), 3)
    expect_equal(idw(c(1, 2, 3, 22), c(2, 1, 2, 3), p = 0 ), 7)
})


test_that("idw returns mean of identicals (0 dist) if found, when p != 0", {
    expect_equal(idw(c(1, 2, 55), c(1, 0, 12), p = .1), 2)
    expect_equal(idw(c(1, 2, 55), c(0, 0, 12), p = -1), 1.5)
    expect_equal(idw(c(1, 2, 3, 22, 32), c(0, 0, 0, .1, 12), p = 11), 2)
})


test_that("idw returns mean when all points equally distant", {
    expect_equal(idw(c(3, 4, 5), c(1, 1, 1), p = 2), 4)
})



test_that("idw returns proper interpolated values when no identicals or equals", {
    expect_equal(round(idw(c(3, 4, 5), c(1, 2, 3), p = 2), digits = 2), 3.35)
    expect_equal(round(idw(c(3, 4, 5), c(3, 2, .5), p = 2), digits = 2), 4.89)
    expect_equal(round(idw(c(2, 4, 22, 3, 22), c(1, 2, .2, 15.5, 3), p = 3.5),
                            digits = 2), 21.92)
})


checkMat <- matrix(c(15, 9, 11, 34, 23, 1, 37, 3, 44), 3, 3, byrow = T)

test_that("cpp_knnPredict has the same input as when this test was written on tiny, reliable examples", {
    expect_equal(cpp_knnPredict(checkMat, c(1, 2, 3), k = 2, p = 2, levels = 3),
                 c(1, 3, 2))
    expect_equal(cpp_knnPredict(checkMat, c(1, 2, 3), k = 2, p = 0, levels = 0),
                 c(1.5, 2.0, 1.5))
})
