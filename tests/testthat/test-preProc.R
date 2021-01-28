# Testing .babyScale()

a <- c(1, 2, 3, 4, 5)
b <- c(1, 5, 2, 6, 6, 20, 11, 3)

test_that(".babyScale returns values between 0 and 1",{
    expect_equal(min(.babyScale(a)), 0)
    expect_equal(min(.babyScale(b)), 0)

    expect_equal(max(.babyScale(a)), 1)
    expect_equal(max(.babyScale(b)), 1)
})


# Testing stringsToFactors()

hasChar <- iris
hasChar$Sepal.Length <- as.character(hasChar$Sepal.Length)
hasChar$Species <- as.character(hasChar$Species)

test_that("stringsToFactors() doesn't leave remaining character cols", {
    expect_false(any(sapply(stringsToFactors(hasChar), is.character)))
    expect_false(any(sapply(stringsToFactors(as.matrix(hasChar)), is.character)))
    expect_false(any(sapply(stringsToFactors(as.character(as.matrix(matrix(cars))))
                            , is.character)))
})

test_that("stringsToFactors() doesn't modify numeric data", {
    expect_true(is.numeric(as.matrix(stringsToFactors(cars))))
})


# Testing ensureDf()

test_that("ensureDf() returns data.frame from matrix input", {
    expect_true(is.data.frame(ensureDf(as.matrix(hasChar))))
})

# Testing naHandling()

df <- data.frame(matrix(c(1, 2, 3, 4), 4, 4))
df[c(1, 2), c(2, 4)] <- NA
hasChar[3:45, 1] <- NA

test_that("naHandling() removes NA values when na.rm",{
    expect_false(anyNA(naHandling(df, TRUE)))
    expect_false(anyNA(naHandling(hasChar, TRUE)))
})


# Testing .findClosest

a <- c(1, 2, 3, 4, 5, 6, 7)
b <- c(2, 4, 6, 9, 11, 23, 44, 52, 67, 83)

# casting and sorting so that I know which element is which
resA1 <- sort(as.numeric(.findClosest(3.5, a)))
resA2 <- sort(as.numeric(.findClosest(7, a)))
resB1 <- sort(as.numeric(.findClosest(2.01, b)))
resB2 <- sort(as.numeric(.findClosest(49.5, b)))

test_that(".findClosest() finds the two nearest elements", {
    expect_equal(resA1, c(3, 4))
    expect_equal(resA2, c(6, 7))
    expect_equal(resB1, c(2, 4))
    expect_equal(resB2, c(44, 52))
})


# Testing .toClosest()

resA1 <- .toClosest("3.5", a)
resA2 <- .toClosest("7", a)
resB1 <- .toClosest("2.01", b)
resB2 <- .toClosest("49.5", b)

test_that(".toClosest() returns a char", {
    expect_equal(class(resA1), "character")
    expect_equal(class(resB2), "character")
})

test_that(".toClosest() formats the two nearest elements properly", {
    expect_equal(resA1, "{3, 4}")
    expect_equal(resA2, "{6, 7}")
    expect_equal(resB1, "{2, 4}")
    expect_equal(resB2, "{44, 52}")
})


# A rough test of preProc on one example.

# col 4 is char data
dirtyData <- as.data.frame(matrix(c(1, 2, 3, 4), 4, 4))
dirtyData[, 4] <- as.character(dirtyData[, 4])
# 2, 4 AND 2, 2 are NA
dirtyData[2, c(2, 4)] <- NA
dat <- preProc(dirtyData, nBins = 2, binCols = 1, scale = TRUE, na.rm = TRUE)

test_that("preProc removes NA rows when na.rm is true", {
    expect_false(anyNA(dat))
    expect_equal(nrow(dat), nrow(dirtyData) -1) # because one row had NA value
})

test_that("preProc converts character to factor", {
    expect_true(is.factor(dat[, 4]))
})

test_that("preProc correctly bins toy example", {
    expect_identical(dat[,1], as.factor(c("{1, 3}", "{3, 4}", "{3, 4}")))
    expect_true(is.factor(dat[, 1]))
})

test_that("preProc scaling sets min 0 max 1", {
    expect_true(all(max(dat[, 2]) == 1))
    expect_true(all(max(dat[, 3]) == 1))
    expect_true(all(min(dat[, 2]) == 0))
    expect_true(all(min(dat[, 3]) == 0))
})

