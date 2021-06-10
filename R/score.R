### Handles testing
## John Pintar


###############################
# Private Workhorse Functions #
###############################


# Calculate percent misclassified
#
# Calculates percent misclassified from a vector of discrete predictions and a vector of known values.
#
# @usage misClass(predicted, reals)
#
# @param predicted A vector containing discrete predicted values.
# @param reals A vector containing predicted's ground-truth values.
# @return A MisClass object.
#
# @details MisClass object inherits from numeric. Its only method is print
misClass <- function(predicted, reals)
{
    # casting to character to ensure equality is meaningful
    a <- as.character(reals)
    b <- as.character(predicted)

    # creating an equivalent length vector where matches become true, else false
    tfVect <- a == b
    # subsetting to create a vector containing the false values alone
    falseVect <- tfVect[tfVect==FALSE]

    # calculating percent misclassified
    nWrong <- length(falseVect)
    percWrong <- nWrong / length(a)

    # Class allows custom print method. Inheriting from numeric allows for
    # further computation by user.
    class(percWrong) <- c("MisClass", "numeric")


    return(percWrong)
}


# Calculate MAPE
#
# Calculates mean absolute percentage error from a vector of continuous predictions
# and a vector of known values
#
# @usage mape(predicted, reals)
#
# @param predicted A vector containing continuous predicted values.
# @param reals A vector containing "predicted's" ground-truth values.
# @return A Mape object
#
# @details Mape object inherits from numeric. Its only method is print.
mape <- function(predicted, reals)
{
    # calculating error for each prediction
    error <- reals - predicted
    # calculating percentage error
    percError <- error / reals

    # Taking absolute value of each prediction's error, then taking the mean.
    absError <- abs(percError)
    MAPE <- mean(absError)

    # Class allows custom print method. Inheriting from numeric allows for
    # further computation by user.
    class(MAPE) <- c("Mape", "numeric")


    return (MAPE)
}



##########################
# Public test() function #
##########################



#' Test model performance
#'
#' Calculates an error score using a model and testing set
#'
#' @usage score(trainedModel, testSet)
#'
#' @param trainedModel A classification or regression model trained by babyCaret. Currently,
#' only trainedKnn and trainedTree models are supported.
#' @param testSet A data.frame. Must match the format of the training set and contain
#' target values.
#' @return the predicton error as a Mape object for regression models and a MisClass object for classification models.
#' These objects are numeric with a special print method.
#'
#' @details Mean absolute percentage error is computed for regression models. Percent misclassified
#' is computed for classification models.
#'
#' @export
score <- function(trainedModel, testSet)
{
    # Ensuring the model is compatible with testing (classification or regression).
    if (!is(trainedModel, "trainedModel"))
        stop("\"trainedModel\" must be a classification or regression model
             trained by babyCaret")
    if (is(trainedModel, "trainedKproto"))
        stop("Testing kproto is unsupported.")
    if (is(trainedModel, "trainedApriori"))
        stop("Testing apriori is unsupported.")

    # Dropping NA rows and alerting user.
    if (anyNA(testSet))
    {
        ogLength <- nrow(testSet)
        testSet <- na.omit(testSet)
        message(paste (c("Ignoring testSet rows containing NA. Testing will involve ",
                         nrow(testSet), "/", ogLength , " observations"), sep = ""))
    }


    # a is predictions, b is its ground truth.
    a <- predict(trainedModel, testSet, FALSE)
    # The comma ensures a vector is returned as opposed to a single col data.frame
    b <- testSet[, trainedModel$vars$dvName]


    # MAPE for regression, percent misclassified for classification.
    if (class(a) == "numeric")
        return(mape(predicted = a, reals = b))

    else
        return (misClass(predicted = a, reals = b))
}



