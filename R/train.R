### Handles model training
## John Pintar


#TODO: Do you need to use train() from the generics package?
#TODO: Default behavior when no targ should be standardFrame interpretation
#TODO: Should explicit train functions even be included?
#TODO: if kept, explicit functions should warn when being used for wrong things.
#TODO: y/x input might be wonky. look at caret. S4 method may be the best way.
#TODO: Full warnings
#TODO: be aware of ~. issues.
#TODO: what about matricies and tibbles?
#TODO: remember to lock down minbucket for calls to rpart()
#TODO: consider making a trainedModel parent class.
#TODO: add support for perceptron eventually.


#############################
# Private warning functions #
#############################


# Check method input
#
# ensures that the user's 'method' input is being used in the proper context
#
# @usage methodContext(x, from)
# @param x The user's method argument
# @param x A string specifying which of train's S3 methods it is being called in
# @return NULL
methodContext <- function(x, from)
{
    # Checks for train.default()
    if (from == "default")
    {
        # proper user-method
        if (x == "knn" || x == "tree")
            return()

        # improper user-method given context
        else if (x == "kproto" || x == "apriori")
        {
            warning(paste("\"", x, "\"", " does not use a target. The entire data.frame will be used.", sep = ""))
        }


        # user-method DNE
        else
            stop(paste (x, "is not a valid babyCaret algorithm"))
    }


    # Warnings for train.formula()
    if (from == "formula")
    {
        # proper method
        if (x == "knn" || x == "tree")
            return()

        # improper user-method given context
        else if (x == "kproto" || x == "apriori")
        {
            warning(paste("\"", x, "\"", " does not use a formula. The formula's model frame will be used instead (all cols in formula).", sep = ""))
        }

        # user-method DNE
        else
            stop(paste (x, "is not a valid babyCaret algorithm"))
    }


    # Warnings for train.data.frame()
    if (from == "data.frame")
    {
        # proper user-method
        if (x == "kproto" || x == "apriori")
            return()
        # improper user-method given context
        else if (x == "knn" || x == "tree")
        {
            warning(paste("\"", x, "\"", " requires a formula or target. The data.frame will be interpreted as a model frame (col 1 as target).", sep = ""))
        }

        # user-method DNE
        else
            stop(paste(x, "is not a valid babyCaret algorithm."))
    }

    return()
}


# Ensure tune matches method
#
# Checks to see if user is using proper tune function to match their method
# argument. If not, errors.
#
# @usage methodWarnings(method, tune)
# @param method user's method argument
# @param tune user's tune argument
# @return NULL
#
methodMatchTune <- function(method, tune)
{
    if (is(tune, "tune"))
    {
        # if the method argument doesn't match with its proper tune object, error.
        if (method == "knn" && !is(tune, "tuneKnn"))
            stop("\"knn\" requires tune = tuneKnn(...)")

        if (method == "tree" && !is(tune, "tuneTree"))
            stop("\"tree\" requires tune = tuneTree(...)")

        if (method == "kproto" && !is(tune, "tuneKproto"))
            stop("\"kproto\" requires tune = tuneKproto(...)")

        if (method == "apriori" && !is(tune, "tuneApriori"))
            stop("\"apriori\" requires tune = tuneApriori(...)")
    }

    return ()
}


# Remove NA values from data.frame
#
# Removes all rows containing NA values. If Rows are removed, the user is warned.
#
# @usage noNA(data)
# @param data A data.frame possibly containing NA values
# @return A data.frame with no NA values.
#
# @details called in all train S3 methods on the users data argument.
noNA <- function(data)
{
    # if data has any NA values
    if (anyNA(data))
    {
        # warn then drop those rows
        warning("data has missing values. All rows containing NA will be dropped.")
        return (na.omit(data))
    }

    # if there aren't any NA values, return data unmodified.
    return(data)
}


# Error if data is character type
#
# Hopefully errors when the user forgets to include the data argument and instead
# includes their method after x.
#
# @usage dataCharError(data)
# @param data the user's data argument
# @return NULL
#
# @details Called in all train S3 methods.
dataCharError <- function(data)
{
    if (class(data) == "character")
        stop("The data argument must be provided with a data.frame, not character data.")

    return ()
}



#############################
# Public train() S3 generic #
#############################



## S3 generic
#' Train ML Models
#'
#' Handles training of machine learning models
#'
#' @usage train(x, ...)
#'
#' ## Default S3 method:
#' train(x, data, method, tune)
#'
#' ## S3 method for class 'formula'
#' train(x, data, method, tune)
#'
#' ## S3 method for class 'data.frame'
#' train(x, method, tune)
#'
#' @param x In the default method, x is a vector containing target values.
#' In the formula method, x is a formula specifying the predictors and the target
#' attribute/feature. In the data.frame method, x is the entire training set.
#' @param data A data.frame object containing the training data. Must include
#' target values.
#' @param method A character object specifying the algorithm to be used: "knn",
#' "tree", "kproto", or "apriori"
#' @param tune A tune object specifying hyperparameter values. Can be set
#' using setTune().
#'
#' @return A trained ML model.
#' @export
train <- function(x, ...)
{
    UseMethod(generic = "train", object = x)
}


#TODO: not very DRY. Consider changes.
# combine default and formula, feed class(x) to method context to get proper warnings.
##############################
# Public train() S3 generics #
##############################



## Default handles target-vector input. Used for classification and regression algorithms.
#  default S3 method
#' ML training from target-vector
#'
#' Trains a machine learning model using target-vector & data.frame input.
#'
#' @usage (x, data, method, tune)
#'
#' @param x A target-vector. Must be contained in data.
#' @param data A data.frame containing the entire training set.
#' @param method A string indicating the ML algorithm to be used. See generic function.
#' @param tune A model-specific tune object. See generic function.
#' @return A trained ML model.
#'
#' @details If used for clustering or rule mining, the model frame created from data
#' will be used for training
#'
#' @export
train.default <- function(x, y, method, tune = "cv", ...)
{
    data <- x
    x <- y
    
    # checking input (warnings and errors)
    dataCharError(data)
    methodContext(method, "default")
    methodMatchTune(method, tune)
    data <- noNA(data)


    # initializing object used to feed the fit to algorithms.
    vars <- userVars(x, data)

    # Cross validate, or dispatch training
    if (identical(tune, "cv"))
        crossVal(x, data, method, tune)
    else
        dispatchTraining(tune, vars)

}


## Handles formula input. Used for classification and regression algorithms.
#' ML training from formula
#'
#' Trains a machine learning model using formula & data.frame input
#'
#' @usage (x, data, method, tune)
#'
#' @param x A formula.
#' @param data A data.frame containing the entire training set.
#' @param method A string indicating the ML algorithm to be used. See generic function.
#' @param tune A model-specific tune object. See generic function.
#' @return A trained ML model.
#'
#' @details If used for clustering or rule mining, the model frame specified by
#' the formula will be used for training
#'
#' @export
train.formula <- function(x, data, method, tune, ...)
{
    # checking input (warnings and errors)
    dataCharError(data)
    methodContext(method, "formula")
    methodMatchTune(method, tune)
    data <- noNA(data)

    vars <- userVars(x, data)

    # Cross validate, or dispatch training
    if (identical(tune, "cv"))
        crossVal(x, data, method, tune)
    else
    dispatchTraining(tune, vars)
}



#TODO: Maybe not have CV here. cv is for prediction only. Could set it up with a warn
        # Or just error and direct them to use setTune
## Handles target-less input. Used for clustering and association rule algos.
#' ML training from data.frame
#'
#' Trains a machine learning model using only data.frame input
#'
#' @usage (data, method, tune)
#'
#' @param  A data.frame containing the entire training set.
#' @param method A string indicating the ML algorithm to be used. See generic function.
#' @param tune A model-specific tune object. See generic function.
#' @return A trained ML model.
#'
#' @details If used for classification or regression, data will be interpreted
#' as a model frame
#'
#' @export
# train.data.frame <- function(x, method, tune, ...)
# {
#     # checking input (warnings and errors)
#     methodContext(method, "data.frame")
#     methodMatchTune(method, tune)
#     data <- noNA(x)
# 
#     # Initializing user variables.
#     vars <- userVars(data = data)
# 
#     # Cross validate, or dispatch training
#     if (identical(tune, "cv"))
#         crossVal(x, method, tune)
#     else
#     dispatchTraining(tune, vars)
# }
