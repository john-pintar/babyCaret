### Sends user-input to the appropriate algorithm for training.
## John Pintar


# TODO: Document.
# TODO: Figure out the KNN stuff
# TODO: Experiment with pretty frames, standard frames, etc.
# TODO: more formal handling of trained objects?
# TODO: pretty sure you have to @export the methods
# TODO: model frame issues someday?



##################################
# Notes on the 'trained' objects #
##################################


# Each method returns a trained object, trainedKnn, etc. this is just a list
# containing the model returned by the particular training function, myVars, and myTune.
# These objects are what is eventually returned by train().
# currently, this is loosely constructed in each of dispatchTraining()'s methods.
# This will likely change as training output is revised.



#########################################
# dispatchTraining() Private S3 Generic #
#########################################



# S3 Generic
dispatchTraining <- function(myTune, myVars, ...)
{
    UseMethod(generic = "dispatchTraining", object = myTune)
}



#########################################
# dispatchTraining() Private S3 methods #
#########################################



# knn's S3 Method
# might need to change to a data.frame which drops unneeded cols
# TODO: A bug will show up for prediction
# Dispatch knn training algorithm.
#
# Calls knn training algorithm in knn.R using info from tune and myVars objects.
#
# @param myTune The tune object created from user input
# @param myTune The userVars object created from user input.
# @return A trainedKnn object
#
# @details The output from this method will be returned by train()
#' @export
dispatchTraining.tuneKnn <- function(myTune, myVars, ...)
{
    # the actual knn training function
    # Using current tune because future cv feature will update that
    myModel <- init_knn(myVars$modelFrame, myTune$currentTune$k,
                        myTune$currentTune$p, myTune$currentTune$featWeights)


    # creating trained Model object
    # TODO: make me a function?
    out <- list(myModel, myVars, myTune)
    names(out) <- c("model", "vars", "tune")
    class(out) <- c("trainedKnn", "trainedModel")


    return(out)
}


# tree's S3 Method
# Dispatch tree training algorithm.
#
# Calls tree algorithm from rpart package using info from tune and myVars objects.
#
# @param myTune The tune object created from user input
# @param myTune The userVars object created from user input.
# @return A trainedTree object
#
# @details The output from this method will be returned by train()
#' @export
dispatchTraining.tuneTree <- function(myTune, myVars, ...)
{
    # Grabbing parameters
    mySplit <- myTune$currentTune$minSplit
    myDepth <- myTune$currentTune$maxDepth

    # Locking in settings
    rpartSettings <- rpart::rpart.control(minsplit = mySplit, cp = -1, maxdepth = myDepth,
                                 maxcompete = 0, usesurrogate = 0, xval = 0)

    # Training and returning model
    # Note: may need be explicit with Rpart's method param
    myModel <- rpart::rpart(myVars$modelFrame,
                  control = rpartSettings)


    # creating trainedModel Object
    out <- list(myModel, myVars, myTune)
    names(out) <- c("model", "vars", "tune")
    class(out) <- c("trainedTree", "trainedModel")


    return (out)
}


# kproto's S3 Method
# Dispatch kproto training algorithm.
#
# Calls modified kproto algorithm from kproto.R using info from tune and myVars objects.
#
# @param myTune The tune object created from user input
# @param myTune The userVars object created from user input.
# @return A trainedKproto object
#
# @details The output from this method will be returned by train()
#' @export
dispatchTraining.tuneKproto <- function(myTune, myVars, ...)
{
    # Grabbing parameters
    myK <- myTune$currentTune$k
    myLam <- myTune$currentTune$lambda
    myStart <- myTune$currentTune$nStart

    # Running K-prototypes
    myModel <- kproto(x = myVars$modelFrame, k = myK, lambda = myLam,
                   nstart = myStart, verbose = FALSE)

    # creating trainedModel Object
    out <- list(myModel, myVars, myTune)
    names(out) <- c("model", "vars", "tune")
    class(out) <- c("trainedKproto", "trainedModel")


    return (out)
}


# apriori's S3 Method
# Dispatch apriori training algorithm.
#
# Calls apriori algorithm from arules package using info from tune and myVars objects.
#
# @param myTune The tune object created from user input
# @param myTune The userVars object created from user input.
# @return A trainedApriori object
#
# @details The output from this method will be returned by train()
#' @export
dispatchTraining.tuneApriori <- function(myTune, myVars, ...)
{

    # Grabbing parameters
    mySup <- myTune$currentTune$minSup
    myConf <- myTune$currentTune$minConf
    myLen <- myTune$currentTune$maxLen

    # Creating proper control objects
    apriParams <- new("APparameter", support = mySup, confidence = myConf, maxlen = myLen)
    apriControl <- new("APcontrol", verbose = FALSE)

    # Running the apriori algorithm and casting its output to data frame
    # for ease of interpretation
    apriModel <- arules::apriori(myVars$modelFrame, parameter = apriParams, control = apriControl)
    ruleDf <- as(apriModel, "data.frame")
    rownames(ruleDf) <- NULL

    # sorting by lift to get interesting rules near the top
    myModel <- list(ruleDf[order(ruleDf$lift, decreasing = TRUE),], apriModel)
    names(myModel) <- c("ruleDf", "apriModel")


    # creating trainedModel Object
    out <- list(myModel, myVars, myTune)
    names(out) <- c("model", "vars", "tune")
    class(out) <- c("trainedApriori", "trainedModel")


    return (out)
}
