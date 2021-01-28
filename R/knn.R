### KNN data prep ('model' creation) and prediction (call to cppKNN())
## John Pintar


# NOTE: This file along with knn.cpp found in the src folder work together as
# a standalone KNN if used outside of babyCaret. Will return
# a numeric vector of predictions. Intended to be modular: use the functions from
# this file (simple), the cppKNN function from knn.cpp (allows use of euclidean distance),
# or the separate dist and predict functions in knn.cpp (allows euc dist, faster tuning
# of p and k, and faster predictions since distance matrix doesn't need to be recomputed).


#TODO: Check data.frames to ensure names are same, else warn.
#TODO: Intentional handling of ordered factors. Pretty sure that they're just
           # being treated as normal factors.
#TODO: thought for future: alternative distance matrix that allows tuning of weights
           # after matrix is created. (make attribute wise matrix -> apply weights and
           # condense)



######################
# KNN Model Creation #
######################



# Train KNN models
#
# Creates an object containing information used in KNN prediction
#
# @usage init_knn(modelFrame, k, p, featWeights)
#
# @param modelFrame A data.frame where column 1 contains the target feature
# @param k A positive, non-zero integer. Determines the quantity of neighbors to be averaged for prediction.
# @param p LAUREM IPSUM
# @param featWeights LAUREM IPSUM
#
# @return a babyKnn model
#
# @details Creates the knn model that will be the $model slot in train()'s output.
# More data preparation than training.
init_knn <- function(modelFrame, k, p, featWeights)
{
    # vector specifying which cols are numeric. Used in dist mat creation to
    # determine which dist function to apply. Stripping target col because predict
    # strips target col from the matrices (class/regress decided by levels).
    isNumeric <- as.logical(sapply(modelFrame[, -1], is.numeric))

    # All numeric data for the cpp function. Target is always col 1 in modelframe
    temp <- as.matrix(sapply(modelFrame, as.numeric))
    # cppKnn() always expects a matrix, even if one col. So, forcing matrix.
    predictors <- as.matrix(temp[, -1])
    # Will always be a vector.
    target <- temp[, 1]


    # myLevels is used in cpp_knn() to set up "ballot box" or flag regression.
    # If classification, levels = number of classes in target. If regression = 0.
    myLevels <- 0

    if (class(modelFrame[, 1]) != "numeric")
    {
        myLevels <- length(levels(modelFrame[, 1]))
        if (myLevels < 2)
            stop("Target must have atleast two levels")
    }


    # featWeights should get passed from the vars object, so converting to vector
    # here in this LIST should not cause an issue.
    # TODO: will this cause an issue with CV later?
    if (is.na(featWeights))
        featWeights <- rep(1, length(modelFrame) -1)

    # de-conversion mentioned in setTune.R. was originally done in predict
    if (class(featWeights) == "character")
        featWeights <- as.numeric(unlist(strsplit(featWeights, ",")))



    # creating and returning object.
    knnModel <- list(predictors, target, isNumeric, modelFrame, k, p, featWeights, myLevels)
    names(knnModel) <- c("predictors", "target", "isNumeric", "modelFrame", "k", "p", "featWeights", "myLevels")

    class(knnModel) <- c("babyKnn")

    return (knnModel)
}



##################
# KNN prediction #
##################


#TODO: document
# Predict with a babyKnn model
#
# Makes predictions using a babyKnn model
#
# @param model A babyKnn object
# @param newData A data.frame to predict values from
#
# @return vector of numeric predictions.
#' @export
predict.babyKnn <- function(model, newData)
{
    # Processing newData into a matrix formatted the same as the model's predictor
    # matrix. If attributes are out of order or target is included,
    # distance matrix will be off or or will crash.
    newMf <- model.frame(as.formula(model$modelFrame), newData)
    newMat <- as.matrix(sapply(newMf[,-1], as.numeric))

    # Return predictions as a vector. Where all the work happens.
    preds <- cpp_knn(model$isNumeric, model$predictors, newMat, model$target,
                            TRUE, model$k, model$p, model$featWeights, model$myLevels)
    # Note: if classification, will need to be converted back to factor with appropriate labels
        return(preds)
}
