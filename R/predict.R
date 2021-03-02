### S3 methods for predict()
## John Pintar


#TODO: Obviously, work on knn
#TODO: get knn weights working. might need to change the datatype
#TODO: Since predict is from the stats package, do I need to include a generic?
            #looks like it's automatically imported
#TODO: model frame might cause issues someday. its worth looking into.
#TODO: deal with over reliance on colname vs index. should make matrix and tibble
#       compatability pretty easy
#TODO: knn: whats happening when 50 observations are equally close?
#TODO: Remap factor assignment



####################################
# Private Error / Warning function #
####################################



# Check newData for for correctness.
#
# Checks to ensure that newData is formatted properly in comparison to training
# set and has no non-target NA values
#
# @usage predDataCheck(trainedModel, newData)
#
# @param trainedModel Any trainedModel object (user's arg)
# @param newData a data.frame whose target values will be predicted (user's arg)
# @return NULL
#
# @details If any newData format warnings occur,
predDataCheck <- function(trainedModel, newData)
{
    # TODO: consider turning this into a function for all plublic functions
    # Ensure data.frame input
    if(class(newData) != "data.frame")
        stop("newData must be a data.frame object")

    # erroring if newData has non-target NA's. babyCaret cant't reliably handle this
    # and the user should be dropping or imputing.
    if(anyNA(newData[colnames(newData) != trainedModel$vars$dvName]))
        stop("newData contains non-target NA values")


    # will be used to set off a message after the warnings occur
    flag <- FALSE

    # create vector of column classes for newData and the training set.
    newClasses <- as.character(lapply(newData, class))
    modelClasses <- as.character(lapply(trainedModel$vars$data, class))


    # col count warning
    if(length(modelClasses) != length(newClasses))
    {
        flag <- TRUE
        warning("Column count is not identical between the model's training set and newData")
    }

    # class match warning
    if(!identical(newClasses, modelClasses))
    {
         flag <- TRUE
         warning("Column classes are not identical between the model's training set and newData.")
    }

    # colnames warning
    if(!identical(colnames(newData), colnames(trainedModel$vars$data)))
    {
        flag <- TRUE
        warning("Column names are not identical between the model's training set and newData.")
    }


    # If any warning, error and refer user to relevant documentation.
    if (flag == TRUE)
    {
        stop("newData has different formatting than the model's training set. See Details section in ?predict.TrainedKnn().")
    }

    #TODO: consider implementing this. message is any or all target values are not NA.
    ######### need to tag trainedModel or even newData from test() to do this
    #if (!anyNA(newData[, trainedModel$vars$dvName]))
    #    message("All new observations already contain target values. FYI, test() evaluates model performance, predict() does not.")
}



#############################
# Temp KNN Bandaid function #
#############################



# TODO: rewrite KNN so this becomes unnecessary
# Drop all cols not contained in another data.frame
#
# Drops any column whose name doesn't appear in a reference data.frame
#
# @usage keepInCommon(df, compDf)
#
# @param df A data.frame
# @param compDf A data.frame for reference
# @return A data.frame
keepInCommon <- function(df, compDf)
{
    a <- colnames(df)
    b <- colnames(compDf)

    sect <- intersect(a, b)

    return(df[sect])
}

#########################
# factor mapping bugfix #
#########################

levelMap <- function(preds, ogLevels)
{
    out <- as.factor(preds)
    levels(out) <- ogLevels 
    
    return(out)
}

########################
# predict() S3 Methods #
########################



#TODO: Figure out why this method always places predictions in the last columns
#' Predict from knn models.
#'
#' Predicts values using a trainedKnn model.
#'
#' @param trainedModel A trainedKnn object created by train().
#' @param newData A data.frame with missing target values.
#' @param dfOut A logical. If TRUE, the predictions will be returned as a complete data.frame. If FALSE,
#' the predictions will be returned as a vector.
#' @return Predictions for newData's target values represented as either a data.frame or vector.
#'
#' @details newData should be formatted identical to the model's training set. Column names
#' and order should be identical. All columns present in training must be included. The number of rows may differ.
#' Also, prediction will work even if newData's target values are not missing.
#'
#' @export
predict.trainedKnn <- function(trainedModel, newData, dfOut = TRUE)
{
## Begin final function
    # goes to predict method in knn2.R
    predictions <- predict(trainedModel$model, newData)
    # If doing classification, need to convert back to factor with
    # appropriate labels
    if (trainedModel$model$myLevels > 0)
    {
        predictions <- levelMap(predictions, levels(trainedModel$vars$dv))
        #TODO: get rid of this hackish fix
        if (nrow(newData) == 1)
            predictions <- predictions[1]
    }

    if (dfOut == FALSE)
        return (predictions)
    else
    {
        newData[trainedModel$vars$dvName] <- predictions
        return (newData)
    }
## End final function.




    # # check compatibility between training set and newData
    # predDataCheck(trainedModel, newData)
    #
    # #################################################
    # # Bandaid to enable formula and model frame use #
    # #################################################
    #
    # # need new output variable because newData may be sliced
    # out <- newData
    # # replacing training set with df in original order, but only containing
    # # the variables found in the model frame
    # trainedModel$model$trainingSet <- keepInCommon(trainedModel$vars$data,
    #                                                trainedModel$vars$modelFrame)
    # # doing the same for newData
    # newData <- keepInCommon(newData, trainedModel$vars$modelFrame)
    #
    # # taking the predictions from the output df as a 1 col data.frame.
    # predictions <- predictKnn(trainedModel$model, newData)[trainedModel$vars$dvName]
    #
    #
    # if (dfOut == TRUE)
    # {
    #     # assign predictions to proper column of the original newData
    #     out[trainedModel$vars$dvName] <- predictions
    #     return(out)
    # }
    #
    # else
    #     # subsetting for return as vector.
    #     return (predictions[,1])
}


#' Predict from tree models.
#'
#' Predicts values using a trainedTree model.
#'
#' @param trainedModel A trainedTree object created by train()
#' @param newData A data.frame with missing target values.
#' @param dfOut A logical. If TRUE, the predictions will be returned as a complete data.frame. If FALSE,
#' the predictions will be returned as a vector.
#' @return Predictions for newData's target values represented as either a data.frame or vector.
#'
#' @details newData should be formatted identical to the model's training set. Column names
#' and order should be identical. All columns present in training must be included. The number of rows may differ.
#' Also, prediction will work even if newData's target values are not missing.
#'
#' @export
predict.trainedTree <- function(trainedModel, newData, dfOut = TRUE)
{
    # check compatibility between training set and newData
    predDataCheck(trainedModel, newData)

    # initialize predicted values
    predictedValues <- NULL

    # Getting predicted values. Third argument is needed for appropriate vector
    # output for both regression and classification
    if (class(trainedModel$vars$dv) == "numeric")
        # regression
        predictedValues <- predict(trainedModel$model, newData, "vector")

    else
        #classification
        predictedValues <- predict(trainedModel$model, newData, "class")


    # if dfOut, returning data.frame with predicted values assigned to the
    # proper column
    if (dfOut == TRUE)
    {
        newData[trainedModel$vars$dvName] <- predictedValues
        return (newData)
    }

    else
        return (predictedValues)
}


#' Assign cluster labels
#'
#' Assigns each observation to its most similar prototype/cluster.
#'
#' @param trainedModel A trainedKproto object created by train().
#' @param newData A data.frame
#' @param dfOut A logical. If TRUE, the predictions will be returned as a complete data.frame. If FALSE,
#' the predictions will be returned as a vector.
#' @return The cluster labels as either a data.frame or vector
#'
#' @details newData should be formatted identical to the model's training set. Column names
#' and order should be identical. All columns present in training must be included. The number of rows may differ.
#' If a data.frame is returned, the labels are assigned to a new column named .Cluster
#'
#' @export
predict.trainedKproto <- function(trainedModel, newData, dfOut = TRUE)
{
    # check compatibility between training set and newData
    predDataCheck(trainedModel, newData)

    # Getting cluster labels
    .Cluster <- predict(trainedModel$model, newData)$cluster

    if (dfOut == TRUE)
        # bind new feature (.Cluster) to the data
        return (cbind(newData, .Cluster))

    else
        return (.Cluster)
}


# trainedApriori Error
#' @export
predict.trainedAprori <- function(trainedModel, ...)
{
    stop("predicion not supported for Apriori")
}
