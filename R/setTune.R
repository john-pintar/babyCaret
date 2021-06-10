### Sets up a grid of tuning parameters
## John Pintar


#TODO: Add warnings and errors to public functions
        # checking input
#TODO: Finish unit tests


########################
# Tune object overview #
########################


# tune objects are used for setting hyperparameters in train(). A tune object
# consists of 3 slots. the first is a data.frame object "grid" with 4 col 1 row initalized to NA.
# the first three columns are "p1", "p2", and "p3". These columns hold a record of parameter values.
# The first set of parameters is stored in row 1, while the most recently specifed set is stored in row nrow(tune$grid)
# The 4th column is "score". score is currently unused. The next two slots are 4 col 1 row data.frames
# "currentTune" and "bestTune". currentTune is what is used for setting the parameters.
# bestTune is unused. All unused features are intended to be used for interactive model tuning
# which will be implemted at a later time. generic tune objects are the parent for method specific tune
# objects. Users can only create method specific tune objects. This is done through tuneKnn, tuneTree, etc.



#####################################
# low level tune object constructor #
#####################################



# Create empty tune object
#
# Low level constructor for tune object
#
# @usage new_tune()
# @return tune object
new_tune <- function()
{
    # initializing tuning grid with NA values and generic parameter names
    grid <- data.frame(matrix(nrow = 1, ncol = 4))
    colnames(grid) <- c("p1", "p2", "p3", "score")

    # initializing currentTune and bestTune slots
    currentTune <- grid
    bestTune <- grid

    # constructing main list structure and filling slots
    out <- vector(mode = "list", length = 3)
    out[[1]] <- grid
    out[[2]] <- currentTune
    out[[3]] <- bestTune

    # specifying slot names and class
    names(out) <- c("grid", "currentTune", "bestTune")
    class(out) <- "tune"


    return(out)
}



######################################
# Private functions for tune objects #
######################################



# Update a tune object's currentTune slot
#
# Updates an object's currentTune slot to match grid's nth row
#
# @param myTune A tune object
# @return  A tune object with correct currentTune slot
#
# @details call when new parameters are added.
update_current <- function(myTune)
{
    myTune$currentTune <- myTune$grid[nrow(myTune$grid),]

    return(myTune)
}


## NOTE: only updates from current. does not guarantee finding absolute best
# Update a tune object's bestTune slot
#
# Updates an object's bestTune slot if currentTune has lower score (error)
#
# @param myTune A tune object
#
# @return A tune object
update_best <- function(myTune)
{
    if (myTune$currentTune[1, "score"] < myTune$bestTune[1, "score"])
        myTune$bestTune <- myTune$currentTune

    return (myTune)
}


# Update a tune object's error
#
# Updates a tune object's current/active tune with an error score. This will
#   update both currentTune and grid's nth row (corresponds to current tune).
#
# @param myTune A tune object
#
# @return A tune object
update_score <- function(myTune, score)
{
    # Adding score to the current tune's grid row.
    myTune$grid[nrow(myTune$grid), "score"] <- score
    # Adding score to the currentTune grid slot
    myTune$currentTune[1, "score"] <- score

    return(myTune)
}


# Update a tune object's parameter using string input
#
# Updates a currentTune's (and corresponding grid slot's) parameter from string
# string input
#
# @param myTune A tune object
# @param param A string naming the parameter to be updated
# @param string A string containing the value used for update
# @return A tune object
#
# @details Used for interactive cross validation
string_update <- function(myTune, param, string)
{
    # prepping for input
    myTune <- clone_bottom_row(myTune)
    myTune <- update_current(myTune)

    # string is an object
    castedVal <- as(string, class(myTune$grid[1, param]))

    myTune$currentTune[1, param] <- castedVal
    myTune$grid[nrow(myTune$grid), param] <- castedVal

    return (myTune)
}


# row-bind a matching nth row to a tune's grid
#
# Binds another row identical to "currentTune" at the bottom of a tune object's grid
#
# @param myTune A tune object
# @return A tune object
#
# @details Used when modifying hyperparameters via the public function reTune()
clone_bottom_row <- function(myTune)
{
    myTune$grid <- (rbind(myTune$grid, myTune$grid[nrow(myTune$grid), ]))
    return(myTune)
}


# Set tune object parameter names.
#
# Low level function used for setting parameter names of a model specific tune object.
# (child of tune object).
#
# @param myTune A tune object
# @param params A character vector of length 3 containing parameter names in order
#
# @return A tune object with method-specific parameter names
set_param_names <- function(myTune, params)
{
    # Adding the score column for future interactive tuning
    params <- append(params, "score")
    # lapply() changes class to "list", so saving the class.
    myClass <- class(myTune)

    # Updating parameter names
    myTune <- lapply(myTune, `colnames<-`, params)
    class(myTune) <- myClass


    return(myTune)
}


# Create user-method-specific child of a tune object.
#
# Low level function for creating a method method-specific child of a generic tune object.
#
# @param myTune A tune object
# @param method A string specifying the child's method: "knn", "tree", "kproto", or "apriori"
#
# @return A method specific tune object: "knnTune, "treeTune, etc.
#
# @details Sets the class of the child object and adds method-specific parameter
# columns to each slot. Calls set_paramNames().
specify_method <- function(myTune, method)
{
    names <- NULL

    # Setting algorithm specific parameter names
    if (method == "knn")
        names <- c("k", "p", "featWeights")

    else if (method == "tree")
        names <- c("maxDepth", "minSplit", NA)

    else if (method == "kproto")
        names <- c("k", "lambda", "nStart")

    else if (method == "apriori")
        names <- c("minConf", "minSup", "maxLen")

    # error if method is not included in babyCaret
    else stop(paste(c(method, " is not a valid method"), collapse = ""))

    # setting parameter names
    out <- set_param_names(myTune, names)
    # setting class
    substr(method, 1, 1) <- toupper(substr(method, 1, 1))
    class(out) <- c("tune", paste(c("tune", method),collapse = ""))


    return(out)
}


# The reason for all of these constructors is to enable code completion
########################################
# Public constructors for tune objects #
########################################

# weird things will happen here with list
#' Create a tuneKnn object.
#'
#' Creates an object which holds user parameters for the K-nearest Neighbors algorithm.
#'
#' @param k A positive, non-zero integer. Determines the quantity of neighbors to be averaged for prediction.
#' @param p LAUREM IPSUM
#' @param featWeights LAUREM IPSUM
#'
#' @usage tuneKnn(k)
#' @return A tuneKnn object.
#'
#' @export
#'
#' @details If featWeights is given a numeric argument, the value will be saved in the output as
#' character data. It will be cast back to a numeric vector before prediction.
setKnn <- function(k = 5, p = 0, featWeights = NA)
{
    # create and specify tune object
    myTune <- new_tune()
    myTune <- specify_method(myTune, "knn")


    # numeric vector to string
    # TODO: consider the one element case, i.e. one predictor.
    # bandaid to save weights to one of grid's (a dataframe) slots requires
    # conversion before a call to cpp_knn(). happens in knn.R's predict method.
    if (!anyNA(featWeights))
        featWeights <- paste(featWeights, collapse = ",")
    else
        featWeights <- as.character(featWeights)

    # ^^^ Bandaid for cross validation ^^^
    # TODO: find a better solution


    # fill grid
    myTune$grid[1, "k"] <- k
    myTune$grid[1, "p"] <- p
    myTune$grid[1, "featWeights"] <- featWeights

    # update the currentTune slot to hold the new arguments
    myTune <- update_current(myTune)

    return(myTune)
}


#' Create tuneTree object.
#'
#' Creates an object which holds user parameters for the Decision Tree algorithm.
#'
#' @param maxDepth An integer between 1 and 30. Determines the maximum depth of the tree.
#' @param minSplit An non-negative integer. Sets the number of instances a node can contain before it is no longer a candidate for further splitting.
#' @usage tuneTree(maxDepth = 30, minSplit = 2)
#' @return A tuneTree object.
#'
#' @seealso rpart
#'
#' @export
setTree <- function(maxDepth = 30, minSplit = 2)
{
    # create and specify tune object
    myTune <- new_tune()
    myTune <- specify_method(myTune, "tree")

    # fill grid
    myTune$grid[1, "maxDepth"] <- maxDepth
    myTune$grid[1, "minSplit"] <- minSplit


    myTune$grid <- myTune$grid[, -3]

    # update the currentTune slot to hold the new arguments
    myTune <- update_current(myTune)

    return(myTune)
}


#' Create a tuneKproto object.
#'
#' Creates an object which holds user parameters for the K-prototypes algorithm.
#'
#' @param k A positive, non-zero integer. Sets the number of clusters.
#' @param lambda A positive number that determines the importance of categorical attributes.
#' @param nStart Sets how many times the algorithm will be computed.
#' @usage tuneKproto(k)
#' @return A tuneKproto object.
#'
#' @export
setKproto <- function(k = 3, lambda = 1, nStart = 1)
{
    # create and specify tune object
    myTune <- new_tune()
    myTune <- specify_method(myTune, "kproto")

    # fill grid
    myTune$grid[1, "k"] <- k
    myTune$grid[1, "lambda"] <- lambda
    myTune$grid[1, "nStart"] <- nStart

    # update the currentTune slot to hold the new arguments
    myTune <- update_current(myTune)

    return(myTune)
}


#' Create a tuneApriori object.
#'
#' Creates an object which holds user parameters for the Apriori algorithm.
#'
#' @param minSup A number between 0 and 1. Sets the support at which a frequent itemset will no longer be expanded.
#' @param minConf A number between 0 and 1. Sets the minimum confidence a rule must have to be displayed to the user.
#' @param maxLen A positive integer. Sets the maximum amount of items a rule may contain.
#' @usage tuneApriori(minSup = 0.1, minConf = 0.8, maxLen = 10)
#' @return  A tuneApriori object.
#'
#' @export
setApriori <- function(minSup = 0.1, minConf = 0.8, maxLen = 10)
{
    # create and specify tune object
    myTune <- new_tune()
    myTune <- specify_method(myTune, "apriori")

    # fill grid
    myTune$grid[1, "minSup"] <- minSup
    myTune$grid[1, "minConf"] <- minConf
    myTune$grid[1, "maxLen"] <- maxLen

    # update the currentTune slot to hold the new arguments
    myTune <- update_current(myTune)

    return(myTune)
}



###############################################
# Public function for re-tuning a tune object #
###############################################


#' Change a tune object's parameters
#'
#' Changes any tune object's parameters. Unchanged parameters remain the same.
#' @param myTune A tune object (knnTune, aprioriTune, etc.).
#' @param ... Named arguments with assigned values.
#' @return A tune object
#' @usage reTune(myTune, ...)
#'
#' @details Parameters not explicitly changed will remain the same. Arguments must be explicitly named as shown in the examples.
#'
#' @export
reTune <- function(myTune, ...)
{
    params <- list(...)
    myTune <- clone_bottom_row(myTune)

    # update the new row's arguments with those provided. Must be correctly named.
    myTune$grid[nrow(myTune$grid), names(params)] <- params

    myTune <- update_current(myTune)

    return(myTune)
}
