### Computes and stores user's formula, model frame, and x/y input
## John Pintar

# If you really want to simplify, you could do everything you need from the model frame


############################
# userVars object overview #
############################


# A userVars object is a six-slot list that holds the fit specified by the user. They contain the original data.frame,
# the target vector, The target feature's name, a model frame, a standard frame, and a formula. The userVars object exists to make
# feeding variables and formulas into the algorithms easier. A userVars object is
# created for every call to train() even though much of the information contained by the object
# will not be used. This reduction in efficiency should yield gains in maintenance, expandability,
# and simplicity.

# Slot 1/data: original data.frame
# Slot 2/dvName: the target feature's name as character data
# Slot 3/dv: target vector.
# Slot 4/modelFrame: model frame (col 1 is target, rest are predictors).
# Slot 5/standardFrame: data.frame where the last column is the target
# Slot 6/formula: formula.



##############################
# Private userVars functions #
#############################



# Create empty userVars object
# Low level constructor for userVars object
#
# @usage new_userVars()
# @return An empty userVars object
new_userVars <- function()
{
    # 'initializing' list slots to null
    data <- NA
    dvName <- NA
    dv <- NA
    modelFrame <- NA
    standardFrame <- NA
    form <- NA

    # creating list
    out <- list(data, dvName, dv, modelFrame, standardFrame, form)
    # naming list and assigning class
    names(out) <- c("data", "dvName", "dv", "modelFrame", "standardFrame", "formula")
    class(out) <- "userVars"

    return(out)
}


# Model frame from x/y input
#
# Creates a model frame from the users x/y input.
#
# @param data A data.frame.
# @param target The target vector.
# @usage model_from_default(data, target)
# @return A model frame.
#
# @details called inside of train.default()
model_from_default <- function(data, target)
{
    out <- data

    # Loop over columns
    i <- 1
    while (i <= ncol(data))
    {
        # find the target column/attribute
        if (identical(data[, i], target))
        {
            # if i == 1, then input is already a model frame
            if (i == 1)
                return(out)

            # Save target name, remove target name from column name vector
            targetName <- colnames(data)[i]
            dataNames <- colnames(data)
            dataNames <- dataNames[-i]
            # reinsert the target name in 1st position of the column name vector
            modelNames <- c(targetName, dataNames)

            # Move the target vector to the 1st column
            out <- cbind(target, data[, -i])
            # When cbind binds two vectors, it doesn't make a data.frame, so cast
            out <- as.data.frame(out)
            # assign the updated names
            colnames(out) <- modelNames

            return (out)
        }

        i <- i + 1
    }

    return (out)
}


# TODO: There has to be a more elegant way to do this.
# Convert a model frame to a standard frame
#
# Moves the first column to the last column while maintaining column names
#
# @usage model_to_standard(modelFrame)
# @param modelFrame A data.frame where column 1 is the target.
# @return A data.frame where the last column is the target.
#
# @details used to fill the standardFrame slot of userVars. Although not a
# defined object, a standard frame is a data.frame where the last column is
# a target vector.
model_to_standard <- function(modelFrame)
{
    # grabbing the relevant names
    modelNames <- colnames(modelFrame)
    targetName <- modelNames[1]
    # properly ordered colnames for the eventual standard frame
    standardNames <- c(modelNames[-1], targetName)

    # rearranging the columns
    out <- cbind(modelFrame, modelFrame[,1])
    out <- as.data.frame(out[, -1])
    # assigning properly ordered names
    colnames(out) <- standardNames

    return(out)
}


# TODO: There also has to be a more elegant way to do this
# Homogenize colnames between data.frames
#
# Renames the columns of a data.frame to match the column names from another data.frame
# that contains any number of identical columns
#
# @usage adopt_names(parent, child)
# @param parent A data.frame which has the desired column names
# @param child A data.frame containing (atleast 1) identical column to parent.
# The identical column(s) will take on names from parent.
# @return A data.frame
#
# @details Columns in child that also occur in parent will be reassigned the names from parent
adopt_names <- function(parent, child)
{
    # iterating through child columns
    i <- 1
    while (i <= ncol(child))
    {
        # iterating through parent columns
        j <- 1
        while (j <= ncol(parent))
        {
            # If the columns are identical, give the columns from child
            # the name from parent
            if (identical(child[, i], parent[, j]))
                colnames(child)[i] <- colnames(parent)[j]

            j <- j + 1
        }

        i <- i + 1
    }

    return (child)
}


################################
# Private userVars constructor #
################################



# Create a userVars object.
#
# Creates a userVars object from the user's input.
#
# @param formOrTarg Either a formula or a target vector.
# @param data The original data.frame.
# @usage UserVars(formOrTarg, data)
# @return A userVars object
#
# @details called inside of train().
userVars <- function (formOrTarg = NULL, data)
{
    # initializing empty userVars object.
    out <- new_userVars()

    # if no fit information is provided, data is assumed to be a model frame.
    if (missing(formOrTarg))
        out$modelFrame <- data

    # If fit information is provided, build a model frame from it.
    else
    {
        # create model frame and fill its slot
        if (class(formOrTarg) == "formula")
            # creating model frame from formula input
            out$modelFrame <- model.frame(formOrTarg, data)
        else
            # creating model frame from x/y input
            out$modelFrame <- model_from_default(data, formOrTarg)
    }

    # fill remaining slots
    out$data <- data

    # TODO: figure this out when the time comes.
    # model.frame changes colnames in .formula(). this is a bandaid that should
    # be changed. Also, if encountering formula bugs, this is a good place to look
    out$modelFrame <- adopt_names(out$data, out$modelFrame)

    # back to filling slots normally.
    out$dvName <- colnames(out$modelFrame)[1]
    out$dv <- out$modelFrame[, 1]
    out$standardFrame <- model_to_standard(out$modelFrame)


    # formula always comes from the model frame for consistency.
    out$formula <- as.formula(out$modelFrame)

    return (out)
}
