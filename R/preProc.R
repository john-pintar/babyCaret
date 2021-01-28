## Preprocessing function(s)
# John Pintar


# THU fully Wrap this up. Potentially get print and summary going
# FRI - SUN Wrapup.
# If get done on Satuday, could use sunday and monday to organize music and
    # work on perceptron

# After unit testing, this is done enough for now. Can improve later.


# Note: trying different brace formatting.


#TODO: Add target scale ignore to match process 

#TODO: If you find the time, this file is ripe for improvements. getting rid of
    # loops, using a consistent approach, DRY (preProc and matchProc)
#TODO: Add support for imputation
#TODO: Process another data.frame by the same parameters
#TODO: in findNearest. Could do a cast and sort to order the nearests better.
#TODO: Unit test
#TODO: clean and comment
#TODO: format messages better
#TODO: make the output of binning and scaling less crazy(long)
#TODO: Convert binning to interval notation (-inf, 2], (4, inf)
# what if make it so that binCols can bin all with null, NA, or "all"
#TODO: Unit testing for matchProc and attrs.



################################
# Main pre-processing function #
################################



#' Preprocess data
#'
#' Preprocesses data for use with babyCaret::train().
#'
#' @param data A tabular style data object. Ideally a data.frame. If not a data.frame,
#' conversion to data.frame will be attempted.
#' @param nBins The number of bins to use when binning. Must be a positive integer.
#' @param binCols A numeric specifying which columns to bin. If binCols is null when nBins has a value,
#' all numeric columns will be binned
#' @param scale A logical. If true, all numeric columns will be scaled between 0 and 1.
#' @param na.rm A logical. Drops rows containing NA if TRUE.
#'
#' @return A processed data.frame (may have additional attributes).
#'
#' @details If both scaling and binning are being done, binned columns will not be scaled
#' prior to binning. If binCols is null when nBins has a value,
#' all numeric columns will be binned. if binCols have been specified but nBins is none,
#' nBins will default to 3 after warning.
#'
#' It is recommended that matrices be cast to data.frames PRIOR to preprocessing.
#' However, if passing a matrix to data, numeric matrices will retain all columns as numeric
#' a data.frame. Character matrices will be cast to a data.frame consisting entirely
#' of factor columns.
#'
#' Attributes will be added to the output if the data is binned or scaled. For binning
#' These will be a logical vector, "binCols" which is true for each column
#' That was binned, and a list of numeric vectors "quants" which contain the quantiles
#' used to define the bins for a specific column. The list has as many entries as
#' columns that were binned.
#'
#' For scaling, "scaleCols" is a logical vector TRUE for each column that was scaled.
#' "mins" contain the minimum for each column that was scaled, and "maxes" contain
#' the maximum cor each column that was scaled.
#'
#' These attributes are used by matchProc() in order to process additional datasets
#' by the same metrics. They can also be accessed using attr() or attributes().
#'
#' @export
preProc <- function(data, nBins = NULL, binCols = NULL, scale = FALSE, target = NULL, na.rm = FALSE)
{
    # Change NA's to FALSE for scale and na.rm because NA is logical and
    # and won't be triggered by ArgChk.
    if (is.na(scale)){
        scale <- FALSE
        warning("scale had NA value. This has been changed to FALSE")
    }
    if (is.na(na.rm)){
        na.rm <- FALSE
        warning("na.rm had NA value. This has been changed to FALSE")
    }

    # Ensure non-data arguments are proper
    preProcArgChk(nBins, binCols, scale, na.rm)
    # Inform NA status and drop NA rows if TRUE
    data <- naHandling(data, na.rm)
    # Cast to data.frame if necessary.
    data <- ensureDf(data)
    # Cast chars to factors and inform user
    data <- stringsToFactors(data)


    # The user may specify cols for binning without setting nBins
    # So warn and set for them.
    if (!is.null(binCols) && is.null(nBins)){
        nBins <- 3
        warning("Columns have been specified for binning without setting nBins. nBins has been set to 3.")
    }


    # Binning data. .babyBin interprets null binCols as "bin all numeric"
    if (!is.null(nBins))
        data <- .babyBin(data, nBins, binCols)

    
    
    # Scale remaining numeric columns.
    if (scale){
        numCols <- sapply(data, is.numeric)
        
        if (!is.null(target))
        {
            if (is.character(target))
                targIndex <- which(names(data) == target)
            if (is.numeric(target))
                targIndex <- target
            
            numCols[target] <- FALSE
        }
            
            
                
        # for matchProc. must find max and mins before scaling, else get 0, 1
        # as min max when scaling the next dataset.
        attr(data, "scaleCols") <- numCols
        attr(data, "maxes") <- sapply(data[numCols], max)
        attr(data, "mins") <- sapply(data[numCols], min)

        data[numCols] <- sapply(data[numCols], .babyScale)
    }


    return(data)
}


####################
# match processing #
####################


#' Preprocess new data
#'
#' Preprocesses a new dataset using the metrics used already used to preprocess
#' A dataset.
#'
#' @param x Either a data.frame returned by preProc, or trainedModel object returned by train()
#' @param newData The dataset to be processed.
#' @param na.rm A logical. If TRUE, rows containing NA are dropped.
#'
#' @return A data.frame processed by the same metrics used to process x
#'
#' @details Relies on attributed assigned to x by preProc()
#'
#' @export
matchProc <- function(x, newData, na.rm = FALSE, ...)
{
    UseMethod(generic = "matchProc", object = x)
}


# MatchProc method for data.frame objects
#
# The main matchProc function.
#
# @param x Either a data.frame returned by preProc, or trainedModel object returned by train()
# @param newData The dataset to be processed.
# @param na.rm A logical. If TRUE, rows containing NA are dropped.
#
# @return A data.frame processed by the same metrics used to process x
#
# @details Relies on attributed assigned to x by preProc()
#
#' @export
matchProc.data.frame <- function(x, newData, na.rm = FALSE, ...)
{
    # using preProc for casting and na handling.
    newData <- preProc(newData, na.rm = na.rm)


    ###########
    # Binning #
    ###########

    # binCols is used to subset newData
    binCols <- attr(x, "binCols")
    # Grabbing quants from x so that we can bin according to the same quantile
    myQuants <- attr(x, "quants")

    if (!is.null(binCols)){
        # Casting to character because .toClosest expects char (clunky)
        newData[binCols] <- sapply(newData[binCols], as.character)

        i <- 1
        myLen <- length(binCols[binCols == TRUE])

        # loop through columns to bin. Can probably find a better solution
        while (i <= myLen){
            # Using quantiles to define the bins.
            quants <- myQuants[[i]]

            # finding the proper bins for the column values and assigning them to the df
            newData[binCols][i] <- as.factor(unlist(lapply(newData[binCols][, i],
                                                        .toClosest, values = quants)))
            i <- i + 1
        }
    }


    ###########
    # Scaling #
    ###########

    # used to subset the cols that will be scaled
    scaleCols <- attr(x, "scaleCols")
    # Used to match the scale applied to x.
    mins <- attr(x, "mins")
    maxes <- attr(x, "maxes")


    if (!is.null(scaleCols)){
        # creating matrix to hold scaled columns
        vectMat <- matrix(ncol = length(which(scaleCols)), nrow = nrow(newData))

        # I don't like this loop, but am stuck. It would be nice to
        # have an apply that can map a vector of arguments...
        i <- 1
        for (vect in newData[scaleCols]) {
            # Creating column of binned data
            vect <- .matchScale(vect, oldMin = mins[i], oldMax = maxes[i])
            # matrix will be used to assign to the subset of newData
            vectMat[, i] <- vect

            i <- i + 1
        }

        # final assignment
        newData[scaleCols] <- vectMat
    }

    return(newData)
}


# MatchProc method for trainedModel objects
#
# Gets data from x, then passes to data.frame method.
#
# @param x Either a data.frame returned by preProc, or trainedModel object returned by train()
# @param newData The dataset to be processed.
# @param na.rm A logical. If TRUE, rows containing NA are dropped.
#
# @return A data.frame processed by the same metrics used to process x
#
# @details Relies on attributed assigned to x by preProc()
#
#' @export
matchProc.trainedModel <- function(x, newData, na.rm = FALSE, ...)
{
    matchProc.data.frame(x$vars$data, newData, na.rm = na.rm)
}


# Discretize numeric data
#
# bins numeric data from specified column into factors.
#
# @param data A data.frame with some numeric columns
# @param nBin the number of bins to use
# @param binCols Which columns to bin. can be scalar or vector. If null, all
#                   numeric columns are binned.
#
# @return A data.frame that has had numerics binned into factors
.babyBin <- function(data, nBin, binCols)
{
    #TODO: Can this be deleted?
    datBack <- data


    # Creating logical vector specifying which columns will be binned.
    # this is used to subset the data. if binCols is null, assume all numerics
    # are being binned.
    if (is.null(binCols))
        numCols <- sapply(data, is.numeric)
    else{
        numCols <- logical(length = ncol(data))
        numCols[binCols] <- TRUE
    }

    # casting the binning cols to character for easy creation of output
    data[numCols] <- sapply(data[numCols], as.character)



    ################
    # Binning Loop #
    ################

    #for matchPric()
    quantList <- list()

    i <- 1
    myLen <- length(numCols[numCols == TRUE])

    # loop through columns to bin. Can probably find a better solution
    while (i <= myLen){
        # Using quantiles to define the bins.
        quants <- quantile(as.numeric(data[numCols][, i]),
                                     probs = seq(0, 1, 1/nBin), names = FALSE)

        # finding the proper bins for the column values and assigning them to the df
        data[numCols][i] <- as.factor(unlist(lapply(data[numCols][, i],
                                                .toClosest, values = quants)))

        quantList <- c(quantList, list(quants))

        i <- i + 1
    }

    attr(data, "binCols") <- numCols
    attr(data, "quants") <- quantList

    return (data)
}


# Binary search for two nearest elements
#
# Searches for the two elements nearest to x from a sorted vector.
#
# @param x A scalar
# @param values A sorted numeric array (increasing order) of quantiles
#
# @return A vector containing the two nearest values. [1] is nearest, [2] is
#           2nd nearest
#
# @details Will not return if a max is found. Always carries out the full process
# could be made more efficient. Thought: If finds nearest match and next two steps don't
# result in a closer match, is the nearest match always correct?
.findClosest <- function(x, values)
{
    # midpoint from integer division
    mx <- trunc(length(values) / 2)

    # initializing info on closest
    close <- values[mx]
    dist <- abs(x - close)
    closeIndex <- mx

    # ex and bx define the search space
    ex <- length(values)
    bx <- 1


    # while the search space > 1 element
    while(bx <= ex)
    {
        # calculate midpoint of search space
        mx = trunc(bx + (ex - bx) / 2)

        # if the middle of the search space is closer to x, update info on nearest
        if (abs(x - values[mx]) < dist){
            closeIndex <- mx
            close <- values[mx]
            dist <- abs(x - values[mx])
        }

        # Move left (into smaller values)?
        if (x < values[mx])
            ex <- mx - 1
        # or move right (into larger values)?
        else
            bx <- mx + 1
    }

    # mx should always end at the nearest value, but just to be sure.
    mx <- closeIndex

    # Special cases: closest is 1st element or last element
    if (mx == length(values)){
        nextClose <- values[length(values[-1])]} else
    if (mx == 1){
        nextClose <- values[2]}

     # Is the element to the left or right of closest nearer to x?
     else{
        distL <- abs(x - values[mx+1])
        distS <- abs(x - values[mx-1])

        if (distL < distS)
            nextClose <- values[mx+1]
        else
            nextClose <- values[mx-1]
    }


    return (c(close, nextClose))
}


#TODO: can you change the apply in .babyBin so that it doesn't need a char vect
# Convert a character value to a character bin
#
# @param x A character scalar representing a numeric value
# @param values A numeric vector of values
#
# @return A character bin for x using the values in "values".
#
# @details x is character because .babyBin applies over a character vector
.toClosest <- function(x, values)
{
    # cast to numeric for .findClosest()
    x <- as.numeric(x)
    # get closest and sort to know which is lower and which is upper
    closest <- .findClosest(x, values)
    closest <- sort(closest)

    return(paste("{", closest[1], ", ", closest[2], "}", sep = ""))
}


#TODO: figure out why tests failed when trying to directly return.
# Scale numeric data
#
# Max-min scales numeric data between 0 and 1.
#
# @param x A numeric vextor
#
# @return x scaled between 0 and 1
.babyScale <- function(x){
    x <-  (x - min(x)) / (max(x) - min(x))

    return(x)
}


# hopefully a temp function...
.matchScale <- function(x, oldMin, oldMax)
{
    x <- ((x - oldMin) / (oldMax - oldMin))

    return (x)
}


# handle NA values
#
# messages user to inform how many NA values found across how many rows.
# can also remove them
#
# @param data A data.frame
# @param na.rm A logical. if TRUE, drop fows containing NA.
#
# @return a data.frame.
naHandling <- function(data, na.rm)
{
    # true for each row that contains NA.
    temp <- apply(data, 1, anyNA)
    # take length of subsetted temp to get row count
    naRowCt <- length(temp[temp == TRUE])
    # take length of indicies containing NA for raw count
    naRawCt <- length(which(is.na(data)))


    # Informing user of NAs. Lots of conditions for grammar's sake.
    if (naRawCt == 1)
        message("1 NA value has been found.")
    if (naRawCt > 1 && naRowCt == 1)
        message(paste(naRawCt, " NA values have been found in 1 row.", sep = ""))
    if (naRawCt > 1 && naRowCt > 1)
        message(paste(naRawCt, " NA values have been found in " , naRowCt, " rows.", sep = ""))

    # drop rows and inform user
    if (na.rm){
        data <- na.omit(data)

        # Assuming that there will be > 1 row remaining.
        if (naRowCt == 1)
            message(paste("1 row has been dropped. ", nrow(data), " rows remaining.", sep = ""))
        else
            message(paste(naRowCt, " rows have been dropped. ", nrow(data), " rows remaining.", sep = ""))
    }


    return (data)
}


# make sure input is data.frame
#
# if data is not a data.frame. Informs user and then converts.
#
# @param data The user's data input. Can be any object that might cast to df.
#
# @return A data.frame
ensureDf <- function(data)
{
    # message if not a data.frame
    if (!is.data.frame(data)){
        message(paste("\"data\" is a ", class(data)[1], " object. Converting to data.frame."),
                sep = "")

        # cast to data.frame
        data <- as.data.frame(data)
    }

    return (data)
}


# Convert a df with strings to factors
#
# Cast all character rows to factor.
#
# @param data A data.frame with character columns
#
# @return A data.frame
stringsToFactors <- function(data)
{
    # vector with TRUE for each char row (for subsetting)
    charCols <- sapply(data, is.character)

    if (any(charCols)){
        # number of character columns for gramatical output
        charCt <- which(charCols)

        if (length(charCt) == 1)
            message(paste("Column ", charCt, " contains character data. Converting to factor. ", sep = ""))
        else
            # need to paste inside paste otherwise output recurs
            message(paste("Columns ", paste(charCt, collapse = ", "), " contain character data. Converting to factor.", sep = "", collapse = ""))

        # casting to factor and reassigning.
        data[charCols] <- as.data.frame(lapply(data[charCols], as.factor))
    }


    return (data)
}


# Checks input to preProc
#
# Input errors for preProc
#
# @param nBin User's matching argument
# @param nCol User's matching argument
# @param scale User's matching argument
# @param na.rm User's matching argument
#
# @return null
preProcArgChk <- function(nBin, nCol, scale, na.rm)
{
    # Errors for nBins
    if (!is.null(nBin))
    {
        if (!is.numeric(nBin))
            stop("nBin requires a numeric argument.")
        if (length(nBin) > 1)
            stop("nBin must have length of 1")
        if (nBin < 1)
            stop("nBin must be > 0")
    }

    # Errors for nCol
    if (!is.null(nCol))
    {
        if (!is.numeric(nCol))
            stop("nCol requires a numeric argument")
        if (!is.null(dim(nCol)))
            stop("nCol must be a scalar or a 1-D vector")
    }

    # Errors for scale
    if (!is.logical(scale))
        stop("scale requires a logical argument (T/F)")

    # Errors for na.rm
    if (!is.logical(na.rm))
        stop("na.rm requires a logical argument (T/F)")
}

