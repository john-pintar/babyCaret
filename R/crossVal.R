## Interactive cross-validation for train()
# John Pintar


#TODO: figure out a better way to handle the vector input issue.
#TODO: Need guardrails for user.
#TODO: cv should work even when no target. 
#TODO: GUI
#TODO: Parameter search
#TODO: Documentation


tuneFromMeth <- function(method)
{
    if (method == "knn")
        return(setKnn())
    if (method == "tree")
        return(setTree())

    else
        stop(paste(method, " is not compatable with cross-validation", sep = ""))
}


#TODO: problem where last bit gets cut.
    # temp solution: force conditions for first and last folds.
pullData <- function(data, foldPos, k)
{
    # foldSize is used to create the equal size partitions.
    foldSize <- trunc(nrow(data) / k)

    # end of train fold. hard code the last fold in case rounding error
    if (foldPos == k)
        endFold <- nrow(data)
    else
        endFold <- foldPos * foldSize
    # Beginning of train fold. Hard code first fold in case rounding error
    if (foldPos == 1)
        begFold <- 1
    else
        begFold <- endFold - foldSize

    # Create the two separate datasets
    testRows <- begFold:endFold
    trainSet <- data[-testRows, ]
    testSet <- data[testRows, ]

    # Return the object with names
    out <- list(trainSet, testSet)
    names(out) <- c("trainSet", "testSet")
    return (out)
}

# Calculate cumulative error (error summed across folds)
cumError <- function(x, data, method, tune, i, k)
{
    if (i == 0)
        return(0)

    set <- pullData(data, i, k)
    model <- train(set$train, x, method, tune)
    res <- score(model, set$test)

    return(res + cumError(x, data, method, tune, i-1, k))
}


#TODO: add data shuffling here
getError <- function(x, data, method, tune, k)
{
    out <- cumError(x, data, method, tune, k, k)
    return (out / 5)
}


# New best tune bug? I think it's fixed
crossVal <- function(x, data, method, tune)
{
    # replacing the "cv" string with a proper tune object
    # "cv" is only used to get into this function.
    tune <- tuneFromMeth(method)

    # getting error from k-fold cross validation
    error <- getError(x, data, method, tune, 5)

    # Adding error to the proper score row in the tune Obj.
    tune <- update_score(tune, error)
    # Setting to Inf so that the current tune is forced to best tune duing
    # Best tune update
    tune$bestTune[1, 4] <- Inf
    tune <- update_best(tune)

    
    usrSelect <- NULL
    usrInpt <- NULL
    usrFin <-FALSE

    i <- 1
    while(TRUE)
    {
        cvDisplay(tune, usrSelect, i)
        if (i == 1)
        {
            params <- getParams(tune)
            gramParams <- engList(params)
            message("Type \"done\" when finished.")
            usrSelect <- readline(paste("Select a parameter to adjust; either ", gramParams, ": ", sep = ""))

            if(doneCheck(usrSelect))
                usrFin <- TRUE
        }

        message("Type \"done\" when finished.")
        usrInpt <- readline(paste("Enter new value for ", usrSelect, ", or a different parameter to adjust: " , sep = ""))
        if (any(names(tune$grid)[1:3] == usrInpt))
        {
            usrSelect <- usrInpt
            next
        }
        else
            usrVal <- usrInpt


        if (doneCheck(usrInpt))
            return(train(data, x, method, tune))

        
        tune <- string_update(tune, usrSelect, usrVal)
        error <- getError(x, data, method, tune, 5)
        tune <- update_score(tune, error)
        tune <- update_best(tune)

        i <- i + 1
    }
    
    return(tune)
}


#TODO: Change this to the old babyCaret V0 function.
doneCheck <- function(inpt)
{
    if (identical(inpt, "done"))
        return(TRUE)
    else
        return(FALSE)
}


# The main display of cross validation
cvDisplay <- function(tuneObj, usrSelect, i)
{
    cat("************************************************\n")

    showTune(tuneObj, usrSelect, i)

    cat("************************************************\n")
}


showTune <- function(tune, usrSelect, i)
{

    # ERROR
    rec <- Inf


    cur <- tune$currentTune$score[1]
    if (i != 1)
        rec <- tune$grid$score[nrow(tune$grid) -1]
    bes <- tune$bestTune$score[1]

    current <- as.character(round(cur, 6))
    recent <- crayon::blue(as.character(round(rec, 6)))
    best <- crayon::yellow(as.character(round(bes, 6)))

    if (cur <= bes)
        current <- crayon::yellow(current)
    else if (cur > rec)
        current <- crayon::red(current)
    else if (cur < rec)
        current <- crayon::green(current)

    else if (cur == rec)
    {
        current <- crayon::blue(current)
        recent <- crayon::blue(recent)
    }


    if(cur == bes)
        current <- crayon::yellow(current)

    if(rec == bes)
        recent <- crayon::yellow(best)

    if (i == 1)
        recent <- crayon::blue("NA")


        cat(crayon::bold("                     error\n"))


        cat(" lowest:  ", best,
            "\n", " recent:  ", recent,
            "\n", " current: ", current, "\n\n", sep = "")

    # PARAMS

    cat(crayon::bold("                  parameters\n"))


    activeDex <- which(names(tune$grid) == usrSelect)

    j <- 1
    while (j < ncol(tune$currentTune))
    {
        if (j == activeDex && i != 1)
        {
            cat("*", names(tune$grid)[j], "=", tune$currentTune[1, j],
                crayon::blue("was"), crayon::blue(tune$grid[nrow(tune$grid) -1, j]), "\n")
        }
        else
        cat("*", names(tune$currentTune)[j], "=", tune$currentTune[1, j], "\n")

        j <- j + 1
    }
}


engList <- function(charVect)
{
    gramList <- charVect[-length(charVect)]
    gramList <- paste(gramList, sep = "", collapse = ", ")
    gramList <- paste(gramList, ", or ", charVect[length(charVect)], collapse = "", sep = "")

    return(gramList)
}


getParams <- function(tuneObj)
{
    return(names(tuneObj$grid)[-length(names(tuneObj$grid))])
}
