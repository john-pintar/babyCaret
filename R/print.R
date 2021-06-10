### Temporary file for print methods
## John Pintar


#TODO: figure out best practices for print methods. 


#' @export
print.trainedModel <- function(myFullModel, ...)
{
    print(myFullModel$model)
}

#' @export
print.trainedApriori <- function(myApri, ...)
{
    print(myApri$model$ruleDf)
}


#' @export
print.Mape <- function(x, ...)
{
    mapeChar <- as.character(x * 100)
    cat(paste(c("Mean absolute percentage error: ", substr(mapeChar, 1, 4), "%"), sep = "", collapse = ""))
}


#' @export
print.MisClass <- function(x, ...)
{
    percWrong <- as.character(x * 100)
    cat(paste((c("Percent misclassified: ", substr(percWrong, 1, 4), "%")), sep = "", collapse = ""))
}
