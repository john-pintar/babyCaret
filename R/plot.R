### Temp file for plot methods
## John Pintar


#TODO: figure out why apriori is warning

#' @export
plot.trainedKnn <- function(myKnn, ...)
{
    model <- myKnn$model$modelFrame
    predictors <- model[-1]
    target <- model[1]
    names(target) <- "target"

    plot(as.data.frame(cbind(predictors, target)))
}


#' @export
plot.trainedTree <- function(tree, showInterior = FALSE, ...)
{
    if (tree$model$method == "anova")
    {
        if (!showInterior)
            rpart.plot::rpart.plot(tree$model, type = 2, extra = 1, roundint = FALSE)
        else
            rpart.plot::rpart.plot(tree$model, type = 4, extra = 1, roundint = FALSE)
    }


    else
    {
        if (!showInterior)
            rpart.plot::rpart.plot(tree$model, type = 0, extra = 2, roundint = FALSE)
        else
            rpart.plot::rpart.plot(tree$model, type = 4, extra = 2, roundint = FALSE)
    }
}


#' @export
plot.trainedKproto <- function(myProto, ...)
{
    clprofiles(myProto$model, myProto$vars$data)
}


#' @export
plot.trainedApriori <- function(myApri, rules = 3, ...)
{
    plot(myApri$model$apriModel, "graph", "lift", "support", max = rules)
}
