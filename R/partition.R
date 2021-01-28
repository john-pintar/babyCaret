### Partitions a single dataset into training and testing sets
## John Pintar


# TODO: Add support for stratified random sampling
# TODO: Consider adding support for verification sets
# TODO: Should there be vector support?
# TODO: Ensure downstream functions can handle matrix input. Warn and cast?


#' Partition a dataset
#'
#' Partitions a single dataset into separate training and testing sets
#'
#' @param df a data.frame or matrix object
#' @param p a number between 0 and 1
#' @return a list containing training set and testing set
#' @examples
#' Bpartition(iris, .7)
#' @export
partition <- function(df, p = .75, val = FALSE)
{
    ##################
    # Checking input #
    ##################
    
    # Error when val is not logical
    if(!is.logical(val))
        stop("val must recieve a logical argument")

    # Error when df is not a data.frame or matrix
    if (!is.data.frame(df) && !is.matrix(df))
        stop("df must be a data.frame or matrix object")

    # Error when p < 0 or p > 1

    if (p < 0 || p > 1)
        stop("p cannot be less than 0 or greater than 1")

    # Warnings when p = 1 or 0
    # All instances will only go to one set
    if (p == 1)
    {
        warning("when p = 1, all rows will be sent to train set")
        return(list(train = df[sample(nrow(df)), ], test = NA))
    }

    if (p == 0)
    {
        warning("when p = 0, all rows will be sent to test set")
        return(list(train = NA, test = df[sample(nrow(df)), ]))
    }


    ###################
    # Actual function #
    ###################

    # Shuffling deck
    df <- df[sample(nrow(df)), ]
    # Cutting deck
    cutOff <- trunc((nrow(df) * p))
    train <- df[1:cutOff, ]
    test <- df[(cutOff + 1):nrow(df), ]

    if (val == FALSE)
        return(list(train = train, test = test))
    
    val <- test[1:trunc(nrow(test)/2), ]
    test <- test[- trunc(nrow(test)/2:nrow(test)), ]
    
    return (list(train = train, test = test, val = val))
}
