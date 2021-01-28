### Keeps track of automated pre-processing steps.
## John Pintar


#TODO: Decide if this is still something you want to do (leaning no).


##############################
# The idea behind this class #
##############################


# The record class keeps a record of the pre-processing and warnings initiated by babyCaret.
# Eventually, this record will be shared with the user. This sharing
# will be used, essentially, as a report card. It should annoy the user to the
# point where they will aim to train a model while keeping this object empty.
# For warnings, the warning will reach the user twice: at time of offense and
# at time of output. The goal is to encourage the user to implement appropriate
# protocol for each call to train(). A record will be created for each call to
# train(). Each slot of a record object matches the function call which checks
# for appropriateness. Ideally, every warning should be reported to the record
# object. the slots should contain a boolean value. Anything else means the
# function call lacks specificity. If the function warned, the slot == TRUE
#
# Currently, the slots in this object are defined here and cannot be expanded upon at runtime.
# This may change, but it's probably useful to get an error if a typo is made during
# a call to recordFail()
#
# NOTE: Functions that only error and never warn should not be included as a slot in this object
#




new_record <- function()
{
    methodContext <- NA
    noNA <- NA

    out <- list(methodContext, noNA)

    names(out) <- list("methodContext", "noNA")
    class(out) <- "record"
    return(out)
}

recordFail <- function(myRecord, failedFunction)
{
    myRecord[failedFunction] <- TRUE
}
