# Temporary hack for getting data in knn tutorial 
#' @export
getData <- function(s)
{
heart2 <- kmed::heart
heart <- heart2
heart$class[heart$class == 0] <- "no"
heart$class[heart$class != "no"] <- "yes"

heart <- as.data.frame(cbind(heart$sex, heart$cp, heart$trestbps, heart$chol, heart$class))
colnames(heart) <- c("male", "pain", "bp", "chol", "mi")


heart$pain[heart$pain == 1] <- "typical"
heart$pain[heart$pain == 2] <- "atypical"
heart$pain[heart$pain == 3] <- "non-anginal"
heart$pain[heart$pain == 4] <- "asymptomatic"

heart$male <- as.factor(heart$male)
heart$pain <- as.factor(heart$pain)
heart$bp <- as.numeric(heart$bp)
heart$chol <- as.numeric(heart$chol)
heart$mi <- as.factor(heart$mi)

newPatient <- heart[1, ]
unknownBP <- heart[2, ]
heart <- heart[-1, ]
heart <- heart[-2, ]

newPatient[, 5] <- NA
unknownBP[, 3] <- NA

if (s=="heart")
  return(heart)
if (s=="newPatient")
  return(newPatient)
if (s=="unknownBP")
  return(unknownBP)

return(NA)
}

