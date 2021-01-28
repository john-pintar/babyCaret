//TODO: something is up with IDW, but it is not here...

// K-Nearest Neighbors classification and regression
// John Pintar


// tunable inverse distance weighting for classification and regression.
// Accepts feature weights for classification and regression.
// Accepts mixed numeric and categorical predictors
// Hamming distance for categorics, Manhattan or Euclidean for Numerics
// Class ties are settled pseudo-randomly. If a class tie occurs when p > 0,
//          it likely cannot be settled meaningfully.
// Currently does not allow trailing ties for the nearest neighbors.


//TODO: Unit tests.
//TODO: Organize functions
//TODO: Should class ties be resolved randomly when p = 0? That's how I have it now.
//TODO: consider switching to iterators vs indexing.


//loading libraries and namespace
#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;



// Inverse distance weighted interpolation
//
// Calculates a weighted average which favors nearer points
//
// @param values A NumericVector of values to be used for interpolation
// @param dists A NumericVector of distances distances corresponding to "values"
//                  (how far is the corresponding point from the unknown point)
// @param p A double >= 0 specifying the power of the inverse distance weighting.
//          0 means no weighting by distance will occur (non-weighted KNN).
//          2 is commonly used.
//
// @details As p increases, the influence of relatively distant points will decrease.
//
// [[Rcpp::export]]
double idw(NumericVector values, NumericVector dists, double p)
{
    // if p = 0, then we don't care about IDW and will just use built-in mean.
    // less processing, less worry.
    if (p == 0)
        return mean(values);


    NumericVector identicals;

    // checking for identicals. If found, return the mean of the identicals
    // because now we're doing IDW.               {if dist(x) = 0 IDWI = x}
    // Failure to do so will divide by zero.
    bool foundId = false;
    for (int i = 0; i < dists.length(); i++)
    {
        if (dists(i) == 0)
        {
            identicals.push_front(values(i));
            foundId = true;
        }
    }

    if (foundId)
        return mean(identicals);


    // using distance to calculate weights.
    NumericVector weights = (1 / pow(dists, p)) / sum(1/pow(dists, p));
    // weighted mean.
    double out = sum(values * weights);


    return out;
}


// Voting for KNN
//
// Inverse distance weighted voting.
//
// @param values a NumericVector of integer labels. 0 < any label <= "levels"
// @oaram dists Distances that pair with "values"
// @param levels An int specifying how many unique labels could be in "values"
// @param p A double >= 0 specifying the power of the inverse distance weighting.
//          0 means no weighting by distance will occur (non-weighted KNN).
//          2 is commonly used.
//
// @details As p increases, the influence of relatively distant points will decrease.
//
// [[Rcpp::export]]
int voting(NumericVector values, NumericVector dists, int levels, double p)
{
    // Written while watching Biden's acceptance speech.
    NumericVector voters = values;
    bool foundId = false;


    // search for identicals if doing IDW. If found, replace values/voters with
    // identicals because with IDW, those are the only points we care about (if found).
    if (p != 0)
    {
        NumericVector identicals;

        // scan through dists
        for (int i = 0; i < dists.length(); i++)
        {
            if (dists(i) == 0)
            {
                // fill array and set flag
                identicals.push_front(voters(i));
                foundId = true;
            }
        }

        if (foundId)
            voters = identicals;
    }


    // levels +1 because the ballot boxes indices correspond to factor levels
    // last index of ballot box must = largest integer representation of the target
    NumericVector ballotBox(levels+1);
    NumericVector district(voters.length());

    // init weights to 1 (popular vote)
    for (int i = 0; i < voters.length(); i++)
        district(i) = 1;

    // If no identicals and no IDW, change weight of vote according to distance
    if (p != 0 && !foundId)
        district = (1 / pow(dists, p)) / sum(1/pow(dists, p)); // IDW

    // Each voter casts their vote into the appropriate index of
    // the ballot box.
    for(int i = 0; i < voters.length(); i++)
        ballotBox(voters(i)) += district(i);


    // Will push tied candidates into this
    NumericVector runOff;


    // Counting the votes.
    double max = 0;
    int biden = 0;
    for (int i = 1; i <levels + 1; i++)
    {
        if (ballotBox(i) > max)
        {
            max = ballotBox(i);
            biden = i;
            runOff = i;
        }
        // pushing tied values into here
        else if (ballotBox(i) == max)
            runOff.push_back(i);
    }

    // winner assigned randomly. Should I change this? Although, it's irrelevant when != 0
    // Why would I make relative distance a concern in the user isn't considering it themselves?
    // which is what iteratively decreasing vector length would effectively do.
    if (runOff.length() > 0)
        biden = runOff(rand() % runOff.length());


    return biden;
}


// Merge sorted vector segments
//
// Will merge two sorted vector segments into one sorted segment
//
// @param vA A NumericVector sorted low->high between bx1:ex1 and bx2:ex2
// @param vB A companion vector to vB. vB sorted according to vA
// @param bx1 The beginning index of the first sorted segment
// @param ex1 The last occupied index of the first sorted
// @param bx2 The beginning index of the second sorted segment
// @param ex2 The last occupied index of the second sorted segment
//
// [[Rcpp::export]]
void mergeCompanion(NumericVector vA, NumericVector vB, int bx1, int ex1, int bx2, int ex2)
{
    // bx1:ex1 is one deck/segment bx2:ex2 is another.these segments are already
    // sorted when passed.

    // Temporary work vectors. These will have values from the two decks placed
    // in sorted order and then will merge with original vectors
    NumericVector tA(ex2 - bx1 + 1);
    NumericVector tB(ex2 - bx1 + 1);

    // will keep track of how many cards we've placed in the work array
    int k=0;
    // first section of vector
    int i = bx1;
    // second section of vector
    int j = bx2;


    // while at least one deck still has cards, take the lowest card from
    // the top of a deck and append it to the temp vector. shadow process for vB.
    while (i <= ex1 && j <= ex2)
    {
        if (vA(i) < vA(j))
        {
            tA(k) = vA(i);
            tB(k++) = vB(i++);
        }
        else
        {
            tA(k) = vA(j);
            tB(k++) = vB(j++);
        }
    }


    // If there are still cards in this deck, append them all to the temp vectors
    while (i<=ex1)
    {
        tA(k) = vA(i);
        tB(k++) = vB(i++);
    }

    // if there are still cards in this deck, append them all to the temp vectors
    while (j<=ex2)
    {
        tA(k) = vA(j);
        tB(k++) = vB(j++);
    }


    // merge the temp vectors into their original vectors.
    for (int i = 0; i < k; i++)
    {
        vA(bx1 + i) = tA(i);
        vB(bx1 + i) = tB(i);
    }
}

// Merge sort vectors while retaining pairity.
//
// Merge sorts a vector and shadows that *exact* sorting process for another vector.
//
// @param by A NumericVector that will be sorted.
// @param with A NumericVector that will shadow the sorting process.
// @param bx The beginning index of the segment to be sorted (typically 0 in the
//            non-recursive call)
// @param ex The ending index of the segment to be sorted (typically length -1
//            in the non-recursive call)
//
// [[Rcpp::export]]
void mergeSortCompanion(NumericVector by, NumericVector with, int bx, int ex)
{
    // When one a one element vector segment is reached, unwind recursion
    if (ex<=bx)
        return;

    // calculate midpoint
    int m = (bx+ex)/2;

    // recursively split (via midpoint index)
    mergeSortCompanion(by, with, bx,m);
    mergeSortCompanion(by, with, m+1,ex);
    // merge into sorted array
    mergeCompanion(by, with, bx, m,m+1,ex);
}



// NOTE: This documentation is less than clear.
// Insert values in sorted order while maintaining pairity with a companion
// array
//
// If a value (byCand) is less than the largest value in an array (by), it will
// replace that value and then the array will be sorted (decreasing insertion sort) and a
// companion array (with) along with a companion value (withCand) will shadow
// the *exact* same process.
//
// @param by The NumericVector which will have its largest value replaced with
//          "byCand" if "byCand" is less than that value.
// @param with A companion vector of with equal length and values paired to "by"
//          this vector will be sorted by "by"
// @param byCand candidate value for by
// @param withCand "with's" paired value to "byCand"
//
// [[Rcpp::export]]
void insertCompanion(NumericVector by, NumericVector with, double byCand, double withCand)
{
    // maybe naming vectors verbs isn't great...

    // last occupied index
    int lastSlot = by.length() -1;

    // only want to insert if the value is less than the largest current
    if (by(lastSlot) < byCand)
        return;

    // doesn't allow trailing ties. This is where you would start changing that
    if (by(lastSlot) == byCand)
        return;


    // insert into largest valued position (last index)
    by(lastSlot) = byCand;
    with(lastSlot) = withCand;


    // flipping slots until the value is in sorted position
    while (lastSlot > 0 && by(lastSlot) < by(lastSlot - 1))
    {

        by(lastSlot) = by(lastSlot -1);
        by(lastSlot -1) = byCand;

        // to maintain pairity (sorting by "by")
        with(lastSlot) = with(lastSlot -1);
        with(lastSlot -1) = withCand;

        lastSlot--;
    }
}



// Compute a distance matrix
//
// Creates a distance matrix for use in KNN or k-prototypes
//
// @param isNum A LogicalVector designating which predictors (cols) are numeric.
// @param trainMat A NumericMatrix used to create a distance matrix. Will be rows
// @param testMat A NumericMatrix  used to create a distance matrix. Will be cols
// @param manhattan A bool if true, manhattan distance is used; if false, euclidean.
// @param w A Numeric vector of feature weights. length must equal ncols.
//
// @details The matrices don't necessarily need to be training or testing sets,
// They are just named as such to ensure proper use of the function. trainMat
// will take on the rows of the matrix while testMat takes on the columns of the
// matrix.
//
// [[Rcpp::export]]
NumericMatrix cpp_makeDist(LogicalVector isNum, NumericMatrix trainMat,
                      NumericMatrix testMat, bool manhattan, NumericVector w)
{
    int cols = trainMat.ncol();

    // Making separate matrices for sqrt in euclidean distance
    NumericMatrix numDists(trainMat.nrow(), testMat.nrow());
    NumericMatrix catDists(trainMat.nrow(), testMat.nrow());


    /////////////
    // Numeric //
    /////////////


    // for each (original) column.
    for (int i = 0; i < cols; i++)
    {
        if (isNum[i])
        {   // for each train Instance
            for (int j = 0; j < trainMat.nrow(); j++)
            {
                // for each row of testing Num data
                for (int l = 0; l < testMat.nrow(); l++)
                {
                    // calculate appropriate 1D distance and add to matrix
                    if (manhattan)
                        numDists(j, l) +=  w[i] * std::abs(trainMat(j, i) - testMat(l, i));
                    else
                        numDists(j, l) +=  w[i] * pow(trainMat(j, i) - testMat(l, i), 2);
                }
            }
        }
    }


    // If euclidean dist, taking square root.
    // Must be done after the sum of squares and before combining matrices.
    if (!manhattan)
        for (int i = 0; i < numDists.ncol(); i++)
            for (int j = 0; j < numDists.nrow(); j++)
                numDists(j, i) = sqrt(numDists(j, i));


    /////////////////
    // Categorical //
    /////////////////


    // for each original column
    for (int i = 0; i < cols; i++)
    {
        if (!isNum[i])
        {
            // for each train Instance
            for (int j = 0; j < trainMat.nrow(); j++)
            {
                // for each row of testing cat data
                for (int l = 0; l < testMat.nrow(); l++)
                {
                    // If the values match
                    if (trainMat(j, i) != testMat(l, i))
                        // Update dist matrix
                        catDists(j, l) += w[i];
                }
            }
        }
    }


    // summing numeric and categorical distance
    for (int i = 0; i < numDists.ncol(); i++)
        for (int j = 0; j < numDists.nrow(); j++)
            numDists(j, i) += catDists(j, i);


    // After summing, numDists is total distance
    return numDists;
}



// Make predictions using the KNN algorithm
//
// Performs k-Nearest Neighbors classification or regression.
//
// @param target A NumericVector containing trainMat's known target values.
//                  Length must be equal to trainMat's row count.
// @param k An int specifying how many of the nearest neighbors to use in a prediction.
//                  0<k<nrow(trainMat)
// @param p An int specifying the power used for inverse distance weighting. 0 results
//          in no IDW.
// @param levels An int specifying how many labels the target has. If regression, 0.
//
// [[Rcpp::export]]
NumericVector cpp_knnPredict(NumericMatrix dists, NumericVector target,
                                 int k, double p, int levels)
{
    // Just making it obvious
    bool isRegress = true;
    if (levels > 0)
        isRegress = false;

    // matrix columns relate to test instances
    // matrix rows relate to train instances

    NumericVector predictions(dists.ncol());


    // making predictions for each test instance.
    for (int i = 0; i < dists.ncol(); i++)
    {
         ////////////////////////////
        // Find Neighbors' values //
       ////////////////////////////


        // need to retain parity between these vectors. the values must
        // share an index with *their* distance. Otherwise, we will be making
        // predictions using the k-??? neighbors.
        NumericVector distances(k);
        NumericVector values(k);

        // Fill them with the first k values and their paired distances
        for (int temp = 0; temp < k; temp++)
        {
            values(temp) = target(temp);
            distances(temp) = dists(temp, i);
        }


        // Sort by distance while retaining value's index parity
        mergeSortCompanion(distances, values, 0, k -1);


        // Insert sort remaining dists into the sorted array while retaining index parity.
        // TODO: experimentally determine if this is faster than one big merge sort.
        for (int j = k; j < dists.nrow(); j++) // (for each train instance)
            insertCompanion(distances, values, dists(j,i), target(j));


         ///////////////////////
        // Make a Prediction //
       ///////////////////////


        if (isRegress)
            predictions(i) = idw(values, distances, p);
        else
            predictions(i) = voting(values, distances, levels, p);
    } // end outer loop


    return predictions;
}



// Make predictions using the KNN algorithm
//
// Performs k-Nearest Neighbors classification or regression.
//
// @param isNumeric A LogicalVector designating which predictors (cols) are numeric.
// @param trainMat A NumericMatrix of predictors used for training. Cols are features,
//                  rows are instances. Categorical data should be integers.
// @param testMat A NumericMatrix of predictors used for testing. Must be formatted
//                  identically to trainMat. Only row count may differ.
// @param target A NumericVector containing trainMat's known target values.
//                  Length must be equal to trainMat's row count.
// @param manhattan A bool if true, manhattan distance is used; if false, euclidean.
// @param k An int specifying how many of the nearest neighbors to use in a prediction.
//                  0<k<nrow(trainMat)
// @param p An int specifying the power used for inverse distance weighting. 0 results
//          in no IDW.
// @param featWeights A NumericVector of feature weights
// @param levels An int specifying how many labels the target has. If regression, 0.
//
// @details For classification, integers > 0 have been tested. using 0 as a class
//          has not been tested. Your target should use a sequence of integers beginning with
//          1 and ending at levels. e.g for binary classification, target values
//          should be 1 or 2, and levels must = 2.
//
// [[Rcpp::export]]
NumericVector cpp_knn(LogicalVector isNumeric, NumericMatrix trainMat, NumericMatrix testMat,
                            NumericVector target, bool manhattan, int k, double p,
                            NumericVector featWeights, int levels)
{
    NumericMatrix myDists = cpp_makeDist(isNumeric, trainMat, testMat,
        manhattan, featWeights);

    return cpp_knnPredict(myDists, target, k, p, levels);
}
