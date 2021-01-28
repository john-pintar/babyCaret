#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix calculateDist(LogicalVector isNum, NumericMatrix dataNums,
                            NumericMatrix dataCats, NumericMatrix protoNums,
                            NumericMatrix protoCats, int rows, int cols, int k,
                            double lambda)
{
    NumericMatrix dists(rows, cols);
    NumericMatrix numDists(rows, cols);
    NumericMatrix catDists(rows, cols);


    // distance for each prototype
    for (int i = 0; i < k; i++)
    {
        int numIt = -1;
        int catIt = -1;
        // for each column of data
        for (int j = 0; j < cols; j++)
        {
            if (isNum[j] == true)
            {
                numIt ++;
                // for each row of num data
                for (int l = 0; l < rows; l++)
                {
                    // calculating numeric distance
                    {
                        numDists(l, i) += ((dataNums(l, numIt) - protoNums(i, numIt))
                                * (dataNums(l, numIt) - protoNums(i, numIt)));
                    }
                }
            }

            else
            {
                catIt ++;
                // for each row of cat data
                for (int l = 0; l < rows; l++)
                {
                    // calculating cat distance
                    {
                        if (dataCats(l, catIt) != protoCats(i, catIt))
                            catDists(l, i) += 1 * lambda;
                    }
                }
            }
        }
    }

    for (int i = 0; i < cols; i++)
        for (int j = 0; j < rows; j++)
            dists(j, i) = sqrt(numDists(j, i)) + catDists(j, i);


    return(dists);
}


