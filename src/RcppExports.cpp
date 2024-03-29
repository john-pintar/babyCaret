// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// idw
double idw(NumericVector values, NumericVector dists, double p);
RcppExport SEXP _babyCaret_idw(SEXP valuesSEXP, SEXP distsSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type values(valuesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type dists(distsSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(idw(values, dists, p));
    return rcpp_result_gen;
END_RCPP
}
// voting
int voting(NumericVector values, NumericVector dists, int levels, double p);
RcppExport SEXP _babyCaret_voting(SEXP valuesSEXP, SEXP distsSEXP, SEXP levelsSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type values(valuesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type dists(distsSEXP);
    Rcpp::traits::input_parameter< int >::type levels(levelsSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(voting(values, dists, levels, p));
    return rcpp_result_gen;
END_RCPP
}
// mergeCompanion
void mergeCompanion(NumericVector vA, NumericVector vB, int bx1, int ex1, int bx2, int ex2);
RcppExport SEXP _babyCaret_mergeCompanion(SEXP vASEXP, SEXP vBSEXP, SEXP bx1SEXP, SEXP ex1SEXP, SEXP bx2SEXP, SEXP ex2SEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type vA(vASEXP);
    Rcpp::traits::input_parameter< NumericVector >::type vB(vBSEXP);
    Rcpp::traits::input_parameter< int >::type bx1(bx1SEXP);
    Rcpp::traits::input_parameter< int >::type ex1(ex1SEXP);
    Rcpp::traits::input_parameter< int >::type bx2(bx2SEXP);
    Rcpp::traits::input_parameter< int >::type ex2(ex2SEXP);
    mergeCompanion(vA, vB, bx1, ex1, bx2, ex2);
    return R_NilValue;
END_RCPP
}
// mergeSortCompanion
void mergeSortCompanion(NumericVector by, NumericVector with, int bx, int ex);
RcppExport SEXP _babyCaret_mergeSortCompanion(SEXP bySEXP, SEXP withSEXP, SEXP bxSEXP, SEXP exSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type by(bySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type with(withSEXP);
    Rcpp::traits::input_parameter< int >::type bx(bxSEXP);
    Rcpp::traits::input_parameter< int >::type ex(exSEXP);
    mergeSortCompanion(by, with, bx, ex);
    return R_NilValue;
END_RCPP
}
// insertCompanion
void insertCompanion(NumericVector by, NumericVector with, double byCand, double withCand);
RcppExport SEXP _babyCaret_insertCompanion(SEXP bySEXP, SEXP withSEXP, SEXP byCandSEXP, SEXP withCandSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type by(bySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type with(withSEXP);
    Rcpp::traits::input_parameter< double >::type byCand(byCandSEXP);
    Rcpp::traits::input_parameter< double >::type withCand(withCandSEXP);
    insertCompanion(by, with, byCand, withCand);
    return R_NilValue;
END_RCPP
}
// cpp_makeDist
NumericMatrix cpp_makeDist(LogicalVector isNum, NumericMatrix trainMat, NumericMatrix testMat, bool manhattan, NumericVector w);
RcppExport SEXP _babyCaret_cpp_makeDist(SEXP isNumSEXP, SEXP trainMatSEXP, SEXP testMatSEXP, SEXP manhattanSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< LogicalVector >::type isNum(isNumSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type trainMat(trainMatSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type testMat(testMatSEXP);
    Rcpp::traits::input_parameter< bool >::type manhattan(manhattanSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type w(wSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_makeDist(isNum, trainMat, testMat, manhattan, w));
    return rcpp_result_gen;
END_RCPP
}
// cpp_knnPredict
NumericVector cpp_knnPredict(NumericMatrix dists, NumericVector target, int k, double p, int levels);
RcppExport SEXP _babyCaret_cpp_knnPredict(SEXP distsSEXP, SEXP targetSEXP, SEXP kSEXP, SEXP pSEXP, SEXP levelsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type dists(distsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type target(targetSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type levels(levelsSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_knnPredict(dists, target, k, p, levels));
    return rcpp_result_gen;
END_RCPP
}
// cpp_knn
NumericVector cpp_knn(LogicalVector isNumeric, NumericMatrix trainMat, NumericMatrix testMat, NumericVector target, bool manhattan, int k, double p, NumericVector featWeights, int levels);
RcppExport SEXP _babyCaret_cpp_knn(SEXP isNumericSEXP, SEXP trainMatSEXP, SEXP testMatSEXP, SEXP targetSEXP, SEXP manhattanSEXP, SEXP kSEXP, SEXP pSEXP, SEXP featWeightsSEXP, SEXP levelsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< LogicalVector >::type isNumeric(isNumericSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type trainMat(trainMatSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type testMat(testMatSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type target(targetSEXP);
    Rcpp::traits::input_parameter< bool >::type manhattan(manhattanSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type featWeights(featWeightsSEXP);
    Rcpp::traits::input_parameter< int >::type levels(levelsSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_knn(isNumeric, trainMat, testMat, target, manhattan, k, p, featWeights, levels));
    return rcpp_result_gen;
END_RCPP
}
// calculateDist
NumericMatrix calculateDist(LogicalVector isNum, NumericMatrix dataNums, NumericMatrix dataCats, NumericMatrix protoNums, NumericMatrix protoCats, int rows, int cols, int k, double lambda);
RcppExport SEXP _babyCaret_calculateDist(SEXP isNumSEXP, SEXP dataNumsSEXP, SEXP dataCatsSEXP, SEXP protoNumsSEXP, SEXP protoCatsSEXP, SEXP rowsSEXP, SEXP colsSEXP, SEXP kSEXP, SEXP lambdaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< LogicalVector >::type isNum(isNumSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type dataNums(dataNumsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type dataCats(dataCatsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type protoNums(protoNumsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type protoCats(protoCatsSEXP);
    Rcpp::traits::input_parameter< int >::type rows(rowsSEXP);
    Rcpp::traits::input_parameter< int >::type cols(colsSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    rcpp_result_gen = Rcpp::wrap(calculateDist(isNum, dataNums, dataCats, protoNums, protoCats, rows, cols, k, lambda));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_babyCaret_idw", (DL_FUNC) &_babyCaret_idw, 3},
    {"_babyCaret_voting", (DL_FUNC) &_babyCaret_voting, 4},
    {"_babyCaret_mergeCompanion", (DL_FUNC) &_babyCaret_mergeCompanion, 6},
    {"_babyCaret_mergeSortCompanion", (DL_FUNC) &_babyCaret_mergeSortCompanion, 4},
    {"_babyCaret_insertCompanion", (DL_FUNC) &_babyCaret_insertCompanion, 4},
    {"_babyCaret_cpp_makeDist", (DL_FUNC) &_babyCaret_cpp_makeDist, 5},
    {"_babyCaret_cpp_knnPredict", (DL_FUNC) &_babyCaret_cpp_knnPredict, 5},
    {"_babyCaret_cpp_knn", (DL_FUNC) &_babyCaret_cpp_knn, 9},
    {"_babyCaret_calculateDist", (DL_FUNC) &_babyCaret_calculateDist, 9},
    {NULL, NULL, 0}
};

RcppExport void R_init_babyCaret(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
