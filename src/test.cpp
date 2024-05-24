//
// #include <Rcpp.h>
// using namespace Rcpp;
//
// // [[Rcpp::export]]
// NumericMatrix reconstruct3(NumericVector m_lower, bool diag = true, bool symmetric = true) {
//   int l = m_lower.size();
//   int n = (sqrt(1 + 8*l) + (diag ? -1 : 1)) / 2;
//   NumericMatrix m(n, n);
//
//   // Reconstruct
//   int idx = 0;
//   for (int i = 0; i < n; ++i) {
//     for (int j = 0; j <= i; ++j) {
//       if (diag || i != j) {
//         m(i, j) = m_lower[idx++];
//       } else {
//         m(i, j) = 0; // Placeholder for diagonal elements if not filled
//       }
//     }
//   }
//
//   if (symmetric) { // If symmetric, fill also upper half
//     for (int i = 0; i < n; ++i) {
//       for (int j = i + 1; j < n; ++j) {
//         m(i, j) = m(j, i);
//       }
//     }
//   }
//
//   return m;
// }
