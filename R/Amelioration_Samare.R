#
#
#
# # generate residus
#
# Rcpp::cppFunction('
#
#   NumericMatrix generate_residus(int horizon, NumericMatrix residus, double residual, double gamma, double rho) {
#     int n = residus.nrow();
#     NumericVector rand_nums(n);
#
#     for (int i = 1; i <= horizon; i++) {
#       double sd = sqrt(residual * gamma * pow(rho, i - 1));
#       for (int j = 0; j < n; j++) {
#         rand_nums[j] = R::rnorm(0, sd);
#       }
#       residus(_, i) = rand_nums;
#     }
#
#     return residus;
#   }
# ')
#
#
#
#
# #Résidus de l'arbre
# Rcpp::cppFunction('
#   NumericMatrix Residus_arbre(int k, int horizon, NumericMatrix residusRec, double residual, double gamma, double rho) {
#     int n = residusRec.nrow();
#
#     for (int i = k + 1; i <= horizon; i++) {
#       double sd = sqrt(residual * gamma * pow(rho, i - k - 1));
#       for (int j = 0; j < n; j++) {
#         residusRec(j, i) = R::rnorm(0, sd);
#       }
#     }
#
#     return residusRec;
#   }
# ')
#
#
#
#

