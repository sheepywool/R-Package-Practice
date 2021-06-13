#' Two Sample Proportions Test
#'
#' @param s1 Number of success in sample 1
#' @param n1 Sample size 1
#' @param s2 Number of success in sample 2
#' @param n2 Sample size 2
#'
#' @return p-value for 95 percent confidence
#' @importFrom stats prop.test
#' @export
#'
#' @examples prop_test(0.5, 30, 0.8, 30)
prop_test <- function(s1, n1, s2, n2) {

  success = c(s1, s2)
  sample = c(n1, n2)
  test = prop.test(success, sample)

  return(test$p.value)
}


#' Two Sample Proportions Sample Size Calculator
#'
#' @param p1 Success rate in sample 1
#' @param p2 Success rate in sample 2
#' @param alpha Significance level
#' @param power Power, 1-beta
#' @param ratio Ratio between sample size 1 and 2
#'
#' @return Sample size needed for significance in two samples proportions test
#' @importFrom stats qnorm
#' @export
#'
#' @examples prop_nfind(0.5, 0.8, 0.05, 0.8, 1)
prop_nfind <- function(p1, p2, alpha, power, ratio) {

  n2 = ceiling((p1 * (1 - p1) / ratio +
                  p2 * (1 - p2)) *
                 ((qnorm(1 - alpha / 2)+ qnorm(power)) / (p1 - p2))^2)

  n1 = ceiling(n2 * ratio)

  return(c(n1,n2))
}


#' Binomial Sample
#'
#' A dataset containing two columns of randomly generated binary values
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{x}{has a 30 percent chance of being a 1, success rate is thus 30 percent}
#'   \item{y}{has a 70 percent chance of being a 1, success rate is thus 70 percent}
#' }
"binom_sample"

