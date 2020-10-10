#' Acquire income statement.
#'
#' Extracts and displays income statement from the quarterly report of a given company.
#'
#' @export
#' @param symbol A character vector specifying the stock symbol of the company of interest
#' @param year A numeric vector specifying the year during which the report was filed
#' @param quarter A character vector specifying the quarter during which the report was filed
#' @return A data frame representing a company's quarterly income statement from a given filing year
#' @examples
#' \dontrun{
#' GetIncome("FB", 2016, "Q1")
#' }

GetIncome <- function(symbol, year, quarter) {

     income.descriptions <- c("CONSOLIDATED STATEMENTS OF INCOME (UNAUDITED)",
                              "CONSOLIDATED STATEMENTS OF INCOME",
                              "CONSOLIDATED STATEMENT OF INCOME (UNAUDITED)",
                              "CONSOLIDATED STATEMENT OF INCOME",
                              "CONSOLIDATED STATEMENTS OF OPERATIONS (UNAUDITED)",
                              "CONSOLIDATED STATEMENTS OF OPERATIONS",
                              "CONSOLIDATED STATEMENT OF OPERATIONS (UNAUDITED)",
                              "CONSOLIDATED STATEMENT OF OPERATIONS",
                              "CONSOLIDATED STATEMENTS OF OPERATIONS AND COMPREHENSIVE OPERATIONS (UNAUDITED)",
                              "CONSOLIDATED STATEMENTS OF OPERATIONS AND COMPREHENSIVE OPERATIONS",
                              "CONSOLIDATED STATEMENT OF EARNINGS (UNAUDITED)",
                              "CONSOLIDATED STATEMENT OF EARNINGS",
                              "CONSOLIDATED STATEMENTS OF EARNINGS (UNAUDITED)",
                              "CONSOLIDATED STATEMENTS OF EARNINGS",
                              "INCOME STATEMENTS (UNAUDITED)",
                              "INCOME STATEMENTS",
                              "CONSOLIDATED RESULTS OF OPERATIONS (UNAUDITED)",
                              "CONSOLIDATED RESULTS OF OPERATIONS",
                              "CONDENSED CONSOLIDATED STATEMENT OF INCOME (UNAUDITED)",
                              "CONDENSED CONSOLIDATED STATEMENT OF INCOME",
                              "CONDENSED CONSOLIDATED STATEMENTS OF INCOME (UNAUDITED)",
                              "CONDENSED CONSOLIDATED STATEMENTS OF INCOME",
                              "CONDENSED CONSOLIDATED STATEMENTS OF OPERATIONS (UNAUDITED)",
                              "CONDENSED CONSOLIDATED STATEMENTS OF OPERATIONS",
                              "CONDENSED CONSOLIDATED STATEMENT OF EARNINGS (UNAUDITED)",
                              "CONDENSED CONSOLIDATED STATEMENT OF EARNINGS",
                              "CONDENSED CONSOLIDATED STATEMENTS OF EARNINGS (UNAUDITED)",
                              "CONDENSED CONSOLIDATED STATEMENTS OF EARNINGS",
                              "CONDENSED CONSOLIDATED RESULTS OF OPERATIONS (UNAUDITED)",
                              "CONDENSED CONSOLIDATED RESULTS OF OPERATIONS",
                              "CONDENSED CONSOLIDATED STATEMENTS OF OPERATIONS AND COMPREHENSIVE INCOME (LOSS) (UNAUDITED)",
                              "CONDENSED CONSOLIDATED STATEMENTS OF OPERATIONS AND COMPREHENSIVE INCOME (LOSS)",
                              "CONDENSED STATEMENTS OF OPERATIONS AND COMPREHENSIVE LOSS (UNAUDITED)",
                              "CONDENSED STATEMENTS OF OPERATIONS AND COMPREHENSIVE LOSS",
                              "CONSOLIDATED AND CONDENSED STATEMENTS OF OPERATIONS (UNAUDITED)",
                              "CONSOLIDATED AND CONDENSED STATEMENTS OF OPERATIONS")

     # income.descriptions <- c("STATEMENTS OF INCOME",
     #                          "STATEMENT OF INCOME",
     #                          "STATEMENTS OF OPERATIONS",
     #                          "STATEMENT OF OPERATIONS",
     #                          "STATEMENT OF EARNINGS",
     #                          "STATEMENTS OF EARNINGS",
     #                          "INCOME STATEMENTS",
     #                          "RESULTS OF OPERATIONS")


     GetFinancial(income.descriptions, symbol, year, quarter)
}
