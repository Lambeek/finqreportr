#' Acquire statement of cash flow.
#'
#' Extracts and displays statement of cash flow from the quarterly report of a given company.
#'
#' @export
#' @param symbol A character vector specifying the stock symbol of the company of interest
#' @param year A numeric vector specifying the year during which the report was filed
#' @param quarter A character vector specifying the quarter during which the report was filed
#' @return A data frame representing a company's quarterly cash flow statement from a given filing year
#' @examples
#' \dontrun{
#' GetCashFlow("FB", 2016, "Q1")
#' }

GetCashFlow <- function(symbol, year, quarter) {

     cash.flow.descriptions <- c("CONSOLIDATED STATEMENT OF CASH FLOWS",
                                 "CONSOLIDATED STATEMENT OF CASH FLOWS (UNAUDITED)",
                                 "CONSOLIDATED STATEMENTS OF CASH FLOWS",
                                 "CONSOLIDATED STATEMENTS OF CASH FLOWS (UNAUDITED)",
                                 "CASH FLOWS STATEMENTS",
                                 "CASH FLOWS STATEMENTS (UNAUDITED)",
                                 "CONSOLIDATED STATEMENT OF CASH FLOW",
                                 "CONSOLIDATED STATEMENT OF CASH FLOW (UNAUDITED)",
                                 "CONDENSED CONSOLIDATED STATEMENT OF CASH FLOWS",
                                 "CONDENSED CONSOLIDATED STATEMENT OF CASH FLOWS (UNAUDITED)",
                                 "CONDENSED CONSOLIDATED STATEMENTS OF CASH FLOWS",
                                 "CONDENSED CONSOLIDATED STATEMENTS OF CASH FLOWS (UNAUDITED)")

     GetFinancial(cash.flow.descriptions, symbol, year, quarter)

}
