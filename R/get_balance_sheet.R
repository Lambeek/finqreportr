#' Acquire balance sheet.
#'
#' Extracts and displays balance sheet from the quarterly report of a given company.
#'
#' @export
#' @param symbol A character vector specifying the stock symbol of the company of interest
#' @param year A numeric vector specifying the year during which the report was filed
#' @param quarter A character vector specifying the quarter during which the report was filed
#' @return A data frame representing a company's quarterly balance sheet from a given filing year
#' @examples
#' \dontrun{
#' GetBalanceSheet("FB", 2016, "Q1")
#' }

GetBalanceSheet <- function(symbol, year, quarter) {

     balance.sheet.descriptions <- c("CONSOLIDATED BALANCE SHEET",
                                     "CONSOLIDATED BALANCE SHEET (UNAUDITED)",
                                     "CONSOLIDATED BALANCE SHEETS",
                                     "CONSOLIDATED BALANCE SHEETS (UNAUDITED)",
                                     "CONDENSED BALANCE SHEETS",
                                     "CONDENSED BALANCE SHEETS (UNAUDITED)",
                                     "CONSOLIDATED STATEMENT OF FINANCIAL POSITION",
                                     "CONSOLIDATED STATEMENT OF FINANCIAL POSITION (UNAUDITED)",
                                     "CONSOLIDATED STATEMENTS OF FINANCIAL POSITION",
                                     "CONSOLIDATED STATEMENTS OF FINANCIAL POSITION (UNAUDITED)",
                                     "BALANCE SHEETS",
                                     "BALANCE SHEETS (UNAUDITED)",
                                     "CONSOLIDATED FINANCIAL POSITION",
                                     "CONSOLIDATED FINANCIAL POSITION (UNAUDITED)",
                                     "CONDENSED CONSOLIDATED BALANCE SHEET",
                                     "CONDENSED CONSOLIDATED BALANCE SHEET (UNAUDITED)",
                                     "CONDENSED CONSOLIDATED BALANCE SHEETS",
                                     "CONDENSED CONSOLIDATED BALANCE SHEETS (UNAUDITED)",
                                     "CONSOLIDATED AND CONDENSED BALANCE SHEETS",
                                     "CONSOLIDATED AND CONDENSED BALANCE SHEETS (UNAUDITED)")

     GetFinancial(balance.sheet.descriptions, symbol, year, quarter)
}
