## ---- include=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(width = 300)

## ----setup, message=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(finqreportr)

## ----example_1--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(GetIncome("TSLA", 2018, "Q1"))

## ----example_2--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(GetBalanceSheet("AMZN", 2017, "Q2"))

## ----example_3--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(GetCashFlow("NFLX", 2016, "Q3"))

## ----case_study-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
symbol_list <- c("AMZN", "APRN", "PRTS", "CVNA", "LIVE", "QRTEA", "QVCC",
                "EBAY", "ETSY", "EXPE", "GRUB", "RVLV", "OSTK",
                "CNXN", "SAML", "PETS", "SYX", "STMP","TRIP","W")

## ----statement_lists, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
BS_list <- lapply(symbol_list, GetBalanceSheet, 2020, "Q2")
IS_list <- lapply(symbol_list, GetIncome, 2020, "Q2")

## ----ratio_calculation------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(scales)

GetCurrentRatio <- function(x) {
  currentRatio <- 
    x[x$Metric %in% c("Assets, Current", 
                       "Assets Current", 
                       "us-gaap_AssetsCurrent") & 
        grepl("2020-06", x$endDate, fixed = TRUE), ]$Amount /
    x[x$Metric %in% c("Liabilities, Current", 
                       "Liabilities Current", 
                       "us-gaap_LiabilitiesCurrent") &
        grepl("2020-06", x$endDate, fixed = TRUE), ]$Amount
  return(currentRatio)
}

GetDERatio <- function(x) {
  deRatio <- 
    (x[x$Metric %in% c("Liabilities and Equity", 
                       "Liabilities And Stockholders Equity", 
                       "us-gaap_LiabilitiesAndStockholdersEquity") & 
        grepl("2020-06", x$endDate, fixed = TRUE), ]$Amount - 
       x[x$Metric %in% c("Stockholders' Equity Attributable to Parent", 
                         "Stockholders Equity",
                         "Stockholders' deficiency",
                         "us-gaap_StockholdersEquity") & 
        grepl("2020-06", x$endDate, fixed = TRUE), ]$Amount)/
    x[x$Metric %in% c("Stockholders' Equity Attributable to Parent", 
                      "Stockholders Equity", 
                      "Stockholders' deficiency",
                      "us-gaap_StockholdersEquity") & 
        grepl("2020-06", x$endDate, fixed = TRUE), ]$Amount
  return(deRatio)
}

GetNetProfitMargin <- function(x) {
  npMargin <- 
    x[x$Metric %in% c("Net Income (Loss) Attributable to Parent", 
                      "Net Income Loss", 
                      "Net income",
                      "Comprehensive Income (Loss), Net of Tax, Attributable to Parent",
                      "Net Income Loss Available To Common Stockholders Basic") & 
        grepl("2020-06", x$endDate, fixed = TRUE) & 
        (grepl("2020-04", x$startDate, fixed = TRUE) |
           grepl("2020-03", x$startDate, fixed = TRUE)), ]$Amount /
    x[x$Metric %in% c("Revenues",
                      "Sales",
                      "Revenue, Net",
                      "Revenues from sales of products",
                      "Revenue From Contract With Customer Excluding Assessed Tax", 
                      "Revenue from Contract with Customer, Excluding Assessed Tax") &
        grepl("2020-06", x$endDate, fixed = TRUE) & 
        (grepl("2020-04", x$startDate, fixed = TRUE) |
           grepl("2020-03", x$startDate, fixed = TRUE)), ]$Amount
  return(npMargin)
}

currentRatio <- sapply(BS_list, GetCurrentRatio)
deRatio <- sapply(BS_list, GetDERatio)
npMargin <- unlist(sapply(IS_list, GetNetProfitMargin)) * 100

ratio_df <- data.frame(currentRatio, deRatio, npMargin)
names(ratio_df) <- c("Current Ratio", "D/E Ratio", "Net Profit Margin")
rownames(ratio_df) <- symbol_list

## ----desc_statistics, echo=FALSE, results = "asis"--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(qwraps2)
options(qwraps2_markup = "markdown")
stat_summary <-
  list("Current Ratio" =
       list("min"       = ~ round(min(`Current Ratio`), digits = 2),
            "median"    = ~ round(median(`Current Ratio`), digits = 2),
            "max"       = ~ round(max(`Current Ratio`), digits = 2),
            "mean ± sd" = ~ qwraps2::mean_sd(`Current Ratio`)),
       "D/E Ratio" =
       list("min"       = ~ round(min(`D/E Ratio`), digits = 2),
            "median"    = ~ round(median(`D/E Ratio`), digits = 2),
            "max"       = ~ round(max(`D/E Ratio`), digits = 2),
            "mean ± sd" = ~ qwraps2::mean_sd(`D/E Ratio`)),
       "Net Profit Margin (%)" =
       list("min"       = ~ round(min(`Net Profit Margin`), digits = 2),
            "median"    = ~ round(median(`Net Profit Margin`), digits = 2),
            "max"       = ~ round(max(`Net Profit Margin`), digits = 2),
            "mean ± sd" = ~ qwraps2::mean_sd(`Net Profit Margin`))
       )

stat_table <- summary_table(ratio_df, stat_summary)
stat_table

## ----bar_plots, echo=FALSE, fig.height = 4, fig.width = 7-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)

ggplot(ratio_df, aes(x=row.names(ratio_df), y=`Current Ratio`)) + 
  geom_bar(stat = "identity", fill="steelblue") + 
  geom_hline(yintercept = 1, color = "red") + 
  labs(x = "Ticker Symbol") + 
  geom_text(aes(label=round(`Current Ratio`, digits = 2)), vjust=-0.3, size=3.5) + 
  scale_x_discrete(guide = guide_axis(n.dodge=4))

ggplot(ratio_df, aes(x=row.names(ratio_df), y=`D/E Ratio`)) + 
  geom_bar(stat = "identity", fill="steelblue") + 
  geom_hline(yintercept = 2, color = "red") +
  labs(x = "Ticker Symbol") + 
  geom_text(aes(label=round(`D/E Ratio`, digits = 2)), vjust=-0.3, size=3.5) +
  scale_x_discrete(guide = guide_axis(n.dodge=4))

ggplot(ratio_df, aes(x=row.names(ratio_df), y=`Net Profit Margin`)) + 
  geom_bar(stat = "identity", fill="steelblue") + 
  labs(x = "Ticker Symbol") + 
  geom_text(aes(label=round(`Net Profit Margin`, digits = 2)), vjust=-0.3, size=3.5) +
  scale_x_discrete(guide = guide_axis(n.dodge=4))

