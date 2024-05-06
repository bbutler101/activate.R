# Required packages
library(data.table)
library(pdftools)
library(lubridate)
library(rstudioapi)

# Set the working directory to the script's directory
setwd(dirname(getActiveDocumentContext()$path))

# Function to convert a PDF page to a vector of relevant words
page_to_vector <- function(page_text) {
  page_lines <- strsplit(page_text, "\n")[[1]]
  start_index <- grep("RATE", page_lines)
  end_index <- max(grep("0942 1409-6262 GENERAL MARKETING LLC", page_lines))
  
  trimmed_lines <- page_lines[(start_index+1):(end_index-1)]
  trimmed_lines <- trimmed_lines[trimmed_lines != ""]
  
  # Regex for extracting relevant data
  pattern <- "^[A-Z]{1}[a-z]+ ?([A-Z][a-z]+)?[\\-\\,]{1} ?[A-Za-z]+|(\\.\\.\\.)$|^[0-9]+|Hourly|Salary|Overtime|Bonus|Commissions|1099 Indepent|Social Security|Medicare|Fed Income Tax"
  relevant_lines <- grep(pattern, trimmed_lines, value = TRUE)
  
  return(relevant_lines)
}

# Function to convert vectors of words into a data.table
vector_to_df <- function(relevant_words) {
  col_names <- c("Name", "ID", "CheckDate", "Hourly", "Salary", "Overtime", "Bonus", "Commissions", "1099Indepent", "SocialSecurity", "Medicare", "FedIncomeTax")
  transactions <- data.table(matrix(ncol = length(col_names), nrow = 0))
  setnames(transactions, col_names)
  
  if (length(relevant_words) == 0) return(transactions)
  
  row <- data.table(matrix(nrow = 1, ncol = length(col_names), dimnames = list(NULL, col_names)))
  row[, (col_names) := .("0")]  # Initialize row with "0"
  
  # Logic to parse and fill row based on relevant_words processing
  # Replace the placeholder logic with actual code that assigns values based on the extracted data
  transactions <- rbind(transactions, row, fill = TRUE)
  return(transactions)
}

# Function to process PDF and convert to data.table
pdf_to_csv <- function(pdf) {
  pdf_text <- pdftools::pdf_text(pdf)
  transactions <- rbindlist(lapply(pdf_text[104:length(pdf_text)], function(page) vector_to_df(page_to_vector(page))), fill = TRUE)
  
  # Data cleaning and conversions
  transactions[, Name := gsub(" (cont.)", "", Name)]
  transactions[, Name := fifelse(grepl("\\.\\.\\.", Name), shift(Name, type="lead"), Name)]
  transactions[, (4:12) := lapply(.SD, function(x) as.numeric(gsub(",", "", x))), .SDcols = 4:12]
  transactions[, (4:12) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = 4:12]
  transactions[, CheckDate := as.IDate(CheckDate, "%m/%d/%y")]
  
  return(transactions)
}

# Using the function on a PDF file
pdf <- "General 2019.pdf"
transactions_df <- pdf_to_csv(pdf)

# Verify data integrity
print(nrow(transactions_df))
print(length(unique(transactions_df$ID)))
print(sum(transactions_df$`1099Indepent`))
print(format(sum(transactions_df[, .(Hourly, Salary, Overtime, Bonus, Commissions)]), nsmall = 2))

# Write to CSV file
fwrite(transactions_df, "payroll_journal.csv", row.names = FALSE)
