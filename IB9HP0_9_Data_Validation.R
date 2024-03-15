# Load Packages
library(dplyr)
library(ggplot2)
library(tidyverse)

# Read Datafile
advertisements_file <- read_csv("data_uploads/R_synth_advertisements.csv") 
advertisers_file <- read_csv("data_uploads/R_synth_advertisers.csv") 
categories_file <- read_csv("data_uploads/R_synth_categories.csv") 
customer_queries_file <- read_csv("data_uploads/R_synth_customer_queries.csv") 
customers_file <- read_csv("data_uploads/R_synth_customers.csv") 
memberships_file <- read_csv("data_uploads/R_synth_memberships.csv") 
orders_file <- read_csv("data_uploads/R_synth_orders.csv") 
payment_file <- read_csv("data_uploads/R_synth_payment.csv") 
products_file <- read_csv("data_uploads/R_synth_products.csv") 
shipments_file <- read_csv("data_uploads/R_synth_shipment.csv") 
suppliers_file <- read_csv("data_uploads/R_synth_suppliers.csv") 
supplies_file <- read_csv("data_uploads/R_synth_supply.csv") 

# Data Validation

## Advertisement file
### Checking the date format for ads_start_date and ads_end_date
if (all(!inherits(try(as.Date(advertisements_file$ads_start_date, format = "%d-%m-%Y")),"try-error"))) {
  print("Dates are already in the correct format")
} else {
  print("Dates are not in the correct format")
}

if (all(!inherits(try(as.Date(advertisements_file$ads_end_date, format = "%d-%m-%Y")),"try-error"))) {
  print("Dates are already in the correct format")
} else {
  print("Dates are not in the correct format")
}

## Ensuring advertisement end date is after the advertisement start date
for (i in 1:length(as.Date(advertisements_file$ads_start_date, format = "%d-%m-%Y"))) {
  if (as.Date(advertisements_file$ads_end_date, format = "%d-%m-%Y")[i] > as.Date(advertisements_file$ads_start_date, format = "%d-%m-%Y")[i]) {
    print("Ends date happened after the starts date")
  } else {
    print(paste("Error!","Query",i,": ends date happened before the starts date"))
  }
}

### Checking duplicate values for ads_id and prod_id
if(length(advertisements_file$ads_id[duplicated(advertisements_file$ads_id)]) > 0) {
  print("Duplicate ads_ids found")
} else {
  print("No duplicate ads_ids found")
}

if(length(advertisements_file$prod_id[duplicated(advertisements_file$prod_id)]) > 0) {
  print("Duplicate prod_ids found")
} else {
  print("No duplicate prod_ids found")
}

## Advertisers file
### Checking duplicate values for advertiser_id, advertiser_email, and advertisers name
if(length(advertisers_file$advertiser_id[duplicated(advertisers_file$advertiser_id)]) > 0) {
  print("Duplicate advertiser_ids found")
} else {
  print("No duplicate advertiser_ids found")
}

if(length(advertisers_file$advertiser_email[duplicated(advertisers_file$advertiser_email)]) > 0) {
  print("Duplicate advertisers' emails found")
} else {
  print("No duplicate advertisers' emails found")
}

if(length(advertisers_file$advertiser_name[duplicated(advertisers_file$advertiser_name)]) > 0) {
  print("Duplicate advertisers' names found")
} else {
  print("No duplicate advertisers' names found")
}

## Checking the advertiser_email format
if(length(grep(("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.com$"),advertisers_file$advertiser_email, value = TRUE)) == length(advertisers_file$advertiser_email)) {
  print("All email format are correct")
} else {
  print(paste("There are:", length(advertisers_file$advertiser_email) - length(grep(("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.com$"),advertisers_file$advertiser_email, value = TRUE)),"wrong emails found"))
}

## Customer_queries file
### Checking duplicate values for query_id
if(length(customer_queries_file$query_id[duplicated(customer_queries_file$query_id)]) > 0) {
  print("Duplicate queries_ids found")
} else {
  print("No duplicate queries_ids found")
}

### Checking the date format for query_submission_date and query_closure_date
if (all(!inherits(try(as.Date(customer_queries_file$query_submission_date, format = "%d-%m-%Y")),"try-error"))) {
  print("Dates are already in the correct format")
} else {
  print("Dates are not in the correct format")
}

if (all(!inherits(try(as.Date(customer_queries_file$query_closure_date, format = "%d-%m-%Y")),"try-error"))) {
  print("Dates are already in the correct format")
} else {
  print("Dates are not in the correct format")
}

## Ensuring queries closure date is after the submission date
for (i in 1:length(as.Date(customer_queries_file$query_submission_date, format = "%d-%m-%Y"))) {
  if (as.Date(customer_queries_file$query_closure_date, format = "%d-%m-%Y")[i] > as.Date(customer_queries_file$query_submission_date, format = "%d-%m-%Y")[i]) {
    print("Closure date happened after the submission date")
  } else {
    print(paste("Error!","Query",i,": closure date happened before the submission date"))
  }
}





