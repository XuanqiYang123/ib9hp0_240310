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
if(length(advertisements_file$ads_id[duplicated(advertisements_file$ads_id)]) == 0) {
  print("No duplicate ads_ids found")
} else {
  print("Duplicate ads_ids found")
}

if(length(advertisements_file$prod_id[duplicated(advertisements_file$prod_id)]) == 0) {
  print("No duplicate prod_ids found")
} else {
  print("Duplicate prod_ids found")
}

## Advertisers file
### Checking duplicate values for advertiser_id, advertiser_email, and advertisers name
if(length(advertisers_file$advertiser_id[duplicated(advertisers_file$advertiser_id)]) == 0) {
  print("No uplicate advertiser_ids found")
} else {
  print("Duplicate advertiser_ids found")
}

if(length(advertisers_file$advertiser_email[duplicated(advertisers_file$advertiser_email)]) == 0) {
  print("No duplicate advertisers' emails found")
} else {
  print("Duplicate advertisers' emails found")
}

if(length(advertisers_file$advertiser_name[duplicated(advertisers_file$advertiser_name)]) == 0) {
  print("No duplicate advertisers' names found")
} else {
  print("Duplicate advertisers' names found")
}

## Checking the advertiser_email format
if(length(grep(("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.com$"),advertisers_file$advertiser_email, value = TRUE)) == length(advertisers_file$advertiser_email)) {
  print("All email format are correct")
} else {
  print(paste("There are:", length(advertisers_file$advertiser_email) - length(grep(("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.com$"),advertisers_file$advertiser_email, value = TRUE)),"wrong emails found"))
}

## Customer_queries file
### Checking duplicate values for query_id
if(length(customer_queries_file$query_id[duplicated(customer_queries_file$query_id)]) == 0) {
  print("No duplicate queries_ids found")
} else {
  print("Duplicate queries_ids found")
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


## Memberships file
### Checking NA values inside membership_id and membership_type
if (any(!is.na(memberships_file))) {
  print("There are no NA values in the dataset")
} else {
  print("Error! There are NA values in the dataset")
}

## Orders file
### Checking NA values inside order_id, cust_id, order_quantity, order_price, prod_id
if (any(!is.na(orders_file[,c("order_id", "cust_id", "order_quantity", "order_price", "prod_id")]))) {
  print("There are no NA values in the dataset")
} else {
  print("Error! There are NA values in the dataset")
}

### Checking date format for the order_date
if (all(!inherits(try(as.Date(orders_file$order_date, format = "%d-%m-%Y")),"try-error"))) {
  print("Dates are already in the correct format")
} else {
  print("Dates are not in the correct format")
}

## Payment_file
### Checking NA values inside payment_id, payment_method, payment_status, and order_id 
if (any(!is.na(payment_file[,c("payment_id", "payment_method", "payment_status", "order_id")]))) {
  print("There are no NA values in the dataset")
} else {
  print("Error! There are NA values in the dataset")
}

### Checking date format for the payment_date
if (all(!inherits(try(as.Date(payment_file$payment_date, format = "%d-%m-%Y")),"try-error"))) {
  print("Dates are already in the correct format")
} else {
  print("Dates are not in the correct format")
}

## Products_file
### Checking duplicate values in prod_id and review_id
if(length(products_file$prod_id[duplicated(products_file$prod_id)]) == 0) {
  print("No duplicate prod_ids found")
} else {
  print("Duplicate ad_ids found")
}

if(length(products_file$review_id[duplicated(products_file$review_id)]) == 0) {
  print("No duplicate review_ids found")
} else {
  print("Duplicate review_ids found")
}

### Checking NA values inside prod_id, review_id, prod_url, prod_unit_price, prod_rating, review_date
if (any(!is.na(products_file[,c("prod_id", "review_id", "prod_url", "prod_unit_price", "prod_rating", "review_date")]))) {
  print("There are no NA values in the dataset")
} else {
  print("Error! There are NA values in the dataset")
}

### Checking date format for the review_date
if (all(!inherits(try(as.Date(products_file$review_date, format = "%d-%m-%Y")),"try-error"))) {
  print("Dates are already in the correct format")
} else {
  print("Dates are not in the correct format")
}

### Checking the URL format of the prod_url
if(length(grep(("^(http|https)://[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}(\\S*)$"),products_file$prod_url, value = TRUE)) == length(products_file$prod_url)) {
  print("All product url format are correct")
} else {
  print(paste("There are:", length(products_file$prod_url) - length(grep(("^(http|https)://[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}(\\S*)$"),products_file$prod_url, value = TRUE)),"wrong product urls found"))
}

## Shipments file 
### Checking duplicate values in shipment_id
if(length(shipments_file$shipment_id[duplicated(shipments_file$shipment_id)]) == 0) {
  print("No duplicate shipment_ids found")
} else {
  print("Duplicate shipment_ids found")
}

### Checking NA values inside shipment_id, prod_id, order_id
if (any(!is.na(shipments_file[,c("prod_id", "order_id", "shipment_id")]))) {
  print("There are no NA values in the dataset")
} else {
  print("Error! There are NA values in the dataset")
}

### Checking date format for the delivery_departed_date, delivery_received_date, est_delivery_date
if (all(!inherits(try(as.Date(shipments_file$delivery_departed_date, format = "%d-%m-%Y")),"try-error"))) {
  print("Dates are already in the correct format")
} else {
  print("Dates are not in the correct format")
}

if (all(!inherits(try(as.Date(shipments_file$delivery_received_date, format = "%d-%m-%Y")),"try-error"))) {
  print("Dates are already in the correct format")
} else {
  print("Dates are not in the correct format")
}

if (all(!inherits(try(as.Date(shipments_file$est_delivery_date, format = "%d-%m-%Y")),"try-error"))) {
  print("Dates are already in the correct format")
} else {
  print("Dates are not in the correct format")
}

### Checking whether the recipient names contains ' and ,
if (any(grepl("[',]",shipments_file$delivery_recipient))) {
  print("Error! Some names contain invalid characters")
} else {
  print("All names are valid")
}







