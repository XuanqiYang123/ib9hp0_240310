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

# Normalising the Table into 3NF

##Normalising Products Table
products_table <- products_file %>%
  select(prod_id,prod_name,prod_desc,prod_unit_price,voucher,prod_url)

##Normalising Reviews Table
reviews_table <- products_file %>%
  select(review_id,prod_id, prod_rating, review_date)

##Normalising Memberships Table
memberships_table <- memberships_file %>%
  select(membership_type_id,membership_type)
memberships_table <- memberships_table[!duplicated(memberships_table$membership_type_id),]

##Normalising Customers Table
customers_table <- customers_file %>%
  select(cust_id, first_name, last_name, cust_email,password, cust_birth_date, block_num, postcode, address_type,cust_telephone)
customers_table <- merge(customers_table,memberships_file, by = "cust_id")
customers_table$...1 <- NULL
customers_table$membership_type <- NULL

##Normalising Orders Table
orders_table <- orders_file %>%
  select(order_id, cust_id)
orders_table <- orders_table[!duplicated(orders_table$order_id),]

##Normalising Order details Table
order_details_table <- orders_file %>%
  select(order_id,prod_id, order_quantity, order_date, order_value, order_price) 

##Normalising Payments Table
payments_table <- payment_file %>%
  select(payment_id,order_id,payment_amount,payment_method,payment_status,payment_date)

##Normalising Shipments Table
shipments_table <- shipments_file %>%
  select(shipment_id, order_id, prod_id, delivery_departed_date, delivery_received_date,est_delivery_date,shipper_name, delivery_recipient, delivery_fee,delivery_status)


##Normalising Suppliers Table
suppliers_table <- suppliers_file %>%
  select(supplier_id, supplier_name, supplier_contact, supplier_postcode)

##Normalising Supplies Table
supplies_table <- supplies_file %>%
  select(supply_id,supplier_id,prod_id,inventory_quantity,sold_quantity)

##Normalising Customer Queries Table
customer_queries_table <- customer_queries_file

##Normalising Categories Table
categories_table <- categories_file %>%
  select(category_id,category_name)
categories_table <- categories_table[!duplicated(categories_table$category_id),]

##Normalising Product Categories Table
product_categories_table <- categories_file %>%
  select(prod_id, category_id)

##Normalising Advertiser Table
advertisers_table <- advertisers_file

##Normalising Advertisement Table
advertisements_table <- advertisements_file

# Data Validation

## Advertisement table
### Checking the date format for ads_start_date and ads_end_date
if (all(!inherits(try(as.Date(advertisements_table$ads_start_date, format = "%d-%m-%Y")),"try-error"))) {
  print("Dates are already in the correct format")
} else {
  print("Dates are not in the correct format")
}

if (all(!inherits(try(as.Date(advertisements_table$ads_end_date, format = "%d-%m-%Y")),"try-error"))) {
  print("Dates are already in the correct format")
} else {
  print("Dates are not in the correct format")
}

## Ensuring advertisement end date is after the advertisement start date
for (i in 1:length(as.Date(advertisements_table$ads_start_date, format = "%d-%m-%Y"))) {
  if (as.Date(advertisements_table$ads_end_date, format = "%d-%m-%Y")[i] > as.Date(advertisements_table$ads_start_date, format = "%d-%m-%Y")[i]) {
    print("Ends date happened after the starts date")
  } else {
    print(paste("Error!","Query",i,": ends date happened before the starts date"))
  }
}

### Checking duplicate values for ads_id and prod_id
if(length(advertisements_table$ads_id[duplicated(advertisements_table$ads_id)]) > 0) {
  print("Duplicate ads_ids found")
} else {
  print("No duplicate ads_ids found")
}

if(length(advertisements_table$prod_id[duplicated(advertisements_table$prod_id)]) > 0) {
  print("Duplicate prod_ids found")
} else {
  print("No duplicate prod_ids found")
}

## Advertisers file
### Checking duplicate values for advertiser_id, advertiser_email, and advertisers name

if(length(advertisers_table$advertiser_id[duplicated(advertisers_table$advertiser_id)]) > 0) {
  print("Duplicate advertiser_ids found")
} else {
  print("No duplicate advertiser_ids found")
}

if(length(advertisers_table$advertiser_email[duplicated(advertisers_table$advertiser_email)]) > 0) {
  print("Duplicate advertisers' emails found")
} else {
  print("No duplicate advertisers' emails found")
}

if(length(advertisers_table$advertiser_name[duplicated(advertisers_table$advertiser_name)]) > 0) {
  print("Duplicate advertisers' names found")
} else {
  print("No duplicate advertisers' names found")
}

## Checking the advertiser_email format
if(length(grep(("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.com$"),advertisers_table$advertiser_email, value = TRUE)) == length(advertisers_table$advertiser_email)) {
  print("All email format are correct")
} else {
  print(paste("There are:", length(advertisers_table$advertiser_email) - length(grep(("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.com$"),advertisers_table$advertiser_email, value = TRUE)),"wrong emails found"))
}

## Customer_queries file
### Checking duplicate values for query_id

if(length(customer_queries_table$query_id[duplicated(customer_queries_table$query_id)]) > 0) {
  print("Duplicate queries_ids found")
} else {
  print("No duplicate queries_ids found")
}

### Checking the date format for query_submission_date and query_closure_date
if (all(!inherits(try(as.Date(customer_queries_table$query_submission_date, format = "%d-%m-%Y")),"try-error"))) {
  print("Dates are already in the correct format")
} else {
  print("Dates are not in the correct format")
}

if (all(!inherits(try(as.Date(customer_queries_table$query_closure_date, format = "%d-%m-%Y")),"try-error"))) {
  print("Dates are already in the correct format")
} else {
  print("Dates are not in the correct format")
}

## Ensuring queries closure date is after the submission date
for (i in 1:length(as.Date(customer_queries_table$query_submission_date, format = "%d-%m-%Y"))) {
  if (as.Date(customer_queries_table$query_closure_date, format = "%d-%m-%Y")[i] > as.Date(customer_queries_table$query_submission_date, format = "%d-%m-%Y")[i]) {
    print("Closure date happened after the submission date")
  } else {
    print(paste("Error!","Query",i,": closure date happened before the submission date"))
  }
}


## Memberships file
### Checking NA values inside membership_id and membership_type
if (any(!is.na(memberships_table))) {
  print("There are no NA values in the dataset")
} else {
  print("Error! There are NA values in the dataset")
}

## Orders file
### Checking NA values inside order_id, cust_id, order_quantity, order_price, prod_id
if (any(!is.na(order_details_table[,c("order_id", "order_quantity", "order_price", "prod_id")]))) {
  print("There are no NA values in the dataset")
} else {
  print("Error! There are NA values in the dataset")
}

### Checking date format for the order_date
if (all(!inherits(try(as.Date(order_details_table$order_date, format = "%d-%m-%Y")),"try-error"))) {
  print("Dates are already in the correct format")
} else {
  print("Dates are not in the correct format")
}

## Payment_file
### Checking NA values inside payment_id, payment_method, payment_status, and order_id 
if (any(!is.na(payments_table[,c("payment_id", "payment_method", "payment_status", "order_id")]))) {
  print("There are no NA values in the dataset")
} else {
  print("Error! There are NA values in the dataset")
}

### Checking date format for the payment_date
if (all(!inherits(try(as.Date(payments_table$payment_date, format = "%d-%m-%Y")),"try-error"))) {
  print("Dates are already in the correct format")
} else {
  print("Dates are not in the correct format")
}

## Products_file
### Checking duplicate values in prod_id and review_id
if(length(products_table$prod_id[duplicated(products_table$prod_id)]) == 0) {
  print("No duplicate prod_ids found")
} else {
  print("Duplicate ad_ids found")
}

if(length(reviews_table$review_id[duplicated(reviews_table$review_id)]) == 0) {
  print("No duplicate review_ids found")
} else {
  print("Duplicate review_ids found")
}

### Checking NA values inside prod_id, prod_url, prod_unit_price
if (any(!is.na(products_table[,c("prod_id", "prod_url", "prod_unit_price")]))) {
  print("There are no NA values in the dataset")
} else {
  print("Error! There are NA values in the dataset")
}

### Checking date format for the review_date
if (all(!inherits(try(as.Date(reviews_table$review_date, format = "%d-%m-%Y")),"try-error"))) {
  print("Dates are already in the correct format")
} else {
  print("Dates are not in the correct format")
}

### Checking the URL format of the prod_url
if(length(grep(("^(http|https)://[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}(\\S*)$"),products_table$prod_url, value = TRUE)) == length(products_table$prod_url)) {
  print("All product url format are correct")
} else {
  print(paste("There are:", length(products_table$prod_url) - length(grep(("^(http|https)://[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}(\\S*)$"),products_table$prod_url, value = TRUE)),"wrong product urls found"))
}

## Shipments file 
### Checking duplicate values in shipment_id
if(length(shipments_table$shipment_id[duplicated(shipments_table$shipment_id)]) == 0) {
  print("No duplicate shipment_ids found")
} else {
  print("Duplicate shipment_ids found")
}

### Checking NA values inside shipment_id, prod_id, order_id
if (any(!is.na(shipments_table[,c("prod_id", "order_id", "shipment_id")]))) {
  print("There are no NA values in the dataset")
} else {
  print("Error! There are NA values in the dataset")
}

### Checking date format for the delivery_departed_date, delivery_received_date, est_delivery_date
if (all(!inherits(try(as.Date(shipments_table$delivery_departed_date, format = "%d-%m-%Y")),"try-error"))) {
  print("Dates are already in the correct format")
} else {
  print("Dates are not in the correct format")
}

if (all(!inherits(try(as.Date(shipments_table$delivery_received_date, format = "%d-%m-%Y")),"try-error"))) {
  print("Dates are already in the correct format")
} else {
  print("Dates are not in the correct format")
}

if (all(!inherits(try(as.Date(shipments_table$est_delivery_date, format = "%d-%m-%Y")),"try-error"))) {
  print("Dates are already in the correct format")
} else {
  print("Dates are not in the correct format")
}

### Checking whether the recipient names contains ' and ,
if (any(grepl("[',]",shipments_table$delivery_recipient))) {
  print("Error! Some names contain invalid characters")
} else {
  print("All names are valid")
}

## Customer Table
### Checking duplicate values in customer_id
if(length(customers_table$cust_id[duplicated(customers_table$cust_id)]) == 0) {
  print("No duplicate customer_ids found")
} else {
  print("Duplicate customer_ids found")
}

### Checking whether the customer's first name and last name contains ' and ,
if (any(grepl("[',]",customers_table$first_name))) {
  print("Error! Some names contain invalid characters")
} else {
  print("All names are valid")
}

if (any(grepl("[',]",customers_table$last_name))) {
  print("Error! Some names contain invalid characters")
} else {
  print("All names are valid")
}

### Checking the customer_email format
if(length(grep(("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.com$"),customers_table$cust_email, value = TRUE)) == length(customers_table$cust_email)) {
  print("All customer email format are correct")
} else {
  print(paste("There are:", length(customers_table$cust_email) - length(grep(("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.com$"),customers_table$cust_email, value = TRUE)),"wrong emails found"))
}

### Checking the customer birth_date format
if (all(!inherits(try(as.Date(customers_table$cust_birth_date, format = "%d-%m-%Y")),"try-error"))) {
  print("Dates are already in the correct format")
} else {
  print("Dates are not in the correct format")
}




