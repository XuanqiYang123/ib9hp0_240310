## Load Packages
library(dplyr)
library(ggplot2)
library(tidyverse)
library(RSQLite)

# Read Datafile
advertisements_file <- read_csv("/Users/dila/ib9hp0_group_9/data_uploads/R_synth_advertisements.csv") 
advertisers_file <- read_csv("/Users/dila/ib9hp0_group_9/data_uploads/R_synth_advertisers.csv") 
categories_file <- read_csv("/Users/dila/ib9hp0_group_9/data_uploads/R_synth_categories.csv") 
customer_queries_file <- read_csv("/Users/dila/ib9hp0_group_9/data_uploads/R_synth_customer_queries.csv") 
customers_file <- read_csv("/Users/dila/ib9hp0_group_9/data_uploads/R_synth_customers.csv") 
memberships_file <- read_csv("/Users/dila/ib9hp0_group_9/data_uploads/R_synth_memberships.csv") 
orders_file <- read_csv("/Users/dila/ib9hp0_group_9/data_uploads/R_synth_orders.csv") 
payment_file <- read_csv("/Users/dila/ib9hp0_group_9/data_uploads/R_synth_payment.csv") 
products_file <- read_csv("/Users/dila/ib9hp0_group_9/data_uploads/R_synth_products.csv") 
shipments_file <- read_csv("/Users/dila/ib9hp0_group_9/data_uploads/R_synth_shipment.csv") 
suppliers_file <- read_csv("/Users/dila/ib9hp0_group_9/data_uploads/R_synth_suppliers.csv") 
supplies_file <- read_csv("/Users/dila/ib9hp0_group_9/data_uploads/R_synth_supply.csv") 

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
  select(cust_id, first_name, cust_email,password, cust_birth_date, block_num, postcode, address_type,cust_telephone)
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

# Create connection to SQL database
db_connection <- RSQLite::dbConnect(RSQLite::SQLite(),"IB9HP0_9.db")

# Inserting Dataframe into the sql database

## Inserting Products table
dbWriteTable(db_connection,"products",products_table, append = TRUE)

## Inserting Reviews table
dbWriteTable(db_connection,"reviews",reviews_table, append = TRUE)

## Inserting Memberships table
dbWriteTable(db_connection,"memberships",memberships_table, append = TRUE)

## Inserting Customers table
dbWriteTable(db_connection,"customers",customers_table, append = TRUE)

## Inserting Orders table
dbWriteTable(db_connection,"orders",orders_table, append = TRUE)

## Inserting Payment table
dbWriteTable(db_connection,"payments",payments_table, append = TRUE)

## Inserting Shipment table
dbWriteTable(db_connection,"shipments",shipments_table, append = TRUE)

## Inserting Order details table
dbWriteTable(db_connection,"order_details",order_details_table, append = TRUE)

## Inserting Suppliers table
dbWriteTable(db_connection,"suppliers",suppliers_table, append = TRUE)

## Inserting Supplies table
dbWriteTable(db_connection,"supplies",supplies_table, append = TRUE)

## Inserting Customer queries table
dbWriteTable(db_connection,"customer_queries",customer_queries_table, append = TRUE)

## Inserting Categories table
dbWriteTable(db_connection,"categories",categories_table, append = TRUE)

## Inserting Product Categories table
dbWriteTable(db_connection,"product_categories",product_categories_table, append = TRUE)

## Inserting Advertisers table
dbWriteTable(db_connection,"advertisers",advertisers_table, append = TRUE)

## Inserting Advertisements table
dbWriteTable(db_connection,"advertisements",advertisements_table, append = TRUE)
