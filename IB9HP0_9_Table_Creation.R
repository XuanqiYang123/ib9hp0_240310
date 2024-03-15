install.packages("tidyverse")
## Load Packages
library(dplyr)
library(ggplot2)
library(tidyverse)
library(RSQLite)

db_file <- "IB9HP0_9.db"

# Check if the database file exists and remove it
if (file.exists(db_file)) {
  file.remove(db_file)
}

# Create connection to SQL database
db_connection <- RSQLite::dbConnect(RSQLite::SQLite(),"IB9HP0_9.db")

# Create table for products
dbExecute(db_connection, 
          "CREATE TABLE IF NOT EXISTS products (
              prod_id VARCHAR (50) PRIMARY KEY,
              prod_name VARCHAR (50) NOT NULL,
              prod_desc VARCHAR (100) NOT NULL,
              voucher VARCHAR (50),
              prod_url VARCHAR (250) NOT NULL,
              prod_unit_price DECIMAL NOT NULL
              )"
          )

#Create table for reviews
dbExecute(db_connection, 
          "CREATE TABLE IF NOT EXISTS reviews (
              review_id VARCHAR (50) PRIMARY KEY,
              prod_rating DECIMAL NOT NULL,
              review_date DATE NOT NULL,
              prod_id VARCHAR (50),
              FOREIGN KEY (prod_id)
              REFERENCES products(prod_id)
              )"
          )

#Create table for memberships
dbExecute(db_connection, 
          "CREATE TABLE IF NOT EXISTS memberships (
              membership_type_id VARCHAR (50) PRIMARY KEY,
<<<<<<< HEAD
<<<<<<< HEAD
              membership_desc VARCHAR (50) NOT NULL
=======
              membership_type VARCHAR (50) NOT NULL
>>>>>>> fc5b28f05fee16ead44168da75fdf3b9b12c9075
              )")
=======
              membership_type VARCHAR (50) NOT NULL
              )"
          )
>>>>>>> 797c30b906ada5962583a85d1a018c32ab61d099

#Create table for customers
dbExecute(db_connection, 
          "CREATE TABLE IF NOT EXISTS customers (
              cust_id VARCHAR (50) PRIMARY KEY,
              first_name VARCHAR (50) NOT NULL,
              last_name VARCHAR (50),
              cust_email VARCHAR (50) UNIQUE,
              password VARCHAR (50) NOT NULL,
              cust_birth_date DATE,
              address_type VARCHAR (50),
              block_num VARCHAR (50),
              postcode VARCHAR (50),
              cust_telephone INT UNIQUE,
              membership_type_id VARCHAR (50),
              FOREIGN KEY (membership_type_id)
                REFERENCES memberships(membership_type_id)
              )"
          )

#Create table for orders
dbExecute(db_connection, 
          "CREATE TABLE IF NOT EXISTS orders (
              order_id VARCHAR (50) PRIMARY KEY,
              cust_id VARCHAR (50),
              FOREIGN KEY (cust_id)
                REFERENCES customers(cust_id)
              )"
          )

#Create table for order details
dbExecute(db_connection, 
          "CREATE TABLE IF NOT EXISTS order_details (
              order_quantity INT NOT NULL,
              order_date DATE,
              order_price DECIMAL,
              order_value DECIMAL,
              prod_id VARCHAR (50),
              order_id VARCHAR (50),
              FOREIGN KEY (prod_id)
                REFERENCES products(prod_id),
              FOREIGN KEY (order_id)
                REFERENCES orders(order_id)
              )"
          )

#Create table for payment
dbExecute(db_connection, 
          "CREATE TABLE IF NOT EXISTS payments (
              payment_id VARCHAR (50) PRIMARY KEY,
              payment_method VARCHAR (100) NOT NULL,
              payment_amount DECIMAL,
              payment_status VARCHAR (100) NOT NULL,
              payment_date DATE,
              order_id VARCHAR (50),
              FOREIGN KEY (order_id)
                REFERENCES orders(order_id)
              )"
          )

#Create table for shipment
dbExecute(db_connection, 
          "CREATE TABLE IF NOT EXISTS shipments (
              shipment_id VARCHAR (50) PRIMARY KEY,
              delivery_status VARCHAR (50),
              delivery_fee DECIMAL,
              delivery_recipient VARCHAR (50),
              shipper_name VARCHAR (50),
              est_delivery_date DATE,
              delivery_departed_date DATE,
              delivery_received_date DATE,
              prod_id VARCHAR (50),
              order_id VARCHAR (50),
              FOREIGN KEY (prod_id)
                REFERENCES products(prod_id),
              FOREIGN KEY (order_id)
                REFERENCES orders(order_id)
            )"
          )

#Create table for supplier
dbExecute(db_connection, 
          "CREATE TABLE IF NOT EXISTS suppliers (
              supplier_id VARCHAR (50) PRIMARY KEY,
              supplier_name VARCHAR (50) NOT NULL UNIQUE,
              supplier_postcode VARCHAR (100) NOT NULL UNIQUE,
              supplier_contact INT NOT NULL UNIQUE
            )"
          )

#Create table for supplies
dbExecute(db_connection, 
          "CREATE TABLE IF NOT EXISTS supplies (
              supply_id VARCHAR (50) PRIMARY KEY,
              inventory_quantity INT NOT NULL,
              sold_quantity INT NOT NULL,
              supplier_id VARCHAR (50),
              prod_id VARCHAR (50),
              FOREIGN KEY (supplier_id)
                REFERENCES suppliers(supplier_id),
              FOREIGN KEY (prod_id)
                REFERENCES products(prod_id)
            )"
          )

#Create table for customer queries
dbExecute(db_connection, 
          "CREATE TABLE IF NOT EXISTS customer_queries (
              query_id VARCHAR (50) PRIMARY KEY,
              query_title VARCHAR (50) NOT NULL,
              query_submission_date DATE,
              query_closure_date DATE,
              query_status VARCHAR (50) NOT NULL,
              cust_id VARCHAR (50),
              FOREIGN KEY (cust_id)
                REFERENCES customers(cust_id)
            )"
          )

#Create table for categories
dbExecute(db_connection, 
          "CREATE TABLE IF NOT EXISTS categories (
              category_id VARCHAR (50) PRIMARY KEY,
              category_name VARCHAR (50) NOT NULL UNIQUE
            )"
          )

#Create table for product categories
dbExecute(db_connection, 
          "CREATE TABLE IF NOT EXISTS product_categories (
              category_id VARCHAR (50),
              prod_id VARCHAR (50),
              FOREIGN KEY (prod_id)
                REFERENCES categories(category_id),
              FOREIGN KEY (prod_id)
                REFERENCES products(prod_id)
            )"
          )

#Create table for advertiser
dbExecute(db_connection, 
          "CREATE TABLE IF NOT EXISTS advertisers (
              advertiser_id VARCHAR (50) PRIMARY KEY,
              advertiser_name VARCHAR (50) NOT NULL UNIQUE,
              advertiser_email VARCHAR (50) UNIQUE
            )"
          )

#Create table for advertisements
dbExecute(db_connection, 
          "CREATE TABLE IF NOT EXISTS advertisements (
              ads_id VARCHAR (50) PRIMARY KEY,
              ads_start_date DATE,
              ads_end_date DATE,
              prod_id VARCHAR (50) UNIQUE,
              advertiser_id VARCHAR (50),
              FOREIGN KEY (prod_id)
                REFERENCES products(prod_id),
              FOREIGN KEY (advertiser_id)
                REFERENCES advertisers(advertiser_id)  
            )"
          )




