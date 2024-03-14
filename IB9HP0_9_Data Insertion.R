## Load Packages
library(dplyr)
library(ggplot2)
library(tidyverse)
library(RSQLite)


# Create connection to SQL database
db_connection <- RSQLite::dbConnect(RSQLite::SQLite(),"IB9HP0_9.db")

# Create table for products
dbExecute(db_connection, 
           "CREATE TABLE products (
              prod_id INT PRIMARY KEY,
              prod_name VARCHAR (50) NOT NULL,
              prod_desc VARCHAR (100) NOT NULL,
              voucher VARCHAR (50),
              prod_url VARCHAR (250) NOT NULL,
              prod_unit_price DECIMAL NOT NULL
              )")

#Create table for reviews
dbExecute(db_connection, 
          "CREATE TABLE reviews (
              review_id INT PRIMARY KEY,
              prod_rating DECIMAL NOT NULL,
              review_date DATE NOT NULL,
              prod_id INT,
              FOREIGN KEY (prod_id)
              REFERENCES products(prod_id)
              )")

#Create table for memberships
dbExecute(db_connection, 
          "CREATE TABLE memberships (
              membership_type_id INT PRIMARY KEY,
              membership_desc VARCHAR (50) NOT NULL
              )")

