## Load Packages
library(dplyr)
library(ggplot2)
library(tidyverse)
library(RSQLite)

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

<<<<<<< HEAD
<<<<<<< HEAD
=======
=======
>>>>>>> 797c30b906ada5962583a85d1a018c32ab61d099
# Create connection to SQL database
db_connection <- RSQLite::dbConnect(RSQLite::SQLite(),"IB9HP0_9.db")

# Inserting Dataframe into the sql database

## Inserting Products table
dbWriteTable(db_connection,"products",products_table, append = TRUE)

## Inserting Reviews table
dbWriteTable(db_connection,"reviews",reviews_table, append = TRUE)

## Inserting Memberships table
dbWriteTable(db_connection,"memberships",memberships_table, append = TRUE)

<<<<<<< HEAD
## Inserting Memberships table
dbWriteTable(db_connection,"memberships",memberships_table, append = TRUE)
>>>>>>> fc5b28f05fee16ead44168da75fdf3b9b12c9075
=======
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

# Advanced Data Analysis
## Monthly Sales Trend Analysis by Value
(sales_value_performance <- dbGetQuery(db_connection, "SELECT strftime('%m-%Y', order_date)  as month, SUM(order_value) as total_sales
                                      FROM order_details
                                      GROUP BY month
                                      ORDER BY month"))

ggplot(sales_value_performance, aes(x = month, y = total_sales)) +
  geom_line(group = 1) +
  labs(y = "Sales Value", x = "Month", title = "Monthly Trend by Sales Value") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

## Monthly Sales Trend Analysis by Quantity
(sales_quantity_performance <- dbGetQuery(db_connection, "SELECT strftime('%m-%Y', order_date)  as month, SUM(order_quantity) as total_sales
                                      FROM order_details
                                      GROUP BY month
                                      ORDER BY month"))

ggplot(sales_value_performance, aes(x = month, y = total_sales)) +
  geom_line(group = 1) +
  labs(y = "Sales Quantity", x = "Month", title = "Monthly Trend by Sales Quantity") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


## Product Ratings Analysis
(product_rating <- dbGetQuery(db_connection, "SELECT p.prod_name, AVG(r.prod_rating) as avg_rating
                             FROM products p
                             JOIN reviews r ON p.prod_id = r.prod_id
                             GROUP BY p.prod_name
                             ORDER BY avg_rating DESC"))

ggplot(product_rating, aes (y = prod_name, x = avg_rating)) +
  geom_bar(stat = "identity") +
  labs(y = "Products", x = "Average Rating", title = "Product Rating Analysis") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



## Highest Spent based on Customer Segment
(membership_segmentation <- dbGetQuery(db_connection, "SELECT c. membership_type_id, m. membership_type,  SUM(o.order_value) as total_spent
                                      FROM customers c
                                      JOIN memberships m ON c. membership_type_id = m. membership_type_id
                                      JOIN orders d ON c. cust_id = d. cust_id
                                      JOIN order_details o ON d. order_id = o. order_id
                                      GROUP BY c.membership_type_id
                                      ORDER BY total_spent DESC"))

ggplot(membership_segmentation, aes(x = membership_type, y = total_spent)) +
  geom_bar(stat = "identity") +
  labs(x = "Memberships Type", y = "Total Spent", title = "Top Spender based on Membership Tyoe") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

## Products Popularity
(top_products <- dbGetQuery(db_connection, "SELECT a.prod_name AS products, SUM(b.order_quantity) AS total_sales
                            FROM products a
                            JOIN order_details b ON a.prod_id = b.prod_id
                            GROUP BY a.prod_id
                            ORDER BY total_sales DESC
                            LIMIT 5"))

ggplot(top_products, aes(x= products, y = total_sales)) +
  geom_bar(stat = "identity") +
  labs(x = "Products", y = "Total Sales", title = "Top 5 Products of All Time") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

## Suppliers Popularity
(top_suppliers <- dbGetQuery(db_connection, "SELECT a.supplier_name AS suppliers, SUM(b.sold_quantity) AS total_sales
                            FROM suppliers a
                            JOIN supplies b ON a.supplier_id = b.supplier_id
                            GROUP BY a.supplier_id
                            ORDER BY total_sales DESC
                            LIMIT 5"))

ggplot(top_suppliers, aes(x= suppliers, y = total_sales)) +
  geom_bar(stat = "identity") +
  labs(x = "Suppliers", y = "Total Sales", title = "Top 5 Suppliers of All Time") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

## Most Popular Payment Method
(top_payment <- dbGetQuery(db_connection,"SELECT payment_method, COUNT(*) as frequencies
                           FROM payments
                           GROUP BY payment_method
                           ORDER BY frequencies DESC
                           LIMIT 5"))

ggplot(top_payment, aes(x= payment_method, y = frequencies)) +
  geom_bar(stat = "identity") +
  labs(x = "Payment Method", y = "Frequencies", title = "Frequently Used Payment Method") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


## Most Frequent Queries
(queries_frequencies <- dbGetQuery(db_connection,"SELECT query_title, COUNT(*) as frequencies
                                   FROM customer_queries
                                   GROUP BY query_title
                                   ORDER BY frequencies DESC"))

ggplot(queries_frequencies, aes(x= query_title, y = frequencies)) +
  geom_bar(stat = "identity") +
  labs(x = "Queries", y = "Frequencies", title = "Most Frequent Customer Issues") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

## Response Time Analyis for Customer Queries
(response_time <- dbGetQuery(db_connection,"SELECT query_id, query_closure_date-query_submission_date as response_time
                             FROM customer_queries
                             ORDER BY response_time DESC"))

ggplot(response_time, aes(x= query_id, y = response_time)) +
  geom_bar(stat = "identity") +
  labs(x = "Queries ID", y = "Response Time", title = "Response Time Trend on Customer Queries") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
>>>>>>> 797c30b906ada5962583a85d1a018c32ab61d099

