## Load Packages
library(dplyr)
library(ggplot2)
library(tidyverse)
library(RSQLite)

# Create connection to SQL database
db_connection <- RSQLite::dbConnect(RSQLite::SQLite(),"IB9HP0_9.db")

# Inserting Dataframe into the sql database

## Inserting Products table
for(i in 1:nrow(products_table)){
  dbExecute(db_connection, paste(
    "INSERT INTO products(prod_id,prod_name,prod_desc,voucher,prod_url,prod_unit_price) VALUES(",
    "'", products_table$prod_id[i], "',",
    "'", products_table$prod_name[i], "',",
    "'", products_table$prod_desc[i], "',",
    "'", products_table$voucher[i], "',",
    "'", products_table$prod_url[i], "',",
    products_table$prod_unit_price[i], ");",sep = "") 
  )
}

## Inserting Reviews table
for(i in 1:nrow(reviews_table)){
  dbExecute(db_connection, paste(
    "INSERT INTO reviews(review_id,prod_rating,review_date,prod_id) VALUES(",
    "'", reviews_table$review_id[i], "',",
    "'", reviews_table$prod_rating[i], "',",
    "'", reviews_table$review_date[i], "',",
    "'", reviews_table$prod_id[i], "');",sep = "") 
  )
}

## Inserting Memberships table
for(i in 1:nrow(memberships_table)){
  dbExecute(db_connection, paste(
    "INSERT INTO memberships(membership_type_id, membership_type) VALUES(",
    "'", memberships_table$membership_type_id[i], "',",
    "'", memberships_table$membership_type[i], "');",sep = "") 
  )
}

## Inserting Customers table
for(i in 1:nrow(customers_table)){
  dbExecute(db_connection, paste(
    "INSERT INTO customers(cust_id,first_name,last_name,cust_email,password,cust_birth_date,address_type,block_num,postcode,cust_telephone,membership_type_id) VALUES(",
    "'", customers_table$cust_id[i], "',",
    "'", customers_table$first_name[i], "',",
    "'", customers_table$last_name[i], "',",
    "'", customers_table$cust_email[i], "',",
    "'", customers_table$password[i], "',",
    "'", customers_table$cust_birth_date[i], "',",
    "'", customers_table$address_type[i], "',",
    "'", customers_table$block_num[i], "',",
    "'", customers_table$postcode[i], "',",
    "'", customers_table$cust_telephone[i], "',",
    "'", customers_table$membership_type_id[i], "');",sep = "") 
  )
}

## Inserting Orders table
for(i in 1:nrow(orders_table)){
  dbExecute(db_connection, paste(
    "INSERT INTO orders(order_id,cust_id) VALUES(",
    "'", orders_table$order_id[i], "',",
    "'", orders_table$cust_id[i],"');",sep = "") 
  )
}

## Inserting Payment table
for(i in 1:nrow(payments_table)){
  dbExecute(db_connection, paste(
    "INSERT INTO payments(payment_id, payment_method, payment_amount, payment_status, payment_date, order_id) VALUES(",
    "'", payments_table$payment_id[i], "',",
    "'", payments_table$payment_method[i], "',",
    payments_table$payment_amount[i], ",",
    "'", payments_table$payment_status[i], "',",
    "'", payments_table$payment_date[i], "',",
    "'", payments_table$order_id[i], "');",sep = "") 
  )
}

## Inserting Shipment table
for(i in 1:nrow(shipments_table)){
  dbExecute(db_connection, paste(
    "INSERT INTO shipments(shipment_id, delivery_status, delivery_fee, delivery_recipient, shipper_name, est_delivery_date, delivery_departed_date, delivery_received_date, prod_id, order_id) VALUES(",
    "'", shipments_table$shipment_id[i], "',",
    "'", shipments_table$delivery_status[i], "',",
    shipments_table$delivery_fee[i], ",",
    "'", shipments_table$delivery_recipient[i], "',",
    "'", shipments_table$shipper_name[i], "',",
    "'", shipments_table$est_delivery_date[i], "',",
    "'", shipments_table$delivery_departed_date[i], "',",
    "'", shipments_table$delivery_received_date[i], "',",
    "'", shipments_table$prod_id[i], "',",
    "'", shipments_table$order_id[i], "');",sep = "") 
  )
}

## Inserting Order details table
for(i in 1:nrow(order_details_table)){
  dbExecute(db_connection, paste(
    "INSERT INTO order_details(order_quantity,order_date,order_price,order_value,prod_id,order_id) VALUES(",
    order_details_table$order_quantity[i], ",",
    "'", order_details_table$order_date[i], "',",
    order_details_table$order_price[i], ",",
    order_details_table$order_value[i], ",",
    "'", order_details_table$prod_id[i], "',",
    "'", order_details_table$order_id[i], "');",sep = "") 
  )
}

## Inserting Suppliers table
for(i in 1:nrow(suppliers_table)){
  dbExecute(db_connection, paste(
    "INSERT INTO suppliers(supplier_id,supplier_name,supplier_postcode,supplier_contact) VALUES(",
    "'", suppliers_table$supplier_id[i], "',",
    "'", suppliers_table$supplier_name[i], "',",
    "'", suppliers_table$supplier_postcode[i], "',",
    "'", suppliers_table$supplier_contact[i], "');",sep = "") 
  )
}

## Inserting Supplies table
for(i in 1:nrow(supplies_table)){
  dbExecute(db_connection, paste(
    "INSERT INTO supplies(supply_id, inventory_quantity, sold_quantity, supplier_id, prod_id) VALUES(",
    "'", supplies_table$supply_id[i], "',",
    supplies_table$inventory_quantity[i], ",",
    supplies_table$sold_quantity[i], ",",
    "'", supplies_table$supplier_id[i], "',",
    "'", supplies_table$prod_id[i], "');",sep = "") 
  )
}

## Inserting Customer queries table
for(i in 1:nrow(customer_queries_table)){
  dbExecute(db_connection, paste(
    "INSERT INTO customer_queries(query_id, query_title, query_submission_date, query_closure_date, query_status, cust_id) VALUES(",
    "'", customer_queries_table$query_id[i], "',",
    "'", customer_queries_table$query_title[i], "',",
    "'", customer_queries_table$query_submission_date[i], "',",
    "'", customer_queries_table$query_closure_date[i], "',",
    "'", customer_queries_table$query_status[i], "',",
    "'", customer_queries_table$cust_id[i], "');",sep = "") 
  )
}

## Inserting Categories table
for(i in 1:nrow(categories_table)){
  dbExecute(db_connection, paste(
    "INSERT INTO categories(category_id, category_name) VALUES(",
    "'", categories_table$category_id[i], "',",
    "'", categories_table$category_name[i], "');",sep = "") 
  )
}

## Inserting Product Categories table
for(i in 1:nrow(product_categories_table)){
  dbExecute(db_connection, paste(
    "INSERT INTO product_categories(category_id, prod_id) VALUES(",
    "'", product_categories_table$category_id[i], "',",
    "'", product_categories_table$prod_id[i], "');",sep = "") 
  )
}

## Inserting Advertisers table
for(i in 1:nrow(advertisers_table)){
  dbExecute(db_connection, paste(
    "INSERT INTO advertisers(advertiser_id, advertiser_name, advertiser_email) VALUES(",
    "'", advertisers_table$advertiser_id[i], "',",
    "'", advertisers_table$advertiser_name[i], "',",
    "'", advertisers_table$advertiser_email[i], "');",sep = "") 
  )
}

## Inserting Advertisements table
for(i in 1:nrow(advertisements_table)){
  dbExecute(db_connection, paste(
    "INSERT INTO advertisements(ads_id, ads_start_date, ads_end_date, prod_id, advertiser_id) VALUES(",
    "'", advertisements_table$ads_id[i], "',",
    "'", advertisements_table$ads_start_date[i], "',",
    "'", advertisements_table$ads_end_date[i], "',",
    "'", advertisements_table$prod_id[i], "',",
    "'", advertisements_table$advertiser_id[i], "');",sep = "") 
  )
}

# Advanced Data Analysis
## Monthly Sales Trend Analysis by Value
(sales_value_performance <- dbGetQuery(db_connection, "SELECT strftime('%m-%Y', order_date)  as month, SUM(order_value) as total_sales
                                      FROM order_details
                                      GROUP BY month
                                      ORDER BY month"))

ggplot(sales_value_performance, aes(x = as.Date(paste("01",sales_value_performance$month,sep = "-"), format = "%d-%m-%Y"), y = total_sales)) +
  geom_line(group = 1) +
  labs(y = "Sales Value", x = "Month", title = "Monthly Trend by Sales Value") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

## Monthly Sales Trend Analysis by Quantity
(sales_quantity_performance <- dbGetQuery(db_connection, "SELECT strftime('%m-%Y', order_date)  as month, SUM(order_quantity) as total_sales
                                      FROM order_details
                                      GROUP BY month
                                      ORDER BY month"))

ggplot(sales_value_performance, aes(x = as.Date(paste("01",sales_quantity_performance$month,sep = "-"), format = "%d-%m-%Y"), y = total_sales)) +
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

