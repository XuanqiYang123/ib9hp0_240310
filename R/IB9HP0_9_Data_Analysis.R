## Load Packages
library(dplyr)
library(ggplot2)
library(tidyverse)
library(RSQLite)

# Create connection to SQL database
db_connection <- RSQLite::dbConnect(RSQLite::SQLite(),"IB9HP0_9.db")


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

