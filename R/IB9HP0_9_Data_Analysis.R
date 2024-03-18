## Load Packages
library(dplyr)
library(ggplot2)
library(tidyverse)
library(RSQLite)
library(gridExtra)
library(scales) # for label_wrap()

# Create connection to SQL database
db_connection <- RSQLite::dbConnect(RSQLite::SQLite(),"IB9HP0_9.db")

# Advanced Data Analysis

## Analysis 1: Monthly Sales trend in Value and Volume
### Monthly Sales Trend Analysis by Value
#### Get the data
(sales_value_performance <- 
    dbGetQuery(db_connection, 
               "SELECT strftime('%m-%Y', order_date) as month, 
               SUM(order_value) as total_sales
               FROM order_details
               GROUP BY month
               ORDER BY month"))

#### Plot monthly value sales trend
p.mnth.val <- ggplot(sales_value_performance, 
       aes(x = as.Date(paste("01",sales_value_performance$month,sep = "-"), 
                        format = "%d-%m-%Y"), 
           y = total_sales)) +
  geom_line(group = 1) +
  labs(y = "Sales Value (GBP)", x = "Month", 
       subtitle = "Sales Value") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "1 year") +
  scale_y_continuous(labels = comma)

### Monthly Sales Trend Analysis by Quantity
#### Get the data
(sales_quantity_performance <- 
    dbGetQuery(db_connection, 
               "SELECT strftime('%m-%Y', order_date)  as month, 
               SUM(order_quantity) as total_sales
               FROM order_details
               GROUP BY month
               ORDER BY month"))

#### Plot monthly volume sales trend
p.mnth.vol <- ggplot(sales_quantity_performance, 
                     aes(x = as.Date(paste("01",sales_value_performance$month,sep = "-"), 
                                     format = "%d-%m-%Y"), 
                         y = total_sales)) +
  geom_line(group = 1) +
  labs(y = "Sales Volume (Units)", x = "Month", 
       subtitle = "Sales Volume") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "1 year") +
  scale_y_continuous(labels = comma)

#### Combine value and volume sales graphs
gridExtra::grid.arrange(p.mnth.val, p.mnth.vol, ncol = 2,
                        top = ggpubr::text_grob("Monthly Sales Trend", size = 15, face = "bold"))

## Analysis 2: Product Ratings and Their Sales
### Product Ratings Analysis
(product_rating <- 
    dbGetQuery(db_connection,
               "SELECT p.prod_name, AVG(r.prod_rating) as avg_rating
               FROM products p
               JOIN reviews r ON p.prod_id = r.prod_id
               GROUP BY p.prod_name
               ORDER BY avg_rating DESC"))

### Plot rating plot
ggplot(product_rating, 
       aes (y = avg_rating, x = reorder(prod_name, avg_rating))) +
  geom_bar(stat = "identity") + coord_flip() +
  labs(y = "Products", x = "Average Rating", 
       subtitle = "Product Rating") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

### Products Popularity
#### Get the data
(top_products <- 
    dbGetQuery(db_connection, 
               "SELECT a.prod_name AS products, 
               SUM(b.order_quantity) AS total_sales
               FROM products a
               JOIN order_details b ON a.prod_id = b.prod_id
               GROUP BY a.prod_id
               ORDER BY total_sales DESC
               LIMIT 5"))

#### Plot product graph
ggplot(top_products, aes(x= reorder(products, total_sales), y = total_sales)) +
  geom_bar(stat = "identity") +
  labs(x = "Products", y = "Total Sales", 
       subtitle = "Top 5 Products of All Time") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

## Analysis 3: Membership segment
### Highest Spent based on Customer Segment
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

