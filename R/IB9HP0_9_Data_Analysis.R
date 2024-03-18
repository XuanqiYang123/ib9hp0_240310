## Load Packages
library(dplyr)
library(ggplot2)
library(tidyverse)
library(RSQLite)
library(gridExtra)
library(scales) # for label_wrap()
library(lubridate)

# Create connection to SQL database
db_connection <- RSQLite::dbConnect(RSQLite::SQLite(),"IB9HP0_9.db")

# Advanced Data Analysis

## Analysis 1: Monthly Sales trend in Value and Volume
### Monthly Sales Trend Analysis by Value
#### Get the data
(sales_performance <- 
    dbGetQuery(db_connection, 
               "SELECT order_date AS date, 
               SUM(order_value) AS value_sales,
               SUM(order_quantity) AS volume_sales,
               SUM(order_value)/SUM(order_quantity) AS avg_price
               FROM order_details
               GROUP BY date
               ORDER BY date"))

#### Transform data to get month and year
sales_performance <- sales_performance %>%
  mutate(month = format(as.Date(date), "%m"),
         year = format(as.Date(date), "%Y")) %>%
  group_by(month, year) %>%
  summarise(value_sales = sum(value_sales),
            volume_sales = sum(volume_sales)) %>%
  mutate(avg_price = value_sales/volume_sales)

#### Plot monthly value sales trend

p.mnth.val <- ggplot(filter(sales_performance, 
                             year %in% c("2022", "2023")), 
       aes(x = month, group = year, color = year,
           y = value_sales)) +geom_smooth(se = F, show.legend = F) +
  labs(y = "Sales Value (GBP)", x = "Month", 
       subtitle = "Sales Value", color = "Year") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = comma, 
                     limits = c(0, 20000))

#### Plot monthly volume sales trend
p.mnth.vol <- ggplot(filter(sales_performance, 
                             year %in% c("2022", "2023")), 
                      aes(x = month, group = year, color = year,
                          y = volume_sales)) +
    geom_smooth(se = F, show.legend = F) + 
    labs(y = "Sales Volume (Units)", x = "Month", 
         subtitle = "Sales Volume", color = "Year") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(labels = comma,
                       limits = c(0, 500))
#### Plot monthly avg price trend
p.mnth.price <- ggplot(filter(sales_performance, 
                            year %in% c("2022", "2023")), 
                     aes(x = month, group = year, color = year,
                         y = avg_price)) +
  geom_smooth(se = F) + 
  labs(y = "Average Price (GBP/Unit)", x = "Month", 
       subtitle = "Average Price", color = "Year") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = comma,
                     limits = c(0, 100))
#### Combine value and volume sales graphs
gridExtra::grid.arrange(p.mnth.val, p.mnth.vol, p.mnth.price, ncol = 3, widths = c(0.9, 0.9, 1.1),
                        top = ggpubr::text_grob("Monthly Sales Trend", 
                                                size = 15, face = "bold"))

## Analysis 2: Product Ratings and Their Sales
### Products Popularity
#### Get the data
(top_products_rating <- 
    dbGetQuery(db_connection, 
               "SELECT a.prod_name AS products, 
               SUM(b.order_quantity) AS volume_sales,
               AVG(r.prod_rating) as avg_rating
               FROM products a
               JOIN order_details b ON a.prod_id = b.prod_id
               JOIN reviews r ON a.prod_id = r.prod_id
               GROUP BY a.prod_id
               ORDER BY volume_sales DESC
               LIMIT 10"))

#### Plot product sales graph
p.top_prod_sales <- ggplot(top_products_rating, 
       aes(x= reorder(products, volume_sales))) +
  geom_bar(aes(y = volume_sales), stat = "identity") + coord_flip() +
  labs(x = "Products", y = "Volume Sales",
       subtitle = "Volume Sales") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_blank())
#### Plot product ratings graph
p.top_prod_rating <- ggplot(top_products_rating, 
       aes(x= reorder(products, volume_sales))) +
  geom_bar(aes(y = avg_rating), stat = "identity",
           fill = "gray") + coord_flip() +
  labs(y = "Product Rating",
       subtitle = "Rating") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank()) +
  scale_y_continuous(limits = c(0,5))
#### Combine value and volume sales graphs
gridExtra::grid.arrange(p.top_prod_sales, p.top_prod_rating, ncol = 2, 
                        widths = c(1.1, 0.5),
                        top = ggpubr::text_grob("Top 10 Products and Their Rating", 
                                                size = 15, face = "bold"))

## Analysis 3: Membership segment
### Highest Spent based on Customer Segment
(membership_segmentation <- 
    dbGetQuery(db_connection, 
               "SELECT c.membership_type_id, 
               m.membership_type,  
               SUM(o.order_value) as total_spent
               FROM customers c
               JOIN memberships m ON c.membership_type_id = m.membership_type_id
               JOIN orders d ON c.cust_id = d.cust_id
               JOIN order_details o ON d.order_id = o.order_id
               GROUP BY c.membership_type_id
               ORDER BY total_spent DESC"))

ggplot(membership_segmentation, aes(x = membership_type, y = total_spent)) +
  geom_bar(stat = "identity") +
  labs(x = "Memberships Type", y = "Total Spent", title = "Top Spender based on Membership Tyoe") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

## Analysis 4: Suppliers Popularity
### Get the data
(top_suppliers <- 
    dbGetQuery(db_connection, 
               "SELECT a.supplier_name AS suppliers,
               p.prod_name AS product,
              
               SUM(b.sold_quantity) AS volume_sales
               FROM suppliers a
               JOIN supplies b ON a.supplier_id = b.supplier_id
               JOIN products p ON b.prod_id = p.prod_id
               
               GROUP BY a.supplier_id 
               ORDER BY volume_sales DESC
               LIMIT 10"))
# d.order_date AS date,
# JOIN order_details d ON b.prod_id = d.prod_id
### Trasform the data
suppliers_shr <- top_suppliers %>%
  mutate(vol_share = volume_sales/sum(volume_sales)) %>%
  group_by(suppliers, product) %>%
  summarise(vol_share = sum(vol_share))
  

ggplot(suppliers_shr, aes(fill= product, y = 100*vol_share, x = suppliers)) +
  geom_col() + coord_flip() +
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

# Disconnect connection to SQL database
dbDisconnect(db_connection)
