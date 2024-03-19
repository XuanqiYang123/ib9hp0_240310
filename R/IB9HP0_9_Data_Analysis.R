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
plot1 <- gridExtra::grid.arrange(p.mnth.val, p.mnth.vol, p.mnth.price, ncol = 3, widths = c(0.9, 0.9, 1.1),
                        top = ggpubr::text_grob("Monthly Sales Trend", 
                                                size = 15, face = "bold"))
ggsave("Data Analysis Results/Monthly Sales trend in Value and Volume.png", plot = plot1, width = 10, height = 6, dpi = 300)


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
plot2 <- gridExtra::grid.arrange(p.top_prod_sales, p.top_prod_rating, ncol = 2, 
                        widths = c(1.1, 0.5),
                        top = ggpubr::text_grob("Top 10 Products and Their Rating", 
                                                size = 15, face = "bold"))
ggsave("Data Analysis Results/Product Ratings and Their Sales.png", plot = plot2, width = 10, height = 6, dpi = 300)


## Analysis 3: Membership segment
### Highest Spent based on Customer Segment
(membership_segmentation <- 
    dbGetQuery(db_connection, 
               "SELECT c.membership_type_id, 
               m.membership_type,  
               SUM(o.order_value) as total_spent,
               o.order_date AS date
               FROM customers c
               JOIN memberships m ON c.membership_type_id = m.membership_type_id
               JOIN orders d ON c.cust_id = d.cust_id
               JOIN order_details o ON d.order_id = o.order_id
               GROUP BY o.order_date, c.membership_type_id
               ORDER BY total_spent DESC"))
### Transform the data
membership_by_mnth_date <- membership_segmentation %>%
  mutate("month" = format(as.Date(date), "%m"),
         "year" = format(as.Date(date), "%Y")) %>%
  group_by(membership_type, year) %>%
  filter(year != 2024) %>%
  summarise(total_spend = sum(total_spent))

### Plot spending by membership type
p.membership <- ggplot(filter(membership_segmentation,
                              format(as.Date(date), "%Y") != 2024), 
         aes(x = membership_type, 
             y = total_spent)) +
  geom_bar(stat = "identity", show.legend = F) +
  labs(x = "Membership Type", y = "Total Spend (£)", 
       subtitle = "Spending") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_y_continuous(labels = comma,
                     limits = c(0, 90000))

### Plot spending by membership type by year
p.membership_mnth <- ggplot(membership_by_mnth_date, 
       aes(fill = year, y = total_spend, 
           x = membership_type)) +
  geom_col(position = "dodge", color = "white") + 
  labs(fill = "Year", x = "Membership Type", y = "Total Spend (£)", 
       subtitle = "Spending by Year") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_blank(),
        #axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_y_continuous(labels = comma,
                     limits = c(0, 90000))
### Combine two membership charts 
plot3 <- gridExtra::grid.arrange(p.membership, p.membership_mnth, ncol = 2,
                        widths = c(0.5, 1.1),
                        top = ggpubr::text_grob("Spending by Membership Type", 
                                                size = 15, face = "bold"))
ggsave("Data Analysis Results/Membership segment.png", plot = plot3, width = 10, height = 6, dpi = 300)


## Analysis 4: Customer QUeries
### Most Frequent Queries - get data from db
(queries_frequencies <- 
    dbGetQuery(db_connection,
               "SELECT query_title, COUNT(*) as frequencies
               FROM customer_queries
               GROUP BY query_title
               ORDER BY frequencies DESC"))

### Plot query types in terms of frequency
p.query_freq <- ggplot(queries_frequencies, 
       aes(x= reorder(query_title, desc(frequencies)), 
                                y = frequencies)) +
  geom_bar(stat = "identity") +
  labs(x = "Query Type", y = "Frequency", 
       subtitle = "Frequency") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

### Response Time Analyis for Customer Queries - get data from the db
(response_time <- 
    dbGetQuery(db_connection,
               "SELECT query_id, 
               query_title,
               query_closure_date,
               query_submission_date
               FROM customer_queries"))

### Transform data - get turnaround time
response_time <- filter(response_time, query_closure_date != "NA") %>%
  mutate(turnaround_time = round(difftime(query_closure_date, query_submission_date,
                                    units = "weeks"),0) ) %>%
  group_by(query_title) %>%
  summarise(avg_turnaround_time = round(mean(turnaround_time),1)) %>%
  merge(queries_frequencies, by = "query_title", remove = F)

### Plot query by response time
h_line <- mean(response_time$avg_turnaround_time)
p.query_time <- ggplot(response_time, 
       aes(x= reorder(query_title, desc(frequencies)), 
           y = avg_turnaround_time)) +
  geom_bar(stat = "identity") + 
  geom_hline(yintercept = h_line, 
             color = "magenta", linetype = "dashed", size = 1.1) +
  geom_text(aes(1, h_line, label = "Avg of all types"),
            vjust = -1, color = "magenta") +
  labs(x = "Query Type", y = "Avg Turnaround Time (weeks)", 
       subtitle = "Turnaround Time") +
  theme_light()
### Combine frequency and turnaround time
plot4 <- gridExtra::grid.arrange(p.query_freq, p.query_time, ncol = 2,
                        top = ggpubr::text_grob("Customer Queries", 
                                                size = 15, face = "bold"))
ggsave("Data Analysis Results/Customer Queries.png", plot = plot4, width = 10, height = 6, dpi = 300)


## Analysis 5: Payment Method
### Get data from the db
(top_payment <- 
    dbGetQuery(db_connection,
               "SELECT payment_method, COUNT(*) AS frequencies,
               SUM(payment_amount) AS pymnt_amnt
               FROM payments
               GROUP BY payment_method
               ORDER BY frequencies DESC"))
### Plot payment method by frequency
p.frequency <- ggplot(top_payment, aes(x= reorder(payment_method, desc(frequencies)), 
                        y = frequencies)) +
  geom_bar(stat = "identity") +
  labs(x = "Payment Method", y = "Frequency", 
       subtitle = "Frequently Used Payment Method") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_y_continuous(labels = comma)
### Plot payment method by value
p.payment_amnt <- ggplot(top_payment, 
                         aes(x= reorder(payment_method, desc(frequencies)), 
                                       y = pymnt_amnt)) +
  geom_bar(stat = "identity") +
  labs(x = "Payment Method", y = "Payment Amount (£)", 
       subtitle = "Payment Value") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_y_continuous(labels = comma)
### Combine payment method by frequency and value
plot5 <- gridExtra::grid.arrange(p.frequency, p.payment_amnt, ncol = 2,
                        top = ggpubr::text_grob("Payment Methods", 
                                                size = 15, face = "bold"))
ggsave("Data Analysis Results/Payment Method.png", plot = plot5, width = 10, height = 6, dpi = 300)



# Disconnect connection to SQL database
dbDisconnect(db_connection)
