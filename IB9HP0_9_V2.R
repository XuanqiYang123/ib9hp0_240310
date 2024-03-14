---
  title: "IB9HPO_9"
output: html_document
date: "2024-03-02"
editor_options: 
  chunk_output_type: console
---
  
  ## Load Packages
  

knitr::opts_chunk$set(echo = TRUE,attr.source='.numberLines', eval = FALSE)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
#packages for synthetic data generation
library(conjurer) 
library(randomNames)
library(Pareto)
library(uuid)
library(writexl)
library(charlatan) #for credit card number
library(RSQLite)
library(stringi) #random strings
library(lubridate)


## Db Connection

# Create connection to SQL database
db_connection <- RSQLite::dbConnect(RSQLite::SQLite(),"IB9HP0_9.db")

## SQL Schema


Product <- "CREATE TABLE 'products' (
  'prod_id' INT PRIMARY KEY,
  'prod_name' VARCHAR (50) NOT NULL,
  'prod_desc' VARCHAR (100) NOT NULL,
  'prod_uom' VARCHAR(20) NOT NULL,
  'voucher' VARCHAR (50),
  'prod_url' VARCHAR (250) NOT NULL,
  'prod_unit_price' DECIMAL NOT NULL
)"

Review <- "CREATE TABLE 'reviews' (
  'review_id' INT PRIMARY KEY,
  'prod_rating' DECIMAL NOT NULL,
  'Review_date' DATE NOT NULL,
  'comment' TEXT,
  'prod_id' INT,
  FOREIGN KEY ('prod_id')
  REFERENCES products('prod_id')
);"


Membership <- "CREATE TABLE 'memberships' (
  'membership_type_id' INT PRIMARY KEY,
  'membership_desc' VARCHAR (50) NOT NULL
);"


Customer <- "CREATE TABLE 'customers' (
  'cust_id' INT PRIMARY KEY,
  'first_name' VARCHAR (50) NOT NULL,
  'last_name' VARCHAR (50) NOT NULL,
  'cust_email' VARCHAR (50) UNIQUE,
  'password' VARCHAR (50) NOT NULL,
  'cust_bod' DATE,
  'shipping_address' VARCHAR (100) UNIQUE,
  'billing_address' VARCHAR (100) NOT NULL,
  'cust_telephone' INT UNIQUE,
  'membership_type_id' INT,
  FOREIGN KEY ('membership_type_id')
  REFERENCES memberships('membership_type_id')
);"


Orders <- "CREATE TABLE 'orders' (
  'order_id' INT PRIMARY KEY,
  'cust_id' INT,
  FOREIGN KEY ('cust_id')
  REFERENCES customers('cust_id')
);"



Payments <- "CREATE TABLE 'payments' (
  'payment_method' VARCHAR (100) NOT NULL,
  'payment_amount' DECIMAL NOT NULL,
  'payment_status' VARCHAR (100) NOT NULL,
  'payment_date' DATE,
  'order_id' INT,
  FOREIGN KEY ('order_id')
  REFERENCES orders('order_id')
);"


Orders_Details <- "CREATE TABLE 'order_details' (
  'order_quantity' INT NOT NULL,
  'order_date' DATE,
  'order_price' DECIMAL NOT NULL,
  'order_value' DECIMAL NOT NULL,
  'delivery_status' VARCHAR (50) NOT NULL,
  'delivery_fee' DECIMAL NOT NULL,
  'delivery_recipient' VARCHAR (50) NOT NULL,
  'shipper_name' VARCHAR (50) NOT NULL,
  'estimated_delivery_date' DATE,
  'delivery_departed_date' DATE,
  'delivery_received_date' DATE,
  'prod_id' INT,
  'order_id' INT,
  FOREIGN KEY ('prod_id')
  REFERENCES products('prod_id'),
  FOREIGN KEY ('order_id')
  REFERENCES orders('order_id')
);"



Supplier <- "CREATE TABLE 'suppliers' (
  'supplier_id' INT PRIMARY KEY,
  'supplier_name' VARCHAR (50) NOT NULL UNIQUE,
  'supplier_address' VARCHAR (100) NOT NULL UNIQUE,
  'contact_info' INT NOT NULL UNIQUE
);"

Supply <- "CREATE TABLE 'supplies' (
  'inventory_quantity' INT NOT NULL,
  'sold_quantity' INT NOT NULL,
  'supplier_id' INT,
  'prod_id' INT,
  FOREIGN KEY ('supplier_id')
  REFERENCES suppliers('supplier_id'),
  FOREIGN KEY ('prod_id')
  REFERENCES products('prod_id')
);"


Customer_Query <- "CREATE TABLE 'customer_queries' (
  'query_id' INT PRIMARY KEY,
  'query_title' VARCHAR (50) NOT NULL,
  'query_detail' TEXT NOT NULL,
  'query_submission_date' DATE,
  'query_status' VARCHAR (50) NOT NULL,
  'cust_id' INT,
  FOREIGN KEY ('cust_id')
  REFERENCES customers('cust_id')
);"


Category <- "CREATE TABLE 'categories' (
  'category_id' INT PRIMARY KEY,
  'category_name' VARCHAR (50) NOT NULL UNIQUE
);"


Product_Category <- "CREATE TABLE 'product_categories' (
  'category_id' INT,
  'prod_id' INT,
  FOREIGN KEY ('prod_id')
  REFERENCES categories('category_id'),
  FOREIGN KEY ('prod_id')
  REFERENCES products('prod_id')
);"


Advertiser <- "CREATE TABLE 'advertisers' (
  'advertiser_id' INT PRIMARY KEY,
  'ddvertiser_name' VARCHAR (50) NOT NULL UNIQUE,
  'advertiser_email' VARCHAR (50) UNIQUE
);"


Advertisement <- "CREATE TABLE 'advertisements' (
  'ads_id' INT PRIMARY KEY,
  'ads_start_date' DATE,
  'ads_end_date' DATE,
  'prod_id' INT UNIQUE,
  'advertiser_id' INT,
  FOREIGN KEY ('prod_id')
  REFERENCES products('prod_id'),
  FOREIGN KEY ('advertiser_id')
  REFERENCES advertisers('advertiser_id')  
);"

## Synthetic Data Generation

### 'customers' table

#Define parameters for customers
n_customers <- 50
birthdate <- sample(seq(from = as.Date(today() - years(80)), 
                        to = as.Date(today() - years(18)), by = "day"),
                    n_customers)
cv_postcode <- 
  read.csv("data_uploads/ONSPD_AUG_2023_UK_CV.csv")[, 1] %>% 
  data.frame() %>% 
  setNames("pcd")
address_type <- c("Home", "Office")

#Create data
customers_data <- 
  #Create n unique customer IDs with random names
  data.frame("cust_id" = conjurer::buildCust(n_customers),
             "cust_name" = randomNames::randomNames(n_customers)) %>% 
  separate(cust_name, into = c("last_name", "first_name"), sep = ", ") %>%
  #Create email column, by merging last & first name with email domain @gmail.com
  unite(cust_email, c(last_name, first_name), sep = ".", remove = F) %>%
  mutate(
    "cust_email" = paste(cust_email,"gmail.com", sep = "@"),
    #Generate user's password, using random string generation package
    "password" = 
      stringi::stri_rand_strings(n=n_customers, length=8, pattern="[A-Za-z0-9]"),
    #Adding customer BOD
    "cust_birth_date" = sample(birthdate, n_customers, replace = T),
    #Adding the phone code in UK
    "phone_domain" = "075",
    #create unique random strings of 7 digits
    "cust_telephone" = 
      stringi::stri_rand_strings(n=n_customers, length=7, pattern="[0-9]"),
    "block_num" = 
      sprintf("%s%s", 
              stri_rand_strings(n=n_customers, length=1, pattern="[A-Z]"),
              stri_rand_strings(n=n_customers, length=2, pattern="[0-99]")),
    #randomly assign postcode to each customer
    "postcode" = cv_postcode[sample(nrow(cv_postcode), n_customers),],
    #randomly assign address type to each customer
    "address_type" = sample(address_type, n_customers, replace = T)) %>%
  #Adding customer's telephone number by merging two phone number columns
  unite(cust_telephone, 
        c(phone_domain, cust_telephone), sep = "", remove = T) %>%
  #reorder the columns
  select(1,4,3,2,5,6,8,9,10,7)

#Save data to data file
write.csv(customers_data, "data_uploads/R_synth_customers.csv")


### 'products' table

#Getting brand and product names from Gemini
gemini_prods <- 
  readxl::read_excel("data_uploads/gemini_prod_cate_supplier.xlsx", 
                     .name_repair = "universal") %>%
  setNames(c("seller_name", "category", "prod_name", "prod_desc"))

#Define parameters for products
n_prods <- 20
voucher <- c("10%", "20%", "50%")
ratings <- c(1,2,3,4,5)
date <- #assuming company was established on Mar 06th 2004
  sample(seq(from = as.Date("2004/03/06"), 
             to = as.Date(lubridate::today()), by = "day"), 12)

#Assign product ID, and adding product names and URL
products_data <- 
  #generate product id
  conjurer::buildProd(n_prods, minPrice = 1, maxPrice = 100) %>% 
  #add product name and description from gemini's file
  mutate("prod_name" = sample(gemini_prods$prod_name, 20)) %>%
  left_join(select(gemini_prods, -c(seller_name, category)), 
            by = join_by(prod_name)) %>%
  #rename columns to fit schema
  rename(prod_id = SKU, prod_unit_price = Price) %>%
  #rename `sku` with `prod`
  mutate("prod_id" = gsub("sku", "prod", prod_id)) %>%
  #add product url
  mutate("web_prefix" = "https://group9.co.uk/",
         "prod_url1" = gsub(" ", "-", prod_name)) %>%
  unite(prod_url, c(prod_url1, prod_id), sep = "-", remove = F) %>%
  unite(prod_url, c(web_prefix, prod_url), sep = "", remove = T) %>%
  mutate(
    #Create ratings
    "prod_rating" = sample(ratings, n_prods, replace = T),
    #Review date
    "review_date" = sample(date, n_prods, replace = T),
    #Assign review ID
    "review_id" = 
      conjurer::buildCust(sum(!is.na(prod_rating))),
    "review_id" = gsub("cust", "rev", review_id)) %>%
  #drop temp url
  select(-prod_url1)

#Create vouchers
products_data$voucher[!(products_data$prod_id %in% c("prod10", "prod11")) ] <- 
  sample(voucher, sum(!(products_data$prod_id %in% c("prod10", "prod11"))), 
         replace = T)

products_data <- 
  products_data %>%
  #rearrange order of columns
  select(2,4,5,8,6,7,3,9,1)

#Save to .csv file
write.csv(products_data, "data_uploads/R_synth_products.csv")


### 'categories' table

#create lookup table for category_id and category name
category_lookup <- 
  data.frame("category_id" = seq(1, length(unique(gemini_prods$category)),1),
             "category" = unique(gemini_prods$category),
             "cate_code" = "cate") %>%
  unite(category_id, c(cate_code, category_id), sep = "", remove = T)

#Create categories table
categories_data <- 
  #Pull category name and product name from gemini file
  select(gemini_prods, c(category, prod_name)) %>%
  #Only keep the products included in the products table
  right_join(select(products_data, c(prod_id, prod_name)), by = "prod_name") %>%
  #lookup category_id
  merge(category_lookup, by = "category") %>%
  #rename to have category_name column
  rename(category_name = category) %>%
  #drop product name column
  select(-prod_name) %>%
  #reorder the columns to match with table schema
  select(3,2,1)

#Save to .csv file
write.csv(categories_data, "data_uploads/R_synth_categories.csv")

### 'orders' table

#Define parameters
origin_date <- "1970-01-01"
n_orders <- 100
pymt_method <- 
  c("Bank Transfer", "Visa", "Mastercard", "PayPal", "GPay", "Apple Pay")
pymt_status <- c("Done", "Verifying")
shipper_lookup <- 
  data.frame("shipper_name" = c("DHL", "Group9DL", "DPD"),
             "delivery_fee" = c(5,2,3),
             "ETA" = c(1,5,3))
delivery_status <- c("Delivered", "In Progress", 
                     "Failed to contact", "Delayed")
orders_col_order <- 
  c("order_id", "cust_id", "prod_id", "order_detail_id", "order_quantity",
    "order_date", "order_value", "order_price", "payment_id", "payment_method",
    "payment_status", "payment_date", "delivery_departed_date",
    "delivery_received_date", "est_delivery_date", "shipper_name", 
    "delivery_recipient", "delivery_fee", "delivery_status")

#generate n order IDs and assign customers to them, including order date
orders_data <- 
  #Create n unique order IDs
  data.frame("order_id" = conjurer::buildCust(n_orders)) %>%
  mutate(order_id = gsub("cust", "o", order_id),
         payment_id = gsub("o", "pm", order_id),
         cust_id = sample(customers_data$cust_id, n_orders, replace = T),
         order_date = sample(date, n_orders, replace = T),
         payment_method = sample(pymt_method, n_orders, replace = T),
         payment_status = sample(pymt_status, n_orders, replace = T),
         delivery_recipient = randomNames::randomNames(n_orders))

#adding payment date with logic dependent on payment status
orders_data <- orders_data %>%
  mutate("payment_date" = ifelse(payment_status == "Done", order_date, NA)) %>%
  mutate("payment_date" = as.Date(payment_date, 
                                  origin = origin_date))

#randomly replicate certain orders to map with products
set.seed(123)
orders_data <- orders_data %>% bind_rows() %>%
  rbind(sample_n(orders_data, 0.4*nrow(orders_data)),
        sample_n(orders_data, 0.5*nrow(orders_data)),
        sample_n(orders_data, 0.8*nrow(orders_data)))

#assign products to orders
orders_data <- orders_data %>%
  mutate(
    "prod_id" = sample(products_data$prod_id, 
                       nrow(orders_data), replace = T),
    "order_detail_id" = paste("od",row_number(), sep = ""),
    #generate order quantity
    "order_quantity" = sample(seq(1,10,1), nrow(orders_data), replace = T)) %>%
  merge(select(products_data, c(prod_id, prod_unit_price, voucher)), 
        by = "prod_id")

#Order value and shipper
orders_data <- orders_data %>%
  #order price and value
  mutate(
    voucher = as.numeric(gsub("%", "", voucher))/100,
    order_price = prod_unit_price * voucher,
    order_value = order_price * order_quantity,
    #assign shippers to products
    shipper_name = 
      sample(shipper_lookup$shipper_name, nrow(orders_data), replace = T),
    #add delivery status
    delivery_status = 
      ifelse(payment_status != "Done", "Not Started",
             sample(delivery_status, nrow(orders_data), replace = T)) ) %>%
  #lookup delivery fee
  merge(shipper_lookup, by = "shipper_name")

#dates of delivery
orders_data <- orders_data %>%
  #departure and ETA
  mutate(
    delivery_departed_date = 
      ifelse(!is.na(payment_date), (payment_date + days(2)), NA),
    est_delivery_date = delivery_departed_date + ETA) %>%
  #departure and ETA - format as date
  mutate(
    delivery_departed_date = 
      as.Date(delivery_departed_date, origin = origin_date),
    est_delivery_date = 
      as.Date(est_delivery_date, origin = origin_date)) %>%
  #received
  mutate(
    delivery_received_date = 
      ifelse(delivery_status != "Delivered", NA, est_delivery_date)) %>%
  #drop ETA
  select(-ETA)

#reorder the columns
orders_data <- orders_data[,orders_col_order]

#Save data to data file
write.csv(orders_data, "data_uploads/R_synth_orders.csv")

### 'suppliers' table

#Define parameters for suppliers table
n_suppliers <- length(unique(gemini_prods$seller_name))
wc_postcode <- read.csv("data_uploads/ONSPD_AUG_2023_UK_WC.csv")[,1]

#Create suppliers table
suppliers_data <- 
  #Pull seller name from gemini file
  distinct(select(gemini_prods, seller_name)) %>%
  rename("supplier_name" = "seller_name") %>%
  mutate("supplier_id" = seq(1, n_suppliers,1),
         "prefix" = "s") %>%
  unite(supplier_id, c(prefix, supplier_id), sep = "", remove = T) %>%
  mutate(
    "supplier_postcode" =
      sample(wc_postcode, n_suppliers, replace = T),
    #Adding the phone code in UK
    "phone_domain" = "079",
    #create unique random strings of 7 digits
    "supplier_contact" = 
      stringi::stri_rand_strings(n=n_suppliers, length=7, pattern="[0-9]")) %>%
  #Adding supplier's telephone number by merging two phone number columns
  unite(supplier_contact, 
        c(phone_domain, supplier_contact), sep = "", remove = T) %>%
  select(2,1,4,3)

#Save data to data file
write.csv(suppliers_data, "data_uploads/R_synth_suppliers.csv")

### 'supply' table

#Define parameters for supply table
order_quant_by_prod <- orders_data %>%
  group_by(prod_id) %>% summarise(sold_quantity = sum(order_quantity))
supply_col_order <- c("supply_id", "supplier_id", "prod_id", 
                      "inventory_quantity", "sold_quantity")

#Create supply table
supply_data <- select(products_data, c(prod_id, prod_name)) %>%
  merge(order_quant_by_prod, by = "prod_id") %>%
  mutate(sold_quantity = as.integer(sample(seq(0.2,1),1)*sold_quantity)) %>%
  mutate(inventory_quantity = 
           as.integer(sold_quantity * sample(seq(1.1, 2.3), 1))) %>%
  merge(select(gemini_prods, c(seller_name, prod_name)), by = "prod_name") %>%
  rename("supplier_name" = "seller_name") %>%
  merge(select(suppliers_data, c(supplier_id, supplier_name)), 
        by = "supplier_name")

#Create competitors for M:N relationship
supply_competitors <- select(products_data, c(prod_id, prod_name)) %>%
  mutate(supplier_name = 
           sample(suppliers_data$supplier_name, n_prods, replace = T)) %>%
  merge(select(suppliers_data, c(supplier_id, supplier_name)), 
        by = "supplier_name") %>%
  merge(order_quant_by_prod, by = "prod_id") %>%
  mutate(sold_quantity = as.integer(sample(seq(0.2,1),1)*sold_quantity)) %>%
  mutate(inventory_quantity = 
           as.integer(sold_quantity * sample(seq(1.1, 2.3), 1))) %>%
  select(2,3,1,5,6,4)

#Combine supply and competitors
supply_data <- 
  rbind(supply_data, supply_competitors) %>% 
  mutate(supply_id = row_number()) %>%
  select(-c(supplier_name, prod_name))

#reorder columns
supply_data <- supply_data[, supply_col_order]

#Save data to data file
write.csv(supply_data, "data_uploads/R_synth_supply.csv")

### 'memberships' table

membership_lookup <- 
  data.frame(
    "membership_type" =  c("Student", "Trial", "Premium")) %>%
  mutate("membership_type_id" = row_number())

#Start with the foreign key cust_id 
memberships_data <- data.frame(customers_data$cust_id) 

memberships_data <- memberships_data %>%
  #Randomly assign membership type to all customers
  mutate("membership_type" = 
           sample(membership_lookup$membership_type, 
                  nrow(memberships_data), replace = T)) %>%
  #Lookup membership_id
  merge(membership_lookup, by = "membership_type") %>%
  rename(cust_id = customers_data.cust_id) %>%
  select(3,2,1)

#Save to .csv file
write.csv(memberships_data, "data_uploads/R_synth_memberships.csv")

### 'customer_queries' table


# customer_queries table
n_queries <- 20
customer_queries_data <- data.frame(
  query_id = sprintf("Q%d", 1:n_queries),
  cust_id = sample(customers_data$cust_id, n_queries, replace = TRUE),
  query_title = sample(c("Delivery Issue", "Payment Issue", "Purchase Return", "Damaged Product", "Wrong Delivery"), n_queries, replace = TRUE),
  query_submission_date = sample(seq(as.Date('2023-01-01'), as.Date('2023-1-31'), by="day"), n_queries, replace = TRUE),
  query_closure_date = sample(seq(as.Date('2023-02-01'), as.Date('2023-03-31'), by="day"), n_queries, replace = TRUE),
  query_status = sample(c("Closed", "On Progress", "Submitted"), n_queries, replace = TRUE)
)

#Save to .csv file
write.csv(customer_queries_data, "data_uploads/R_synth_customer_queries.csv", row.names = FALSE)

### 'advertisers' table

# advertisers table
n_advertisers <- 5
advertisers_data <- data.frame(
  advertiser_id = sprintf("ADV%d", 1:n_advertisers),
  advertiser_name = c("Ads Life", "Ads Idol", "Ads is Life", "Ads Master", "Ads Expert"),
  adverstiser_email = sprintf("advertiser%d@example.com", 1:n_advertisers)
)
write.csv(advertisers_data, "data_uploads/R_synth_advertisers.csv", row.names = FALSE)

### 'advertisements' table

# advertisements table
n_ads <- 10
advertisements_data <- data.frame(
  ads_id = sprintf("ADS%d", 1:n_ads),
  prod_id = sample(products_data$prod_id, n_ads, replace = TRUE),
  advertiser_id = sample(advertisers_data$advertiser_id, n_ads, replace = TRUE),
  ads_start_date = sample(seq(as.Date('2023-01-01'), as.Date('2023-12-31'), by="day"), n_ads, replace = TRUE),
  ads_end_date = sample(seq(as.Date('2024-01-01'), as.Date('2024-12-31'), by="day"), n_ads, replace = TRUE)
)

#Save to .csv file
write.csv(advertisements_data, "data_uploads/R_synth_advertisements.csv", row.names = FALSE)
