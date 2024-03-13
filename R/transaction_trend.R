library(RSQLite)
library(ggplot2)
library(dplyr)

# link to database
db_path <- "database/orders.db"
connection <- dbConnect(SQLite(), db_path)

# extract data
query <- 'SELECT order_date, order_value FROM "order"'
orders_data <- dbGetQuery(connection, query)

# disconnect the database
dbDisconnect(connection)

# process order
orders_data$order_date <- as.Date(orders_data$order_date)
daily_totals <- orders_data %>%
  group_by(order_date) %>%
  summarise(total_value = sum(order_value))

# plot
plot <- ggplot(daily_totals, aes(x=order_date, y=total_value)) +
  geom_line() +
  labs(title="Daily Transaction Value", x="Date", y="Total Transaction Value")

# 保存图表到文件
output_folder <- "transaction_charts"
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}
output_file_path <- file.path(output_folder, "daily_transaction_value.png")
ggsave(output_file_path, plot, width=10, height=6)

cat("Chart saved to:", output_file_path)
