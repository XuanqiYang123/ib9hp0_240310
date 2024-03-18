# load packages
library(readxl)
library(DBI)
library(RSQLite)


new_order_details <- read_excel("data_update/new_data_202401.xlsx")

excel_numbers <- gsub("\\D", "", tools::file_path_sans_ext(basename("data_update/new_data_202401.xlsx")))
log_file_path <- sprintf("data_update/insertion_error_log_%s.txt", excel_numbers)


write_log <- function(message, path) {
  write(message, file = path, append = TRUE, sep = "\n")
}


db_connection <- RSQLite::dbConnect(RSQLite::SQLite(),"IB9HP0_9.db")

#loop
for(i in 1:nrow(new_order_details)) {
  if(is.na(new_order_details$order_id[i]) || new_order_details$order_id[i] == "") {
    error_message <- sprintf("The orderid is missing in row %d.\n", i)
    write_log(error_message, log_file_path)
  } else {
    tryCatch({
      # insert data
      dbExecute(db_connection, paste0(
        "INSERT INTO order_details (order_quantity, order_date, order_price, order_value, prod_id, order_id) VALUES (",
        new_order_details$order_quantity[i], ", '",
        new_order_details$order_date[i], "', ",
        new_order_details$order_price[i], ", ",
        new_order_details$order_value[i], ", '",
        new_order_details$prod_id[i], "', '",
        new_order_details$order_id[i], "')"
      ))
      write_log(sprintf("Row %d inserted successfully.\n", i), log_file_path)
    }, error = function(e) {
      
      write_log(sprintf("Failed to insert row %d: %s\n", i, e$message), log_file_path)
    })
  }
}


# dbDisconnect(db_connection)
