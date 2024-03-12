library(RSQLite)

# Load the data
print("Loading the data")
data_files <- list.files("data_uploads", pattern = "MOCK_DATA_PRODUCTS", full.names = TRUE)
data_to_write <- data.frame()
# for each csv file 
for (file in data_files) {
  print(paste("Reading file:",file))
  this_file_rows <- read.csv(file, stringsAsFactors = FALSE)
  data_to_write <- rbind(data_to_write, this_file_rows)
}
print("Write them to the database")

# 检查data_to_write是否为空
if(nrow(data_to_write) == 0) {
  stop("data_to_write is empty.")
}

# 检查data_to_write的结构
print(str(data_to_write))

# 尝试连接数据库
connection <- RSQLite::dbConnect(RSQLite::SQLite(), "database/products.db")
if(is.null(connection)) {
  stop("Failed to connect to the database.")
}

# 写入数据前的最后检查
if(!dbExistsTable(connection, "products")) {
  print("Table 'products' does not exist. It will be created.")
} else if(overwrite) {
  print("Table 'products' will be overwritten.")
}

# 执行写入操作
RSQLite::dbWriteTable(connection, "products", data_to_write, overwrite = TRUE)

# 断开连接
RSQLite::dbDisconnect(connection)
print("Done!")

#connection <- RSQLite::dbConnect(RSQLite::SQLite(), "database/products.db")
#RSQLite::dbWriteTable(connection, "products", data_to_write, overwrite = TRUE)
#RSQLite::dbDisconnect(connection)
#print("Done!")
