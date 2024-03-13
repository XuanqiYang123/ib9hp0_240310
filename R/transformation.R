library(RSQLite)

# Load the data
print("Loading the data")
data_files <- list.files("data_uploads", pattern = "R_synth_products.csv", full.names = TRUE)
data_to_write <- data.frame()
# for each csv file 
for (file in data_files) {
  print(paste("Reading file:",file))
  this_file_rows <- read.csv(file, stringsAsFactors = FALSE)
  data_to_write <- rbind(data_to_write, this_file_rows)
}
print("Write them to the database")


connection <- RSQLite::dbConnect(RSQLite::SQLite(), "database/products.db")
RSQLite::dbWriteTable(connection, "products", data_to_write, append = TRUE)
RSQLite::dbDisconnect(connection)
print("Done!")
