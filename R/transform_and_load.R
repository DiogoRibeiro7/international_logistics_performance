library(readxl)
library(openxlsx)
library(dplyr)

# Specify the path to your Excel file
file_path <- "International_LPI_from_2007_to_2023.xlsx"

# Get the names of all sheets in the workbook
sheet_names <- excel_sheets(file_path)

# Initialize an empty data frame to store the combined data
combined_data <- data.frame()

# Loop through each sheet name
for (sheet in sheet_names) {
  # Read the sheet
  sheet_data <- read_excel(file_path, sheet = sheet)

  # Add a new column 'Year' with the sheet name (assuming the sheet name is the year)
  # If the sheet name is not exactly the year or you need to extract it, adjust this accordingly
  sheet_data$Year <- sheet

  # Combine the data from this sheet with the previously combined data
  combined_data <- bind_rows(combined_data, sheet_data)
}

# View the combined data frame
print(combined_data)

# Optional: Write the combined data to a new Excel file
write.xlsx(combined_data, "Combined_Data.xlsx")
