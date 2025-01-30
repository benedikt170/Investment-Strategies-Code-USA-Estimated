# Install and load necessary libraries
install.packages("readxl")
library(readxl)
library(plm)

# File paths for input data
file_path1 <- "scope1.xlsx"
file_path2 <- "scope2.xlsx"
file_path3 <- "scope3.xlsx"
file_path4 <- "price.xlsx"

# Read data from Excel files
scope1 <- read_excel(file_path1)
scope2 <- read_excel(file_path2)
scope3 <- read_excel(file_path3)
p <- read_excel(file_path4)

# Prepare data ------------------------------------------------------------

# Calculate log returns for price data
r <- p
r[,3:11] <- NA
for(i in 3:10) r[,i] <- log(p[,i]/p[,i+1])
rm(p)

# Keep only the common "ticks" across all datasets
ticks <- Reduce(intersect, list(r$tick, scope1$tick, scope2$tick, scope3$tick))
r <- r[which(r$tick %in% ticks),]
scope1 <- scope1[which(scope1$tick %in% ticks),]
scope2 <- scope2[which(scope2$tick %in% ticks),]
scope3 <- scope3[which(scope3$tick %in% ticks),]

# Save row names for further processing
names <- r$name

# Function to reshape data into long format for panel data analysis
prep <- function(df){
  df_name <- deparse(substitute(df))
  print(df_name)
  rownames(df) <- ticks
  df$tick <- df$name <- NULL
  df <- reshape2::melt(as.matrix(df))
  colnames(df) <- c("tick", "year", df_name)
  df$year <- as.numeric(stringr::str_remove(df$year, "X"))
  df[,3] <- as.numeric(as.character(df[,3]))
  return(df)  
}

# Reshape all datasets into long format
scope1 <- prep(scope1)
scope2 <- prep(scope2)
scope3 <- prep(scope3)
r <- prep(r)

# Merge scope variables and return data into a single dataframe
df <- r
df$scope1 <- scope1$scope1
df$scope2 <- scope2$scope2
df$scope3 <- scope3$scope3

# Remove rows with missing or invalid values
df <- df[which(rowSums(is.na(df[,4:6])) < 3), ]
df <- df[which(!is.na(df$r)), ]

# Apply log transformation to scope variables ----------------------------

# Create new columns with log-transformed scope variables
df$log_scope1 <- log(df$scope1)
df$log_scope2 <- log(df$scope2)
df$log_scope3 <- log(df$scope3)

# Remove rows with infinite or NA values caused by log transformation
df <- df[!is.infinite(df$log_scope1) & !is.infinite(df$log_scope2) & !is.infinite(df$log_scope3), ]

# Panel regression --------------------------------------------------------

# Convert to panel data format
panel_df <- pdata.frame(df, index = c("tick", "year"))

# Perform fixed effects panel regression with log-transformed variables
mdl <- plm(r ~ log_scope1 + log_scope2 + log_scope3, data = panel_df, model = "within")

# Output summary statistics for the data and model
summary(df)
summary(mdl)
