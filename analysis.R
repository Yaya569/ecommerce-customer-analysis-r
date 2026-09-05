# E-commerce Customer Analysis
# Data cleaning, customer analysis and exploratory analysis

# Load data
df_purchase <- read.csv("purchases.csv")

# Inspect the dataset
str(df_purchase)
head(df_purchase)

# Check missing values
missing_proportion <- colMeans(is.na(df_purchase))
print(missing_proportion)

# Handle missing gender values
df_purchase$Gender <- ifelse(
  is.na(df_purchase$Gender),
  "Others",
  df_purchase$Gender
)

# Create age groups
df_purchase$AgeGroup <- cut(
  df_purchase$Age,
  breaks = c(-Inf, 20, 30, 40, 50, 60, Inf),
  labels = c("<20", "20-30", "30-40", "40-50", "50-60", ">60"),
  right = FALSE
)

df_purchase$AgeGroup <- as.character(df_purchase$AgeGroup)
df_purchase$AgeGroup[is.na(df_purchase$AgeGroup)] <- "Unknown"

# Create total payable amount
df_purchase$TotalPayable <-
  df_purchase$PurchaseAmount +
  df_purchase$InCartAmount

# Average purchase amount by gender
average_purchase_gender <- aggregate(
  PurchaseAmount ~ Gender,
  data = df_purchase,
  FUN = mean
)

print(average_purchase_gender)

# Total in-cart amount by age group
total_in_cart_age <- aggregate(
  InCartAmount ~ AgeGroup,
  data = df_purchase,
  FUN = sum
)

print(total_in_cart_age)

# Top 5 customers by total payable amount
top_customers <- df_purchase[
  order(-df_purchase$TotalPayable),
  c("CustomerID", "PurchaseAmount", "InCartAmount", "TotalPayable")
]

print(head(top_customers, 5))

# Purchase amount by country
boxplot(
  PurchaseAmount ~ Country,
  data = df_purchase,
  main = "Purchase Amount by Country",
  xlab = "Country",
  ylab = "Purchase Amount ($)",
  las = 2
)

grid(
  nx = NA,
  ny = NULL,
  lty = "dotted"
)
