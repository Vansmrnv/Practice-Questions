library(dslabs)
library(tidyverse)
library(ggplot2)
library(dplyr)

# Structure
structure = str(SalesData)

# Identify the top 3 best-selling products based on the quantity sold.

top1topsellingobjects = SalesData$Product[which.max(SalesData$Quantity)]

top3topsellingobjects = SalesData %>% group_by(Product) %>%
  summarise(Top3 = sum(Quantity)) %>% arrange(desc(Top3)) %>% head(3)

#Total revenue

totalRevenue = SalesData %>% group_by(Product) %>%  
  summarise(revenueProduct = Price*Quantity) %>% summarise(revenue = sum(revenueProduct))




# Monthly Trend sales

SalesData$Date = as.Date(SalesData$Date)
SalesData$MonthYear <- format(SalesData$Date, "%Y-%m")
monthlysales = SalesData %>%
  group_by(MonthYear) %>%
  summarise(TotalSales = sum(Price*Quantity))

line_plot = ggplot(monthlysales, aes(x = MonthYear, y = TotalSales))+
  geom_line() +
  xlab('Date')+
  ylab('Sales')+
  ggtitle('Monthly Sales Trend')
print(line_plot)

# Use dplyr to group by customer and calculate the total spending
customer_spending <- SalesData %>%
  group_by(CustomerID) %>%
  summarise(TotalSpending = sum(Price * Quantity))

# Categorize customers into three segments: Low, Medium, High
customer_spending <- mutate(customer_spending, SpendingCategory = cut(TotalSpending,
                                                                      breaks = c(-Inf, quantile(TotalSpending, c(1/3, 2/3)), Inf),
                                                                      labels = c("Low", "Medium", "High")))

print(customer_spending)
# Category-wise Revenue

totalRevenuebycategory = SalesData %>% group_by(Category) %>%  
  summarise(revenueProduct = Price*Quantity)

bar_plot <- ggplot(totalRevenuebycategory, aes(x = Category, y = revenueProduct, fill = Category)) +
  geom_bar(stat = "identity") +
  xlab("Category")+
  ylab("Revenue by Product")+
  theme_minimal()
print(bar_plot)

#Average Price per Category

avg_price_per_category = SalesData %>% group_by(Category) %>% 
  summarise(Avg_Price = mean(Price))
print(avg_price_per_category)


#Top loyal customers

customer_loyalty <-SalesData %>%
  group_by(CustomerID) %>%
  summarise(Totalloyalty = sum(Price * Quantity)) %>%
  arrange(desc(Totalloyalty)) %>%
  head(5)

#Correlation Quantity of Products and total spending
correlation_coefficient <- cor(SalesData$Quantity, SalesData$Price * SalesData$Quantity)

print(paste("Correlation Coefficient: ", round(correlation_coefficient, 3)))



