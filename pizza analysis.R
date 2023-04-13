### I will be doing a data analysis on pizza sales.
### I am interested in answering some questions which
### wiil help the owner of the pizza store to adjust accordingly.

### The first step is to install necessary packages that will help in data analysis
install.packages('tidyverse')
install.packages('ggplott2')
install.packages('ggthemes')
install.packages('lubridate')
install.packages('DT')
install.packages("TSA")
install.packages("scales")

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(DT)
library(TSA)
library(scales)

# let us import the data for analysis
orders <- read.csv("C:/Users/Dell E5470/Desktop/pizzza dataset/orders.csv", header = TRUE)
order_details <- read.csv("C:/Users/Dell E5470/Desktop/pizzza dataset/order_details.csv", header = TRUE)
pizzas <- read.csv("C:/Users/Dell E5470/Desktop/pizzza dataset/pizzas.csv", header = TRUE)
pizza_types <- read.csv("C:/Users/Dell E5470/Desktop/pizzza dataset/pizza_types.csv", header = TRUE)


# lets confirm that the datasets have been imported successfully.
View(orders)
view(order_details)
view(pizzas)
view(pizza_types)


# lets us check the data types in each datasets
summary(orders)
summary(order_details)
summary(pizzas)
summary(pizza_types)

str(orders)
str(order_details)
str(pizzas)
str(pizza_types)

### date and time in order details needs to be chnaged from character to date and time
head(orders)
orders$date<-strptime(orders$date,"%Y-%m-%d")
orders$time<- strptime(orders$time,"%H:%M:%S")
summary(orders)
orders$time<-format(as.POSIXct(orders$time, format="%m/%d/%Y %H:%M:%S"),format = "%H:%M:%S")
orders$date<-as.POSIXct(orders$date, format="%m/%d/%Y %H:%M:%S")

## let us analyze the data 
# how many customers do we have each day?
# let me first extract the day
orders$day<- format(day(orders$date))
summary(orders)
head(orders)

daily_customers<- orders %>%
  group_by(date) %>%
  summarize(num_customers = n_distinct(order_id))

datatable(daily_customers)


# which hour during the day has the most customers
# first extract hours

orders$hour<- factor(hour(hms(orders$time)))

peak_hours<- orders %>%
  group_by(hour) %>%
  summarise(Total=n())

datatable(peak_hours)

ggplot(peak_hours, aes(hour, Total))+
  geom_bar(stat="identity" ,fill="Yellow", color="red")+
  ggtitle("peak hours")


## How many pizzas are there in an order

No_of_pizza_orders<-order_details %>%
  group_by(order_id) %>%
  summarise(count=n())

datatable(No_of_pizza_orders)


# which pizza is the best seller
# we Will first merge tow data sets- order_details and pizzas

orders_details_pizza <- merge(order_details, pizzas, by="pizza_id")
view(orders_details_pizza)

best_selling_pizza <- orders_details_pizza %>% 
  group_by(pizza_id) %>% 
  summarise(total_sales = sum(quantity), total_price = sum(quantity * price)) %>% 
  arrange(desc(total_price))  
  

datatable(best_selling_pizza)

## how much many did the pizza company make in the year?
total_revenue <- orders_pizza %>%
  mutate(total_price = price * quantity) %>%
  summarise(yearly_revenue = sum(total_price))

total_revenue


## is there any seasonality in the sales of pizza?
## We first merge order_details_pizza and orders table
orders_pizza<- merge(orders_details_pizza, orders, by= "order_id")
view(orders_pizza)

orders_pizza$sales<-(orders_pizza$quantity * orders_pizza$price )

## lets plot the pizza sales over time
ggplot(orders_pizza, aes(x = date, y = sales)) +
  geom_line() +
  ggtitle("Pizza sales over time")

## which is the preferred size of pizza. - pizza size with most sales

preferred_pizza_size <- orders_pizza %>% 
  group_by(size) %>% 
  summarise(total_sales = sum(quantity), total_price = sum(quantity * price)) %>% 
  arrange(desc(total_price))

datatable(preferred_pizza_size)

ggplot(preferred_pizza_size, aes(x = size, y = total_price)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Pizza Sales by Size",
       x = "Pizza Size",
       y = "Total Sales (in USD)") +
  scale_y_continuous(labels = label_comma())+
  theme_minimal()

### which is the preferred pizza type
preferred_pizza_type <- orders_pizza %>% 
  group_by(pizza_type_id) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))

datatable(preferred_pizza_type)











