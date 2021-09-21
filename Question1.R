library(dplyr)
library(ggplot2)

######################################## Question 1 #####################
#
# First I load the data into sales variable
# I used readr::read_csv as it is usually the faster
#

sales <- readr::read_csv('2019 winter Data Science Intern Challenge Data Set.csv')

# browse the data
head(sales)

########## part a ################

#
# Naive AOV: Calculate Average of order_amount to get 3145.128
# 

mean(sales$order_amount)

#
# The problem is, there are outliers. 
# From the summary I can see there are extremely high-priced sneakers
#
summary(sales$order_amount)

# histogram chart shows it clearly
hist(sales$order_amount)

# ggplot histogram looks better
ggplot(sales,aes(order_amount)) + geom_histogram()

# The problem is with the large orders from shop 42 and 78 
sales %>% select(shop_id, order_amount, total_items) %>% unique() %>% arrange(desc(order_amount)) %>% print(n=30)

# from the data, one can observe two outlier patterns
#  1. there are orders with may items (shop 42, user 607 with 2000 items)
#  2. highly priced items (shop 78)

########## part b ################

# in this case, we'd better use more robust measures such as median 
# as median would not be impacted by the presence of outliers.
# b1. one can report median of order amount
# b2. we may take average EXCLUDING shops 42 and 78 as these two shops looks different from the others.
#   say shop 42 (2000 orders from user 607) looks like a reseller or a wholesale seller
#   and shop 78 (priced at 30K per an item) sells high-priced luxury sneakers.
# b3. we may first take median of each shops, then take median of the shop-medians.
#
# Among the three I think b2 would be the best estimate for 'cheap' sneakers to 'ordinary' buyers.
#

######### part c #################

# c1. median order value is $284.
median(sales$order_amount)

# c2. mean value except for the two shops $300.16
mean(sales$order_amount[ sales$shop_id != 42 & sales$shop_id != 78])

# c3. first calculate aggregates by shop_id

sales %>% group_by(shop_id) %>% 
  summarize(orders = length(order_id), aov = median(order_amount)) -> summary.shop

# c3. median-of-meidans is $306

median(summary.shop$aov)
