used_packages <- c("bindrcpp", "data.table", "datasets", "dplyr", 
                   "futile.logger", "grDevices", "graphics", "hms", 
                   "lubridate", "methods", "parallel", "stats", "utils")
lapply(used_packages, require, character.only = TRUE)

flog.info("__author__ : Oğuzhan YEDİEL")
# It was taken in consideration Google R Style Guide 
# for codes to be read easily, shared, and verified!
# For example; Maximum Line Length = 80

restaurant_name=c()
for(i in 1:10) {
  restaurant_name[i] <- paste("Rest", i)
}
restaurant_list <- as.data.frame(restaurant_name)
restaurant_list$restaurant_id <- seq(1:10)
restaurant_list$restaurant_locx <- round(runif(10, min = 0, max = 1), 4)
restaurant_list$restaurant_locy <- round(runif(10, min = 0, max = 1), 4)

customer=c()
for(i in 1:5000) {
  customer[i] <- paste("Customer", i)
}
customer_list <- as.data.frame(customer)
customer_list$customer_id <- seq(1:5000)
customer_list$customer_age <- sample(16:60, 5000, replace = TRUE)
customer_list$customer_gender <- sample(LETTERS[c(6,13)], 5000, replace=TRUE)
customer_list$customer_locx <- round(runif(5000, min = 0, max = 1),4)
customer_list$customer_locy <- round(runif(5000, min = 0, max = 1),4)

staff=c()
for(i in 1:100) {
  staff[i] <- paste("Staff", i)
}
staff_list <- as.data.frame(staff)
staff_list$staff_id <- seq(1:100)
staff_list$restaurant_id <- staff_list$staff_id %% 10
staff_list$restaurant_id <- ifelse(staff_list$restaurant_id == 0, 
                                   10, 
                                   staff_list$restaurant_id)
staff_list$supplier_id <- staff_list$restaurant_id 


supplier_name=c()
for(i in 1:10) {
  supplier_name[i] <- paste("Supplier", i)
}
supplier_list <- as.data.frame(supplier_name)
supplier_list$supplier_id <- seq(1:10)
supplier_list$supplier_locx <- round(runif(10, min = 0, max = 1), 4)
supplier_list$supplier_locy <- round(runif(10, min = 0, max = 1), 4)


product=c()
for(i in 1:100) {
  product[i] <- paste("Product", i)
}
product_list <- as.data.frame(product)
product_list$product_id <- seq(1:100)
product_list$product_unit_price <- sample(1:200, 100, replace = TRUE)
product_list$product_expiration_date <- sample(seq(as.Date('2018/01/01'), 
                                                   as.Date('2018/01/31'), 
                                                   by="day"), 
                                               100, 
                                               replace = TRUE)
food=c()
for(i in 1:100) {
  food[i] <- paste("Food", i)
}
food <- as.data.frame(food)
food$food_id <- seq(1:100)

product_idList <- split(product_list$product_id, f = seq(nrow(product_list)))

for(i in 1:nrow(food)) {
  random_idList = unique(sample(1:nrow(food), sample(5:20)[1], replace = TRUE))
  values = as.numeric(unname(unlist(product_idList[random_idList])))
  names = paste("Product", random_idList)
  food$product_idList[i] <- list(values)
  food$product_nameList[i] <- list(names)
  food$product_amountList[i] <- list(round(as.numeric(runif(length(values), 
                                                            min = 0.1, 
                                                            max = 10)),1))
}

restaurant_food_price = data.frame()
x = c(min(restaurant_list$restaurant_id):max(restaurant_list$restaurant_id))
for(i in 1:max(restaurant_list$restaurant_id)) {
  if(x[i] > 0) {
    restaurant_id <- c(x[i])
    food_id <- c(unique(sample(min(food$food_id):max(food$food_id), 
                               sample(10:20)[1], 
                               replace = TRUE)))
    inter_df <- data.frame(restaurant_id, food_id)
    restaurant_food_price = rbind(restaurant_food_price,inter_df)
  }
}
restaurant_food_price$food_price <- sample(10:100, 
                                           nrow(restaurant_food_price), 
                                           replace = TRUE)

restaurant_stock = data.frame()
for(i in 1:nrow(restaurant_food_price)) {
  restaurant_id <- restaurant_food_price$restaurant_id[i]
  food_id <- restaurant_food_price$food_id[i]
  product_id <- c(food$product_idList[food_id][[1]])  
  inter_df2 <- data.frame(restaurant_id, food_id, product_id)
  restaurant_stock = rbind(restaurant_stock, inter_df2)
}
restaurant_stock$product_stock <- 1000
restaurant_stock$week_number <- 1
restaurant_stock$used_amount <- 0

restaurant_stock <- restaurant_stock[,c("restaurant_id", "product_id", 
                                        "product_stock", "week_number", 
                                        "used_amount")]
restaurant_stock <- unique(restaurant_stock)

customer_visit_count = data.frame(customer_id = c(1:5000), visit_count=c(0))

sell_transaction = data.frame()
for(i in 1:nrow(restaurant_list)) {
  restaurant_id <- restaurant_list$restaurant_id[i]
  restaurant_locx <- restaurant_list$restaurant_locx[i] 
  restaurant_locy <- restaurant_list$restaurant_locy[i]
  for(j in 1:nrow(staff_list)) {
    staff_id <- ifelse(restaurant_id == staff_list$restaurant_id[j], 
                       staff_list$staff_id[j], 
                       'Warning!')
    inter_df3 <- data.frame(restaurant_id, restaurant_locx, 
                            restaurant_locy, staff_id)
    sell_transaction = rbind(sell_transaction, inter_df3)
    sell_transaction = sell_transaction[!sell_transaction$staff_id == 'Warning!',]
  }
}

hav.dist <- function(long1, lat1, long2, lat2) {
  R <- 6371
  diff.long <- (long2 - long1)
  diff.lat <- (lat2 - lat1)
  a <- sin(diff.lat/2)^2 + cos(lat1) * cos(lat2) * sin(diff.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d)
}

flog.info("The function that finds the nearest customers to restaurants.")
customer_selection_cvc <- function() {
  restaurant_loc_list = c()
  for (ind in 1:nrow(sell_transaction)) {
    if ((ind %% 10) == 1){
      restaurant_loc_list <- append(restaurant_loc_list, ind)
    }
  }
  
  rest_cust_min_dist <- data.frame()
  for(i in 1:length(restaurant_loc_list)){
    for(j in 1:nrow(customer_list)){
      restaurant_id <- sell_transaction$restaurant_id[restaurant_loc_list[i]]
      restaurant_locx <- sell_transaction$restaurant_locx[restaurant_loc_list[i]]
      restaurant_locy <- sell_transaction$restaurant_locy[restaurant_loc_list[i]]
      customer_id <- customer_list$customer_id[j]
      customer_locx <- customer_list$customer_locx[j]
      customer_locy <- customer_list$customer_locy[j]
      distance <- hav.dist(sell_transaction$restaurant_locx[restaurant_loc_list[i]], 
                           sell_transaction$restaurant_locy[restaurant_loc_list[i]], 
                           customer_list$customer_locx[j], customer_list$customer_locy[j])
      inter_df4 <- data.frame(restaurant_id, customer_id, restaurant_locx, 
                              restaurant_locy, customer_locx, customer_locy, 
                              distance)
      rest_cust_min_dist = rbind(rest_cust_min_dist, inter_df4)
    }
  }
  
  nearest_customer <<- rest_cust_min_dist %>% 
    group_by(restaurant_id) %>% 
    filter(distance==min(distance))
}
nearest_customer <- customer_selection_cvc()

flog.info("Base Date for Customers.")
customer_visit_count$base_date_in <- as.Date("2017-01-01", origin = "1970-01-01")

recursive_cust_cond <- function() {
  for(i in 1:nrow(nearest_customer)){
    for(j in 1:nrow(customer_visit_count)){
      customer_visit_count$visit_count[j] <<- 
        ifelse(nearest_customer$customer_id[i] == customer_visit_count$customer_id[j],
               ifelse(customer_visit_count$visit_count[j] < 20, 
                      as.numeric(customer_visit_count$visit_count[j]) + 1, 
                      ifelse(nrow(customer_selection_cvc()) == 10, 
                             recursive_cust_cond(), 
                             customer_visit_count$visit_count[j])), 
               customer_visit_count$visit_count[j])
    }
  }
}  
recursive_cust_cond()

date_in_df <- data.frame()
date_in_func <- function() {
  for(i in 1:nrow(nearest_customer)){
      n <- as.Date(
        ifelse(customer_visit_count$visit_count[nearest_customer$customer_id[i]] >= 0, 
               customer_visit_count$base_date_in[nearest_customer$customer_id[i]] + 
                 as.difftime(1, units="days"), 
               customer_visit_count$base_date_in[nearest_customer$customer_id[i]]), 
        origin = "1970-01-01"
        )
      date_in_df <- rbind(date_in_df, n)
      colnames(date_in_df) <- "date_in"
  }
  date_in = as_date(date_in_df$date_in)
  for(r in 1:length(date_in)) {
    nearest_customer$date_in[r] <<- date_in[r]
  }
  nearest_customer$date_in <<- as.Date(nearest_customer$date_in, 
                                       origin = "1970-01-01")
} 
date_in_func()

update_date_in_CVC <- function(){ 
  for(i in 1:nrow(nearest_customer)){
    customer_visit_count$base_date_in[nearest_customer$customer_id[i]] <<- 
      as_date(
        ifelse(nearest_customer$customer_id[i] == 
                 customer_visit_count$customer_id[nearest_customer$customer_id[i]], 
               nearest_customer$date_in[i],
               customer_visit_count$base_date_in[nearest_customer$customer_id[i]]))
  }
}
update_date_in_CVC()

net_nearest_customer <- nearest_customer[,c("restaurant_id","customer_id",
                                            "distance","date_in")]
customers_for_ST <- merge(net_nearest_customer, customer_list, by = "customer_id")
sell_transaction <- merge(sell_transaction, customers_for_ST, by = "restaurant_id")

setDT(sell_transaction)[, table_id := sample(1:10, 1, replace = TRUE), 
                        by = restaurant_id]
setDT(sell_transaction)[, date_reservation := sample(c("True", "False"), 
                                                     1, replace = TRUE), 
                        by = restaurant_id]

food_id_list <- split(restaurant_food_price$food_id, 
                      f = seq(nrow(restaurant_food_price)))
food_for_ST = data.frame()
inter_df5 = data.frame(restaurant_id="")
x = c(min(restaurant_list$restaurant_id):max(restaurant_list$restaurant_id))
for(i in seq_along(x)) {
  if(x[i] > 0) {
    inter_df5$restaurant_id <- c(x[i])
    random_idList_2 <- unique(sample(1:nrow(restaurant_food_price), 
                                     sample(1:5)[1], 
                                     replace = TRUE))
    values_2 <- as.numeric(unname(unlist(food_id_list[random_idList_2])))
    names_2 = paste("Food", food_id_list[random_idList_2])
    inter_df5$food_idList <- list(values_2)
    inter_df5$food_nameList <- list(names_2)
    inter_df5$food_amountOrderedList <- list(sample(sample(1:5), 
                                                    size = length(values_2), 
                                                    replace = TRUE))
    food_for_ST = rbind(food_for_ST,inter_df5)
  }
}

empty_list = list()
for(i in 1:length(food_for_ST$restaurant_id)) {
  for(j in 1:length(food_for_ST$food_idList[i][[1]])) {
    n <- food_for_ST$food_idList[i][[1]][j] * food_for_ST$food_amountOrderedList[i][[1]][j]
    if(j == 1){
      empty_list[[1]] <- n
    }
    else{
      empty_list[[1]][j] <- n
    }
  }
  food_for_ST$food_priceList[i] <- empty_list
}

sell_transaction <- merge(sell_transaction, food_for_ST, by = "restaurant_id")

i_list = c()
for (ind in 1:nrow(sell_transaction)) {
  if((ind %% 10) == 1) {
    i_list <- append(i_list, ind)
  }
}

j_list = unique(restaurant_stock$restaurant_id)

flog.info("The loop that finds foods in sell-transaction:")
foods_in_ST = data.frame()
for(i in j_list){
  foods_in_ST_pre <- food[food$food_id%in%sell_transaction$food_idList[i_list[i]][[1]],]
  foods_in_ST_pre$restaurant_id <- i
  foods_in_ST_pre$food_amountOrdered <- food_for_ST$food_amountOrderedList[[i]]
  foods_in_ST <- rbind(foods_in_ST, foods_in_ST_pre)
}

foods_in_ST$intcode <- 1:nrow(foods_in_ST)
foods_in_ST <- foods_in_ST[,c(8,6,1,2,3,4,5,7)]
rownames(foods_in_ST) <- NULL

flog.info("Product information of foods in sell-transaction, such as id, amount etc.")
temp_products_in_ST = data.frame()
for(i in 1:nrow(foods_in_ST)){
  foods_in_restaurant_id = as.data.frame(foods_in_ST$restaurant_id[i])
  foods_in_food_id = as.data.frame(foods_in_ST$food_id[i])
  foods_in_food_amountOrdered = as.data.frame(foods_in_ST$food_amountOrdered[i])
  foods_in_products_id = as.data.frame(unlist(foods_in_ST$product_idList[[i]]))
  foods_in_products_amount = as.data.frame(unlist(foods_in_ST$product_amountList[[i]]))
  inter_df6 = cbind.data.frame(foods_in_restaurant_id, foods_in_food_id, 
                               foods_in_food_amountOrdered, foods_in_products_id, 
                               foods_in_products_amount)
  temp_products_in_ST = rbind(temp_products_in_ST, inter_df6)
}

colnames(temp_products_in_ST) <- c("restaurant_id","food_id",
                                   "food_amountOrdered","product_id",
                                   "product_amount")
temp_products_in_ST <- temp_products_in_ST[,c(1,2,4,5,3)]

temp_table <- merge(temp_products_in_ST, restaurant_stock, 
                    by=c("restaurant_id","product_id"), all.x = TRUE)
temp_table$week_number <- NULL
temp_table$used_amount <- NULL
temp_table$product_stock <- ifelse(is.na(temp_table$product_stock) == FALSE, 
                                   temp_table$product_stock, 
                                   0)
temp_table$condition <- 
  ifelse(temp_table$product_stock >= 
           (temp_table$product_amount * temp_table$food_amountOrdered), 
         1, 
         0)
temp_table$updated_product_stock <- 
  ifelse(temp_table$condition == 1, 
         temp_table$product_stock - 
           (temp_table$product_amount * temp_table$food_amountOrdered),
         0)

for_updated_stock <- temp_table[,c("restaurant_id","product_id",
                                   "updated_product_stock")]
restaurant_stock <- merge(restaurant_stock, for_updated_stock, 
                          by=c("restaurant_id","product_id"), 
                          all.x = TRUE)
restaurant_stock$updated_product_stock <- 
  ifelse(is.na(restaurant_stock$updated_product_stock) == TRUE, 
         restaurant_stock$product_stock,
         restaurant_stock$updated_product_stock)
restaurant_stock$used_amount <- 
  restaurant_stock$product_stock - restaurant_stock$updated_product_stock
restaurant_stock$week_number <- restaurant_stock$week_number + 1
restaurant_stock$product_stock <- NULL
colnames(restaurant_stock) <- c("restaurant_id", "product_id",
                                "week_number", "used_amount", 
                                "product_stock")
restaurant_stock <- restaurant_stock[,c(1,2,5,3,4)]
