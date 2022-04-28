#Project_3_Ecommerce

raw.data = read.csv(choose.files())

data <- raw.data #Take the backup of the data

str(data)

############################################################################################
#Step 1 : Remove unwanted columns and rows from the dataset
#       : There is unwanted extra column with name "X" with all the blank rows
#       : There are 135080 rows with blank customer IDs
#       : Since we are analyzing UK based stores; remove(361878) rest of the countries
#       : There are few entries with negative unit prices, which are due to return of the 
#       :    product most of them are from last years so remove those entries from analysis
############################################################################################

data$X = NULL

colSums(is.na(data)) # This will give the blank rows - remove all the rows with blank custID
data = subset(data, !is.na(data$CustomerID))

table(data$Country != "United Kingdom")
data <- subset(data, Country == "United Kingdom")

data$return_invoice = grepl(pattern = "C", x = data$InvoiceNo)
data$purchase_invoice <- ifelse(data$return_invoice=="TRUE", 0, 1)
#########################################################################################

############################################################################################
#Step 2 : Add some additional features so that it helps in identifying the significant customers
#       : Add column recency to identify the number of days passed after last visit
#       : Add a column frequency to identify how freqent customer visits the store
#       : create feature invoice_amout using quantity and unit price
############################################################################################

data$recency <- as.Date("2017-12-8") - as.Date(data$InvoiceDate, "%d-%b-%y")
data$recency = as.integer(data$recency)

data$invoice_amout <- data$Quantity * data$UnitPrice

install.packages("dplyr")
library(dplyr)


temp <- subset(data, purchase_invoice == 1)
df_customers = summarise(group_by(.data = temp, CustomerID), recency = min(recency), frequency = length(unique(InvoiceNo)))

inv_amt = summarise(group_by(.data = data, CustomerID), invoice_amout = sum(invoice_amout))

df_customers = merge(df_customers, inv_amt, by = "CustomerID",  all.x=TRUE, sort=TRUE)

remove(inv_amt, temp)

df_customers$invoice_amout <- ifelse(df_customers$invoice_amout <= 0, 0.1, df_customers$invoice_amout)

#########################################################################################


############################################################################################
#Step 3 : customer data frame looks to be right skewed, scale the data first using logs
############################################################################################

boxplot(x = df_customers)

df_customers$log_recency = log(df_customers$recency)
df_customers$log_frequency = log(df_customers$frequency)
df_customers$log_inv_amount = log(df_customers$invoice_amout)


df_customers$recency.z <- scale(df_customers$log_recency, center=TRUE, scale=TRUE)
df_customers$frequency.z <- scale(df_customers$log_frequency, center=TRUE, scale=TRUE)
df_customers$inv_amount.z <- scale(df_customers$log_inv_amount, center=TRUE, scale=TRUE)

#########################################################################################

############################################################################################
#Step 4 : Execute k-means cluster algorithm in loop for k=10 and identify the best 
#         cluster size
############################################################################################

k = 10
df_Rsquared = data.frame(Centers = integer(),
                         Rsquared=integer())

for (i in (1:k))
{
  kmeans(df_customers[, c(8,9,10)], centers = i, nstart = 20)-> dfCluster
  Rsquared = round(dfCluster$betweenss/ dfCluster$totss,digits = 4)
  cur_row = data.frame(i, Rsquared)
  df_Rsquared = rbind(df_Rsquared, cur_row)
  output = paste("Centers = ", i, " R-squared = ", Rsquared)
 
  df_cluster_centers = data.frame(Recency = integer(),
                                  Frequency = integer(),
                                  Invoice_amount = integer())
  
  for (j in (1:i))
  {
    
    cur_row = summarise(.data = df_customers[,c(2,3,4)][dfCluster$cluster == j, ], 
                        Recency = median(recency), 
                        Frequency = median(frequency), 
                        Invoice_amount = median(invoice_amout))
    
    df_cluster_centers = rbind(df_cluster_centers, cur_row)
  }
  print(output)
  print(df_cluster_centers)
  cat("\n")
} 

plot(x = df_Rsquared$i, y = df_Rsquared$Rsquared, type = "l", xlab = "K - Centers", ylab = "RSquared")

remove(cur_row, df_cluster_centers, df_Rsquared, dfCluster, i, j, k, output, Rsquared)
############################################################################################
#Conclusion : From the graph 2 clusters looks to be the best case.
############################################################################################

############################################################################################
#hierarchical clustering
############################################################################################

library(cluster)

dist(df_customers[,c(8,9,10)])-> df_scaled

clust = hclust(d = df_scaled, method = "ward.D")
clust

plot(as.dendrogram(clust))

rect.hclust(tree = clust,k = 5)

k = cutree(clust, 5)



aggregate(df_customers[,c(2,3,4)], by = list(k), median)

############################################################################################
#Looking at the graph 5 clusters seems to be best case.
############################################################################################

