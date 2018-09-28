#install.packages('tidyverse')
#### https://towardsdatascience.com/a-gentle-introduction-on-market-basket-analysis-association-rules-fa4b986a40ce


library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)


retail <- read_excel('OnlineRetail.xlsx')
retail <- retail[complete.cases(retail), ]
retail <- retail %>% mutate(Description = as.factor(Description))
retail <- retail %>% mutate(Country = as.factor(Country))
retail$Date <- as.Date(retail$InvoiceDate)
retail$Time <- format(retail$InvoiceDate,"%H:%M:%S")
retail$InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))
glimpse(retail)


#### What time do people often purchase online?

retail$Time <- as.factor(retail$Time)
a <- hms(as.character(retail$Time))
retail$Time = hour(a)
retail %>% 
  ggplot(aes(x=Time)) + 
  geom_histogram(stat="count",fill="indianred")


##### How many items each customer buy?

detach("package:plyr", unload=TRUE)
retail %>% 
  group_by(InvoiceNo) %>% 
  summarize(n_items = mean(Quantity)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(fill="indianred", bins = 100000) + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))

#### Top 10 best sellers

tmp <- retail %>% 
  group_by(StockCode, Description) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
tmp <- head(tmp, n=10)
tmp
tmp %>% 
  ggplot(aes(x=reorder(Description,count), y=count))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()



##### Before using any rule mining algorithm, we need to transform the data
##### from the data frame format, into transactions such that we have all the items
##### bought together in one row.

retail_sorted <- retail[order(retail$CustomerID),]
library(plyr)
itemList <- ddply(retail,c("CustomerID","Date"), 
                  function(df1)paste(df1$Description, 
                                     collapse = ","))


itemList$CustomerID <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("items")

write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = TRUE)


tr <- read.transactions('market_basket.csv', format = 'basket', sep=',')
tr
summary(tr)



itemFrequencyPlot(tr, topN=20, type='absolute')


### Creating rules

## We use the Apriori algorithm in Arules library to mine frequent itemsets and 
## association rules. The algorithm employs level-wise search for frequent itemsets.

## We pass supp=0.001 and conf=0.8 to return all the rules that have a support of at least
## 0.1% and confidence of at least 80%.


rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)

inspect(rules[1:10])

topRules <- rules[1:10]
plot(topRules)

plot(topRules, method="graph")

plot(topRules, method = "grouped")



