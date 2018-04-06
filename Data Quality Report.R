library(ggplot2)
library(dplyr)
library(readxl)

# Import data file
carddata = read_excel("card transactions.xlsx")
head(carddata)

# Variable types
sapply(carddata,class)

# Number of unique values
nrow(unique(carddata[1]))
nrow(unique(carddata[2]))
nrow(unique(carddata[3]))
nrow(unique(carddata[4]))
nrow(unique(carddata[5]))
nrow(unique(carddata[6]))
nrow(unique(carddata[7]))
nrow(unique(carddata[8]))
nrow(unique(carddata[9]))
nrow(unique(carddata[10]))

# Number of missing values
sum(is.na(carddata$`Merch Description`))
sum(is.na(carddata$`Merchant State`))
sum(is.na(carddata$Merchantnum))
sum(is.na(carddata$`Merchant Zip`))


# Transform date into data format
carddata$Date = as.Date(carddata$Date, "%y-%m-%d")

# Card Number Distribution
carddata %>%
  group_by(Cardnum) %>%
  summarise(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(reorder(Cardnum,count), count)) +
  geom_bar(stat = "identity", fill= "lightblue") +
  coord_flip() +
  ylab("Number of Card Transactions") +
  xlab("Card Number") +
  ggtitle("Top 20 Card Numbers with Highest Transaction Count") +
  theme_bw() +
  theme(panel.border = element_blank()) +
  geom_text(aes(label=count), size = 3.4, hjust = -0.05)

# Card Number Distribution - Fraud
carddata %>%
  filter(Fraud == 1) %>%
  group_by(Cardnum) %>%
  summarise(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(reorder(Cardnum,count), count)) +
  geom_bar(stat = "identity", fill= "lightblue") +
  coord_flip() +
  ylab("Number of Fraudulent Transactions") +
  xlab("Card Number") +
  ggtitle("Top 20 Card Numbers with Highest Fraudulent Transaction Count") +
  theme_bw() +
  theme(panel.border = element_blank()) +
  geom_text(aes(label=count), size = 3.4, hjust = -0.05)

# Date Distribution
carddata %>%
  group_by(Date) %>%
  summarise(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(reorder(Date,count), count)) +
  geom_bar(stat = "identity", fill= "lightblue") +
  coord_flip() +
  ylab("Number of Card Transactions") +
  xlab("Date") +
  ggtitle("Top 20 Dates with Highest Transaction Count") +
  theme_bw() +
  theme(panel.border = element_blank()) +
  geom_text(aes(label=count), size = 3.4, hjust = -0.05)

# Date Distribution - Fraud
carddata %>%
  filter(Fraud == 1) %>%
  group_by(Date) %>%
  summarise(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(reorder(Date,count), count)) +
  geom_bar(stat = "identity", fill= "lightblue") +
  coord_flip() +
  ylab("Number of Fraudulent Transactions") +
  xlab("Date") +
  ggtitle("Top 20 Dates with Highest Fraudulent Transaction Count") +
  theme_bw() +
  theme(panel.border = element_blank()) +
  geom_text(aes(label=count), size = 3.4, hjust = -0.05)

# Date Distribution by Month
library(lubridate)
carddata2 = mutate(carddata,  mon = month(as.Date(Date, "%y-%m-%d")))
carddata2 = mutate(carddata2, monabb = month.abb[mon])

carddata2 %>%
  group_by(monabb) %>%
  summarise(count=n()) %>%
  ggplot(aes(factor(monabb, levels = month.abb), count)) +
  geom_bar(stat = "identity", fill= "lightblue") +
  ylab("Number of Card Transactions") +
  xlab("Month") +
  ggtitle("Transaction Count by Month in the Year of 2010") +
  theme_bw() +
  theme(panel.border = element_blank()) +
  geom_text(aes(label=count), size = 3.4, vjust = 0.08)


carddata2 %>%
  filter(Fraud == 1) %>%
  group_by(monabb) %>%
  summarise(count=n()) %>%
  ggplot(aes(factor(monabb, levels = month.abb), count)) +
  geom_bar(stat = "identity", fill= "lightblue") +
  ylab("Number of Fraudulent Card Transactions") +
  xlab("Month") +
  ggtitle("Fraudulent Transaction Count by Month in the Year of 2010") +
  theme_bw() +
  theme(panel.border = element_blank()) +
  geom_text(aes(label=count), size = 3.4, vjust = 0.08)


# Merchantnum Distribution
carddata %>%
  filter(!is.na(Merchantnum)) %>%
  group_by(Merchantnum) %>%
  summarise(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(reorder(Merchantnum,count), count)) +
  geom_bar(stat = "identity", fill= "lightblue") +
  coord_flip() +
  ylab("Number of Card Transactions") +
  xlab("Merchant Number") +
  ggtitle("Top 20 Merchant Number with Highest Transaction Count") +
  theme_bw() +
  theme(panel.border = element_blank()) +
  geom_text(aes(label=count), size = 3.4, hjust = -0.05)

# Merchantnum Distribution - Fraud
carddata %>%
  filter(!is.na(Merchantnum)) %>%
  filter(Fraud == 1) %>%
  group_by(Merchantnum) %>%
  summarise(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(reorder(Merchantnum,count), count)) +
  geom_bar(stat = "identity", fill= "lightblue") +
  coord_flip() +
  ylab("Number of Fraudulent Card Transactions") +
  xlab("Merchant Number") +
  ggtitle("Top 20 Merchant Number with Highest Fraudulent Transaction Count") +
  theme_bw() +
  theme(panel.border = element_blank()) +
  geom_text(aes(label=count), size = 3.4, hjust = -0.05)



# Merchant Description Distribution
carddata %>%
  filter(!is.na(`Merch Description`)) %>%
  group_by(`Merch Description`) %>%
  summarise(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(reorder(`Merch Description`,count), count)) +
  geom_bar(stat = "identity", fill= "lightblue") +
  coord_flip() +
  ylab("Number of Card Transactions") +
  xlab("Merchant Description") +
  ggtitle("Top 20 Merchant Names with Highest Transaction Count") +
  theme_bw() +
  theme(panel.border = element_blank()) +
  geom_text(aes(label=count), size = 3.4, hjust = -0.05)

# Merchantnum Distribution - Fraud
carddata %>%
  filter(Fraud == 1) %>%
  filter(!is.na(`Merch Description`)) %>%
  group_by(`Merch Description`) %>%
  summarise(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(reorder(`Merch Description`,count), count)) +
  geom_bar(stat = "identity", fill= "lightblue") +
  coord_flip() +
  ylab("Number of Fraudulent Card Transactions") +
  xlab("Merchant Description") +
  ggtitle("Top 20 Merchant Names with Highest Fraudulent Transaction Count") +
  theme_bw() +
  theme(panel.border = element_blank()) +
  geom_text(aes(label=count), size = 3.4, hjust = -0.05)



# Merchant State Distribution
carddata %>%
  filter(!is.na(`Merchant State`)) %>%
  group_by(`Merchant State`) %>%
  summarise(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(reorder(`Merchant State`,count), count)) +
  geom_bar(stat = "identity", fill= "lightblue") +
  coord_flip() +
  ylab("Number of Card Transactions") +
  xlab("State") +
  ggtitle("Top 20 Merchant States with Highest Transaction Count") +
  theme_bw() +
  theme(panel.border = element_blank()) +
  geom_text(aes(label=count), size = 3.4, hjust = -0.05)

# Merchant State Distribution - Fraud
carddata %>%
  filter(!is.na(`Merchant State`)) %>%
  filter(Fraud == 1) %>%
  group_by(`Merchant State`) %>%
  summarise(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(reorder(`Merchant State`,count), count)) +
  geom_bar(stat = "identity", fill= "lightblue") +
  coord_flip() +
  ylab("Number of Fraudulent Card Transactions") +
  xlab("State") +
  ggtitle("Top 20 Merchant States with Highest Fraudulent Transaction Count") +
  theme_bw() +
  theme(panel.border = element_blank()) +
  geom_text(aes(label=count), size = 3.4, hjust = -0.05)


# Merchant Zip Distribution
carddata %>%
  filter(!is.na(`Merchant Zip`)) %>%
  group_by(`Merchant Zip`) %>%
  summarise(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(reorder(`Merchant Zip`,count), count)) +
  geom_bar(stat = "identity", fill= "lightblue") +
  coord_flip() +
  ylab("Number of Card Transactions") +
  xlab("Zip Code") +
  ggtitle("Top 20 Merchant Zip Code with Highest Transaction Count") +
  theme_bw() +
  theme(panel.border = element_blank()) +
  geom_text(aes(label=count), size = 3.4, hjust = -0.05)

# Merchant Zip Distribution - Fraud
carddata %>%
  filter(!is.na(`Merchant Zip`)) %>%
  filter(Fraud == 1) %>%
  group_by(`Merchant Zip`) %>%
  summarise(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(reorder(`Merchant Zip`,count), count)) +
  geom_bar(stat = "identity", fill= "lightblue") +
  coord_flip() +
  ylab("Number of Fraudulent Card Transactions") +
  xlab("Zip Code") +
  ggtitle("Top 20 Merchant Zip Code with Highest Fraudulent Transaction Count") +
  theme_bw() +
  theme(panel.border = element_blank()) +
  geom_text(aes(label=count), size = 3.4, hjust = -0.05)

# Transaction Types Distribution
carddata %>%
  group_by(Transtype) %>%
  summarise(count=n()) %>%
  ggplot(aes(Transtype, count)) +
  geom_bar(stat = "identity", fill= "lightblue") +
  ylab("Number of Card Transactions") +
  xlab("Transaction Types") +
  ggtitle("Transaction Count by Transaction Type") +
  theme_bw() +
  theme(panel.border = element_blank()) +
  geom_text(aes(label=count), size = 3.4, vjust = 0.1)


# Fraud Distribution
carddata %>%
  group_by(Fraud) %>%
  summarise(count=n()) %>%
  ggplot(aes(Fraud, count)) +
  geom_bar(stat = "identity", fill= "lightblue") +
  ylab("Count") +
  xlab("Fraud") +
  ggtitle("Number of Fraudulent/Not Fraudulent Transactions") +
  theme_bw() +
  theme(panel.border = element_blank()) +
  geom_text(aes(label=count), size = 3.4, vjust = 0.05) +
  scale_x_continuous(breaks = c(0:1), labels = c("Not Fraudulent", "Fraudulent"))

# Transaction Amount Distribution
summary(carddata$Amount)
sd(carddata$Amount)

carddata %>%
  group_by(Amount) %>%
  summarise(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(reorder(Amount,count), count)) +
  geom_bar(stat = "identity", fill= "lightblue") +
  coord_flip() +
  ylab("Number of Card Transactions") +
  xlab("Transaction Amount") +
  ggtitle("Top 20 Transaction Amounts with Highest Transaction Count") +
  theme_bw() +
  theme(panel.border = element_blank()) +
  geom_text(aes(label=count), size = 3.4, hjust = -0.05)

carddata %>%
  filter(Fraud==1) %>%
  group_by(Amount) %>%
  summarise(count=n()) %>%
  arrange(-count) %>%
  head(10) %>%
  ggplot(aes(reorder(Amount,count), count)) +
  geom_bar(stat = "identity", fill= "lightblue") +
  coord_flip() +
  ylab("Number of Card Transactions") +
  xlab("Fraudulent Transaction Amount") +
  ggtitle("Top 10 Fraud Amounts with Highest Transaction Count") +
  theme_bw() +
  theme(panel.border = element_blank()) +
  geom_text(aes(label=count), size = 3.4, hjust = -0.05)
