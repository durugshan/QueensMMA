pacman::p_load("readxl", "caret","partykit","ROCR","lift","rpart","e1071", "xgboost","glmnet","MASS", "randomForest")

#load in transactions data
transactions <- read.csv("C:/Users/s5662866/Downloads/telecom-customer/Telecom_customer churn.csv")

#take a look at how the dataset looks like
# summary(transactions)

#remove rows with missing data for monthly revenue
transactions <- transactions[!is.na(transactions$rev_Mean),]
transactions <- transactions[!is.na(transactions$eqpdays),]
transactions <- transactions[!is.na(transactions$change_mou),]
# summary(transactions)

# transactions <- subset(transactions, select = c("eqpdays", "change_mou", "mou_Mean", "months", "totmrc_Mean", "avgqty", "hnd_price", "change_rev", "uniqsubs", "avg3qty",
#                                  "ethnic", "peak_vce_Mean", "opk_vce_Mean", "avg3mou", "adjrev", "avgrev", "avgmou", "totrev", "drop_vce_Mean", "mou_opkv_Mean",
#                                  "mouowylisv_Mean", "crclscod", "totmou", "mou_cvce_Mean", "lor", "churn", "Customer_ID"))


#clean up phones column to change NA to 0
transactions$phones <- ifelse(is.na(transactions$phones), 0, transactions$phones)
transactions$forgntvl <- ifelse(is.na(transactions$forgntvl), 0, transactions$forgntvl)
transactions$truck <- ifelse(is.na(transactions$truck), 0, transactions$truck)
transactions$rv <- ifelse(is.na(transactions$rv), 0, transactions$rv)
transactions$models <- ifelse(is.na(transactions$models), 1, transactions$models)
transactions$numbcars <- ifelse(is.na(transactions$numbcars), 0, transactions$numbcars)
transactions$kid0_2 <- ifelse(transactions$kid0_2 == "Y", 1, 0)
transactions$kid3_5 <- ifelse(transactions$kid3_5 == "Y", 1, 0)
transactions$kid6_10 <- ifelse(transactions$kid6_10 == "Y", 1, 0)
transactions$kid11_15 <- ifelse(transactions$kid11_15 == "Y", 1, 0)
transactions$kid16_17 <- ifelse(transactions$kid16_17 == "Y", 1, 0)

#set up factors
transactions$churn <- as.factor(transactions$churn)
transactions$new_cell <- as.factor(transactions$new_cell)
transactions$crclscod <- as.factor(transactions$crclscod)
transactions$asl_flag <- as.factor(transactions$asl_flag)
transactions$prizm_social_one <- as.factor(transactions$prizm_social_one)
transactions$area <- as.factor(transactions$area)
transactions$dualband <- as.factor(transactions$dualband)
transactions$refurb_new <- as.factor(transactions$refurb_new)
transactions$truck<- as.factor(transactions$truck)
transactions$rv <- as.factor(transactions$rv)
transactions$ownrent <- as.factor(transactions$ownrent)
transactions$dwlltype <- as.factor(transactions$dwlltype)
transactions$marital <- as.factor(transactions$marital)
transactions$infobase <- as.factor(transactions$infobase)
transactions$HHstatin <- as.factor(transactions$HHstatin)
transactions$dwllsize <- as.factor(transactions$dwllsize)
transactions$forgntvl <- as.factor(transactions$forgntvl)
transactions$ethnic <- as.factor(transactions$ethnic)
transactions$kid0_2 <- as.factor(transactions$kid0_2)
transactions$kid3_5 <- as.factor(transactions$kid3_5)
transactions$kid6_10 <- as.factor(transactions$kid6_10)
transactions$kid11_15 <- as.factor(transactions$kid11_15)
transactions$kid16_17 <- as.factor(transactions$kid16_17)
transactions$creditcd <- as.factor(transactions$creditcd)

summary(transactions)

# transactions <- transactions[,!(names(transactions) %in% c("avg6mou", "avg6qty", "avg6rev", "hnd_price", "hnd_webcap", "lor",
                                 # "adults", "income"))]

#feature engineering
transactions$mean_mins_per_phone <- transactions$mou_Mean/transactions$phones
transactions$z_month_rev <- (transactions$rev_Mean - mean(transactions$rev_Mean)) / sd(transactions$rev_Mean)
transactions$z_custcare <- (transactions$custcare_Mean - mean(transactions$custcare_Mean)) / sd(transactions$custcare_Mean)
transactions$eqp_months_ratio <- transactions$eqpdays/transactions$months
transactions$eqp_rev_ratio <- transactions$eqpdays / transactions$rev_Mean
transactions$eqp_300 <- ifelse(transactions$eqpdays >= 300, 1, 0)
transactions$has_kids <- ifelse(transactions$kid0_2 == 1 | transactions$kid3_5 == 1 | transactions$kid6_10 == 1 | transactions$kid11_15 == 1 | transactions$kid16_17 == 1, 1, 0)
transactions$mins_per_call <- transactions$avgmou/transactions$avgqty
transactions$z_uniq_subs <- (transactions$uniqsubs - mean(transactions$uniqsubs)) / sd(transactions$uniqsubs)
transactions$z_actv_subs <- (transactions$actvsubs - mean(transactions$actvsubs)) / sd(transactions$actvsubs)
transactions$rev_per_z_actv_subs <- transactions$rev_Mean/transactions$z_actv_subs

summary(transactions)


# write.csv(transactions, "C:/Users/s5662866/Downloads/telecom-customer/Telecom_customer_churn_updated_2.csv")


