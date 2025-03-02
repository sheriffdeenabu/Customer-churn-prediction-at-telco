data <- read.csv("Telco_customer_churn - Copy.csv")

data$Multiple.Lines[data$Multiple.Lines == 'No phone service'] <- 'No'
data$Online.Security[data$Online.Security == 'No internet service'] <- 'No'
data$Online.Backup[data$Online.Backup == 'No internet service'] <- 'No'
data$Device.Protection[data$Device.Protection == 'No internet service'] <- 'No'
data$Tech.Support[data$Tech.Support == 'No internet service'] <- 'No'
data$Streaming.TV[data$Streaming.TV == 'No internet service'] <- 'No'
data$Streaming.Movies[data$Streaming.Movies == 'No internet service'] <- 'No'

str(data)
summary(data)


# Setting the variables with more than two categories to factor
# lm() in regression automatically creates the dummy variables

data$Internet.Service <- factor(data$Internet.Service) 
data$Payment.Method <- factor(data$Payment.Method)
data$Contract <-factor(data$Contract)
data$Gender <- factor(data$Gender)
data$Senior.Citizen <- factor(data$Senior.Citizen)
data$Partner <- factor(data$Partner)
data$Dependents <- factor(data$Dependents)
data$Phone.Service <- factor(data$Phone.Service)
data$Multiple.Lines <- factor(data$Multiple.Lines)
data$Online.Security <- factor(data$Online.Security)
data$Online.Backup <- factor(data$Online.Backup)
data$Device.Protection <- factor(data$Device.Protection)
data$Tech.Support <- factor(data$Tech.Support)
data$Streaming.TV <- factor(data$Streaming.TV)
data$Streaming.Movies <- factor(data$Streaming.Movies)
data$Paperless.Billing <- factor(data$Paperless.Billing)
data$Churn.Label <- factor(data$Churn.Label)


# Data Partition

RanNum <- runif(7043)
length(RanNum)

Index <- order(RanNum)
Index

Train <- data[Index[1:4543],]
Test <- data[Index[4543:7043],]

# Model building

Model3 <- lm(Churn.Score~Total.Charges+Monthly.Charges+Zip.Code+Gender+Senior.Citizen+Partner+Dependents+Tenure.Months
             +Phone.Service+Multiple.Lines+Internet.Service+Online.Security+Online.Backup+Device.Protection
             +Tech.Support+Streaming.TV+Streaming.Movies+Contract+Paperless.Billing+Payment.Method+Monthly.Charges
             +Total.Charges+Churn.Label+CLTV, data = Train)

summary(Model3)

Model3.1 <- lm(Churn.Score~Total.Charges+Monthly.Charges+Zip.Code+Gender+Senior.Citizen+Partner+Dependents+Tenure.Months
             +Phone.Service+Multiple.Lines+Internet.Service+Online.Security+Online.Backup+Device.Protection
             +Tech.Support+Streaming.TV+Streaming.Movies+Monthly.Charges
             +Total.Charges+Churn.Label+CLTV, data = Train)

summary(Model3.1)

Model3.2 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.2)


Model3.3 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.3)


Model3.4 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.4)


Model3.5 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.5)


Model3.6 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.6)

Model3.7 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.7)

Model3.8 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.8)

Model3.9 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.9)

Model3.10 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.10)


Model3.11 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.11)


Model3.12 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.12)

Model3.13 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.13)


Model3.14 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.14)

Model3.15 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.15)


Model3.16 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.16)

Model3.17 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.17)

Model3.18 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.17)
Model3.18 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.18)

Model3.19 <- lm(Churn.Score ~ Total.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.19)

Model3.20 <- lm(Churn.Score ~ Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.20)

Model3.21 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + CLTV, data = Train)
summary(Model3.21)

Model3.22 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label, data = Train)
summary(Model3.22)

Model3.23 <- lm(Churn.Score ~ Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Churn.Label + CLTV, data = Train)
summary(Model3.23)

Model3.24 <- lm(Churn.Score ~ Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Churn.Label + CLTV, data = Train)
summary(Model3.24)

Model3.25 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.25)

Model3.26 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.26)

Model3.27 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.27)

Model3.28 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.28)

Model3.29 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.29)

Model3.30 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.30)

Model3.31 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.31)

Model3.32 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.32)

Model3.33 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train)
summary(Model3.33)

Model3.34 <- lm(Churn.Score ~ Total.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.34) 

Model3.35 <- lm(Churn.Score ~ Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.35) 

Model3.36 <- lm(Churn.Score ~ Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Churn.Label + CLTV, data = Train) 
summary(Model3.36) 

Model3.37 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Gender + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.37) 

Model3.38 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Senior.Citizen + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.38) 

Model3.39 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Tenure.Months + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.39) 

Model3.40 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Phone.Service + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.40) 

Model3.41 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Online.Security + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.41) 

Model3.42 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Backup + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.42) 

Model3.43 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Streaming.TV + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.43) 

Model3.44 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.Movies + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.44) 

Model3.45 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Payment.Method + Monthly.Charges + Total.Charges + CLTV, data = Train) 
summary(Model3.45) 

Model3.46 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Monthly.Charges + Total.Charges + Churn.Label, data = Train) 
summary(Model3.46) 

Model3.47 <- lm(Churn.Score ~ Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Churn.Label + CLTV, data = Train) 
summary(Model3.47) 

Model3.48 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.48) 

Model3.49 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.49) 

Model3.50 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.50) 
  
Model3.51 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.51) 

Model3.52 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.52) 

Model3.53 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Monthly.Charges + Total.Charges + CLTV, data = Train) 
summary(Model3.53) 

Model3.54 <- lm(Churn.Score ~ Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Churn.Label, data = Train) 
summary(Model3.54) 

Model3.55 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.55) 

Model3.56 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.56) 

Model3.57 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.57) 

Model3.58 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.58) 

Model3.59 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.59) 

Model3.60 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.60) 

Model3.61 <- lm(Churn.Score ~ Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Churn.Label + CLTV, data = Train) 
summary(Model3.61) 

Model3.62 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.62) 

Model3.63 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.63) 

Model3.64 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.64) 

Model3.65 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.65) 

Model3.66 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.66) 

Model3.67 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Monthly.Charges + Total.Charges + CLTV, data = Train) 
summary(Model3.67) 

Model3.68 <- lm(Churn.Score ~ Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Churn.Label, data = Train) 
summary(Model3.68) 

Model3.69 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.69) 

Model3.70 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.70) 

Model3.71 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.71) 

Model3.72 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.72) 

Model3.73 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.73) 

Model3.74 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.74) 

Model3.75 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Monthly.Charges + Total.Charges + Churn.Label + CLTV, data = Train) 
summary(Model3.75) 

Model3.76 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Churn.Label + CLTV, data = Train) 
summary(Model3.76) 

Model3.77 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Churn.Label + CLTV, data = Train) 
summary(Model3.77) 

Model3.78 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Churn.Label + CLTV, data = Train) 
summary(Model3.78) 

Model3.79 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Churn.Label + CLTV, data = Train) 
summary(Model3.79) 

Model3.80 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Churn.Label + CLTV, data = Train) 
summary(Model3.80) 

Model3.81 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Churn.Label + CLTV, data = Train) 
summary(Model3.81) 

Model3.82 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Churn.Label + CLTV, data = Train) 
summary(Model3.82) 

Model3.83 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Churn.Label + CLTV, data = Train) 
summary(Model3.83) 

Model3.84 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Churn.Label + CLTV, data = Train) 
summary(Model3.84) 

Model3.85 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Churn.Label + CLTV, data = Train) 
summary(Model3.85) 

Model3.86 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Backup + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Churn.Label + CLTV, data = Train) 
summary(Model3.86) 

Model3.87 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Device.Protection + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Churn.Label + CLTV, data = Train) 
summary(Model3.87) 

Model3.88 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Tech.Support + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Churn.Label + CLTV, data = Train) 
summary(Model3.88) 

Model3.89 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Streaming.TV + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Churn.Label + CLTV, data = Train) 
summary(Model3.89) 

Model3.90 <- lm(Churn.Score ~ Total.Charges + Monthly.Charges + Zip.Code + Gender + Senior.Citizen + Partner + Dependents + Tenure.Months + Phone.Service + Multiple.Lines + Internet.Service + Online.Security + Online.Backup + Device.Protection + Tech.Support + Streaming.Movies + Contract + Paperless.Billing + Payment.Method + Churn.Label + CLTV, data = Train) 
summary(Model3.90)


