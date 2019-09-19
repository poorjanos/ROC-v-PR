library(mlbench)
library(dplyr)
library(PRROC)

# Get data
data("PimaIndiansDiabetes")
PimaIndiansDiabetes$diabetes <- as.numeric(PimaIndiansDiabetes$diabetes)
PimaIndiansDiabetes[PimaIndiansDiabetes$diabetes == 1, "diabetes"] <- 0
PimaIndiansDiabetes[PimaIndiansDiabetes$diabetes == 2, "diabetes"] <- 1
names(PimaIndiansDiabetes)[names(PimaIndiansDiabetes) == "diabetes"] <- "y"


# Split 0 and 1 labels
label_0 <- PimaIndiansDiabetes[PimaIndiansDiabetes$y == 0, ]
label_1 <- PimaIndiansDiabetes[PimaIndiansDiabetes$y == 1, ]


# Gen rebalanced dataset
label_1_rate = 0.2
sample_size = round(label_1_rate * nrow(PimaIndiansDiabetes), 0)

sample_ind <- sample(seq_len(nrow(label_1)), size = sample_size)
label_1_rebalanced <- label_1[sample_ind, ]

data <- rbind(label_0, label_1_rebalanced)

  
# Train test split
smp_size <- floor(0.8 * nrow(data))

set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]


# Fit glm
fit <- glm(y~., data = train, family = "binomial")


# Predict test
preds <- predict(fit, test, type="response")


# ROCR
roc <- roc.curve(scores.class0 = preds, weights.class0 = test$y, curve = TRUE)
pr <- pr.curve(scores.class0 = preds, weights.class0 = test$y, curve = TRUE)
plot(roc)
plot(pr)


