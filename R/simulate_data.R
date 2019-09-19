library(purrr)
library(dplyr)
library(PRROC)


# Probability based pipeline
PROB <- 0.5
NEG_UPPER_BOUND = 0.7
POS_LOWER_BOUND = 0.3

# Define helper to compute pred vals to label
gen_prob <- function(label, neg_upper_bound, pos_lower_bound){
  if(label == 1){
    runif(1, min = pos_lower_bound, max = 1)
  } else {
    runif(1, min = 0, max = neg_upper_bound)
  }
}

# Simulate dataset for ROC and PC computation
df <- data.frame(y_true = rbinom(10000, 1, PROB))

df$probs <- purrr::map_dbl(
  df$y_true,
  ~ gen_prob(
    label = .x,
    neg_upper_bound = NEG_UPPER_BOUND,
    pos_lower_bound = POS_LOWER_BOUND
  )
)


# Model based pipeline
n <- 10000
beta0 <- -1.6
beta1 <- 0.03
x <- runif(n=n, min=18, max=60)
pi_x <- exp(beta0 + beta1 * x) / (1 + exp(beta0 + beta1 * x))
y <- rbinom(n=length(x), size=1, prob=pi_x)
data <- data.frame(x, pi_x, y)
names(data) <- c("age", "pi", "y")


# Train test split
# 75% of the sample size
smp_size <- floor(0.8 * nrow(data))

# Split
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]

# Fit glm and predict probabilities
fit <- glm(y ~ age, data = train, family = "binomial")

preds <- predict(fit, test, type="response")


# ROC and PR plots
roc <- roc.curve(scores.class0 = preds, weights.class0 = test$y, curve = TRUE)
pr <- pr.curve(scores.class0 = preds, weights.class0 = test$y, curve = TRUE)
plot(roc)
plot(pr)

