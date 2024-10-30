 install.packages("rpart.plot")
library(rpart.plot)
data(ptitanic) 
head(ptitanic)

#------------------------------------------------------------------------------------
# i)
#------------------------------------------------------------------------------------

# a)

set.seed(10)
training.rows <- sample(1:nrow(ptitanic), 0.5 * nrow(ptitanic))

ptitanic.train <- ptitanic[training.rows, ]

# b)
head(ptitanic.train, 4)

#------------------------------------------------------------------------------------
# ii)
#------------------------------------------------------------------------------------

# install.packages("randomForest")
library(randomForest)

set.seed(6047)
titanic_rf <- randomForest(survived ~ pclass + sex + sibsp + parch,
                           ntree = 1500, data = ptitanic.train)

#------------------------------------------------------------------------------------
# iii)
#------------------------------------------------------------------------------------

ptitanic.test <- ptitanic[-training.rows, ]
test_preds <- predict(titanic_rf, newdata = ptitanic.test, type = "class")

#------------------------------------------------------------------------------------
# iv)
#------------------------------------------------------------------------------------

# a)
(conf.mat <- table(ptitanic.test$survived, test_preds))

conf.mat <- conf.mat[2:1, 2:1]
conf.mat

# b)

(r <- conf.mat["survived", "survived"] / sum(conf.mat["survived", ]))
# conf.mat[1, 1] / sum(conf.mat[1, ])
# 164/(164 + 105)

(p <- conf.mat["survived", "survived"] / sum(conf.mat[, "survived"]))

(f1 = 2 * r * p / (r + p))

(fpr <- conf.mat["died", "survived"] / sum(conf.mat["died", ]))

(a <- sum(diag(conf.mat)) / sum(conf.mat))

#------------------------------------------------------------------------------------
# v)
#------------------------------------------------------------------------------------

nb.prob <- function(data, y_col, y_val, x_col, x_val){
  data.y <- data[data[, y_col] == y_val, ]
  data.yx <- data.y[data.y[, x_col] == x_val, ]
  nrow(data.yx) / nrow(data.y)
}

#------------------------------------------------------------------------------------
# vi)
#------------------------------------------------------------------------------------

nb.prob(ptitanic.train, "survived", "survived", "sibsp", 0)
nb.prob(ptitanic.train, "survived", "died", "sibsp", 0)

#------------------------------------------------------------------------------------
# vii)
#------------------------------------------------------------------------------------

# a)
(y_probs <- table(ptitanic.train$survived) / nrow(ptitanic.train))

# b)
prop.to <- function(s_status, sex_val, sibsp_val, parch_val, pclass_val){
  nb.prob(ptitanic.train, "survived", s_status, "sex", sex_val) *
    nb.prob(ptitanic.train, "survived", s_status, "sibsp", sibsp_val) *
    nb.prob(ptitanic.train, "survived", s_status, "parch", parch_val) *
    nb.prob(ptitanic.train, "survived", s_status, "pclass", pclass_val) *
    y_probs[s_status]
}

#------------------------------------------------------------------------------------
# viii)
#------------------------------------------------------------------------------------

# a)
pass.preds <- data.frame(ID = 1:nrow(ptitanic.test))

pass.preds$surv <- apply(ptitanic.test, 1, function(row){
  prop.to("survived", row["sex"], row["sibsp"], row["parch"], row["pclass"])
})

pass.preds$died <- apply(ptitanic.test, 1, function(row){
  prop.to("died", row["sex"], row["sibsp"], row["parch"], row["pclass"])
})

head(pass.preds)

pass.preds$pred <- ifelse(pass.preds$surv > pass.preds$died,
                          "survived", "died")

head(pass.preds)

# b)
conf.mat.nb <- table(ptitanic.test$survived, pass.preds$pred)
conf.mat.nb <- conf.mat.nb[2:1, 2:1]
conf.mat.nb

# c)
r
(r.nb <- conf.mat.nb["survived", "survived"] / sum(conf.mat.nb["survived",]))

(p.nb <- conf.mat.nb["survived", "survived"] / sum(conf.mat.nb[,"survived"]))

(f1.nb <- 2 * r.nb * p.nb / (r.nb + p.nb))

(fpr.nb <- conf.mat.nb["died", "survived"] / sum(conf.mat.nb["died", ]))

(a.nb = sum(diag(conf.mat.nb)) / sum(conf.mat.nb))


#------------------------------------------------------------------------------------
# viii)
#------------------------------------------------------------------------------------

conf.mat.nb
conf.mat