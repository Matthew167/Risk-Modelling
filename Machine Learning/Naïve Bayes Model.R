whisky <- read.csv("whisky.csv")
head(whisky)

#------------------------------------------------------------------------------------
# i)
#------------------------------------------------------------------------------------

nb.probs <- function(data, y_col, y_val, x_col){
  data.y <- data[data[, y_col] == y_val, ]
  table(data.y[, x_col]) / nrow(data.y)
}

#------------------------------------------------------------------------------------
# ii)
#------------------------------------------------------------------------------------

# a)
set.seed(842231)
training.rows <- sample(1:nrow(whisky), 0.6 * nrow(whisky))
whisky.train <- whisky[training.rows, ]

# b)

nb.probs(whisky.train, "whisky.name", "Mactavish", "smoky")

#------------------------------------------------------------------------------------
# iii)
#------------------------------------------------------------------------------------

# a)

str(nb.probs(whisky.train, "whisky.name", "Mactavish", "smoky"))

smoky <- as.data.frame(nb.probs(whisky.train, "whisky.name", "Mactavish", "smoky"))

smoky

colnames(smoky) = c("Value", "Mactavish")

smoky

table(whisky.train$whisky.name)

smoky$Dogavulin <- nb.probs(whisky.train, "whisky.name", "Dogavulin", "smoky")
smoky$Glenragh <- nb.probs(whisky.train, "whisky.name", "Glenragh", "smoky")
smoky$Western_Isle <- nb.probs(whisky.train, "whisky.name", "Western_Isle", "smoky")
smoky

# b)

fruity <- as.data.frame(nb.probs(whisky.train, "whisky.name", "Mactavish", "fruity"))
colnames(fruity) <- c("Value", "Mactavish")

fruity$Dogavulin <- nb.probs(whisky.train, "whisky.name", "Dogavulin", "fruity")
fruity$Glenragh <- nb.probs(whisky.train, "whisky.name", "Glenragh", "fruity")
fruity$Western_Isle <- nb.probs(whisky.train, "whisky.name", "Western_Isle", "fruity")
fruity

# c)

colour <- as.data.frame(nb.probs(whisky.train, "whisky.name", "Mactavish", "colour"))
colnames(colour) <- c("Value", "Mactavish")

colour$Dogavulin <- nb.probs(whisky.train, "whisky.name", "Dogavulin", "colour")
colour$Glenragh <- nb.probs(whisky.train, "whisky.name", "Glenragh", "colour")
colour$Western_Isle <- nb.probs(whisky.train, "whisky.name", "Western_Isle", "colour")
colour

#------------------------------------------------------------------------------------
# iv)
#------------------------------------------------------------------------------------

(y_probs <- table(whisky.train$whisky.name) / nrow(whisky.train))

#------------------------------------------------------------------------------------
# v)
#------------------------------------------------------------------------------------

prop.to <- function(whisky_name, smoky_val, fruity_val, colour_val){
  smoky[smoky_val, whisky_name] *
    fruity[fruity_val, whisky_name] *
    colour[colour_val, whisky_name] * 
    y_probs[whisky_name]
}

# prop.to("Mactavish", whisky.train$smoky, whisky.train$fruity, whisky.train$colour)

#------------------------------------------------------------------------------------
# vi)
#------------------------------------------------------------------------------------

# a)

whisky.test <- whisky[-training.rows, ]

preds <- data.frame(ID = 1:nrow(whisky.test))

for(y in names(y_probs)){
  preds[, y] <- prop.to(y, whisky.test$smoky,
                        whisky.test$fruity,
                        whisky.test$colour)
}

head(preds)

preds$prediction <- names(preds)[-1][max.col(preds[, -1])]

head(preds)

# b)
(mat <- table(whisky.test$whisky.name, preds$prediction))

#------------------------------------------------------------------------------------
# vii)
#------------------------------------------------------------------------------------

sum(diag(mat)) / nrow(whisky.test)