
#Test Set has different factor levels than train

foo <- data.frame(response=rnorm(3),predictor=as.factor(c("A","B","C")))
model <- lm(response~predictor,foo)
foo.new <- data.frame(predictor=as.factor(c("A","B","C","D")))
predict(model,newdata=foo.new)

#Remove extra levels before predicting

id <- which(!(foo.new$predictor %in% levels(foo$predictor)))
foo.new$predictor[id] <- NA
predict(model,newdata=foo.new)



#This one actually works
test$val <- factor(test$val, levels=levels(train$val))
