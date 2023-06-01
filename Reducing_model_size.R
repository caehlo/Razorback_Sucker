library(dplyr)
library(ggplot2)
library(caret)

model <- readRDS('XYTE_model.rds')


#create a function to look at memory breakdown
memory_breakdown.fn <- function(model){
  
  MB <- sapply(model, FUN = function(t){ 
    
    10^(-6) * as.numeric(gsub(pattern = "bytes", replacement = "", x = utils::object.size(t))) 
    
  }, simplify = TRUE)
  return(MB)
  
}

sort(round(memory_breakdown.fn(model = model)))

#create a function to null out big memory things

cleanModel2 = function(cm) {
  cm$y = c()
  cm$residuals = c()
  cm$fitted.values = c()
  cm$effects = c()
  cm$qr$qr = c()  
  cm$linear.predictors = c()
  cm$weights = c()
  cm$prior.weights = c()
  cm$data = c()
  cm
}

reduced_model = cleanModel2(model)

#reduce and look at new file size
sum(memory_breakdown.fn(model = reduced_model))

#test the reduced model by predicting on new data
predict <- expand.grid(TL = seq(300,450), MSCP_REACH = c('2', '3', '4'), ReleaseSeason = c('spawn', 'pre-spawn', 'post-spawn'))
fit<- predict(model, newdata = predict, type = "response", se.fit = TRUE)
predict <- cbind(predict, fit)
se.fit <- predict$se.fit
predict$lowerCI <- predict$fit - (1.96 * predict$se.fit)
predict$upperCI <- predict$fit + (1.96 * predict$se.fit)


#visualizing to insure it fits the same as previous model
ggplot(predict, aes(x = TL, y = pred, color = factor(MSCP_REACH))) + geom_line() + facet_grid(.~ReleaseSeason)

#saving reduced model
saveRDS(reduced_model, 'XYTE_reduced_model.rds')

reduced_model <- model
reduced_model$qr$qr <- NULL

