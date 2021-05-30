# model construction####################
library(abind)
library(stringr)
library(caret)
library(keras)
base_in = layer_input(shape(c(length(mlp_asc_in_tr))),name = "base_in")
an_out = base_in %>%  
  layer_dense(units = 128,activation = 'relu',name = 'an_dense_128') %>%
  layer_dense(units = 64,activation = 'relu',name = 'an_dense_64') %>%
  layer_dense(units = 1,activation = 'sigmoid',name = 'an_dense_1')
model <- keras_model(base_in,an_out)
model
model %>% compile(
  optimizer = 'rmsprop',
  loss = 'binary_crossentropy',
  #loss = rep("binary_crossentropy",length(mlp_names)),
  metrics = metric_binary_accuracy
)
model
history <- model %>% fit(as.matrix(mlp_asc_in_tr),
                        tr_out_list[[4]],
                         epochs=20,
                         batch_size=256,
                         validation_split=0.15)
# epochs = 5
# reset the model
model <- keras_model(base_in,an_out)

model %>% compile(
  optimizer = 'rmsprop',
  loss = 'binary_crossentropy',
  #loss = rep("binary_crossentropy",length(mlp_names)),
  metrics = metric_binary_accuracy
)
model
history <- model %>% fit(as.matrix(mlp_asc_in_tr),
                         tr_out_list[[4]],
                         epochs=11,
                         batch_size=256
                         #validation_split=0.15
                         )
asc_predict <- model %>% predict(as.matrix(mlp_asc_in_test))
asc_predict[asc_predict>0.1]=1
asc_predict[asc_predict != 1]=0
#ord_an = data.frame('real'= mlp_asc_tag_test[,1], 'predict'= asc_predict_original[[1]])
#ord_an$predict[ord_an$predict >0.1] = 1
#ord_an$predict[ord_an$predict != 1] = 0
ord_an_con = confusionMatrix(as.factor(asc_predict),as.factor(mlp_asc_tag_test[,4]))
ord_an_con
                                 