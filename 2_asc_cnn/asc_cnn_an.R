library(abind)
library(readr)
library(stringr)
library(caret)
base_in <- layer_input(shape=c(8,length(base_list),1))
asc_ex_in <- layer_input(shape=c(8,length(asc_ex_list),1))

asc_con <- layer_concatenate(list(base_in,asc_ex_in),axis=2)
asc_model <- asc_con %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3 ), activation = "relu",
                padding = 'same') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                padding = 'same') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                padding = 'same') %>%
  layer_max_pooling_2d(pool_size = c(1, 2)) #%>%
# layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
#               padding = 'same') %>%
# layer_max_pooling_2d(pool_size = c(1, 2))

asc_model

asc_out <- asc_model %>%
  layer_flatten() %>%
  layer_dense(units = 128, activation = "relu") %>%
  #layer_dropout(rate = 0.5) %>%
  layer_dense(units = 64, activation = "relu") %>%#,kernel_regularizer = regularizer_l2(0.001)) %>%
  #layer_dense(units = 13, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")
asc_out

model <- keras_model(list(base_in,asc_ex_in),asc_out)
model
model %>% compile(
  optimizer = 'rmsprop',
  loss = 'binary_crossentropy',
  metrics = metric_binary_accuracy
)
model
model %>% save_model_hdf5('./2_asc_cnn/asc_cnn_an.h5')
history = model %>% fit(list(base_in_tr,asc_ex_tr),
                        asc_tag_tr[[4]],
                        epochs = 10, batch_size =128,validation_split =0.15)#,callbacks=callbacks)


model <- keras_model(list(base_in,asc_ex_in),asc_out)
model
model %>% compile(
  optimizer ='rmsprop',
  loss = 'binary_crossentropy',
  metrics = metric_binary_accuracy
)
model

history = model %>% fit(list(base_in_tr,asc_ex_tr),
                        asc_tag_tr[[4]],
                        epochs = 5, batch_size =128)#,validation_split =0.15)
result = model %>% evaluate(list(base_in_test,asc_ex_test),
                            asc_tag_test[[4]])

asc_predict <- model %>% predict(list(base_in_test,asc_ex_test))
asc_predict[asc_predict>0]=1
asc_predict[asc_predict != 1]=0
#ord_an = data.frame('real'= mlp_asc_tag_test[,1], 'predict'= asc_predict_original[[1]])
#ord_an$predict[ord_an$predict >0.1] = 1
#ord_an$predict[ord_an$predict != 1] = 0
ord_an_con = confusionMatrix(as.factor(asc_predict),as.factor(asc_tag_test[,4]))
ord_an_con
