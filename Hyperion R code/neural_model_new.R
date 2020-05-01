

#change csv file into dataframe
df <- read.csv("profit.csv",header= TRUE)

#creating training and test sets
index <- createDataPartition(df$loan_default,p=0.9,list=FALSE)
not_trained <- c("pri_active_accounts", "credit_hist_len_mon", "pri_sanc_amount", 
                 "pri_disbursed_ammount", "sec_no_accounts", "sec_sanc_ammount", 
                 "sec_disbursed_amount", "emp.empSalaried", "ID" , "pincode",
                 "state_ID", "employee_ID")
train_data <- df[index,]
test_data <- df[-index,]

x_train <- data.matrix(train_data[, !(names(train_data) %in% not_trained)])
y_train <- data.matrix(train_data[, "loan_default"])

x_test <- data.matrix(train_data[, (names(train_data) %in% not_trained)])
y_test <- data.matrix(train_data[, "loan_default"])

#table of the column loan default
tbl <- table(train_data$loan_default)

#class distribution
print("class distribution:")
print(prop.table(tbl))


# creating oversampling dataset 
balanced_data_over <- ovun.sample(loan_default~.,data=train_data, method = "over" )$data

x_train_over <- data.matrix(balanced_data_over[,!(names(train_data) %in% not_trained)])
y_train_over <- data.matrix(balanced_data_over[,"loan_default"])

print("oversampling")
print(table(balanced_data_over$loan_default))

# creating Undersampling dataset
balanced_data_under <- ovun.sample(loan_default~.,data=train_data, method = "under" )$data

x_train_under <- data.matrix(balanced_data_under[,!(names(balanced_data_under) %in% not_trained)])
y_train_under <- data.matrix(balanced_data_under[,"loan_default"])

print("undersampling")
print(table(balanced_data_under$loan_default))

# creating Dataset using both under and over sampling 
balanced_data_both <- ovun.sample(loan_default~.,data=train_data, method = "both" )$data

x_train_both <- data.matrix(balanced_data_both[,!(names(train_data) %in% not_trained)])
y_train_both <- data.matrix(balanced_data_both[,"loan_default"])

print("both under and over sampling")
print(table(balanced_data_both$loan_default))

# creating  Dataset using ROSE sampling 
balanced_data_rose <- ROSE(loan_default~.,data=train_data )$data

x_train_rose <- data.matrix(balanced_data_rose[,!(names(train_data) %in% not_trained)])
y_train_rose <- data.matrix(balanced_data_rose[,"loan_default"])

print("ROSE sampling")
print(table(balanced_data_rose$loan_default))


#Neural network Architecture below
model1 <-
  keras_model_sequential()%>%
  layer_dense(units=8,activation = 'relu', input_shape =c(ncol(x_train))) %>%
  #layer_dropout(0.5)%>%
  layer_dense(units = 8,activation='relu' )%>%
  #layer_dropout(0.5)%>%
  layer_dense(units=1, activation = 'sigmoid')
 


model1 %>% compile(
  loss = 'binary_crossentropy',
  optimizer ='adam',
  metrics = c('accuracy')
  )
    
model1 %>% summary()    

#fitting model with unbalanced dataset
print("fitting model: Unbalanced")

model1_history_unbalanced <- model1 %>% fit(
  x_train, y_train,
  epochs = 10,
  batch_size =16,
  validation_split = 0.2,
  verbose=2
  )

model1 %>% save_model_hdf5("model_unbalanced.h5")

#fitting model with under-sampling dataset
print("fitting model: Under sampling")
  
model1_history_under <- model1 %>% fit(
  x_train_under, y_train_under,
  epochs = 10,
  batch_size =16,
  validation_split = 0.2,
  verbose=2
)

model1 %>% save_model_hdf5("model_under.h5")

#fitting model with over-sampling dataset
print("fitting model: Over sampling")

model1_history_over <- model1 %>% fit(
  x_train_over, y_train_over,
  epochs = 10,
  batch_size =16,
  validation_split = 0.2,
  verbose =2
)

model1 %>% save_model_hdf5("model_over.h5")

#fitting model with under and over-sampling dataset
print("fitting model: Under & Over sampling")

model1_history_both <- model1 %>% fit(
  x_train_both, y_train_both,
  epochs = 10,
  batch_size =16,
  validation_split = 0.2,
  verbose =2
)

model1  %>% save_model_hdf5("model_both.h5")

#fitting model with ROSE-sampling dataset
print("fitting model: ROSE sampling")

model1_history_rose <- model1 %>% fit(
  x_train_rose, y_train_rose,
  epochs = 10,
  batch_size =16,
  validation_split = 0.2,
  verbose =2 
)


model1%>% save_model_hdf5("model_rose.h5")

compare_cx <- data.frame(
  unbalanced_train = model1_history_unbalanced$metrics$loss,
  unbalanced_val = model1_history_unbalanced$metrics$val_loss,
  under_train = model1_history_under$metrics$loss,
  under_val = model1_history_under$metrics$val_loss,
  over_train = model1_history_over$metrics$loss,
  over_val = model1_history_over$metrics$val_loss,
  both_train =  model1_history_both$metrics$loss,
  both_val =  model1_history_both$metrics$val_loss,
  rose_train =  model1_history_rose$metrics$loss,
  rose_val =  model1_history_rose$metrics$val_loss
  
) %>%
  rownames_to_column() %>%
  mutate(rowname = as.integer(rowname)) %>%
  gather(key = "type", value = "value", -rowname)

 plot_sample <- ggplot(compare_cx, aes(x = rowname, y = value, color = type)) +
  geom_line() +
  xlab("epoch") +
  ylab("loss")

print(plot_sample)

