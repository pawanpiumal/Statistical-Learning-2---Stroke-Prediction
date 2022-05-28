#' @filter cors
cors <- function(req, res) {
  
  res$setHeader("Access-Control-Allow-Origin", "*")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods","*")
    res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200 
    return(list())
  } else {
    plumber::forward()
  }
  
}


#* Return the prediction for one
#* @param age Age
#* @param hypertension Hypertension
#* @param heart_disease heart_disease
#* @param residence_type residence_type
#* @param avg_glucose_level avg_glucose_level
#* @param bmi bmi
#* @param smoking_status smoking_status
#* @post /api/predict
function(age,hypertension,heart_disease,
         residence_type,avg_glucose_level,bmi,smoking_status) {
  data = data.frame(
#                    gender,
                    as.numeric(age),
                    hypertension,
                    heart_disease,
#                    ever_married,
#                    work_type,
                    residence_type,
                    as.numeric(avg_glucose_level),
                    as.numeric(bmi),
                    smoking_status
  )
  colnames(data) = colnames(train)[-8]
  
#  data$gender = factor(data$gender, levels= levels(train$gender))
  data$hypertension = factor(data$hypertension, levels= levels(train$hypertension))
  data$heart_disease = factor(data$heart_disease, levels= levels(train$heart_disease))
#  data$ever_married = factor(data$ever_married, levels= levels(train$ever_married))
#  data$work_type = factor(data$work_type, levels= levels(train$work_type))
  data$Residence_type = factor(data$Residence_type, levels= levels(train$Residence_type))
  data$smoking_status = factor(data$smoking_status, levels= levels(train$smoking_status))
  
#  data$stroke = factor(0,levels = levels(train$stroke))
  pred =predict(model,newx=model.matrix(~.-1,data=data), type="response")
  
  saveRDS(data,"Pred.RDS")
  list(Probability=round(pred,4),
       Outcome=ifelse(pred<0.5,"Low probability to get a stroke!","High probability to get a stroke!"),
       Stroke = ifelse(pred<0.5,FALSE,TRUE))
}



#* Return the Range p1rediction
#* @param range Range Variable
#* @param age Age
#* @param hypertension Hypertension
#* @param heart_disease heart_disease
#* @param residence_type residence_type
#* @param avg_glucose_level avg_glucose_level
#* @param bmi bmi
#* @param smoking_status smoking_status
#* @post /api/predictRange
function(range,age,hypertension,heart_disease,
         residence_type,avg_glucose_level,bmi,smoking_status) {
  
  data = data.frame(matrix(nrow=0,ncol=7))
  colnames(data) = colnames(train)[-8]
  
  if(range == 'age'){
    ages = as.numeric(age)+0:9
    for(i in 1:10){
      data[i,] = c(as.numeric(ages[i]),
                   hypertension,
                   heart_disease,
                   residence_type,
                   as.numeric(avg_glucose_level),
                   as.numeric(bmi),
                   smoking_status)
    }
  }else if(range == 'hypertension'){
    for(i in 1:length(levels(train$hypertension))){
      data[i,] = c(as.numeric(age),
                   levels(train$hypertension)[i],
                   heart_disease,
                   residence_type,
                   as.numeric(avg_glucose_level),
                   as.numeric(bmi),
                   smoking_status
      )
    }
  }else if(range == 'heart_disease'){
    for(i in 1:length(levels(train$heart_disease))){
      data[i,] = c(as.numeric(age),
                   hypertension,
                   levels(train$heart_disease)[i],
                   residence_type,
                   as.numeric(avg_glucose_level),
                   as.numeric(bmi),
                   smoking_status
      )
    }
  }else if(range == 'residence_type'){
    for(i in 1:length(levels(train$Residence_type))){
      data[i,] = c(as.numeric(age),
                   hypertension,
                   heart_disease,
                   levels(train$Residence_type)[i],
                   as.numeric(avg_glucose_level),
                   as.numeric(bmi),
                   smoking_status
      )
    }
  }else if(range == 'smoking_status'){
    for(i in 1:length(levels(train$smoking_status))){
      data[i,] = c(as.numeric(age),
                   hypertension,
                   heart_disease,
                   residence_type,
                   as.numeric(avg_glucose_level),
                   as.numeric(bmi),
                   levels(train$smoking_status)[i]
      )
    }
  }else if(range == 'bmi'){
    bmis = c(15, 21.75 ,27.5 ,35)
    for(i in 1:4){
      data[i,] = c(as.numeric(age),
                   hypertension,
                   heart_disease,
                   residence_type,
                   as.numeric(avg_glucose_level),
                   bmis[i],
                   smoking_status
      )
    }
  }else{
    list(error="Error")
  }
  
  data$age = as.numeric(data$age)
  data$bmi = as.numeric(data$bmi)
  data$avg_glucose_level = as.numeric(data$avg_glucose_level)
  data$hypertension = factor(data$hypertension, levels= levels(train$hypertension))
  data$heart_disease = factor(data$heart_disease, levels= levels(train$heart_disease))
  data$Residence_type = factor(data$Residence_type, levels= levels(train$Residence_type))
  data$smoking_status = factor(data$smoking_status, levels= levels(train$smoking_status))

  pred =predict(model,newx=model.matrix(~.-1,data=data), type="response")
  
  if(range == 'age'){
    pred = data.frame(round(pred,4),ages)
    
  }else if(range == 'hypertension'){
    labels = c("Have Hypertension","Do not have Hypertension")
    pred = data.frame(round(pred,4),labels) 
    
  }else if(range == 'heart_disease'){
    labels = c("Have a Heart Disease","Do not have a Heart Disease")
    pred = data.frame(round(pred,4),labels)
    
  }else if(range == 'residence_type'){
    labels = levels(train$Residence_type)
    pred = data.frame(round(pred,4),labels)
    
  }else if(range == 'smoking_status'){
    labels = c("Formerly Smoked","Never Smoked","Smokes","Unknown")
    pred = data.frame(round(pred,4),labels)
    
  }else if(range == "bmi"){
    labels = c("Underweight(15)","Healthy(21.75)","Overweight(27.5)","Obese(35)")
    pred = data.frame(round(pred,4),labels)
  }
#0.4631198
  pred[,3] = ifelse(pred[,1]>0.5,TRUE,FALSE)
  
  print(data)
  print(pred)
  list(probability = pred[,1],
       variable = pred[,2],
       stroke = pred[,3])
}


