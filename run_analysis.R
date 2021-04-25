#limpiar el enviroment
  rm(list = ls(all.names = TRUE)) 

library(dplyr)
setwd("C:/Users/Rafael/Downloads/getdata_projectfiles_UCI HAR Dataset")

#identificar sujetos de prueba y prueba
  
  sujetos_test<- read.table("C:/Users/Rafael/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
  sujetos_train <- read.table("C:/Users/Rafael/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
  sujetos<-rbind(sujetos_test,sujetos_train)
  colnames(sujetos)<-"Sujetos"
  actividad_test<- read.table("C:/Users/Rafael/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
  actividad_train <- read.table("C:/Users/Rafael/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
  actividad<-rbind(actividad_test,actividad_train)
  colnames(actividad)<-"actividad"
  
#Listar archivos de test y de train
  
  directorio_test<-  "C:/Users/Rafael/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/Inertial Signals"
  directorio_train<- "C:/Users/Rafael/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/Inertial Signals"
  archivos_test <- list.files("C:/Users/Rafael/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/Inertial Signals")
  archivos_train<-list.files("C:/Users/Rafael/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/Inertial Signals")

#Lectura de archivos
  
  b<- vector()
  for (i in 1:9) {b[i]<- print(paste(directorio_test, archivos_test[i],sep = "/")) }
  lista_tablas_test <- list()
  for(i in 1:9) {lista_tablas_test[i] <- read.table(b[i])}
  c<- vector()
  for (i in 1:9) {c[i]<- print(paste(directorio_train, archivos_train[i],sep = "/")) }
  lista_tablas_train <- list()
  for(i in 1:9) {lista_tablas_train[i] <- read.table(c[i])}
  
#Generación de data_frame
  
  tests_dataframe <- data.frame(matrix(unlist(lista_tablas_test), nrow=length(lista_tablas_test), byrow=TRUE))
  tests_dataframe <- t(tests_dataframe)
  #Generación de variable dicotoma de test=1 o train=0
  tests_dataframe <- cbind(tests_dataframe,1)
  colnames(tests_dataframe) <- c(gsub(".txt","",archivos_test),"test")
  train_dataframe <- data.frame(matrix(unlist(lista_tablas_train), nrow=length(lista_tablas_train), byrow=TRUE))
  train_dataframe <- t(train_dataframe)
  #Generación de variable dicotoma de test=1 o train=0
  train_dataframe <- cbind(train_dataframe,0)
  colnames(train_dataframe) <- c(gsub(".txt","",archivos_train),"test")
  general_dataframe <- rbind(tests_dataframe,train_dataframe)
  general_dataframe<- cbind(general_dataframe,sujetos,actividad)
  colnames(general_dataframe) <- gsub("_test","",colnames(general_dataframe))
 
#Get mean and standart deviation
  
  column_mean <- colMeans(general_dataframe)
  #elnúmero 2 índica columnas y el 1 filas, c(1,2)es ambas
  col_sd <- apply(general_dataframe,2,sd)
  
#Agrupación y tidy data por sujeto/actividad
  
  por_grupo <- group_by(general_dataframe, Sujetos, actividad)
  media_por_grupo<-summarize_all(por_grupo, mean)
  