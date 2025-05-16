# Python-Umgebung einrichten
library(reticulate)

virtualenv_create("py_env", python = "3.10")
virtualenv_install("py_env", packages = c("tensorflow==2.18", "keras==3.6"))

use_virtualenv("py_env") 
tf<-import("tensorflow")

library(tensorflow)
library(keras)

py_version() # 3.10
keras$`__version__` # 3.6.0
tf$`__version__` # 2.18.0

#Pakete
library(reservr) # 0.0.3
library(tidymodels) # 2.15.0
library(tibble) # 3.2.1
library(ggplot2) # 3.5.1
library(patchwork) # 1.2.0
library(VGAM) # 1.1-11
library(iml) # 0.11.3
library(rcompanion) # 2.4.36

