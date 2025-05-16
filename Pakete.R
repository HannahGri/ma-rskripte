
#Das hier klappt: python 3.10.0, tf 2.18.0, keras 3.6.0
library(reticulate)

virtualenv_create("r-tf-env")
virtualenv_install("r-tf-env", packages = c("tensorflow", "keras"))

library(tensorflow)
library(keras)

use_virtualenv("r-tf-env", required = TRUE) 
tf<-import("tensorflow")

py_version() # 3.10
keras$`__version__` # 3.6.0
tf$`__version__` # 2.18.0

library(reservr) # 0.0.3
library(tidymodels) # 2.15.0
library(tibble) # 3.2.1
library(ggplot2) # 3.5.1
library(patchwork) # 1.2.0
library(VGAM) # 1.1-11
library(iml) # 0.11.3


