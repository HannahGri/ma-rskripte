
#Das hier klappt: python 3.10.0, tf 2.18.0, keras 3.6.0
library(reticulate)
library(tensorflow)
library(keras)

py_version() #3.10
keras$`__version__` # 3.6.0
tf$`__version__` #2.18.0


virtualenv_create("r-tf-env")
virtualenv_install("r-tf-env", packages = c("tensorflow", "keras"))



use_virtualenv("r-tf-env", required = TRUE) 
tf<-import("tensorflow")



library(reservr)
library(keras3)
library(tidymodels)
library(tibble)
library(ggplot2)
library(patchwork)
library(lubridate)
library(VGAM)
library(DALEX)
library(kableExtra)
library(iml)


