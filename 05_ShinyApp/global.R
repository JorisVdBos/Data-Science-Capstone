####################
# Load scripts
####################

# Source the libraries and settings
for (script in list.files("configScripts")) {
  source(file.path("configScripts", script))
}

# Source the model
for (script in list.files("modelFunctions")) {
  source(file.path("modelFunctions", script))
}
load("modelObject/model10percent.RData")