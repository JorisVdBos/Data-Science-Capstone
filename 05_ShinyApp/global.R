####################
# Load scripts
####################

# Source the libraries and settings
for (script in list.files("configScripts")) {
  source(file.path("configScripts", script))
}

# Source the model functions and models themselves
for (script in list.files("modelFunctions")) {
  source(file.path("modelFunctions", script))
}

load(freqModelPath)
load(KNModelPath)