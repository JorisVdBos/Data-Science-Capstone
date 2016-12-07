
##############
# Titles
##############

applicationTitle <- "Data Science capstone project: Predicting the next text input"
model1Title <- "Try out the model"
moreInfoTitle <- "More info"


##############
# Widths
##############
sidebarWidth      <- 250
titleWidth        <- 250

##############
# Model settings
##############
modelPath <- "modelObject/freqModel5perc.RData"
defaultNoPos <- 3

doNotPredict <- c("#", "\n", ",")

##############
# Buttons and slider texts
##############
giveNoPosText <- "Give top words:"
placeholderModelInfo <- "Put text here!"
filterText <- "Results filter"
filterNumberText <- "Numbers"
filterEndlineText <- "End line"
filterCommaText <- "Comma's"

##############
# Text
##############
aboutText <- "<p>Welcome to the shiny application of my 'next word' predictor. This app was made for the final course of <a href = \"https://www.coursera.org/specializations/jhu-data-science\">Coursera's Data Science track, the data science capstone project</a>. </p> 
<p>All files and more info regarding the construction of this app, can be found in <a href = \"https://github.com/JorisVdBos/Data-Science-Capstone\">the project's repository</a>. For more information about the setup and the creation of the model, take a look at <a href =  \"http://rpubs.com/JorisVdBos/228218\">the markdown I made</a>.</p>
<br>
<br>
<p>This website was constructed in december 2016 by <a href=\"mailto:joris.vandenbossche@infofarm.be\">Joris Van den Bossche</a>.</p>
<p>Follow me on Twitter!<br>
<a href=\"https://twitter.com/Joris_VdB_\" class=\"twitter-follow-button\" data-show-count=\"false\" data-size=\"large\">Follow @Joris_VdB_</a> <script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document, 'script', 'twitter-wjs');</script></p>
"