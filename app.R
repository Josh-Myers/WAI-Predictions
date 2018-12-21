# shiny app to predict middle ear dysfunction with wai
# to do: could plot just 90% range if not tested one ear (if null plot 90% range and in title, eg, Left ear: Not tested)

# global variables----
library(shiny)
library(plyr)
library(dplyr)
library(gdata)
library(tidyr)
library(xml2)
library(rlist)
library(rms)

theme_set(theme_bw() + theme(legend.key = element_blank()) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                   panel.background = element_blank()) )
load("neonateModel.rda")
neonate.model = f.final
load("sixMonthModel.rda")
six.month.model = f.pca.penal
twelve.month.model = readRDS("twelveMthModel.rds")

# load freq bandwidths tables
load("oct.1.band.rda")
load("oct.2.band.rda")

abs.freqs <- c("abs226", "abs257.33", "abs280.62", "abs297.30", "abs324.21", "abs343.49", "abs363.91", "abs385.55", "abs408.48", "abs432.77", "abs458.50",
               "abs471.94", "abs500.00", "abs514.65", "abs545.25", "abs561.23", "abs577.68", "abs594.60", "abs629.96", "abs648.42", "abs667.42", "abs686.98",
               "abs707.11", "abs727.83", "abs749.15", "abs771.11", "abs793.70", "abs816.96", "abs840.90", "abs865.54", "abs890.90", "abs917.00", "abs943.87",
               "abs971.53", "abs1000.00", "abs1029.30", "abs1059.46", "abs1090.51", "abs1122.46", "abs1155.35", "abs1189.21", "abs1224.05", "abs1259.92", 
               "abs1296.84", "abs1334.84", "abs1373.95", "abs1414.21", "abs1455.65", "abs1498.31", "abs1542.21", "abs1587.40", "abs1633.92", "abs1681.79",
               "abs1731.07", "abs1781.80", "abs1834.01", "abs1887.75", "abs1943.06", "abs2000.00", "abs2058.60", "abs2118.93", "abs2181.02", "abs2244.92",
               "abs2310.71", "abs2378.41", "abs2448.11", "abs2519.84", "abs2593.68", "abs2669.68", "abs2747.91", "abs2828.43", "abs2911.31", "abs2996.61",
               "abs3084.42", "abs3174.80", "abs3267.83", "abs3363.59", "abs3462.15", "abs3563.59", "abs3668.02", "abs3775.50", "abs3886.13", 
               "abs4000.00", "abs4117.21", "abs4237.85", "abs4362.03", "abs4489.85", "abs4621.41", "abs4756.83", "abs4896.21", "abs5039.68", "abs5187.36",
               "abs5339.36", "abs5495.81", "abs5656.85", "abs5822.61", "abs5993.23", "abs6168.84", "abs6349.60", "abs6535.66", "abs6727.17", "abs6924.29",
               "abs7127.19", "abs7336.03", "abs7550.99", "abs7772.26", "abs8000")

mag.freqs <- c("mag226", "mag257.33", "mag280.62", "mag297.30", "mag324.21", "mag343.49", "mag363.91", "mag385.55", "mag408.48", "mag432.77", "mag458.50",
               "mag471.94", "mag500.00", "mag514.65", "mag545.25", "mag561.23", "mag577.68", "mag594.60", "mag629.96", "mag648.42", "mag667.42", "mag686.98",
               "mag707.11", "mag727.83", "mag749.15", "mag771.11", "mag793.70", "mag816.96", "mag840.90", "mag865.54", "mag890.90", "mag917.00", "mag943.87",
               "mag971.53", "mag1000.00", "mag1029.30", "mag1059.46", "mag1090.51", "mag1122.46", "mag1155.35", "mag1189.21", "mag1224.05", "mag1259.92", 
               "mag1296.84", "mag1334.84", "mag1373.95", "mag1414.21", "mag1455.65", "mag1498.31", "mag1542.21", "mag1587.40", "mag1633.92", "mag1681.79",
               "mag1731.07", "mag1781.80", "mag1834.01", "mag1887.75", "mag1943.06", "mag2000.00", "mag2058.60", "mag2118.93", "mag2181.02", "mag2244.92",
               "mag2310.71", "mag2378.41", "mag2448.11", "mag2519.84", "mag2593.68", "mag2669.68", "mag2747.91", "mag2828.43", "mag2911.31", "mag2996.61",
               "mag3084.42", "mag3174.80", "mag3267.83", "mag3363.59", "mag3462.15", "mag3563.59", "mag3668.02", "mag3775.50", "mag3886.13", 
               "mag4000.00", "mag4117.21", "mag4237.85", "mag4362.03", "mag4489.85", "mag4621.41", "mag4756.83", "mag4896.21", "mag5039.68", "mag5187.36",
               "mag5339.36", "mag5495.81", "mag5656.85", "mag5822.61", "mag5993.23", "mag6168.84", "mag6349.60", "mag6535.66", "mag6727.17", "mag6924.29",
               "mag7127.19", "mag7336.03", "mag7550.99", "mag7772.26", "mag8000")

pha.freqs <- c("pha226", "pha257.33", "pha280.62", "pha297.30", "pha324.21", "pha343.49", "pha363.91", "pha385.55", "pha408.48", "pha432.77", "pha458.50",
               "pha471.94", "pha500.00", "pha514.65", "pha545.25", "pha561.23", "pha577.68", "pha594.60", "pha629.96", "pha648.42", "pha667.42", "pha686.98",
               "pha707.11", "pha727.83", "pha749.15", "pha771.11", "pha793.70", "pha816.96", "pha840.90", "pha865.54", "pha890.90", "pha917.00", "pha943.87",
               "pha971.53", "pha1000.00", "pha1029.30", "pha1059.46", "pha1090.51", "pha1122.46", "pha1155.35", "pha1189.21", "pha1224.05", "pha1259.92", 
               "pha1296.84", "pha1334.84", "pha1373.95", "pha1414.21", "pha1455.65", "pha1498.31", "pha1542.21", "pha1587.40", "pha1633.92", "pha1681.79",
               "pha1731.07", "pha1781.80", "pha1834.01", "pha1887.75", "pha1943.06", "pha2000.00", "pha2058.60", "pha2118.93", "pha2181.02", "pha2244.92",
               "pha2310.71", "pha2378.41", "pha2448.11", "pha2519.84", "pha2593.68", "pha2669.68", "pha2747.91", "pha2828.43", "pha2911.31", "pha2996.61",
               "pha3084.42", "pha3174.80", "pha3267.83", "pha3363.59", "pha3462.15", "pha3563.59", "pha3668.02", "pha3775.50", "pha3886.13", 
               "pha4000.00", "pha4117.21", "pha4237.85", "pha4362.03", "pha4489.85", "pha4621.41", "pha4756.83", "pha4896.21", "pha5039.68", "pha5187.36",
               "pha5339.36", "pha5495.81", "pha5656.85", "pha5822.61", "pha5993.23", "pha6168.84", "pha6349.60", "pha6535.66", "pha6727.17", "pha6924.29",
               "pha7127.19", "pha7336.03", "pha7550.99", "pha7772.26", "pha8000")

load("newborn.90.range.rda")
newborn.90.range = newborn.90.range[,c(2,4,5)]
names(newborn.90.range) = c("Frequency", "Ninetyfive", "Five")
load("neonate.90.range.octave.rda")

load("sixmth.90.range.rda")
sixmth.90.range = abs.med.IQR.pass
load("six.mth.90.range.half.octave.rda")
load("sixMonthPCA.rda") # the object is called pca
six.mth.pca = pca
one.freq = c("250", "500", "1000", "2000", "4000", "8000")
two.freq = c("250", "354", "500", "707", "1000", "1414", "2000", "2828", "4000", "5657", "8000")

twelvemth.90.range.2 = readRDS("twelveMth90range.2.rds")
twelvemth.90.range.2 = select(twelvemth.90.range.2, -c(rs, median))
names(twelvemth.90.range.2) = c("Frequency", "Five", "Ninetyfive")
twelvemth.90.range = readRDS("twelveMth90range.rds")
twelvemth.90.range = select(twelvemth.90.range, -c(rs, median))
names(twelvemth.90.range) = c("Frequency", "Five", "Ninetyfive")

# UI ----
ui <- fluidPage(
  headerPanel("WAI Predictions"),
  tabsetPanel(
    tabPanel("Titan", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "age", label = "Select age", choices = c("Newborn", "6-9 months", "10-16 months")),
                 fileInput(inputId = "file1", label = "Choose file", accept = ".xml"),
                 hr(),
                 h5("Example files"),
                 downloadButton("download1", label = "Newborn"),
                 downloadButton("download2", label = "6 months"),
                 downloadButton("download3", label = "12 months A"),
                 downloadButton("download4", label = "12 months B")),
               mainPanel(p(), plotOutput("plot.R"), p(), plotOutput("plot.L"), p())
             )),
    tabPanel("Manual", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput(inputId = "age2", label = "Select age", choices = c("Newborn", "6-9 months", "10-16 months")), 
                            selectInput(inputId = "ear", label = "Ear", choices = c("Right", "Left")),  uiOutput("variables")),
               # issue was having things below the dynamic part - need to put the download somewhere else
               mainPanel(p(), plotOutput("plot.manual"), p(),  
                         tags$style(type="text/css",  # there was an annoying warning message when changing the age selection - this code hides it 
                                    ".shiny-output-error { visibility: hidden; }",
                                    ".shiny-output-error:before { visibility: hidden; }"
                         ))
             )),
    tabPanel("About", fluid = TRUE,
             h4("About the App"),
             p("This web app implements the wideband acoustic immittance (WAI) statistical models for diagnosing middle ear pathology developed by 
               Myers et al. (2018a; 2018b; 2019).
               The models convert a multivariate ambient WAI measurement (226 to 8000 Hz) into a single interpretable value, in the form of a probability 
               between 0 and 1. 
               If probability is 0, there is no chance of middle ear pathology, and if 1, pathology is certain.
               Probability >0.5 indicates that middle ear dysfunction is more likely than not."), 
             h4("Why Predictions?"),
             p("Probabilities are advantageous because they are individualized for each infant, and provide the clinician with more information 
               than a simple pass/refer result. 
               For example, if using probability >0.5 as the diagnostic threshold, 
               one infant may refer with probability of 0.55, and another with 0.95. The diagnosis is much more certain, and the condition likely to 
              be more severe in the latter case, but this information would be 
               lost if both were simply labelled as 'refer'. Furthermore, the decision point can be adjusted depending on the circumstances. 
               For example, if there is no parental concern about hearing or development, a clinician may decide to discharge an infant with an 
               uncertain result (probability = 0.5). 
               However, for another infant with delayed speech and history of recurrent otitis media, the same result may warrent review."),
             h4("Model Variables"),
             p("All of the models use WAI measured at ambient air pressure to make predictions. The newborn model uses WAI 
               results averaged into octave frequency bandwidths, and absorbance at 1000 and 2000 Hz, admittance magnitude at 1000 and 2000 Hz, 
               and admittance phase at 1000 and 4000 Hz are used as variables in the model."),
             p("The model for 6-9 month old infants uses the entire range of absorbance frequencies averaged into half octave bandwidths. 
                Principal component analysis transforms the absorbance variables into a new set of variables called
                 'principal components', which allows the number of variables to be greatly reduced with minimal information loss. 
                The principal components are used as variables in the statistical model to calculate the probability of middle ear dysfunction."),
             p("The 10-16 month model takes 1/2 octave absorbance from 1000 to 5657 Hz as predictors (i.e., 1000, 1414, 2000, 2828, 4000, and 5657 Hz).
                This is an ordinal model, and calculates the probability that the ear has either 'normal', 'mild' or 'severe' middle ear dysfunction. 
                The most likely condtion along with the corresponding probability (P) is presented as the provisional diagnosis. 
                See the articles below for detailed information about the models."), 
             h4("References"),
             p("Myers, J., Kei, J., Aithal, S., Aithal, V., Driscoll, C., Khan, A., Manuel, A., Joseph, A., Malicka, A. N. (2018a). 
               Development of a diagnostic prediction model for conductive conditions in neonates using wideband acoustic immittance. 
               Ear and Hearing, 39(6), 1116-1135."),
             p("Myers, J., Kei, J., Aithal, S., Aithal, V., Driscoll, C., Khan, A., Manuel, A., Joseph, A., Malicka, A. N. (2018b). 
               Diagnosing middle ear pathology in 6- to 9-month-old infants using wideband absorbance: A risk prediction model. 
               Journal of Speech Language and Hearing Research, 61(9), 2386-2404."),
             p("Myers, J., Kei, J., Aithal, S., Aithal, V., Driscoll, C., Khan, A., Manuel, A., Joseph, A., Malicka, A. N. (2019). 
               Diagnosing mild and severe middle ear dysfunction in 10- to 16-month-old infants using wideband absorbance: An ordinal prediction model. 
               Manuscript submitted for publication."),
             br()
             ),
    tabPanel("How to Use", fluid = TRUE,
             br(),
             p("The app uses ambient wideband acoustic immittance (WAI) measurements to calculate the probability of middle ear dysfunction for an infant. 
               You can either upload a file saved from an Interacoustics Titan device, or enter results manually. 
               Currently, the app only has models for newborns,  6-9 month, and 10-16 month old infants.
               The models for the various age groups use WAI results differently to make predictions. 
               The newborn model takes absorbance, admittance magnitude, and admittance phase angle results 
               averaged into octave bandwidths as predictors. 
               The models for the 6-9 month, and 10-16 month infants, however, use absorbance only, averaged into half octave frequency bands."),
             
             h4("Using the App With a Titan Device"),
             p("The 'Titan' tab allows you to upload WAI results using a file created from a session with an Interacoustics Titan device.
               The file saves WAI measurements at 1/24 octave frequency resolution. Once uploaded,
               the app extracts the ambient WAI results and averages them into octave, and half octave bandwidths to be used as model variables in the 
               newborn, 6-9 month, or 10-16 month infant models, respectively."),
             p("To use the app, first save a session from the Titan Suite as an xml file on your computer (Menu > Edit > Export). 
               Then, go to the app, select the 'Titan' tab, select the appropriate age group with the 'Select age' button, 
               and upload your file using the 'Choose file' button."),
             p("A graph of absorbance against the 90% normal range for the selected age group is plotted,
               and the probability of middle ear dysfunction is displayed above the graph. 
               Results for one, or both ears are shown depending if one or two ears were tested in the session."),
             p("The example files (underneath the 'choose file' button) can be downloaded and then uploaded 
               to show how the app works without needing a Titan device."),
             p("A demonstration of using the Titan tab of the app is provided in the video below."),
             HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/KfQRZJ8Xf3k" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>'),
             br(),
             h4("Manually Entering Data"),
             p("Alternatively, you can manually enter your WAI data into the app in the 'Manual' tab. However, you first need to ensure that your results
              are at the appropriate frequency resolution.
              For example, if you are using the newborm model, and your device saved WAI results at 1/12 octave frequency resolution, 
              you would need to first average your results into octave bandwidths before entering data into the app."),
             p("The tables below provide the bandwidths for averaging WAI into octave and half octave frequency bands (for use with the 
               newborn, or infant models, respectively). The first column is the center frequency of the bandwidth, and the second 
               and third columns are the minimum and maximum frequencies of the bandwidth. For example, if you were averaging results 
               into octave bandwidths, for 250 Hz, you would take the average of all frequencies between 226 and 353.55 Hz."),
             h5("Octave Bandwidths"),
             tableOutput("oct1.table"),
             h5("Half Octave Bandwidths"),
             tableOutput("oct2.table"),
              p("An excel file is provided below (click the 'Excel file' button to download it) that takes 1/24 octave WAI results and averages them into 
              octave and half octave frequency bandwidths. You could use this file to average your results if necessary, or you could
              modify it if you need to convert results from a different frequency resolution (e.g., 1/12, or 1/6). 
              There is a demonstration of using the excel file in the video below."),
             p(""),
             downloadButton("download.excel", label = "Excel file"),
             br(),
             p("Once your data are at the appropriate frequency resolution, choose the age, the ear side, enter the results into the text 
               boxes, and press the 'Go' button. A graph of the absorbance against the 90% normal range for the selected age group is shown,
               and the probability of middle ear dysfunction is displayed above the graph.
               The video below gives a demonstration of using the Manual tab."),
             HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/dTTDc6FgRfY?rel=0&amp;start=2" frameborder="0" al>low="autoplay; encrypted-media" allowfullscreen></iframe'),           
             br(),
             p("Please email myers.josh@gmail.com if you have any queries."),
             br()
             )
             ))

# server----    
server <- function(input, output, session) {
  
  # frequency bandwidth tables
  output$oct1.table = renderTable(oct.1.band)
  output$oct2.table = renderTable(oct.2.band)
  
  # create the text boxes for manual data entry
  output$variables <- renderUI({
    switch(input$age2, 
           "Newborn" = list(numericInput("abs250k", "Absorbance 250 Hz", value = NULL), 
                            numericInput("abs500k", "Absorbance 500 Hz", value = NULL), 
                            numericInput("abs1k", "Absorbance 1000 Hz", value = NULL), 
                            numericInput("abs2k", "Absorbance 2000 Hz", value = NULL), 
                            numericInput("abs4k", "Absorbance 4000 Hz", value = NULL), 
                            numericInput("abs8k", "Absorbance 8000 Hz", value = NULL), 
                            numericInput("mag1k", "Admittance Magnitude 1000 Hz (mmho)", value = NULL), 
                            numericInput("mag2k", "Admittance Magnitude 2000 Hz (mmho)", value = NULL), 
                            numericInput("pha1k", "Admittance Phase 1000 Hz (degrees)", value = NULL), 
                            numericInput("pha4k", "Admittance Phase 4000 Hz (degrees)", value = NULL),
                            actionButton("do", "Go")),
           "6-9 months" = list(numericInput("abs250", "Absorbance 250 Hz", value = NULL, min = 0, max = 1), 
                               numericInput("abs354", "Absorbance 354 Hz", value = NULL, min = 0, max = 1),
                               numericInput("abs500", "Absorbance 500 Hz", value = NULL, min = 0, max = 1), 
                               numericInput("abs707", "Absorbance 707 Hz", value = NULL, min = 0, max = 1),
                               numericInput("abs1000", "Absorbance 1000 Hz", value = NULL, min = 0, max = 1), 
                               numericInput("abs1414", "Absorbance 1414 Hz", value = NULL, min = 0, max = 1),
                               numericInput("abs2000", "Absorbance 2000 Hz", value = NULL, min = 0, max = 1), 
                               numericInput("abs2828", "Absorbance 2828 Hz", value = NULL, min = 0, max = 1),
                               numericInput("abs4000", "Absorbance 4000 Hz", value = NULL, min = 0, max = 1), 
                               numericInput("abs5657", "Absorbance 5657 Hz", value = NULL, min = 0, max = 1),
                               numericInput("abs8000", "Absorbance 8000 Hz", value = NULL, min = 0, max = 1),
                               actionButton("do", "Go")),
           "10-16 months" = list(numericInput("a250", "Absorbance 250 Hz", value = NULL, min = 0, max = 1), 
                                 numericInput("a354", "Absorbance 354 Hz", value = NULL, min = 0, max = 1),
                                 numericInput("a500", "Absorbance 500 Hz", value = NULL, min = 0, max = 1), 
                                 numericInput("a707", "Absorbance 707 Hz", value = NULL, min = 0, max = 1),
                                 numericInput("a1000", "Absorbance 1000 Hz", value = NULL, min = 0, max = 1), 
                                 numericInput("a1414", "Absorbance 1414 Hz", value = NULL, min = 0, max = 1),
                                 numericInput("a2000", "Absorbance 2000 Hz", value = NULL, min = 0, max = 1), 
                                 numericInput("a2828", "Absorbance 2828 Hz", value = NULL, min = 0, max = 1),
                                 numericInput("a4000", "Absorbance 4000 Hz", value = NULL, min = 0, max = 1), 
                                 numericInput("a5657", "Absorbance 5657 Hz", value = NULL, min = 0, max = 1),
                                 numericInput("a8000", "Absorbance 8000 Hz", value = NULL, min = 0, max = 1),
                                 actionButton("do", "Go")
           )
    ) 
  })
  # Listen for newborn variables for manual data entry
  abs250k = eventReactive(input$do, {    
    input$abs250k
  })
  abs500k = eventReactive(input$do, {    
    input$abs500k
  })
  abs1k = eventReactive(input$do, {    
    input$abs1k
  })
  abs2k = eventReactive(input$do, {    
    input$abs2k
  })
  abs4k = eventReactive(input$do, {    
    input$abs4k
  })
  abs8k = eventReactive(input$do, {    
    input$abs8k
  })
  mag1k = eventReactive(input$do, {    
    input$mag1k
  })
  mag2k = eventReactive(input$do, {    
    input$mag2k
  })
  pha1k = eventReactive(input$do, {    
    input$pha1k
  })
  pha4k = eventReactive(input$do, {    
    input$pha4k
  })
  # make newborn df (manual)
  newborn.vars = reactive({
    newborn.df = data.frame(abs250k(), abs500k(), abs1k(), abs2k(), abs4k(), abs8k(), mag1k(), mag2k(), pha1k(), pha4k())
    names(newborn.df) = c("abs250", "abs500", "abs1000", "abs2000", "abs4000", "abs8000", "mag1000", "mag2000", "pha1000", "pha4000")
    newborn.df
  })
  
  # Listen for 6mth variables for manual data entry
  abs250 = eventReactive(input$do, {    
    input$abs250
  })
  abs354 = eventReactive(input$do, {    
    input$abs354
  })
  abs500 = eventReactive(input$do, {    
    input$abs500
  })
  abs707 = eventReactive(input$do, {    
    input$abs707
  })
  abs1000 = eventReactive(input$do, {    
    input$abs1000
  })
  abs1414 = eventReactive(input$do, {    
    input$abs1414
  })
  abs2000 = eventReactive(input$do, {    
    input$abs2000
  })
  abs2828 = eventReactive(input$do, {    
    input$abs2828
  })
  abs4000 = eventReactive(input$do, {    
    input$abs4000
  })
  abs5657 = eventReactive(input$do, {    
    input$abs5657
  })
  abs8000 = eventReactive(input$do, {    
    input$abs8000
  })
  # make 6mth df (manual)
  sixmth.vars = reactive({
    sixmth.df = data.frame(abs250(), abs354(), abs500(), abs707(), abs1000(), abs1414(), abs2000(), abs2828(), abs4000(), abs5657(), abs8000())
    names(sixmth.df) = c("abs250", "abs354", "abs500", "abs707", "abs1000", "abs1414", "abs2000", "abs2828", "abs4000", "abs5657", "abs8000")
    sixmth.df
  })
  
  # Listen for 12mth variables for manual data entry
  abs250 = eventReactive(input$do, {    
    input$a250
  })
  abs354 = eventReactive(input$do, {    
    input$a354
  })
  abs500 = eventReactive(input$do, {    
    input$a500
  })
  abs707 = eventReactive(input$do, {    
    input$a707
  })
  
  abs1000 = eventReactive(input$do, {    
    input$a1000
  })
  abs1414 = eventReactive(input$do, {    
    input$a1414
  })
  abs2000 = eventReactive(input$do, {    
    input$a2000
  })
  abs2828 = eventReactive(input$do, {    
    input$a2828
  })
  abs4000 = eventReactive(input$do, {    
    input$a4000
  })
  abs5657 = eventReactive(input$do, {    
    input$a5657
  })
  abs8000 = eventReactive(input$do, {    
    input$a8000
  })
  
  # make 12mth df (manual)
  twelvemth.vars = reactive({
    twelvemth.df = data.frame(abs250(), abs354(), abs500(), abs707(), abs1000(), abs1414(), abs2000(), abs2828(), abs4000(), abs5657(), abs8000())
    names(twelvemth.df) = c("abs250", "abs354", "abs500", "abs707", "abs1000", "abs1414", "abs2000", "abs2828", "abs4000", "abs5657", "abs8000")
    twelvemth.df
  })

  # select the right df for manual 
  manual.data = reactive({
    switch(input$age2, 
           "Newborn" = newborn.vars(),
           "6-9 months" = sixmth.vars(),
           "10-16 months" = twelvemth.vars()
    )
  })
  # select either 1 or 1/2 octave frequencies for manual
  manual.freqs = reactive({
    switch(input$age2, 
           "Newborn" = one.freq,
           "6-9 months" = two.freq,
           "10-16 months" = two.freq)
  })
  
  # clear the data when user changes selection (manual)
  v <- reactiveValues(clearPlot = TRUE)
  
  observeEvent(c(input$age, input$ear), {
    v$clearPlot <- TRUE
  }, priority = 10)
  observeEvent(input$do, {
    v$clearPlot <- FALSE
  }, priority = 10)
  
  # plot the manual data entry (with prediction in title)
  output$plot.manual <- renderPlot({
    
    if (v$clearPlot)
      return()
    else
      
      req(manual.data())
    manual.data = as.data.frame(manual.data())
    
    # str(manual.data)   ## this was useful to see what the variables were when debugging
    pred = vector("double", length = 1)
    if(input$age2 == "Newborn") {
      pred.manual.newborn = predict(neonate.model, manual.data, type = "fitted")
      pred.manual = round(pred.manual.newborn, digits = 2)
    } else if (input$age2 == "6-9 months") {
      pred.manual.pca = predict(six.mth.pca, manual.data)
      pred.manual.pca = pred.manual.pca = pred.manual.pca[,1:5]
      pred.manual = predict(six.month.model, pred.manual.pca, type = "fitted")
      pred.manual = round(pred.manual, digits = 2)
    }
    else {
     pred.manual.12mth = round(predict(twelve.month.model, manual.data, type = "fitted.ind"), 2)
     pred.manual = max(pred.manual.12mth) 
    }
    
    #diagnosis = vector("character", length = 1)
    if (input$age2 == "10-16 months") {
      prob = round(predict(twelve.month.model, manual.data, type = "fitted.ind"), 2)
      names(prob) = c("Normal", "Mild", "Severe")
      pred.12.diagnosis = c("Normal", "Mild", "Severe")[which.max(c(prob))]
    }
    
    plot.data = switch(input$age2, 
                       "Newborn" = manual.data[,1:6],
                       "6-9 months" = manual.data,
                       "10-16 months" = manual.data)
    plot.data =  gather(plot.data, absorbance)
    #str(plot.data)
    req(manual.freqs)
    manual.freqs = as.vector(manual.freqs())
    manual.freqs = as.numeric(manual.freqs)
    #str(manual.freqs)
    plot.data = cbind.data.frame(plot.data, manual.freqs)
    
    names(plot.data) = c("Freq", "Absorbance", "Frequency")
    plot.data$Frequency = as.numeric(plot.data$Frequency)
    #str(plot.data)
    colour = switch(input$ear, 
                    "Right" = "Red",
                    "Left" = "Blue")
    
    range2 = switch(input$age2, 
                    "Newborn" = neonate.90.range.octave,
                    "6-9 months" = sixmth.90.range.half.octave,
                    "10-16 months" = twelvemth.90.range.2)
    
    ggplot(plot.data) +
      scale_x_log10(expand=c(0, 0), breaks=c(250, 500, 1000, 2000, 4000, 8000))  +
      geom_line(aes(x = Frequency, y = Absorbance, colour = colour)) +
      scale_color_manual(values = colour) +
      geom_ribbon(data = range2, aes(x = Frequency, ymin = Five, ymax = Ninetyfive, linetype=NA), alpha = 0.2, show.legend = F) +
      xlab("Frequency, Hz") +
      ylab("Absorbance") +
      scale_y_continuous(expand=c(0, 0), breaks=c(-0.2, 0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0, 1)) +
      theme(legend.text=element_text(size=10), legend.justification=c(0,1)) +
      theme(axis.title.y = element_text(vjust = 0.6)) +
      theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
      ggtitle(
        if (input$age2 == "Newborn" | input$age2 == "6-9 months") {
          paste(input$ear, "ear: Probability =", pred.manual)
        }
        else {
          paste(input$ear, "ear:", pred.12.diagnosis, "(P =", pred.manual, ")")
        }) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.title = element_text(lineheight=.8, face="bold")) +
      theme(plot.title = element_text(vjust=2)) +
      theme(legend.position="none") + 
      theme(aspect.ratio=1/2) +
      #coord_fixed() +
      theme(plot.title = element_text(size=16),
            axis.text=element_text(size=12),
            axis.title=element_text(size=15))
    
  })
  
  ## It might be possible to make the data frames in reactives, rather than the plots - could make it more concise ##
  # right data
  # right predictions - newborn and 6mth
  # left data
  # left predictions - newborn and 6 mth
  # so total of 6 reactives???
  
  # Titan tab
  # import the file
  raw.data <- eventReactive(input$file1, {
    read_xml(input$file1$datapath)
  })
  
  # Titan right data munge, predict and plot
  output$plot.R <- renderPlot({
    data = raw.data()
    data = xml_ns_strip(data)
    names(data)
    nodes = xml_find_all(data, "//Test[./TestName = 'WB Absorbance']")  
    content = xml_contents(nodes)
    content.list = as_list(content)
    wai.data = content.list[[1]][[1]]
    wai.data = wai.data[c(2,3)]
    names(wai.data)
    wai.unlist = unlist(wai.data)
    # first want to know which ears are present
    right.ear.fn = function(x) {
      is.element("Right", x)
    }
    right.present = right.ear.fn(wai.unlist) 
    
    left.ear.fn = function(x) {
      is.element("Left", x)
    }
    left.present = left.ear.fn(wai.unlist)
    ears.present = c(right.present, left.present)
    ears.present = as.vector(ears.present)
    names(ears.present) = c("right.present", "left.present")
    ears = vector("character", length = 1)
    if(ears.present[1] == T & ears.present[2] == T) {
      ears[[1]] <- "both"
    } else if(ears.present[1] == T) {
      ears[[1]] <- "right.only"
    } else if (ears.present[2] == T) {
      ears[[1]] <- "left.only"
    } else {
      ears[[1]] <- NA
    }
    
    if (ears == "both") {
      wai.ear.1 = wai.data[[1]]
      wai.ear.2 = wai.data[[2]]
    } else {
      wai.ear.1 = wai.data[[1]]
      wai.ear.2 = NULL
    }
    # now need to find out if it is L or R
    if (is.element("Right", wai.ear.1$EarSide)) {
      right = wai.ear.1
    } else if (is.element("Right", wai.ear.2$EarSide)) {
      right = wai.ear.2
    } else {
      right = NULL
    }
    
    if (is.element("Left", wai.ear.1$EarSide)) {
      left = wai.ear.1
    } else if (is.element("Left", wai.ear.2$EarSide)) {
      left = wai.ear.2
    } else {
      left = NULL
    }
    
    if (!is.null(right)) {
      right = right$Measurement
      right = right[-c(1:6)]
      freq = list.map(right, Frequency)
      freq = unlist(freq)
      
      abs.r = list.map(right, Absorbance)
      abs.r = unlist(abs.r)
      abs.r = as.numeric(abs.r)
      abs.r[abs.r < 0] <- 0
      
      mag.r = list.map(right, YAdmittance)
      mag.r = unlist(mag.r)
      mag.r = as.numeric(mag.r)
      pha.r = list.map(right, Phase)
      pha.r = unlist(pha.r)
      pha.r = as.numeric(pha.r)
      
      # right.df is abs mag pha long form
      right.data = rbind.data.frame(abs.r, mag.r, pha.r)
      colnames(right.data) = freq
    }
    
    # predictions (titan)
    # newborn (titan)
    req(right.data)
    abs.r = right.data[1,]
    names(abs.r) = abs.freqs
    abs1000.r	<- transmute(abs.r, abs.1000.r = (abs727.83 + abs749.15 + abs771.11 + abs793.70 + abs816.96 + abs840.90 + abs865.54 + abs890.90 + abs917.00 + abs943.87 +
                                                  abs971.53 + abs1000.00 + abs1029.30 + abs1059.46 + abs1090.51 + abs1122.46 + abs1155.35 + abs1189.21 + abs1224.05 + abs1259.92 +
                                                  abs1296.84 + abs1334.84 + abs1373.95 + abs1414.21)/24) # 707.12 - 1414.21
    abs2000.r	<- transmute(abs.r, abs2000.r = (abs1455.65 + abs1498.31 + abs1542.21 + abs1587.40 + abs1633.92 + abs1681.79 +
                                                 abs1731.07 + abs1781.80 + abs1834.01 + abs1887.75 + abs1943.06 + abs2000.00 + abs2058.60 + abs2118.93 + abs2181.02 + abs2244.92 +
                                                 abs2310.71 + abs2378.41 + abs2448.11 + abs2519.84 + abs2593.68 + abs2669.68 + abs2747.91 + abs2828.43)/24) # 1414.22 - 2828.43
    mag.r = right.data[2,]
    names(mag.r) = mag.freqs
    
    mag1000.r	<- transmute(mag.r, mag.1000.r = (mag727.83 + mag749.15 + mag771.11 + mag793.70 + mag816.96 + mag840.90 + mag865.54 + mag890.90 + mag917.00 + mag943.87 +
                                                  mag971.53 + mag1000.00 + mag1029.30 + mag1059.46 + mag1090.51 + mag1122.46 + mag1155.35 + mag1189.21 + mag1224.05 + mag1259.92 +
                                                  mag1296.84 + mag1334.84 + mag1373.95 + mag1414.21)/24) # 707.12 - 1414.21
    mag2000.r	<- transmute(mag.r, mag2000.r = (mag1455.65 + mag1498.31 + mag1542.21 + mag1587.40 + mag1633.92 + mag1681.79 +
                                                 mag1731.07 + mag1781.80 + mag1834.01 + mag1887.75 + mag1943.06 + mag2000.00 + mag2058.60 + mag2118.93 + mag2181.02 + mag2244.92 +
                                                 mag2310.71 + mag2378.41 + mag2448.11 + mag2519.84 + mag2593.68 + mag2669.68 + mag2747.91 + mag2828.43)/24) # 1414.22 - 2828.43
    pha.r = right.data[3,]
    names(pha.r) = pha.freqs
    pha1000.r	<- transmute(pha.r, pha1000.r = (pha727.83 + pha749.15 + pha771.11 + pha793.70 + pha816.96 + pha840.90 + pha865.54 + pha890.90 + pha917.00 + pha943.87 +
                                                 pha971.53 + pha1000.00 + pha1029.30 + pha1059.46 + pha1090.51 + pha1122.46 + pha1155.35 + pha1189.21 + pha1224.05 + pha1259.92 +
                                                 pha1296.84 + pha1334.84 + pha1373.95 + pha1414.21)/24) # 707.12 - 1414.21
    pha4000.r	<- transmute(pha.r, pha4000.r = ( pha2911.31 +  pha2996.61 + pha3084.42 +  pha3174.80 +  pha3267.83 +  pha3363.59 +  pha3462.15 +  pha3563.59 +
                                                  pha3668.02 +  pha3775.50 +  pha3886.13 + pha4000.00 +  pha4117.21 +  pha4237.85 +  pha4362.03 +  pha4489.85 +
                                                  pha4621.41 +  pha4756.83 +  pha4896.21 +  pha5039.68 +  pha5187.36 + pha5339.36 +  pha5495.81 +  pha5656.85)/24) # 2828.44 - 5656.85
    preds.r = cbind.data.frame(abs1000.r, abs2000.r, mag1000.r, mag2000.r, pha1000.r, pha4000.r)
    names(preds.r) = c("abs1000", "abs2000", "mag1000", "mag2000", "pha1000", "pha4000")
    newborn.pred.r = predict(neonate.model, preds.r, type="fitted")
    newborn.pred.r = round(newborn.pred.r, digits = 2)
    
    # 6 mths (titan)
    req(right.data)
    abs.r = right.data[1,]
    names(abs.r) = abs.freqs
    abs250.r <- transmute(abs.r, abs250.r = (abs226 +	abs257.33 +	abs280.62 +	abs297.30)/4) # 226 - 297.30
    abs354.r <- transmute(abs.r, abs354.r = (abs324.21 +	abs343.49 +	abs363.91 +	abs385.55 +	abs408.48)/5) # 297.31 - 420.45
    abs500.r <- transmute(abs.r, abs500.r = (abs432.77 +	abs458.50 +	abs471.94 +	abs500.00 +	abs514.65 +	abs545.25 +	abs561.23 +	
                                               abs577.68 +	abs594.60)/9) # 420.46 - 594.60
    abs707.r <- transmute(abs.r, abs707.r = (abs629.96 +	abs648.42 +	abs667.42 +	abs686.98 +	abs707.11 +	abs727.83 +	abs749.15 +	
                                               abs771.11 +	abs793.70 +	abs816.96 +	abs840.90)/11) # 594.61 - 840.90
    abs1000.r <- transmute(abs.r, abs1000.r = (abs865.54 +	abs890.90 +	abs917.00 +	abs943.87 +	abs971.53 +	abs1000.00 +	abs1029.30 +	
                                                 abs1059.46 +	abs1090.51 +	abs1122.46 +	abs1155.35 +	abs1189.21)/12) # 840.91 - 1189.21
    abs1414.r <- transmute(abs.r, abs1414.r = (abs1224.05 +	abs1259.92 +	abs1296.84 +	abs1334.84 +	abs1373.95 +	abs1414.21 +
                                                 abs1455.65 +	abs1498.31 +	abs1542.21 +	abs1587.40 +	abs1633.92 +	abs1681.79)/12) # 1189.22 - 1681.79
    abs2000.r <- transmute(abs.r, abs2000.r = (abs1731.07 +	abs1781.80 +	abs1834.01 +	abs1887.75 +	abs1943.06 +	abs2000.00 +
                                                 abs2058.60 +	abs2118.93 +	abs2181.02 +	abs2244.92 +	abs2310.71 +	abs2378.41)/12) # 1681.80 - 2378.41
    abs2828.r <- transmute(abs.r, abs2828.r = (abs2448.11 +	abs2519.84 +	abs2593.68 +	abs2669.68 +	abs2747.91 +	abs2828.43 +
                                                 abs2911.31 +	abs2996.61 +	abs3084.42 +	abs3174.80 +	abs3267.83 +	abs3363.59)/12) # 2378.42 - 3363.59
    abs4000.r <- transmute(abs.r, abs4000.r = (abs3462.15 +	abs3563.59 +	abs3668.02 +	abs3775.50 +	abs3886.13 +	abs4000.00 +	
                                                 abs4117.21 +	abs4237.85 +	abs4362.03 +	abs4489.85 +	abs4621.41 +	abs4756.83)/12) # 3363.60 - 4756.83
    abs5657.r <- transmute(abs.r, abs5657.r = (abs4896.21 +	abs5039.68 +	abs5187.36 +	abs5339.36 +	abs5495.81 +	abs5656.85 +
                                                 abs5822.61 +	abs5993.23 +	abs6168.84 +	abs6349.60 +	abs6535.66 +	abs6727.17)/12) # 4756.84 - 6727.17
    abs8000.r <- transmute(abs.r, abs8000.r = (abs6924.29 +	abs7127.19 +	abs7336.03 +	abs7550.99 +	abs7772.26 +	abs8000)/6) # 6727.18 - 8000
    preds.r = cbind.data.frame(abs250.r, abs354.r, abs500.r, abs707.r, abs1000.r, abs1414.r, abs2000.r, abs2828.r, abs4000.r, abs5657.r, abs8000.r)
    names(preds.r) = c("abs250", "abs354", "abs500", "abs707", "abs1000", "abs1414", "abs2000", "abs2828", "abs4000", "abs5657", "abs8000")
    six.mth.pca2 = pca
    sixmth.pca.r = predict(six.mth.pca2, preds.r)
    sixmth.pca.r = sixmth.pca.r[,1:5]
    sixmth.pred.r = predict(six.month.model, sixmth.pca.r, type="fitted")
    sixmth.pred.r = round(sixmth.pred.r, digits = 2)
   
    
    # 12 mths (titan)
    req(right.data)
    abs.r = right.data[1,]
    names(abs.r) = abs.freqs
    abs1000.r <- transmute(abs.r, abs1000.r = (abs865.54 +	abs890.90 +	abs917.00 +	abs943.87 +	abs971.53 +	abs1000.00 +	abs1029.30 +	
                                                 abs1059.46 +	abs1090.51 +	abs1122.46 +	abs1155.35 +	abs1189.21)/12) # 840.91 - 1189.21
    abs1414.r <- transmute(abs.r, abs1414.r = (abs1224.05 +	abs1259.92 +	abs1296.84 +	abs1334.84 +	abs1373.95 +	abs1414.21 +
                                                 abs1455.65 +	abs1498.31 +	abs1542.21 +	abs1587.40 +	abs1633.92 +	abs1681.79)/12) # 1189.22 - 1681.79
    abs2000.r <- transmute(abs.r, abs2000.r = (abs1731.07 +	abs1781.80 +	abs1834.01 +	abs1887.75 +	abs1943.06 +	abs2000.00 +
                                                 abs2058.60 +	abs2118.93 +	abs2181.02 +	abs2244.92 +	abs2310.71 +	abs2378.41)/12) # 1681.80 - 2378.41
    abs2828.r <- transmute(abs.r, abs2828.r = (abs2448.11 +	abs2519.84 +	abs2593.68 +	abs2669.68 +	abs2747.91 +	abs2828.43 +
                                                 abs2911.31 +	abs2996.61 +	abs3084.42 +	abs3174.80 +	abs3267.83 +	abs3363.59)/12) # 2378.42 - 3363.59
    abs4000.r <- transmute(abs.r, abs4000.r = (abs3462.15 +	abs3563.59 +	abs3668.02 +	abs3775.50 +	abs3886.13 +	abs4000.00 +	
                                                 abs4117.21 +	abs4237.85 +	abs4362.03 +	abs4489.85 +	abs4621.41 +	abs4756.83)/12) # 3363.60 - 4756.83
    abs5657.r <- transmute(abs.r, abs5657.r = (abs4896.21 +	abs5039.68 +	abs5187.36 +	abs5339.36 +	abs5495.81 +	abs5656.85 +
                                                 abs5822.61 +	abs5993.23 +	abs6168.84 +	abs6349.60 +	abs6535.66 +	abs6727.17)/12) # 4756.84 - 6727.17
    preds.r = cbind.data.frame(abs1000.r, abs1414.r, abs2000.r, abs2828.r, abs4000.r, abs5657.r)
    names(preds.r) = c("abs1000", "abs1414", "abs2000", "abs2828", "abs4000", "abs5657")
    twelvemth.pred.r = round(predict(twelve.month.model, preds.r, type = "fitted.ind"), 2)
    twelvemth.prob.r = max(twelvemth.pred.r)
    names(twelvemth.pred.r) = c("Normal", "Mild", "Severe")
    pred.12.diagnosis.r = c("Normal", "Mild", "Severe")[which.max(c(twelvemth.pred.r))]
  
    # choose the prediction (titan)
    pred <- switch(input$age, 
                   "Newborn" = newborn.pred.r,
                   "6-9 months" = sixmth.pred.r, 
                   "10-16 months" = twelvemth.prob.r)
    
    # and the correct 90% range (titan)
    range <- switch(input$age, 
                    "Newborn" = newborn.90.range,
                    "6-9 months" = sixmth.90.range, 
                    "10-16 months" = twelvemth.90.range) 
    
    # get data for plotting (titan)
    req(right.data)
    data.r <- right.data[1,]
    data.r =  gather(data.r, Frequency, absorbance, 1:107)
    names(data.r) = c("Frequency", "Absorbance")
    data.r$Frequency = as.numeric(data.r$Frequency)
    
    # plot (titan)
    ggplot(data.r) +
      scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
      geom_line(aes(x= Frequency, y=Absorbance, colour="Red")) +
      scale_color_manual(values = "Red") +
      geom_ribbon(data = range, aes(x = Frequency, ymin = Five, ymax = Ninetyfive, linetype=NA), alpha = 0.2, show.legend = F) +
      xlab("Frequency, Hz") +
      ylab("Absorbance") +
      scale_y_continuous(expand=c(0, 0), breaks=c(-0.2, 0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0, 1)) +
      theme(legend.text=element_text(size=10), legend.justification=c(0,1)) +
      theme(axis.title.y = element_text(vjust = 0.6)) +
      theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
      ggtitle(
        if (input$age == "Newborn" | input$age == "6-9 months") {
          paste("Right", "ear: Probability =", pred)
        }
        else {
          paste("Right ear:", pred.12.diagnosis.r, "(P =", pred, ")")
        }) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.title = element_text(lineheight=.8, face="bold")) +
      theme(plot.title = element_text(vjust=2)) +
      theme(legend.position="none") + 
      theme(aspect.ratio=1/2) +
      #coord_fixed() +
      theme(plot.title = element_text(size=16),
            axis.text=element_text(size=12),
            axis.title=element_text(size=15))
  })
  
  # Titan left munge, predict, plot
  # munge
  output$plot.L <- renderPlot({
    data = raw.data()
    data = xml_ns_strip(data)
    names(data)
    nodes = xml_find_all(data, "//Test[./TestName = 'WB Absorbance']")  
    content = xml_contents(nodes)
    content.list = as_list(content)
    wai.data = content.list[[1]][[1]]
    #jsonedit(wai.data)
    wai.data = wai.data[c(2,3)]
    #jsonedit(wai.data) # Measured.1 was created by jsonedit
    names(wai.data)
    wai.unlist = unlist(wai.data)
    # first want to know which ears are present
    right.ear.fn = function(x) {
      is.element("Right", x)
    }
    right.present = right.ear.fn(wai.unlist) 
    
    left.ear.fn = function(x) {
      is.element("Left", x)
    }
    left.present = left.ear.fn(wai.unlist)
    ears.present = c(right.present, left.present)
    ears.present = as.vector(ears.present)
    names(ears.present) = c("right.present", "left.present")
    ears = vector("character", length = 1)
    if(ears.present[1] == T & ears.present[2] == T) {
      ears[[1]] <- "both"
    } else if(ears.present[1] == T) {
      ears[[1]] <- "right.only"
    } else if (ears.present[2] == T) {
      ears[[1]] <- "left.only"
    } else {
      ears[[1]] <- NA
    }
    
    if (ears == "both") {
      wai.ear.1 = wai.data[[1]]
      wai.ear.2 = wai.data[[2]]
    } else {
      wai.ear.1 = wai.data[[1]]
      wai.ear.2 = NULL
    }
    # now need to find out if it is L or R
    if (is.element("Right", wai.ear.1$EarSide)) {
      right = wai.ear.1
    } else if (is.element("Right", wai.ear.2$EarSide)) {
      right = wai.ear.2
    } else {
      right = NULL
    }
    
    if (is.element("Left", wai.ear.1$EarSide)) {
      left = wai.ear.1
    } else if (is.element("Left", wai.ear.2$EarSide)) {
      left = wai.ear.2
    } else {
      left = NULL
    }
    
    if (!is.null(left)) {
      left = left$Measurement
      left = left[-c(1:6)]
      freq = list.map(left, Frequency)
      freq = unlist(freq)
      abs.l = list.map(left, Absorbance)
      abs.l = unlist(abs.l)
      abs.l = as.numeric(abs.l)
      abs.l[abs.l < 0] <- 0
      mag.l = list.map(left, YAdmittance)
      mag.l = unlist(mag.l)
      mag.l = as.numeric(mag.l)
      pha.l = list.map(left, Phase)
      pha.l = unlist(pha.l)
      pha.l = as.numeric(pha.l)
      left.data = rbind.data.frame(abs.l, mag.l, pha.l)
      colnames(left.data) = freq
    }
    
    # predictions (titan)
    # newborn (titan)
    req(left.data)
    abs.l = left.data[1,]
    names(abs.l) = abs.freqs
    abs1000.l	<- transmute(abs.l, abs.1000.l = (abs727.83 + abs749.15 + abs771.11 + abs793.70 + abs816.96 + abs840.90 + abs865.54 + abs890.90 + abs917.00 + abs943.87 +
                                                  abs971.53 + abs1000.00 + abs1029.30 + abs1059.46 + abs1090.51 + abs1122.46 + abs1155.35 + abs1189.21 + abs1224.05 + abs1259.92 +
                                                  abs1296.84 + abs1334.84 + abs1373.95 + abs1414.21)/24) # 707.12 - 1414.21
    abs2000.l	<- transmute(abs.l, abs2000.l = (abs1455.65 + abs1498.31 + abs1542.21 + abs1587.40 + abs1633.92 + abs1681.79 +
                                                 abs1731.07 + abs1781.80 + abs1834.01 + abs1887.75 + abs1943.06 + abs2000.00 + abs2058.60 + abs2118.93 + abs2181.02 + abs2244.92 +
                                                 abs2310.71 + abs2378.41 + abs2448.11 + abs2519.84 + abs2593.68 + abs2669.68 + abs2747.91 + abs2828.43)/24) # 1414.22 - 2828.43
    mag.l = left.data[2,]
    names(mag.l) = mag.freqs
    
    mag1000.l	<- transmute(mag.l, mag.1000.l = (mag727.83 + mag749.15 + mag771.11 + mag793.70 + mag816.96 + mag840.90 + mag865.54 + mag890.90 + mag917.00 + mag943.87 +
                                                  mag971.53 + mag1000.00 + mag1029.30 + mag1059.46 + mag1090.51 + mag1122.46 + mag1155.35 + mag1189.21 + mag1224.05 + mag1259.92 +
                                                  mag1296.84 + mag1334.84 + mag1373.95 + mag1414.21)/24) # 707.12 - 1414.21
    mag2000.l	<- transmute(mag.l, mag2000.l = (mag1455.65 + mag1498.31 + mag1542.21 + mag1587.40 + mag1633.92 + mag1681.79 +
                                                 mag1731.07 + mag1781.80 + mag1834.01 + mag1887.75 + mag1943.06 + mag2000.00 + mag2058.60 + mag2118.93 + mag2181.02 + mag2244.92 +
                                                 mag2310.71 + mag2378.41 + mag2448.11 + mag2519.84 + mag2593.68 + mag2669.68 + mag2747.91 + mag2828.43)/24) # 1414.22 - 2828.43
    pha.l = left.data[3,]
    names(pha.l) = pha.freqs
    pha1000.l	<- transmute(pha.l, pha1000.l = (pha727.83 + pha749.15 + pha771.11 + pha793.70 + pha816.96 + pha840.90 + pha865.54 + pha890.90 + pha917.00 + pha943.87 +
                                                 pha971.53 + pha1000.00 + pha1029.30 + pha1059.46 + pha1090.51 + pha1122.46 + pha1155.35 + pha1189.21 + pha1224.05 + pha1259.92 +
                                                 pha1296.84 + pha1334.84 + pha1373.95 + pha1414.21)/24) # 707.12 - 1414.21
    pha4000.l	<- transmute(pha.l, pha4000.l = ( pha2911.31 +  pha2996.61 + pha3084.42 +  pha3174.80 +  pha3267.83 +  pha3363.59 +  pha3462.15 +  pha3563.59 +
                                                  pha3668.02 +  pha3775.50 +  pha3886.13 + pha4000.00 +  pha4117.21 +  pha4237.85 +  pha4362.03 +  pha4489.85 +
                                                  pha4621.41 +  pha4756.83 +  pha4896.21 +  pha5039.68 +  pha5187.36 + pha5339.36 +  pha5495.81 +  pha5656.85)/24) # 2828.44 - 5656.85
    preds.l = cbind.data.frame(abs1000.l, abs2000.l, mag1000.l, mag2000.l, pha1000.l, pha4000.l)
    names(preds.l) = c("abs1000", "abs2000", "mag1000", "mag2000", "pha1000", "pha4000")
    newborn.pred.l = predict(neonate.model, preds.l, type="fitted")
    newborn.pred.l = round(newborn.pred.l, digits = 2)
    
    #6 mth prediction (titan)
    req(left.data)
    abs.l = left.data[1,]
    names(abs.l) = abs.freqs
    abs250.l <- transmute(abs.l, abs250.l = (abs226 +	abs257.33 +	abs280.62 +	abs297.30)/4) # 226 - 297.30
    abs354.l <- transmute(abs.l, abs354.l = (abs324.21 +	abs343.49 +	abs363.91 +	abs385.55 +	abs408.48)/5) # 297.31 - 420.45
    abs500.l <- transmute(abs.l, abs500.l = (abs432.77 +	abs458.50 +	abs471.94 +	abs500.00 +	abs514.65 +	abs545.25 +	abs561.23 +	
                                               abs577.68 +	abs594.60)/9) # 420.46 - 594.60
    abs707.l <- transmute(abs.l, abs707.l = (abs629.96 +	abs648.42 +	abs667.42 +	abs686.98 +	abs707.11 +	abs727.83 +	abs749.15 +	
                                               abs771.11 +	abs793.70 +	abs816.96 +	abs840.90)/11) # 594.61 - 840.90
    abs1000.l <- transmute(abs.l, abs1000.l = (abs865.54 +	abs890.90 +	abs917.00 +	abs943.87 +	abs971.53 +	abs1000.00 +	abs1029.30 +	
                                                 abs1059.46 +	abs1090.51 +	abs1122.46 +	abs1155.35 +	abs1189.21)/12) # 840.91 - 1189.21
    abs1414.l <- transmute(abs.l, abs1414.l = (abs1224.05 +	abs1259.92 +	abs1296.84 +	abs1334.84 +	abs1373.95 +	abs1414.21 +
                                                 abs1455.65 +	abs1498.31 +	abs1542.21 +	abs1587.40 +	abs1633.92 +	abs1681.79)/12) # 1189.22 - 1681.79
    abs2000.l <- transmute(abs.l, abs2000.l = (abs1731.07 +	abs1781.80 +	abs1834.01 +	abs1887.75 +	abs1943.06 +	abs2000.00 +
                                                 abs2058.60 +	abs2118.93 +	abs2181.02 +	abs2244.92 +	abs2310.71 +	abs2378.41)/12) # 1681.80 - 2378.41
    abs2828.l <- transmute(abs.l, abs2828.l = (abs2448.11 +	abs2519.84 +	abs2593.68 +	abs2669.68 +	abs2747.91 +	abs2828.43 +
                                                 abs2911.31 +	abs2996.61 +	abs3084.42 +	abs3174.80 +	abs3267.83 +	abs3363.59)/12) # 2378.42 - 3363.59
    abs4000.l <- transmute(abs.l, abs4000.l = (abs3462.15 +	abs3563.59 +	abs3668.02 +	abs3775.50 +	abs3886.13 +	abs4000.00 +	
                                                 abs4117.21 +	abs4237.85 +	abs4362.03 +	abs4489.85 +	abs4621.41 +	abs4756.83)/12) # 3363.60 - 4756.83
    abs5657.l <- transmute(abs.l, abs5657.l = (abs4896.21 +	abs5039.68 +	abs5187.36 +	abs5339.36 +	abs5495.81 +	abs5656.85 +
                                                 abs5822.61 +	abs5993.23 +	abs6168.84 +	abs6349.60 +	abs6535.66 +	abs6727.17)/12) # 4756.84 - 6727.17
    abs8000.l <- transmute(abs.l, abs8000.l = (abs6924.29 +	abs7127.19 +	abs7336.03 +	abs7550.99 +	abs7772.26 +	abs8000)/6) # 6727.18 - 8000
    preds.l = cbind.data.frame(abs250.l, abs354.l, abs500.l, abs707.l, abs1000.l, abs1414.l, abs2000.l, abs2828.l, abs4000.l, abs5657.l, abs8000.l)
    names(preds.l) = c("abs250", "abs354", "abs500", "abs707", "abs1000", "abs1414", "abs2000", "abs2828", "abs4000", "abs5657", "abs8000")
    six.mth.pca2 = pca
    sixmth.pca.l = predict(six.mth.pca2, preds.l)
    sixmth.pca.l = sixmth.pca.l[,1:5]
    sixmth.pred.l = predict(six.month.model, sixmth.pca.l, type="fitted")
    sixmth.pred.l = round(sixmth.pred.l, digits = 2)
    
    # 12 mths (titan)
    req(left.data)
    abs.l = left.data[1,]
    names(abs.l) = abs.freqs
    abs1000.l <- transmute(abs.l, abs1000.l = (abs865.54 +	abs890.90 +	abs917.00 +	abs943.87 +	abs971.53 +	abs1000.00 +	abs1029.30 +	
                                                 abs1059.46 +	abs1090.51 +	abs1122.46 +	abs1155.35 +	abs1189.21)/12) # 840.91 - 1189.21
    abs1414.l <- transmute(abs.l, abs1414.l = (abs1224.05 +	abs1259.92 +	abs1296.84 +	abs1334.84 +	abs1373.95 +	abs1414.21 +
                                                 abs1455.65 +	abs1498.31 +	abs1542.21 +	abs1587.40 +	abs1633.92 +	abs1681.79)/12) # 1189.22 - 1681.79
    abs2000.l <- transmute(abs.l, abs2000.l = (abs1731.07 +	abs1781.80 +	abs1834.01 +	abs1887.75 +	abs1943.06 +	abs2000.00 +
                                                 abs2058.60 +	abs2118.93 +	abs2181.02 +	abs2244.92 +	abs2310.71 +	abs2378.41)/12) # 1681.80 - 2378.41
    abs2828.l <- transmute(abs.l, abs2828.l = (abs2448.11 +	abs2519.84 +	abs2593.68 +	abs2669.68 +	abs2747.91 +	abs2828.43 +
                                                 abs2911.31 +	abs2996.61 +	abs3084.42 +	abs3174.80 +	abs3267.83 +	abs3363.59)/12) # 2378.42 - 3363.59
    abs4000.l <- transmute(abs.l, abs4000.l = (abs3462.15 +	abs3563.59 +	abs3668.02 +	abs3775.50 +	abs3886.13 +	abs4000.00 +	
                                                 abs4117.21 +	abs4237.85 +	abs4362.03 +	abs4489.85 +	abs4621.41 +	abs4756.83)/12) # 3363.60 - 4756.83
    abs5657.l <- transmute(abs.l, abs5657.l = (abs4896.21 +	abs5039.68 +	abs5187.36 +	abs5339.36 +	abs5495.81 +	abs5656.85 +
                                                 abs5822.61 +	abs5993.23 +	abs6168.84 +	abs6349.60 +	abs6535.66 +	abs6727.17)/12) # 4756.84 - 6727.17
    preds.l = cbind.data.frame(abs1000.l, abs1414.l, abs2000.l, abs2828.l, abs4000.l, abs5657.l)
    names(preds.l) = c("abs1000", "abs1414", "abs2000", "abs2828", "abs4000", "abs5657")
    twelvemth.pred.l = round(predict(twelve.month.model, preds.l, type = "fitted.ind"), 2)
    twelvemth.prob.l = max(twelvemth.pred.l)
    names(twelvemth.pred.l) = c("Normal", "Mild", "Severe")
    pred.12.diagnosis.l = c("Normal", "Mild", "Severe")[which.max(c(twelvemth.pred.l))]
    
    
    # choose the prediction (titan)
    pred <- switch(input$age, 
                   "Newborn" = newborn.pred.l,
                   "6-9 months" = sixmth.pred.l,
                   "10-16 months" = twelvemth.prob.l)
    
    # and the correct 90% range (titan)
    range <- switch(input$age, 
                    "Newborn" = newborn.90.range,
                    "6-9 months" = sixmth.90.range, 
                    "10-16 months" = twelvemth.90.range) 
    
    # get data for plotting (titan)
    data.l <- left.data[1,]
    data.l =  gather(data.l, Frequency, absorbance, 1:107)
    names(data.l) = c("Frequency", "Absorbance")
    data.l$Frequency = as.numeric(data.l$Frequency)
    
    # plot (titan)
    ggplot(data.l) +
      scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
      geom_line(aes(x= Frequency, y=Absorbance, colour="Blue")) +
      scale_color_manual(values = "Blue") +
      geom_ribbon(data = range, aes(x = Frequency, ymin = Five, ymax = Ninetyfive, linetype=NA), alpha = 0.2, show.legend = F) +
      xlab("Frequency, Hz") +
      ylab("Absorbance") +
      scale_y_continuous(expand=c(0, 0), breaks=c(-0.2, 0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0, 1)) +
      theme(legend.text=element_text(size=10), legend.justification=c(0,1)) +
      theme(axis.title.y = element_text(vjust = 0.6)) +
      theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
      ggtitle(
        if (input$age == "Newborn" | input$age == "6-9 months") {
          paste("Left", "ear: Probability =", pred)
        }
        else {
          paste("Left ear:", pred.12.diagnosis.l, "(P =", pred, ")")
        }) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.title = element_text(lineheight=.8, face="bold")) +
      theme(plot.title = element_text(vjust=2)) +
      theme(legend.position="none") + 
      theme(aspect.ratio=1/2) +
      #coord_fixed() +
      theme(plot.title = element_text(size=16),
            axis.text=element_text(size=12),
            axis.title=element_text(size=15))
  })
  
  # Titan download files
  output$download1 <- downloadHandler(
    filename <- function() {
      paste("newborn", "xml", sep=".")
    },
    
    content <- function(file) {
      file.copy("newborn.xml", file)
    })
  
  output$download2 <- downloadHandler(
    filename <- function() {
      paste("sixmth", "xml", sep=".")
    },
    
    content <- function(file) {
      file.copy("sixmth.xml", file)
    })
  
  output$download3 <- downloadHandler(
    filename <- function() {
      paste("twelveMild", "xml", sep=".")
    },
    
    content <- function(file) {
      file.copy("twelveMild.xml", file)
    })
  
  output$download4 <- downloadHandler(
    filename <- function() {
      paste("twelveSevere", "xml", sep=".")
    },
    
    content <- function(file) {
      file.copy("twelveSevere.xml", file)
    })
  
  # Excel download file for averaging WAI into 1 and 1/2 octave frequency resolution
  output$download.excel <- downloadHandler(
    filename <- function() {
      paste("WAI averaging", "xlsx", sep=".")
    },
    
    content <- function(file) {
      file.copy("WAI averaging.xlsx", file)
    })
  
}
# Run the application 
shinyApp(ui = ui, server = server)

