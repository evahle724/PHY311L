#R Version 3.4.3
#TJ Wiegman
#PHY311 Nuclear Physics
#Sept 18, 2018

#Setup
orig.options <- options("stringsAsFactors")[[1]]
options(stringsAsFactors = FALSE)

#Input the data
labData <- data.frame(
  #material and sample identifiers
  Material = c(rep(1, times = 2),
               rep(2, times = 4),
               rep(3, times = 4)),
  Sample = as.character(c("E", "F",
             "J", "K", "L", "M",
             "Q", "R", "S", "T")),
  
  #mass in g
  Mass = c(3.88, 5.18,
           12.67, 16.19, 20.95, 26.23,
           46.34, 65.89, 116.70, 218.4),
  Mass.Uncertainty = c(rep(0.01, times = 9),
                      0.1),
  
  #width in cm
  X = c(7.00, 7.00,
        6.98, 6.99, 6.99, 6.90,
        6.31, 5.30, 5.30, 5.30),
  X.Uncertainty = rep(0.05, times = 10),
  
  #height in cm
  Y = c(7,7,
        6.96, 6.91, 6.94, 7.00,
        6.31, 5.30, 5.30, 5.30),
  Y.Uncertainty = rep(0.05, times = 10),
  
  #thickness in mm
  Z = c(0.77, 1.01,
        0.96, 1.26, 1.59, 1.97,
        0.95, 2.55, 4.01, 6.19),
  Z.Uncertainty = rep(0.05, times = 10)
)

#Calculate the area in cm^2
labData$Area <- with(labData, X*Y)
labData$Area.Uncertainty <- with(labData, {
  sqrt((Y^2)*(X.Uncertainty^2) + (X^2)*(Y.Uncertainty^2))
})

#Convert thickness to cm
labData$Z <- labData$Z / 10
labData$Z.Uncertainty <- labData$Z.Uncertainty / 10

#Calculate the volume in cm^3
labData$Volume <- with(labData, Area*Z)
labData$Volume.Uncertainty <- with(labData, {
  sqrt((Z^2)*(Area.Uncertainty^2) + (Area^2)*(Z.Uncertainty^2))
})

#Calculate the density in g/cm^3
labData$Density <- with(labData, Mass/Volume)
labData$Density.Uncertainty <- with(labData, {
  sqrt(
    ((Mass.Uncertainty^2)/(Volume^2)) + 
      (((Mass^2)*Volume.Uncertainty^2)/(Volume^4))
  )
})

textable <- function(x, form = list(1, "col", "pm")) {
  #form is a list showing the format for the columns; integers indicate the column of x to be included, 'col' indicates a column break, 'pm' indicates a plus-minus
  n <- length(x[[1]])
  out <- rep("", times = n)
  for (i in 1:n) {
    for (j in 1:length(form)) {
      if (form[[j]] == "col") out[i] <- paste0(out[i], " & ")
      if (form[[j]] == "pm") out[i] <- paste0(out[i], " \\(\\pm\\) ")
      if (is.double(form[[j]])) out[i] <- paste0(out[i], x[[as.integer(form[[j]])]][i])
    }
    out[i] <- paste0(out[i], " \\\\\n")
  }
  cat(out)
}

textable(x = labData,
         form = list(1, "col", 2, "col", 3, "pm", 4, "col",
                     5, "pm", 6, "col", 7, "pm", 8, "col", 9, "pm", 10))

#Clean up
options(stringsAsFactors = orig.options)