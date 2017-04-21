library("metafor")
library("meta")

title ="Sciatica treatment with gabapentinoids"

data <- read.table(textConnection('
study                       Drug       Size effect.size  variance
"Mathieson (PRECISE), 2017" Pregabalin 207   0.1828  0.019416
"Atkinson, 2016"            Gabapentin  72  -0.2834  0.056285
"Baron, 2010"               Pregabalin 217  -0.132   0.018477
'), header=TRUE)

data$sd <- sqrt(data$variance)
data$se <- data$sd#/sqrt(data$n)

# byvar=method, 
meta <- metagen(data$effect.size, data$se, sm="SMD", comb.fixed = FALSE, studlab=data$study, title = title, data=data)

forest(meta, leftcols=c("studlab","Drug","Size"), label.left="Favors intervention", label.right="Favors control", print.tau2=FALSE, data=meta, main = meta$title)
