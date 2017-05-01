library("meta")

title ="Sciatica treatment with gabapentinoids"

data <- read.table(textConnection('
Study                 Year Drug       Size effect.size  variance
McCleane              2001 Gabapentin  65  -0.4997  0.064451
Yildirim              2003 Pregabalin  43  -1.5762  0.1224
Baron                 2010 Pregabalin 217  -0.132   0.018477
Baron                 2015 Pregabalin 288  -0.04    0.0128
"Markman (LUSTOR)"    2015 Pregabalin  30  -0.0634  0.134
NCT01838044           2016 Pregabalin 156  -0.0746  0.0222
Atkinson              2016 Gabapentin  72  -0.2834  0.056285
Kim                   2016 Pregabalin 122   0.1034  0.0328
"Mathieson (PRECISE)" 2017 Pregabalin 207   0.1828  0.019416
'), header=TRUE)

data <- data[order(data$Year),]
data$Study <- paste(data$Study, data$Year, sep=", ")

data$sd <- sqrt(data$variance)
data$se <- data$sd

# Forest plot
# byvar = Drug, 
meta <- metagen(data$effect.size, data$se, byvar = Drug, sm="SMD", hakn = TRUE, comb.fixed = FALSE, studlab=data$Study, title = title, data=data)

forest(meta, leftcols=c("studlab","Drug","Size"), label.left="Favors intervention", label.right="Favors control", print.I2.ci = TRUE, print.tau2=FALSE, data=meta)

## Metaregression
data$Study[4] = "Baron, 2015\n"
data$Study[6] = "\n\nNCT01838044, 2016" #Lower the placement of the label on the bubble plot
meta <- metagen(data$effect.size, data$se, byvar = Drug, sm="SMD", hakn = TRUE, comb.fixed = FALSE, studlab=data$Study, title = title, data=data)
meta2 <- metareg(meta, Year)
# main = "Metaregression of published trials"
bubble(meta2, main="", lwd=2, col.line="blue", studlab = TRUE, xlim=c(1997,2020), xlab="Year of publication", ylab="Standardized mean difference")
text(par("usr")[2],par("usr")[3]+2.25*strheight("A")+0.75*strheight("A"),cex=1.2,adj=c(1,0),paste("p (correlation) = ",sprintf(meta2$pval[2], fmt='%#.3f'), sep=""), font=1)
text(par("usr")[2],par("usr")[3]+1.25*strheight("A"),cex=1.2,adj=c(1,0),paste("Residual I2 = ",sprintf(meta2$I2, fmt='%#.1f'),'%', sep=""), font=1)

################
# http://www.campbellcollaboration.org/escalc/html/EffectSizeCalculator-Home.php
# Converting confidence interval to the standard deviation
N = 91  
Standard.Error = 0.212
Confidence.Interval.Width = 0.9
Standard.Error = Confidence.Interval.Width/1.96^2
(Standard.Deviation = Standard.Error * sqrt(N))
