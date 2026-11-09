stroke <- read.csv("healthcare-dataset-stroke-data.csv",
                   na.strings = c("", "NA", "N/A"))
dim(stroke)
str(stroke)
colSums(is.na(stroke))
table(stroke$stroke)

#suppression Id Inutile pour l'analyse

stroke$id <- NULL
dim(stroke)

#Suppression de Other car 1 seul patient 

table(stroke$gender)
stroke <- stroke[stroke$gender != "Other", ]
table(stroke$gender)
dim(stroke)

table(stroke$smoking_status)
# BMI manquant sont remplacé par la moyenne 28.1
median(stroke$bmi, na.rm = TRUE)
stroke$bmi[is.na(stroke$bmi)] <- median(stroke$bmi, na.rm = TRUE)
sum(is.na(stroke$bmi))

#convertion des var qualitatives en facteurs pour eviter que R prend hypertension pr un nb
quali <- c("gender", "hypertension", "heart_disease", "ever_married",
           "work_type", "Residence_type", "smoking_status", "stroke")
stroke[quali] <- lapply(stroke[quali], as.factor)
str(stroke)
#8 var qualitatives converti en factor 3 reste numérique

nb_outliers <- function(x) {
  q <- quantile(x, c(.25, .75))
  iqr <- q[2] - q[1]
  sum(x < q[1] - 1.5 * iqr | x > q[2] + 1.5 * iqr)
}
sapply(stroke[c("age", "avg_glucose_level", "bmi")], nb_outliers)

#Visualiser les outliers
boxplot(stroke$avg_glucose_level, main = "Glucose")
boxplot(stroke$bmi, main = "BMI")
#outliers glucose au dessus de 169 ( les 627 outliers possiblement diabétiques)
#outliers BMI mediane 28 (outliers entre 47 et 60 probablement des personnes en situation d'obésité)


colSums(is.na(stroke))
dim(stroke)
summary(stroke)
