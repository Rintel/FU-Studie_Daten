library(plyr)
categorize.trust <- function(data) {
  data$Vertrauen <- as.character(data$Vertrauen)
  data$Vertrauen[startsWith(data$Vertrauen, 'hohes')] <- 'hoch'
  data$Vertrauen[startsWith(data$Vertrauen, 'wenig')] <- 'niedrig'
  data$Vertrauen[data$Vertrauen == ''] <- 'keine Angabe'
  data$Vertrauen <- as.factor(data$Vertrauen)
  data$Vertrauen <- ordered(data$Vertrauen, c('keine Angabe', 'niedrig', 'hoch'))
  return(data)
}

categorize.tan <- function(data) {
  data$TAN <- as.character(data$TAN)
  data$TAN[data$TAN == ''] <- 'keine Angabe'
  data$TAN[startsWith(data$TAN, 'erhöht')] <- 'niedrig'
  data$TAN[startsWith(data$TAN, 'macht')] <- 'hoch'
  data$TAN <- as.factor(data$TAN)
  data$TAN <- ordered(data$TAN, c('keine Angabe', 'niedrig', 'hoch', 'sehr hoch'))
  return(data)
}

ordinal.data <- function(data) {
  data <- as.character(data)
  data[data == ''] <- 'keine Angabe'
  data[data == 'mehrmals am Tag'] <- 'täglich'
  data[data == 'mindestens einmal pro Woche'] <- 'wöchentlich'
  data <- as.factor(data)
  data <- ordered(data, c('keine Angabe', 'gar nicht', 'seltener', 'wöchentlich', 'täglich'))
  return(data)
}

my.read <- function(path) {
  data <- read.csv(path, sep=';', na.strings = c("", "NA"))
  data <- rename(data,
                 c(
                   'Frage.1...Wie.groß.ist.Ihr.Vertrauen.in.Onlinebanking.'='Vertrauen',
                   'Frage.2...Wie.hoch.schätzen.Sie.den.Sicherheitsgewinn.durch.allgemeine.TAN.Verfahren.ein.'='TAN'))
  data <- rename(data, c(
    'Frage.3...Schätzen.Sie.mindestens.eines.Ihrer.Endgeräte..Laptop.Smartphone.Tablet..als.sicher.ein.'='Gerätesicherheit',
    'Frage.4...Wie.häufig.nutzen.Sie.im.Internet......Shopping....Spalten.1.6.'='shopping'))
  data <- rename(data, c(
    'Frage.4...Wie.häufig.nutzen.Sie.im.Internet......Kontostand.einsehen....Spalten.1.6.'='kontostand_einsehen',
    'Frage.4...Wie.häufig.nutzen.Sie.im.Internet......Überweisung.ausführen....Spalten.1.6.'='überweisung'))
  data <- rename(data, c(
    'Frage.6...Wie.alt.sind.Sie.'='alter',
    'Frage.7...Angaben.zum.Geschlecht.'='geschlecht',
    'Frage.8...Welchen.Bildungsgrad.haben.Sie.erreicht.'='ausbildung',
    'Frage.5...Was.ist.Ihnen.persönlich.am.Wichtigsten....Datenschutz'='datanschutz',
    'Frage.5...Was.ist.Ihnen.persönlich.am.Wichtigsten....Sicherheit'='sicherheit',
    'Frage.5...Was.ist.Ihnen.persönlich.am.Wichtigsten....Benutzerfreundlichkeit'='benutzerfreundlichkeit',
    'Frage.5...Was.ist.Ihnen.persönlich.am.Wichtigsten....Reputation.der.Bank'='reputation'))
  data <- categorize.trust(data)
  data <- categorize.tan(data)
  data$überweisung <- ordinal.data(data$überweisung)
  data$kontostand_einsehen <- ordinal.data(data$kontostand_einsehen)
  data$shopping <- ordinal.data(data$shopping)
  return(data)
}

create_plots <- function(path) {
  colors = c('gray50', 'black', 'steelblue1', 'steelblue2', 'steelblue3', 'steelblue4')
  data <- my.read(path)
  valid <- data[data$ID >= 20,]
  png('plot1.png')
  spineplot(überweisung~Vertrauen, valid, main='Vertrauen/Häufigkeit der Nutzung', col=colors)
  dev.off()
  png('plot2.png')
  spineplot(überweisung~TAN, valid, main='Vertrauen in TAN/Häufigkeit der Nutzung', col=colors)
  dev.off()
}

survey = my.read("banking.csv")

age = survey$alter
age = age[age != "", drop=TRUE]
age = relevel(age, "35-44 Jahre")
age = relevel(age, "25-34 Jahre")
age = relevel(age, "20-24 Jahre")
barplot(table(age), main = "Alter der Teilnehmer", ylab = "Anzahl der Teilnehmer", ylim = c(0, 15), cex.names = 0.8)

education = survey$ausbildung
education = education[education != "", drop=TRUE]
pie(table(education), main = "Ausbildung der Teilnehmer") 

age.education = table(survey[c("alter", "ausbildung")])
age.education
age.education.probs = apply(age.education, 2, function(x) x/sum(age.education) * 100)
age.education.probs
