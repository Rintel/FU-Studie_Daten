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

fill.up <- function(data) {
  data <- as.character(data)
  data[data == ''] <- 'keine Angabe'
  return(as.factor(data))
}

my.read <- function(path) {
  data <- read.csv(path, sep=';')
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
  data$geschlecht = fill.up(data$geschlecht)
  data$alter = fill.up(data$alter)
  data$alter = ordered(data$alter, c('keine Angabe', '20-24 Jahre', '25-34 Jahre', '35-44 Jahre', '>44 Jahre'))
  data$überweisung <- ordinal.data(data$überweisung)
  data$kontostand_einsehen <- ordinal.data(data$kontostand_einsehen)
  data$shopping <- ordinal.data(data$shopping)
  data$ausbildung = fill.up(data$ausbildung)
  return(data)
}

create.spine.plots <- function(path) {
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

plot.gender <- function(path) {
  data <- my.read(path)
  valid <- data[data$ID >= 20,]
  tab <- table(valid$geschlecht)
  labels <- paste(names(tab), "\n", tab)
  pie(tab, col=c('gray50', 'blue', 'red'), labels=labels)
}

plot.age <- function(path) {
  data <- my.read(path)
  valid <- data[data$ID >= 20,]
  tab <- table(valid$alter)
  labels <- paste(names(tab), "\n", tab)
  pie(tab, col=c('gray50', 'steelblue1', 'steelblue2', 'steelblue3', 'steelblue4'), labels=labels)
}

plot.edu <- function(path) {
  data <- my.read(path)
  valid <- data[data$ID >= 20,]
  tab <- table(valid$ausbildung)
  labels <- paste(names(tab), '\n', tab)
  pie(tab, col=c('steelblue1', 'steelblue2', 'steelblue3', 'gray50'), labels=labels)
}

plot.h4.1 <- function(path) {
  colors = c('gray50', 'black', 'steelblue1', 'steelblue2', 'steelblue3', 'steelblue4')
  data <- my.read(path)
  valid <- data[data$ID >= 20,]
  spineplot(überweisung~Vertrauen, valid, col=colors)
  par(xpd=TRUE)
  legend('topleft', inset=c(0,-0.15), legend=c('keine Angabe', 'gar nicht', 'seltener', 'wöchentlich', 'täglich'), fill=colors, ncol=3)
  par(xpd=FALSE)
}

plot.h4.2 <- function(path) {
  colors = c('gray50', 'black', 'steelblue1', 'steelblue2', 'steelblue3', 'steelblue4')
  data <- my.read(path)
  valid <- data[data$ID >= 20,]
  spineplot(überweisung~TAN, valid, col=colors)
  par(xpd=TRUE)
  legend('topleft', inset=c(0,-0.15), legend=c('keine Angabe', 'gar nicht', 'seltener', 'wöchentlich', 'täglich'), fill=colors, ncol=3)
  par(xpd=FALSE)
}

create.pie.plots <- function(path) {
  data <- my.read(path)
  valid <- data[data$ID >= 20,]
  hoch <- data[data$Vertrauen == 'hoch',]
  pie(table(hoch$geschlecht), main='Geschlecht', col=c('gray50', 'blue', 'red'))
  pie(table(hoch$alter), main='Alter', col=c('gray50', 'steelblue1', 'steelblue2', 'steelblue3', 'steelblue4'))
  niedrig <- data[data$Vertrauen == 'niedrig',]
  pie(table(niedrig$geschlecht), main='Geschlecht', col=c('gray50', 'blue', 'red'))
  pie(table(niedrig$alter), main='Alter', col=c('gray50', 'steelblue1', 'steelblue2', 'steelblue3', 'steelblue4'))
}


