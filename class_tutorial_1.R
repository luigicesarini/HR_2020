##### SCRIPT FOR HYDROLOGICAL RISK 05/10/2020 

# Working directory
getwd()
setwd()

# This is a comment

# Help
?plot
help(plot)

# combine command 
c(1,4)

# Simple command
5+5

# Boolean Algebra

TRUE * TRUE

TRUE * 0

x <- sample(-1000:1000, size = 15)
y <- sample(-1000:1000, size = 15)

isFALSE(x %% 2 == 0)

isTRUE(x > 0)

x < 0 && is.numeric(y)

x < 0 & is.numeric(y)

x < 0 | is.numeric(y)

x < 0 || is.numeric(y)

# Built in function 
sqrt(9)

# Display string
print("Hello, world")
cat("HelloWorld")
paste("Hello", "World", sep = "_")
paste0("Hello","World")

# Write your own function
pow <- function(x){
  power <- x*x
  return(power)
} 
5*5
5**2
5^2
pow(5)

# Create sequence
seq(1,100,6)
1:100

# Read tabular data from external source
df_airbnb <- read.csv("AB_NYC_2019.csv", header = TRUE)
  
df_covid <- read.csv(url("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"),
                      check.names = FALSE)
str(df_covid)
head(df_covid)

# is. and as. functions
x <- rnorm(n = 50,mean = 5,sd = 2)
x_2 <- rlnorm(n = 50,meanlog = 2,sdlog = 2)

x_c <- c("yellow","red","green","black")

is.character(x_c)

is.numeric(x_2)
is.integer(x_2)

as.integer(x_2)

as.data.frame(x)

is.na(df_covid$long)
# source a script
source("name_of_the_script.R")

# initialize dataframe, matrix, lists

create_matrix <- matrix(runif(25,0,100),5,5)

create_df <- data.frame("number" = 1:10, "name" = rep(c("john","james","luigi","frank","anna"),times = 2), height = runif(10,1.5,2), weight = rnorm(10,60,25))

create_list <- list("names" = c("john","james","luigi","frank","anna"), height = runif(10,1.5,2), weight = rnorm(10,60,25))

# indexing 
x <- c("Hello","I'm","a PhD", "Student", "at Iuss", "in Pavia")
x[1]
x[-3]
x[2:4]
x[-(2:4)]
x[c(1,6)]  
x[0:-5]
x[0:-(length(x)-2)]

head(df_airbnb) 
tail(df_covid, 20) 
summary(df_airbnb)

df_airbnb$room_type <- as.factor(df_airbnb$room_type)
df_airbnb$neighbourhood <- as.character(df_airbnb$neighbourhood)

levels(df_airbnb$neighbourhood)
summary(df_airbnb)

"SoHo" %in% levels(df_airbnb$neighbourhood)

new_ab <- df_airbnb[df_airbnb$neighbourhood %in% c("SoHo", "Tribeca","West Village"),] 

# Control statement
x <- 5
if (x <= 0) {
  print("Negative number")
} else {
  print("it is positive!")
}

df_1$restrictions <- ifelse(df_1$totale_casi > 100,"low", "high")

df_1$restrictions <- ifelse(as.Date(df_1$data) < as.Date("2020-03-08"), "No restrictions",
                            ifelse(as.Date(df_1$data) > as.Date("2020-05-18"),"Masks and Social Distancing","Full Lockdown"))

# While loop
x <- 2
while (x < 1000) {
  x <- x + 1
  if (x %% x == 0 & all(x %% seq(2,(x-1),1) != 0)) {
    print(x)
  }
}

# For loop
for (region in levels(as.factor(df_covid$denominazione_regione))) {
  print(region)
  df_reg <- df_covid[df_covid$denominazione_regione == region,]
  for (i in 1:length(df_reg$data)) {
    if (i == 1) {
      df_covid$daily_variation[i] <- df_covid$totale_casi[i]
    }else{
      df_covid$daily_variation[i] <- df_covid$totale_casi[i]-df_covid$totale_casi[i-1]
    }
  }  
}

# apply family
apply(df_covid,2,as.character) %>% str


# Plotting
# Scatterplot
data()
data("iris")
iris <- iris

str(iris)
     
plot(iris$Sepal.Length,iris$Petal.Length, col = c("blue"), pch = 19, cex = 2 ,xlim = c(0,10), ylim = c(0,10))

coeff <- lm(Petal.Length ~ Sepal.Length, data = iris)$coefficients


abline(a = coeff[1], b = coeff[2])
points(8,10, col = "red", pch = 22, cex = 5)
abline(v = 3)
abline(h = 5)
lines(rnorm(100,5,5), lty = 5, lwd = 5, col = "magenta")
# Histogram
norm_data <- stats::rnorm(n = 5000, mean = 5, sd = 1)
unif_data <- runif(n = 5000, 0,9)

hist(norm_data, main = "Nice histogram !", col = scales::alpha("cadetblue",0.5), border = "black", freq = TRUE, breaks = 25, xlim = c(-2,12))
hist(unif_data, main = "Nice histogram !", col = scales::alpha("indianred",0.5), border = "black", freq = TRUE, breaks = 25, add = TRUE)


#Boxplots
par(mfrow = c(1,3))
boxplot(price ~ room_type+neighbourhood,data = new_ab, subset = neighbourhood == c("SoHo"))
boxplot(price ~ room_type+neighbourhood,data = new_ab, subset = neighbourhood == c("Tribeca"))
boxplot(price ~ room_type+neighbourhood,data = new_ab, subset = neighbourhood == c("West Village"))
title( main = "Boxplots of airbnb's listings in:\nSoho, Tribeca and the West Village")
dev.off()



