#Trang Tran, ALY 6000, Project 3, Jan 30

cat("\014")  # clears console
rm(list = ls())  # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notion for entire R session

library(pacman)
p_load(tidyverse)
p_load(FSA, FSAdata)
p_load(ggplot2)
p_load(testthat)

#question 1
bio <- as_tibble(data.frame(InchLake2))
bio

#question 2
head(bio)
tail(bio)
str(bio)

#question 3
distinct(bio, species)

#question 4
tibble_4 <- group_by(bio, species) |> summarise(count = n())
levels <- arrange(tibble_4, desc(count))
levels

#question 5
bio |> mutate(species = factor(species, levels = levels))
bio

#question 6
ggplot(bio, mapping = aes(fill = factor(year))) +
  geom_histogram(stat = "count", aes(x=factor(species, level=levels$species))) + xlab("Species") + ylab("Count") +
  theme(axis.title.y = element_text(angle=0, hjust = 1, vjust = .5),
  axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1)) +
  ggtitle("Fish Count")

#question 7
specs <- mutate(levels, rel_freq = round(count/sum(count), digits = 3))
specs

#question 8
specs <- mutate(specs, cum_count = cumsum(count), cum_freq = cumsum(rel_freq))
specs

#question 9
ggplot(specs,mapping = aes(x = factor(species, level=levels$species))) +
  geom_col(mapping=aes(y = count), fill = "cyan") +
  geom_point(mapping = aes(y = cum_count), size=2, color="red") +
  geom_line(mapping = aes(y = cum_count), group=1) +
  scale_y_continuous(
    # Features of the first axis
    name = "Counts",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./max(specs$cum_count), labels=scales::percent)) +
  theme(axis.title.y = element_text(angle=0, hjust = 1, vjust = .5),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1)) +
  ggtitle("Species Pareto") + xlab("Species")

#question 10
ggplot(bio, mapping=aes(x=weight)) +
  geom_point(mapping=aes(y=length, color=factor(species, level=levels$species))) +
  ggtitle("Weight vs. Tail Length") + xlab("Weight") + ylab("Tail Length") +
  scale_color_discrete(name = "species")

#question 11
bio |> filter(species=="Bluegill") |> ggplot(mapping=aes(x=log(weight), y=length)) +
  geom_point(mapping=aes(color=factor(year))) +
  geom_smooth(method="lm") +
  ggtitle("Bluegill Log Weight vs. Tail Length") +
  xlab("Log Weight") + ylab("Tail Length") + 
  scale_color_discrete(name = "Year")

#question 13
iris <- tibble(iris)
iris

#question 14
mean_sepal <- sum(iris$Sepal.Length)/length(iris$Sepal.Length)
mean_sepal
mean_sepal <- mean(iris$Sepal.Length)
mean_sepal

#question 15
custom_median <- function(x)
{
  mid <- seq(floor((length(x)+1)/2),ceiling((length(x)+1)/2))
  mean(sort(x)[mid])
}
median_sepal <- custom_median(iris$Sepal.Length)
median_sepal
median_sepal <- median(iris$Sepal.Length)
median_sepal

#question 16

custom_min <- function(x) {
  my_min = Inf
  for (i in seq_along(x)) {
    if (x[i] < my_min) my_min = x[i]
  }
  return(min = my_min)
}
custom_max <- function(x) {
  my_max = 0
  for (i in seq_along(x)) {
    if (x[i] > my_max) my_max = x[i]
  }
  return(max = my_max)
}

min_sepal <- custom_min(iris$Sepal.Length)
min_sepal
max_sepal <- custom_max(iris$Sepal.Length)
max_sepal

min_sepal <- min(iris$Sepal.Length)
min_sepal
max_sepal <- max(iris$Sepal.Length)
max_sepal

#question 17
custom_var <- function(x){
  my_var = 0
  len = length(x)
  my_mean = sum(x)/length(x)
  for (i in seq_along(x)){
    my_var <- my_var + (x[i] - my_mean)**2
  }
  my_var <- my_var/(len-1)
  return (my_var)
}
var_sepal <- custom_var(iris$Sepal.Length)
var_sepal
var_sepal <- var(iris$Sepal.Length)
var_sepal

#question 18
custom_sd <- function(x){
  my_sd = sqrt(custom_var(x))
  return (my_sd)}
sd_sepal <- custom_sd(iris$Sepal.Length)
sd_sepal
sd_sepal <- sd(iris$Sepal.Length)
sd_sepal

#question 19
ggplot(iris, aes(x=Sepal.Length, y=factor(Species))) + geom_boxplot() +
  ylab("Species")

test_file("project3_tests.R")
