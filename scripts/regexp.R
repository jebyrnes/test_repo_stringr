#'-----------------------------
#' Regexp!
#' ----------------------------
 
library(stringr)
library(dplyr)
library(ggplot2)
library(readxl)

portal <- read_excel("../data/portal_messy_regexp.xlsx", 
                     na = c("", "NA"))


str_view_all(portal$Weight, "[^a-zA-Z]", match=TRUE)

#groups
vec <- c("grey", "gray", "gyre", "grayey")
str_view(vec, "(ey|ay)")


c("grey", "gray", "gyre", "grayey") %>%
  str_view("r(ey|ay)$")

#
#EXERCISE
#Create regular expressions to find all words that:
  
#Start with three consonants.
str_subset(words, "^[^aeoui]{3}")


#Have three or more vowels in a row.
str_subset(words, "[aeoui]{3,}")


#Have two or more vowel-consonant pairs in a row.
str_view(words, "([aeiou][^aeiou]){2,}", match = TRUE)

#That only contain consonants. (Hint: thinking about 
#matching "not"-vowels.)
str_subset(words, "^[^aeiou]+$")

#find things where sex is problematic
filter(portal, str_detect(Sex, "[fm]"))

#Replace the word "the" or "The" with "Cat" 
#in stringr::sentences

str_replace_all(sentences, "(^| )[tT]he( |$|\\.)","\\1Cat\\2")[1:10]


str_replace_all(sentences, "(^| )[tT]he", " Cat") %>%
  str_replace("^ ", "")


#Switch the first and last letters in words. 
words_switch <- str_replace(words, "^(.)(.*)(.)$", "\\3\\2\\1")

#Bonus: Use intersect to find out which of those strings 
#are still words?
intersect(words, words_switch)

#Use mutate to remove g and the other
#cruft from weights and then make numeric

portal <- portal %>%
  mutate(Weight = str_replace_all(Weight, "[^0-9]", ""),
         Weight = as.numeric(Weight))

#check!
str_subset(portal$Weight, "[^0-9]")

#fix plot
portal <- portal %>%
  mutate(Plot = as.numeric(Plot))

# if else
vec <- 1:10

vec<5
ifelse(vec < 5, TRUE, FALSE)
ifelse(vec <5, vec+1, vec)
ifelse(vec == 5, "Five!", "Not five!")

#using round and a comparison, return if vec is even or odd
#"Odd", "Even", "Odd", "Even" (use round() and some division by 2)
5/2
round(5/2)
round(4/2)

ifelse(round(vec/2) == vec/2, "Even", "Odd")


#Fix species

portal_test <- portal %>%
  mutate(Species = str_to_upper(Species))


#str_c
abc <- c("a", "b", "c")
str_c("a", "b", "c", sep = ",")

str_c(portal$Weight, "g", sep="")

#collapse a vector
str_c(abc, collapse = ",")

weight_g <- str_c(na.omit(portal$Weight), "g", 
      sep = "", collapse = ", ")

str_split(weight_g, pattern = ", ")

#why a list?
str_split(c("hello_goodbye", "purple_orange"), pattern = "_")

str_split(c("hello_goodbye", "purple_orange"), pattern = "_",
          simplify=TRUE)

str_split(weight_g, pattern = ", ",
          simplify = TRUE)



#Fix species

portal <- portal %>%
  mutate(Species = str_to_upper(Species),
         
         Split_Sex = str_split(Species, 
                               pattern = "_",
                               simplify = TRUE)[,2],
         
         Species = str_split(Species, 
                               pattern = "_",
                               simplify = TRUE)[,1],
         
         Sex = ifelse(is.na(Sex), Split_Sex, Sex)) %>%
         
#         Sex = str_c(str_replace_na(portal_test$Sex, ""), 
#                     portal_test$Split_Sex))
        select(-Split_Sex)



