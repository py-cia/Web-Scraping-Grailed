# Web-Scraping-Grailed
## Introduction
Grailed is an online marketplace where people come together to buy and sell clothes. There are many brands listed on this site, but the focus of this project 
is on Reese Cooper, my favorite clothing brand.

Lately, I've been thinking about selling some pants from this brand, but sometimes I run into the problem of pricing my items fairly. I normally cut my prices to sell fast 
and maintain good customer reviews, but this time is different. I scrape data from Grailed to make a simple linear regression to predict the cost of an item based on the number of likes, type of clothing (pants or shorts),
and condition. There were other predictors, but these were the only ones significant.

lm: price = 106.20 + number_of_likes(.44) + type(42.61) + new(18.20) - used(33.50) + u

Type is a binary indicator, 1 for pants and 0 for shorts. New and used are dummy variables. The variable being compared to is "gently used." 
