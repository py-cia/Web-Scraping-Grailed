# Web-Scraping-Grailed
Grailed is an online marketplace where people come together to buy and sell clothes. There are many brands listed on this site, but the focus of this project 
is on Reese Cooper, my favorite clothing brand.

Lately, I've been thinking about selling some pants from this brand, but sometimes I run into the problem of pricing my items fairly. I normally cut my prices to sell fast 
and maintain good customer reviews, but this time is different. I scrape data from Grailed to make a simple linear regression to predict the cost of an item based on the number of likes, type of clothing (pants or shorts),
and condition. There were other predictors, but these were the only ones significant.

lm: price = 106.20 + number_of_likes(.44) + type(42.61) + new(18.20) - used(33.50) + u

Type is a binary indicator, 1 for pants and 0 for shorts. New and used are dummy variables. The variable being compared to is "gently used." 

**Future Plans for Updating**  
I am currently refining my pricing model for historical RCI listings on Grailed. My focus is on clustering colors into groups based on their RGB values, but this requires thorough data cleaning first. Many sellers input incorrect, incomplete, or missing color information, so tedious data entry and cross-referencing will be necessary to ensure accuracy.

Additionally, I am creating a new feature that incorporates an item's season release, as I believe older collections may hold significant value. Once the data is prepared, I plan to experiment with multiple models and evaluate their performance using a train/test split to improve accuracy and reliability
