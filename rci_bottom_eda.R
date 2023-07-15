# rm(list = ls())
# Interesting Questions
# Relationship between price and number of likes, condition, color, size, image length, and whether or not measurements are provided.

library(tidyverse)
library(fmtr)
library(moments)

files <- list.files("C:\\Users\\valen\\OneDrive\\Documents\\grailed_project")

files <- files[str_detect(files, ".csv")]

file_length <- files %>% str_length() %>% - 4

file_name <- str_sub(files, 1, file_length)

data_list <- list()

for (i in file_name){
  data_list[[i]] <- read.csv(paste0("C:\\Users\\valen\\OneDrive\\Documents\\grailed_project\\", i, ".csv"))
}

data_list %>% names()

data_list %>% pluck() %>% imap(View)

# data cleaning 
bottom_df <- data_list %>% pluck("bottom_df")
View(bottom_df)

bottom_df$color <- str_sub(bottom_df$color, 7, str_length(bottom_df$color))

bottom_df$color[243] <- "Black"
bottom_df$condition[243] <- "Condition Gently Used"

bottom_df$condition[234] <- "Condition New"

bottom_df$color[234] = "Beige"

table(bottom_df$color)

filter(bottom_df, color == ("ion Gently Used"))
# observation 188 and 207 need to be addressed

bottom_df[188, "condition"] = "Condition Gently Used"
bottom_df[188, "color"] = "Green"

bottom_df[207, "condition"] = "Condition Gently Used"
bottom_df[207, "color"] = "Green"

bottom_df["color"] %>% 
  group_by(color) %>% 
  count() %>% 
  ggplot(mapping = aes(x = color, y = n)) +
  geom_col()

# removing "Condition" from condition
bottom_df$condition <- bottom_df$condition %>% str_sub(11, str_length(bottom_df$condition))

# removing "Size Men's / " from size 
bottom_df$size <- bottom_df$size %>% str_sub(14, str_length(bottom_df$size))

# can safely remove the EU sizing
table(bottom_df$size) %>% names()
table(bottom_df$size)

bottom_df$size <- str_sub(bottom_df$size, 1, 5) %>% str_sub(4, 5)
# things to do today, clean rest of data set, then eda, 
# need to be cleaned:
# size, color, verify no img length is 0, add new variable 1 pant 0 short
# mens waist size S:28-30, M:32-34, L:36-38

# turn measurements into bindary indicator
bottom_df$measurements <- ifelse(bottom_df$measurements == TRUE, 1, 0)

# going to add an indicator for whether an item is a pant or short
# write.csv(bottom_df, "C:\\Users\\valen\\OneDrive\\Documents\\grailed_project\\bttm_df.csv")

bttm_df <- read.csv("C:\\Users\\valen\\OneDrive\\Documents\\grailed_project\\bttm_df_type.csv") %>% select(-1, -2)

# eda ---------------------------------------------------------------------
# avg price
class(bttm_df$price)
bttm_df$price <- bttm_df$price %>% str_sub(2, length(bttm_df$price)) %>% as.numeric()

bttm_df %>%
  ggplot(mapping = aes(x = price)) +
  geom_histogram()

# right tailed, average sell price is $157.6 for Reese Cooper Bottoms
summary(bttm_df)
skewness(bttm_df$price)

# size
# Reese Cooper Does not make odd sizes. The odd sizes are due to alterations made to the pant or short, or are based on user judgement.
bttm_df["size"] %>% group_by(size) %>% count() %>%
  ggplot(mapping = aes(x = size, y = n)) +
  geom_col()

# condition
# Big difference in clothing that is used versus clothing that is gently used and new. This can be due to buyer preference, more new/gently used listings
# than used listings, and pricing 
bttm_df["condition"] %>% group_by(condition) %>% count() %>% 
  ggplot(mapping = aes(x = condition, y = n)) +
  geom_col()

# right tailed distribution. Most listings that appear in the RCI feed have 5 images. The median is 6.
bttm_df["img_length"] %>% group_by(img_length) %>% count() %>%
  ggplot(mapping = aes(x = img_length, y = n)) +
  geom_col()

# right tailed distribution. Median likes is 16. The most likes that a sold item has is 192. The minimum is 0.
bttm_df %>% 
  ggplot(mapping = aes(x = number_likes)) +
  geom_histogram()

# More than 70% of sold items have measurements. Measurements indicate whether an item has at least one measurement for wasit, front rise, inseam,
# thigh, leg opening, or knee. Buyers likely want to know how an item fits before they buy.
bttm_df["measurements"] %>% group_by(measurements) %>% 
  count() %>% ungroup() %>% mutate(pct = n/269)

# most sold items are black, green, blue, khaki, and beige. In the fashion landscape these are arguably neutral colors.
bttm_df["color"] %>% group_by(color) %>% count() %>% arrange(desc(n))

# About 80% of the listings are pants. 
bttm_df["type"] %>% group_by(type) %>% count() %>% ungroup() %>% mutate(pct = n/269)

# avg price by color
bttm_df %>% group_by(color) %>% summarise(avg_pc = mean(price),
                                          n = n()) %>% ungroup() %>% arrange(desc(n), avg_pc)

# formatting color --------------------------------------------------------
# to reduce number of predictors, colors will be cool, neutral, and warm
table(bottom_df$color)

# Changing 223, and 231
bttm_df[223, "color"] = "Warm"
bttm_df[231, "color"] = "Cool"

# changing 88 and 247 to Warm
bttm_df[88, "color"] = "Brown"
bttm_df[247, "color"] = "Brown"

# warm color, cool, neutral
color <- value(condition(x == "Baby blue", "Cool"),
               condition(x == "Beige", "Neutral"),
               condition(x == "Black", "Neutral"),
               condition(x == "Blue", "Cool"),
               condition(x == "Blue denim", "Neutral"),
               condition(x == "Blue slate", "Cool"),
               condition(x == "Brown", "Warm"),
               condition(x == "Camel", "Neutral"),
               condition(x == "Canyon", "Warm"),
               condition(x == "Charcoal", "Neutral"),
               condition(x == "Coyote", "Neutral"),
               condition(x == "Cream", "Neutral"),
               condition(x == "Dark blue denim", "Neutral"),
               condition(x == "Dark blue wash", "Neutral"),
               condition(x == "Dark khaki", "Neutral"),
               condition(x == "Denim", "Neutral"),
               condition(x == "Denim jeans", "Neutral"),
               condition(x == "Forest", "Cool"),
               condition(x == "Forest green", "Cool"),
               condition(x == "Gray", "Neutral"),
               condition(x == "Gray/green", "Neutral"),
               condition(x == "Gray/olive", "Neutral"),
               condition(x == "Grayish green?", "Neutral"),
               condition(x == "Green", "Cool"),
               condition(x == "Grey", "Neutral"),
               condition(x == "Indigo", "Cool"),
               condition(x == "Khaki", "Neutral"),
               condition(x == "Natural", "Neutral"),
               condition(x == "Navy", "Neutral"),
               condition(x == "Olive", "Neutral"),
               condition(x == "Orange", "Warm"),
               condition(x == "Purple", "Cool"),
               condition(x == "Red", "Warm"),
               condition(x == "Sage", "Neutral"),
               condition(x == "Tan", "Neutral"),
               condition(x == "Tan camo", "Neutral"),
               condition(x == "Tan mix", "Neutral"),
               condition(x == "Vintage black", "Neutral"),
               condition(x == "Vintage blue", "Cool"),
               condition(x == "Warm", "Warm"), 
               condition(x == "Water color", "Cool"),
               condition(x == "Water colour camo", "Cool"),
               condition(x == "White", "Neutral"),
               condition(x == "Cool", "Cool"))

bttm_df$color <- fapply(bttm_df$color, color)
table(bttm_df$color)

# dummy color
dummy_col <- model.matrix(~ bttm_df$color) %>% as.data.frame() %>% select(-1)

# dummy condition
dummy_cond <- model.matrix(~ bttm_df$condition) %>% as.data.frame() %>% select(-1)

# dummy size
bttm_df$size %>% as.character()

# size small:26 - 30, medium: 31 - 34, large: 35 - 38
table(bttm_df$size)

size <- value(condition(x == "26", "Small"),
              condition(x == "27", "Small"),
              condition(x == "28", "Small"),
              condition(x == "29", "Small"),
              condition(x == "30", "Small"),
              condition(x == "31", "Medium"),
              condition(x == "32", "Medium"),
              condition(x == "33", "Medium"),
              condition(x == "34", "Medium"),
              condition(x == "35", "Large"),
              condition(x == "36", "Large"))

bttm_df$size <- fapply(bttm_df$size, size)

dummy_size <- model.matrix(~ bttm_df$size) %>% as.data.frame() %>% select(-1)

df <- cbind(bttm_df, dummy_col, dummy_cond, dummy_size) %>% select(-c("condition", "color", "size"))

names(df)[9] <- "neutral"
names(df)[10] <- "warm"
names(df)[11] <- "new"
names(df)[12] <- "used"
names(df)[13] <- "medium"
names(df)[14] <- "small"

lm_bttm <- lm(price ~ number_likes + img_length + measurements + type + neutral + warm + new + used + medium + small, data = df)
summary(lm_bttm)

par(mfrow = c(2,2))
plot(lm_bttm)

# removing influential points: 87, 190, 212
df_inf <- df[-c(87, 190, 212), ]

# In the new model, the new dummy variable is now signigicant.
lm2 <- lm(price ~ number_likes + type + new + used, data = df)
summary(lm2)
# yi = b0 + number_likes(x1) + type(x2) + new(x3) + used(x4)
# I have some pants that I am going to sell, so based off of this model, assuming the pants get the average number of likes, it should cost
# 106.2024 + 16*.4434 + 42.6098 + 18.2024 = $174.109
# each additional like is an extra 44cents, used items cost $33 less than gently used items, and new items cost $18 more, pants cost 42 more than shorts

# There is no apparent non-linear pattern in the residual plot. No heteroskedasticity. 
par(mfrow = c(2,2))
plot(lm2)

# I expected color, measurements, and size to play a more significant role, but I think that i was drawing the wrong comparisons. In street fashion,
# in particular, brands such as supreme that make limited quantities of their items color and size play an important role in forecasting prices. The 
# wrong size or color could lead to smaller profit or an item "bricking". Reese Cooper is not one of these brands. Price is not affected by size or 
# color. It would be interesting to see the significance size and color have on Supreme clothing. 




