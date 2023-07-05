
# Libraries
###################
library(RSelenium)#
library(tidyverse)#
library(netstat)  #
library(rvest)    #
library(xml2)     #
###################


# For Future Reference ----------------------------------------------------
# binman::list_versions("chromedriver")
#######################################################################################################################
#  drive_object <- rsDriver(                                                                                          #    
#  browser = "chrome",                                                                                                #                                     
#  chromever = "114.0.5735.90",                                                                                       #
#  verbose = FALSE,                                                                                                   #
#  port = free_port())                                                                                                #
#                                                                                                                     #
#                                                                                                                     #
# remDr <- drive_object$client                                                                                        #
# remDr$maxWindowSize()                                                                                               #
# remDr$navigate("https://www.grailed.com/designers/reese-cooper")                                                    #                                                                       
#                                                                                                                     #
# Login                                                                                                               #
# remDr$findElement(using = "id", "global-header-login-btn")$clickElement()                                           #
#                                                                                                                     #
# remDr$findElement(using = "xpath", "//button[@data-cy = 'login-with-email']")$clickElement()                        #
#                                                                                                                     #  
# email <- remDr$findElement(using = "id", "email")                                                                   #
# email$sendKeysToElement(list('************@gmail.com'))                                                             #
#                                                                                                                     #
# pass <- remDr$findElement(using =  "id", "password")                                                                #
# pass$sendKeysToElement(list('*********'))                                                                           #
#                                                                                                                     #
# remDr$findElement(using = "xpath", "//button[@data-cy = 'auth-login-submit']")$clickElement()                       #
#                                                                                                                     #
# remDr$findElement(using = "xpath", "(//div[starts-with(@class, 'instant-search-collapsible')])[9]")$clickElement()  #
# checkbox <- remDr$findElement(using = "id", "sold-filter")                                                          #
# remDr$executeScript("arguments[0].click();", list(checkbox))                                                        #
#                                                                                                                     #
# save cookies                                                                                                        #
# cookies <- remDr$getAllCookies()                                                                                    #
# saveRDS(cookies, "cookies.rds")                                                                                     #
#                                                                                                                     #  
# drive_object$server$stop()                                                                                          #
#######################################################################################################################

drive_object2 <- rsDriver(
browser = "chrome",
chromever = "114.0.5735.90",
verbose = FALSE,
port = free_port())

rmDr2 <- drive_object2$client
rmDr2$maxWindowSize()
rmDr2$navigate("https://www.grailed.com/designers/reese-cooper")

# Skip --------------------------------------------------------------------
# load cookies

# cookies <- readRDS("cookies.rds")
# for (cookie in cookies){
#   remDr2$addCookie(
#    name = cookie$name,
#    value = cookie$value,
#    domain = cookie$domain,
#    path = cookie$path,
#    expiry = as.numeric(cookie$expiry),
#    secure = cookie$secure,
#    httpOnly = cookie$httpOnly
#  )
# }
# -------------------------------------------------------------------------

# Login
rmDr2$findElement(using = "id", "global-header-login-btn")$clickElement()

rmDr2$findElement(using = "xpath", "//button[@data-cy = 'login-with-email']")$clickElement()

email <- rmDr2$findElement(using = "id", "email")
email$sendKeysToElement(list('************@gmail.com'))

pass <- rmDr2$findElement(using =  "id", "password")
pass$sendKeysToElement(list('*********'))

rmDr2$findElement(using = "xpath", "//button[@data-cy = 'auth-login-submit']")$clickElement()

rmDr2$findElement(using = "xpath", "(//div[starts-with(@class, 'instant-search-collapsible')])[9]")$clickElement()
checkbox <- rmDr2$findElement(using = "id", "sold-filter")
rmDr2$executeScript("arguments[0].click();", list(checkbox))

# scroll to the bottom of the page
scroll_to_bottom <- function(driver) {
  driver$executeScript("window.scrollTo(0, document.body.scrollHeight);")
  Sys.sleep(5)  # Add a short delay to allow content to load
}

# Scroll until no more new content is loaded
previous_height <- -1  # Initialize the previous scroll height
current_height <- as.numeric(rmDr2$executeScript("return document.body.scrollHeight;"))

while (previous_height < current_height) {

  scroll_to_bottom(rmDr2)
  
  previous_height <- current_height
  current_height <- as.numeric(rmDr2$executeScript("return document.body.scrollHeight;"))
  
  Sys.sleep(5)
}

links <- rmDr2$findElements(using = "xpath", "//div[@class = 'feed-item']")

links %>% length()

# Get all the links
web_link <- lapply(links, function(link) {
  childElements <- link$findChildElement(using = "xpath", ".//a")
  childElements$getElementAttribute("href")
})

# Unlist links
web_link <- web_link %>% unlist()

# navigate to link
rmDr2$navigate(web_link[20])

brand_name <- rmDr2$findElement(using = "xpath", "//p[starts-with(@class, 'Headline_headline__')]")$getElementText() %>%
  unlist()

item_name <- rmDr2$findElement(using = "xpath", "//h1[starts-with(@class, 'Body_body__')]")$getElementText() %>%
  unlist()

number_likes <- tryCatch({
  likes <- rmDr2$findElement(using = "xpath", "//*[@id='__next']/div/main/div[2]/div[1]/div[2]/div[1]/button/span")$getElementText() %>%
  unlist()
  likes
}, error = function(err) {
  return(0)
})

description <- rmDr2$findElement(using = "xpath", "(//p[starts-with(@class, 'Body_body__dIg1V')])[7]")$getElementText() %>%
  unlist()

condition <- rmDr2$findElement(using = "xpath", "(//p[starts-with(@class, 'Body_body__dIg1V')])[4]")$getElementText() %>%
  unlist()

color <- rmDr2$findElement(using = "xpath", "(//p[starts-with(@class, 'Body_body__dIg1V')])[3]")$getElementText() %>%
  unlist()

size <- rmDr2$findElement(using = "xpath", "(//p[starts-with(@class, 'Body_body__dIg1V')])[1]")$getElementText() %>%
  unlist()

price <- rmDr2$findElement(using = "xpath", "//*[starts-with(@class, 'Money_root__8lDCT')]")$getElementText() %>%
  unlist()

img_length <- rmDr2$findElements(using = "xpath", "//img[starts-with(@class, 'Thumbnails_thumbnail__ixtme')]") %>% length()

# Attempt to find the element
measurements <- tryCatch({
  element <- rmDr2$findElement(using = "xpath", "//div[@class = 'Table_table__conFW']")
  TRUE  # Element found
}, error = function(err) {
  FALSE  # Element not found
})

# Get Outerwear -----------------------------------------------------------

outer_wear_button <- rmDr2$findElement(using = "xpath", "//*[@id='shop']/div[2]/div[4]/div[1]/div/div[2]/div[2]/div/div[1]/div[2]/div[3]/div[1]/div[1]/span")
rmDr2$executeScript("arguments[0].click();", list(outer_wear_button))

all_ow <- rmDr2$findElement(using = "id", "FilterToggle_checkbox_menswear_Outerwear_all")
rmDr2$executeScript("arguments[0].click();", list(all_ow))

previous_height <- -1
current_height <- as.numeric(rmDr2$executeScript("return document.body.scrollHeight;"))

while (previous_height < current_height) {

  scroll_to_bottom(rmDr2)
  
  previous_height <- current_height
  current_height <- as.numeric(rmDr2$executeScript("return document.body.scrollHeight;"))
  
  Sys.sleep(5)
}

# Get Links
ow_links <- rmDr2$findElements(using = "xpath", "//div[@class = 'feed-item']")
ow_links %>% length()

# this gets all the links
ow_link <- lapply(ow_links, function(link) {
  childElements <- link$findChildElement(using = "xpath", ".//a")
  childElements$getElementAttribute("href")
})

# get data from a link
ow_link <- ow_link %>% unlist()


# get outer wear data -----------------------------------------------------
ow_df <- data.frame()

for (i in ow_link){
  
  rmDr2$navigate(i)
  
  Sys.sleep(3)
  
  brand_name <- rmDr2$findElement(using = "xpath", "//p[starts-with(@class, 'Headline_headline__')]")$getElementText() %>%
    unlist()
  
  item_name <- rmDr2$findElement(using = "xpath", "//h1[starts-with(@class, 'Body_body__')]")$getElementText() %>%
    unlist()
  
  number_likes <- tryCatch({
    likes <- rmDr2$findElement(using = "xpath", "//span[contains(@class, 'Likes_count__lMavB')]")$getElementText() %>%
      unlist()
    likes
  }, error = function(err) {
    return(0)
  })
  
  condition <- rmDr2$findElement(using = "xpath", "(//p[starts-with(@class, 'Body_body__dIg1V')])[4]")$getElementText() %>%
    unlist()
  
  color <- rmDr2$findElement(using = "xpath", "(//p[starts-with(@class, 'Body_body__dIg1V')])[3]")$getElementText() %>%
    unlist()
  
  size <- rmDr2$findElement(using = "xpath", "(//p[starts-with(@class, 'Body_body__dIg1V')])[1]")$getElementText() %>%
    unlist()
  
  price <- rmDr2$findElement(using = "xpath", "//*[starts-with(@class, 'Money_root__8lDCT')]")$getElementText() %>%
    unlist()
  
  img_length <- rmDr2$findElements(using = "xpath", "//img[starts-with(@class, 'Thumbnails_thumbnail__ixtme')]") %>% length()
  
  # Attempt to find the element
  measurements <- tryCatch({
    element <- rmDr2$findElement(using = "xpath", "//div[@class = 'Table_table__conFW']")
    TRUE  # Element found
  }, error = function(err) {
    FALSE  # Element not found
  })
  
  description_elements <- rmDr2$findElements(using = "xpath", "//p[contains(@class, 'ListingPage-Description-Body-Paragraph')]")
  
  description <- map(description_elements, ~ get_text(.)) %>% unlist() %>% paste(collapse = "\n")
  
  current_obs <- data.frame(brand_name,
                            item_name,
                            number_likes,
                            condition,
                            color,
                            size,
                            price,
                            img_length,
                            measurements,
                            description)
  
  ow_df <- rbind(ow_df, current_obs)
  
  Sys.sleep(2)
}


# Get Bottoms -------------------------------------------------------------

previous_height <- -1  # Initialize the previous scroll height
current_height <- as.numeric(rmDr2$executeScript("return document.body.scrollHeight;"))

while (previous_height < current_height) {
  # Scroll to the bottom
  scroll_to_bottom(rmDr2)
  
  previous_height <- current_height  # Update the previous scroll height
  current_height <- as.numeric(rmDr2$executeScript("return document.body.scrollHeight;"))
  
  Sys.sleep(5)  # Add a short delay before checking the scroll height again
}
 
bottom_link <- rmDr2$findElements(using = "xpath", "//div[@class = 'feed-item']")
bottom_link %>% length()

bottom_links <- lapply(bottom_link, function(link) {
  childElements <- link$findChildElement(using = "xpath", ".//a")
  childElements$getElementAttribute("href")
})

# get data from a link
bottom_links <- bottom_links %>% unlist()


# Get Bottoms Data --------------------------------------------------------

bottom_df <- data.frame()

for (i in bottom_links){
  
  rmDr2$navigate(i)
  
  Sys.sleep(3)
  
  brand_name <- rmDr2$findElement(using = "xpath", "//p[starts-with(@class, 'Headline_headline__')]")$getElementText() %>%
    unlist()
  
  item_name <- rmDr2$findElement(using = "xpath", "//h1[starts-with(@class, 'Body_body__')]")$getElementText() %>%
    unlist()
  
  number_likes <- tryCatch({
    likes <- rmDr2$findElement(using = "xpath", "//span[contains(@class, 'Likes_count__lMavB')]")$getElementText() %>%
      unlist()
    likes
  }, error = function(err) {
    return(0)
  })
  
  condition <- rmDr2$findElement(using = "xpath", "(//p[starts-with(@class, 'Body_body__dIg1V')])[4]")$getElementText() %>%
    unlist()
  
  color <- rmDr2$findElement(using = "xpath", "(//p[starts-with(@class, 'Body_body__dIg1V')])[3]")$getElementText() %>%
    unlist()
  
  size <- rmDr2$findElement(using = "xpath", "(//p[starts-with(@class, 'Body_body__dIg1V')])[1]")$getElementText() %>%
    unlist()
  
  price <- rmDr2$findElement(using = "xpath", "//*[starts-with(@class, 'Money_root__8lDCT')]")$getElementText() %>%
    unlist()
  
  img_length <- rmDr2$findElements(using = "xpath", "//img[starts-with(@class, 'Thumbnails_thumbnail__ixtme')]") %>% length()
  
  # Attempt to find the element
  measurements <- tryCatch({
    element <- rmDr2$findElement(using = "xpath", "//div[@class = 'Table_table__conFW']")
    TRUE  # Element found
  }, error = function(err) {
    FALSE  # Element not found
  })
  
  description_elements <- rmDr2$findElements(using = "xpath", "//p[contains(@class, 'ListingPage-Description-Body-Paragraph')]")
  
  description <- map(description_elements, ~ get_text(.)) %>% unlist() %>% paste(collapse = "\n")
  
  current_obs <- data.frame(brand_name,
                            item_name,
                            number_likes,
                            condition,
                            color,
                            size,
                            price,
                            img_length,
                            measurements,
                            description)
  
  bottom_df <- rbind(bottom_df, current_obs)
  
  Sys.sleep(2)
}

View(bottom_df)


# Get t-shirt links -------------------------------------------------------
# (long sleeves and short sleeves)

previous_height <- -1
current_height <- as.numeric(rmDr2$executeScript("return document.body.scrollHeight;"))

while (previous_height < current_height) {
  
  scroll_to_bottom(rmDr2)
  
  previous_height <- current_height
  current_height <- as.numeric(rmDr2$executeScript("return document.body.scrollHeight;"))
  
  Sys.sleep(5)  
}

tshirt_link <- rmDr2$findElements(using = "xpath", "//div[@class = 'feed-item']")
tshirt_link %>% length()

tshirt_links <- lapply(tshirt_link, function(link) {
  childElements <- link$findChildElement(using = "xpath", ".//a")
  childElements$getElementAttribute("href")
})

# get data from links
tshirt_links <- tshirt_links %>% unlist()


# get tshirt data ---------------------------------------------------------
tshirt_df <- data.frame()

for (i in tshirt_links){
  
  rmDr2$navigate(i)
  
  Sys.sleep(3)
  
  brand_name <- rmDr2$findElement(using = "xpath", "//p[starts-with(@class, 'Headline_headline__')]")$getElementText() %>%
    unlist()
  
  item_name <- rmDr2$findElement(using = "xpath", "//h1[starts-with(@class, 'Body_body__')]")$getElementText() %>%
    unlist()
  
  number_likes <- tryCatch({
    likes <- rmDr2$findElement(using = "xpath", "//span[contains(@class, 'Likes_count__lMavB')]")$getElementText() %>%
      unlist()
    likes
  }, error = function(err) {
    return(0)
  })
  
  condition <- rmDr2$findElement(using = "xpath", "(//p[starts-with(@class, 'Body_body__dIg1V')])[4]")$getElementText() %>%
    unlist()
  
  color <- rmDr2$findElement(using = "xpath", "(//p[starts-with(@class, 'Body_body__dIg1V')])[3]")$getElementText() %>%
    unlist()
  
  size <- rmDr2$findElement(using = "xpath", "(//p[starts-with(@class, 'Body_body__dIg1V')])[1]")$getElementText() %>%
    unlist()
  
  price <- rmDr2$findElement(using = "xpath", "//*[starts-with(@class, 'Money_root__8lDCT')]")$getElementText() %>%
    unlist()
  
  img_length <- rmDr2$findElements(using = "xpath", "//img[starts-with(@class, 'Thumbnails_thumbnail__ixtme')]") %>% length()
  
  # Attempt to find the element
  measurements <- tryCatch({
    element <- rmDr2$findElement(using = "xpath", "//div[@class = 'Table_table__conFW']")
    TRUE  # Element found
  }, error = function(err) {
    FALSE  # Element not found
  })
  
  description_elements <- rmDr2$findElements(using = "xpath", "//p[contains(@class, 'ListingPage-Description-Body-Paragraph')]")
  
  description <- map(description_elements, ~ get_text(.)) %>% unlist() %>% paste(collapse = "\n")
  
  current_obs <- data.frame(brand_name,
                            item_name,
                            number_likes,
                            condition,
                            color,
                            size,
                            price,
                            img_length,
                            measurements,
                            description)
  
  tshirt_df <- rbind(tshirt_df, current_obs)
  
  Sys.sleep(2)
}

# get sweatshirts ---------------------------------------------------------
# (sweatshirts and hoodies)

previous_height <- -1  # Initialize the previous scroll height
current_height <- as.numeric(rmDr2$executeScript("return document.body.scrollHeight;"))

while (previous_height < current_height) {
  # Scroll to the bottom
  scroll_to_bottom(rmDr2)
  
  previous_height <- current_height  # Update the previous scroll height
  current_height <- as.numeric(rmDr2$executeScript("return document.body.scrollHeight;"))
  
  Sys.sleep(5)  # Add a short delay before checking the scroll height again
}

hoodie_link <- rmDr2$findElements(using = "xpath", "//div[@class = 'feed-item']")
hoodie_link %>% length()

hoodie_links <- lapply(hoodie_link, function(link) {
  childElements <- link$findChildElement(using = "xpath", ".//a")
  childElements$getElementAttribute("href")
})

# get data from a link
hoodie_links <- hoodie_links %>% unlist()


# hoodie df ---------------------------------------------------------------
hoodie_df <- data.frame()

for (i in hoodie_links){
  
  rmDr2$navigate(i)
  
  Sys.sleep(3)
  
  brand_name <- rmDr2$findElement(using = "xpath", "//p[starts-with(@class, 'Headline_headline__')]")$getElementText() %>%
    unlist()
  
  item_name <- rmDr2$findElement(using = "xpath", "//h1[starts-with(@class, 'Body_body__')]")$getElementText() %>%
    unlist()
  
  number_likes <- tryCatch({
    likes <- rmDr2$findElement(using = "xpath", "//span[contains(@class, 'Likes_count__lMavB')]")$getElementText() %>%
      unlist()
    likes
  }, error = function(err) {
    return(0)
  })
  
  condition <- rmDr2$findElement(using = "xpath", "(//p[starts-with(@class, 'Body_body__dIg1V')])[4]")$getElementText() %>%
    unlist()
  
  color <- rmDr2$findElement(using = "xpath", "(//p[starts-with(@class, 'Body_body__dIg1V')])[3]")$getElementText() %>%
    unlist()
  
  size <- rmDr2$findElement(using = "xpath", "(//p[starts-with(@class, 'Body_body__dIg1V')])[1]")$getElementText() %>%
    unlist()
  
  price <- rmDr2$findElement(using = "xpath", "//*[starts-with(@class, 'Money_root__8lDCT')]")$getElementText() %>%
    unlist()
  
  img_length <- rmDr2$findElements(using = "xpath", "//img[starts-with(@class, 'Thumbnails_thumbnail__ixtme')]") %>% length()
  
  # Attempt to find the element
  measurements <- tryCatch({
    element <- rmDr2$findElement(using = "xpath", "//div[@class = 'Table_table__conFW']")
    TRUE  # Element found
  }, error = function(err) {
    FALSE  # Element not found
  })
  
  description_elements <- rmDr2$findElements(using = "xpath", "//p[contains(@class, 'ListingPage-Description-Body-Paragraph')]")
  
  description <- map(description_elements, ~ get_text(.)) %>% unlist() %>% paste(collapse = "\n")
  
  current_obs <- data.frame(brand_name,
                            item_name,
                            number_likes,
                            condition,
                            color,
                            size,
                            price,
                            img_length,
                            measurements,
                            description)
  
  hoodie_df <- rbind(hoodie_df, current_obs)
  
  Sys.sleep(2)
}


# get tops ----------------------------------------------------------------
# button ups and knitwear

top_link <- rmDr2$findElements(using = "xpath", "//div[@class = 'feed-item']")
top_link %>% length()

top_links <- lapply(top_link, function(link) {
  childElements <- link$findChildElement(using = "xpath", ".//a")
  childElements$getElementAttribute("href")
})

# get data from a link
top_links <- top_links %>% unlist()

tops_df <- data.frame()

for (i in top_links){
  
  rmDr2$navigate(i)
  
  Sys.sleep(3)
  
  brand_name <- rmDr2$findElement(using = "xpath", "//p[starts-with(@class, 'Headline_headline__')]")$getElementText() %>%
    unlist()
  
  item_name <- rmDr2$findElement(using = "xpath", "//h1[starts-with(@class, 'Body_body__')]")$getElementText() %>%
    unlist()
  
  number_likes <- tryCatch({
    likes <- rmDr2$findElement(using = "xpath", "//span[contains(@class, 'Likes_count__lMavB')]")$getElementText() %>%
      unlist()
    likes
  }, error = function(err) {
    return(0)
  })
  
  condition <- rmDr2$findElement(using = "xpath", "(//p[starts-with(@class, 'Body_body__dIg1V')])[4]")$getElementText() %>%
    unlist()
  
  color <- rmDr2$findElement(using = "xpath", "(//p[starts-with(@class, 'Body_body__dIg1V')])[3]")$getElementText() %>%
    unlist()
  
  size <- rmDr2$findElement(using = "xpath", "(//p[starts-with(@class, 'Body_body__dIg1V')])[1]")$getElementText() %>%
    unlist()
  
  price <- rmDr2$findElement(using = "xpath", "//*[starts-with(@class, 'Money_root__8lDCT')]")$getElementText() %>%
    unlist()
  
  img_length <- rmDr2$findElements(using = "xpath", "//img[starts-with(@class, 'Thumbnails_thumbnail__ixtme')]") %>% length()
  
  # Attempt to find the element
  measurements <- tryCatch({
    element <- rmDr2$findElement(using = "xpath", "//div[@class = 'Table_table__conFW']")
    TRUE  # Element found
  }, error = function(err) {
    FALSE  # Element not found
  })
  
  description_elements <- rmDr2$findElements(using = "xpath", "//p[contains(@class, 'ListingPage-Description-Body-Paragraph')]")
  
  description <- map(description_elements, ~ get_text(.)) %>% unlist() %>% paste(collapse = "\n")
  
  current_obs <- data.frame(brand_name,
                            item_name,
                            number_likes,
                            condition,
                            color,
                            size,
                            price,
                            img_length,
                            measurements,
                            description)
  
  tops_df <- rbind(tops_df, current_obs)
  
  Sys.sleep(2)
}

# get boots ---------------------------------------------------------------

boot_link <- rmDr2$findElements(using = "xpath", "//div[@class = 'feed-item']")
boot_link %>% length()

boot_links <- lapply(boot_link, function(link) {
  childElements <- link$findChildElement(using = "xpath", ".//a")
  childElements$getElementAttribute("href")
})

boot_links <- boot_links %>% unlist()

boots_df <- data.frame()

for (i in boot_links){
  
  rmDr2$navigate(i)
  
  Sys.sleep(3)
  
  brand_name <- rmDr2$findElement(using = "xpath", "//p[starts-with(@class, 'Headline_headline__')]")$getElementText() %>%
    unlist()
  
  item_name <- rmDr2$findElement(using = "xpath", "//h1[starts-with(@class, 'Body_body__')]")$getElementText() %>%
    unlist()
  
  number_likes <- tryCatch({
    likes <- rmDr2$findElement(using = "xpath", "//span[contains(@class, 'Likes_count__lMavB')]")$getElementText() %>%
      unlist()
    likes
  }, error = function(err) {
    return(0)
  })
  
  condition <- rmDr2$findElement(using = "xpath", "(//p[starts-with(@class, 'Body_body__dIg1V')])[4]")$getElementText() %>%
    unlist()
  
  color <- rmDr2$findElement(using = "xpath", "(//p[starts-with(@class, 'Body_body__dIg1V')])[3]")$getElementText() %>%
    unlist()
  
  size <- rmDr2$findElement(using = "xpath", "(//p[starts-with(@class, 'Body_body__dIg1V')])[1]")$getElementText() %>%
    unlist()
  
  price <- rmDr2$findElement(using = "xpath", "//*[starts-with(@class, 'Money_root__8lDCT')]")$getElementText() %>%
    unlist()
  
  img_length <- rmDr2$findElements(using = "xpath", "//img[starts-with(@class, 'Thumbnails_thumbnail__ixtme')]") %>% length()
  
  # Attempt to find the element
  measurements <- tryCatch({
    element <- rmDr2$findElement(using = "xpath", "//div[@class = 'Table_table__conFW']")
    TRUE  # Element found
  }, error = function(err) {
    FALSE  # Element not found
  })
  
  description_elements <- rmDr2$findElements(using = "xpath", "//p[contains(@class, 'ListingPage-Description-Body-Paragraph')]")
  
  description <- map(description_elements, ~ get_text(.)) %>% unlist() %>% paste(collapse = "\n")
  
  current_obs <- data.frame(brand_name,
                            item_name,
                            number_likes,
                            condition,
                            color,
                            size,
                            price,
                            img_length,
                            measurements,
                            description)
  
  boots_df <- rbind(boots_df, current_obs)
  
  Sys.sleep(2)
}

# Stop once finished
drive_object2$server$stop()

# saving data frames as csv -----------------------------------------------
write.csv(ow_df, "C:\\Users\\valen\\OneDrive\\Documents\\grailed_project\\ow_df.csv")
write.csv(bottom_df, "C:\\Users\\valen\\OneDrive\\Documents\\grailed_project\\bottom_df.csv")
write.csv(tshirt_df, "C:\\Users\\valen\\OneDrive\\Documents\\grailed_project\\tshirt_df.csv")
write.csv(hoodie_df, "C:\\Users\\valen\\OneDrive\\Documents\\grailed_project\\hoodie_df.csv")
write.csv(tops_df, "C:\\Users\\valen\\OneDrive\\Documents\\grailed_project\\tops_df.csv")
write.csv(boots_df, "C:\\Users\\valen\\OneDrive\\Documents\\grailed_project\\boots_df.csv")











