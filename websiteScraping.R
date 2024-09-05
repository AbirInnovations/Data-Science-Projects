# Load the rvest library for web scraping
install.packages(rvest)
library(rvest)

# Define the URL of the website you want to scrape
url <- "https://www.evercarebd.com/dhaka/specialities/"

# Read the HTML content of the webpage
webpage <- read_html(url)

# Extract specific elements from the webpage using CSS selectors
# For example, to extract all the links on the webpage:
links <- webpage %>% html_nodes("a") %>% html_attr("href")

# Print the extracted links
print(links)
