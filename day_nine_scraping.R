library(rvest)


url_data<-"https://en.wikipedia.org/wiki/Academy_Award_for_Best_Picture"
xpath<-"/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table[12]"

url_data %>% 
  read_html() %>% 
  html_element(xpath=xpath) %>% 
  html_table()

result<-url_data %>% 
  read_html() %>% 
  html_element(xpath=xpath) %>% 
  html_table()
View(result)

bk_url<-"https://en.wikipedia.org/wiki/List_of_countries_with_Burger_King_franchises"
bk<-"/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table[3]"

bk2<-"/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table[3]"
bk_url %>% 
  read_html() %>% 
  html_element(xpath = bk2) %>% 
  html_table()

bk_list<-bk_url %>% 
  read_html() %>% 
  html_element(xpath = bk) %>% 
  html_table()


#lets just steal some text
nyt<-"https://www.nytimes.com/2023/10/19/style/attersee-isabel-wilkinson-schor.html"
nyt_css<-".meteredContent"




nyt %>% 
  read_html() %>% 
  html_element(css = nyt_css) %>% 
  html_text()

dresses<-nyt %>% 
  read_html() %>% 
  html_element(css = "nyt_css") %>% 
  html_text()

#ok, this is a unit three skill...
library(stringr)


dressesB<-str_split(dresses, "/The New York Times", n = Inf, simplify = FALSE)0
dressesC<-data.frame(dressesB)
View(dressesC)


#now we do deeeep chaos
imgsrc <- read_html(nyt) %>%
  html_node(xpath = '//*/img') %>%
  html_attr('src')
imgsrc



read_html(nyt) %>% html_nodes(css = "picture") %>% html_text()


A<-"/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table[2]"
B<-"https://en.wikipedia.org/wiki/The_Game_Awards"

B %>% 
  read_html() %>% 
  html_element(xpath = A) %>% 
  html_table()
library(rvest)  


G<-"/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table[2]"
H<-"https://en.wikipedia.org/wiki/Lady_Gaga_discography"  

H %>% 
  read_html() %>% 
  html_element(xpath = G) %>% 
  html_table()
