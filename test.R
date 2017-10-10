rm(list = ls()); gc(reset = T)

library(rvest)

url = {}

url[1] = "https://ezprice.com.tw/cate10-%E9%A4%8A%E7%94%9F%E4%BF%9D%E5%81%A5/"

for(i in 1 : 19) {
  url[i+1] = paste(url[1], "?p=", (i + 1), sep = "")
}

ez1 = function(html) {
  
  r = read_html(html)
  
  r_html = paste("https://ezprice.com.tw", r%>% html_nodes(".pd-img")%>% html_attr("href"), sep = "")
  
  st = runif(1, 0.2, 0.5)
  Sys.sleep(st)

  return(r_html)
  
}

ez2 = function(html){
  
  r_r = read_html(html)
  
  t0 = r_r %>% html_nodes("ol")%>% html_nodes("span")%>% html_attr("content") # price
  t1 = r_r %>% html_nodes("ol")%>% html_nodes("a")%>% html_attr("title") # products' name
  t2 = r_r %>% html_nodes("ol")%>% html_nodes("a")%>% html_attr("href")
  t2_n = t2[which(is.na(t1) == 0)]
  
  t = data.frame(price = na.omit(t0), title = na.omit(t1), url = t2_n)
  
  st = runif(1, 0.5, 1.2)
  Sys.sleep(st)
  
  return(t)
  
}

u = list()
a = 1

for(i in 1:20)
  for(j in 1 :30) {
    {
      
      u[[a]] = ez2(ez1(url[i])[j]); a = a + 1
      
    }
  }

    
p0 = sapply(1:600, function(x) {u[[x]]$price})

p1 = sapply(1:600, function(x) {u[[x]]$title})

p2 = sapply(1:600, function(x) {u[[x]]$url})

p = data.frame(product = unlist(p1), price = unlist(p0), url = unlist(p2))

write.csv(p, "ezprice_med.csv", row.names = F)
