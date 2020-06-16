library(rvest)
library(XML)
library(stringr)
#library(xlsx)

GetHTML=function()
{
  
  
  
  
  issue_year = 1994
  
  #Converting the given year to journal specific years
  
  journal_year_equivalent = as.integer(issue_year-1993)
  
  #get todays date
  cur_date = Sys.Date()
  
  cur_year = as.integer(substr(cur_date,0,4))
  cur_month = as.integer(substr(cur_date,6,7))
  
  #initialize loop parameters
  max_year_count = as.integer(cur_year - 1993)
  i = 1
  j = 1
  loop_count = 0 
  max_issue = 0
  
  if(max_year_count >= journal_year_equivalent)
  {
    if(cur_month < 2)
    {
      max_issue = 0  + (6 * (max_year_count - journal_year_equivalent))
    }
    if(cur_month > 1 && cur_month < 4)
    {
      max_issue = 1 + (6 * (max_year_count - journal_year_equivalent))
    }
    if(cur_month > 3 &&cur_month < 6)
    {
      max_issue = 2 + (6 * (max_year_count - journal_year_equivalent))
    }
    if(cur_month > 5 &&cur_month < 8)
    {
      max_issue = 3 + (6 * (max_year_count - journal_year_equivalent))
    }
    if(cur_month > 7 &&cur_month < 10)
    {
      max_issue = 4 + (6 * (max_year_count - journal_year_equivalent))
    }
    if(cur_month > 9 &&cur_month < 12)
    {
      max_issue = 5 + (6 * (max_year_count - journal_year_equivalent))
    }
    if(cur_month == 12)
    {
      max_issue = 6 + (6 * (max_year_count - journal_year_equivalent))
    }
  }
  val = c(1,2,3,4,5,6)
  issue_number = 0
  row_count = 0
  years = seq(journal_year_equivalent,max_year_count)
  list_titles = c('')
  list_authors = c('')
  list_cor_author = c('')
  publication_date = c('')
  abstract = c('')
  keywords = c('')
  full_text = c('')
  #Article_data = data.frame(list_titles,list_authors,list_cor_author)
  
  for (k in years)
  {
    for(i in val)
    {
      if(max_issue == 0)
      {
        break;
      }
      else
      {
        link <- paste("https://academic.oup.com/dnaresearch/issue/",k,"/",i,sep="")
        print(link);
        max_issue = max_issue - 1
        main.page = read_html(x =link)
        link_list = main.page %>% html_nodes("div.al-article-items") %>%
          html_nodes("h5.customLink.item-title") %>%
          html_nodes("a")%>%
          html_attr('href') 
        count1 = 1
        for (j in link_list)
        {
          row_count = row_count + 1
          article_link = paste("https://academic.oup.com",j,sep="")
          article_page = read_html(x =article_link)
          #print(article_link)
          
          file_name <- paste("volume",k,"_"," issue_number",i,"Article",count1,".html")
          write_xml(article_page, file=file_name)
          
          count1 = count1 +1
          
          
          
          
          
        }
        
        
      }
      
      Sys.sleep(20)
    }
    
    
    
  }  
 
}

