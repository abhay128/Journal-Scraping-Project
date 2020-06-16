library(rvest)
library(XML)
library(stringr)
#library(xlsx)

F=function()
{
  
  
  # Get user to input a year
  print('Please select a year between 1994 to 2019');
  issue_year = readline(prompt="Enter year: ");
  issue_year = as.integer(issue_year);
  print(paste("Hi, you requested year", issue_year, "for journals."));
  
  #Converting the given year to journal specific years
  
  journal_year_equivalent = as.integer(issue_year-1993)
  
  #get todays date
  cur_date = Sys.Date()
  
  cur_year = as.integer(substr(cur_date,0,4))
  cur_month = as.integer(substr(cur_date,6,7))
  
  if((issue_year > cur_year)|| (issue_year < 1994))
  {
    print("Invalid year");
  }
  else
  {
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
    list_issue_date = c('')
    publication_date = c('')
    List_corresponding_email = c('')
    abstract = c('')
    keywords = c('')
    full_text = c('')
    #Article_data = data.frame(list_titles,list_authors,list_cor_author)
    
    for (k in years)
    {
      
      temp_year = k + 1993;
      
      
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
          for (j in link_list)
          {
            row_count = row_count + 1
            article_link = paste("https://academic.oup.com",j,sep="")
            article_page = read_html(x =article_link)
            #print(article_link)
            
            title = article_page %>% html_node("title") %>% html_text()
            #print(title)
            list_titles[row_count] = title
            
            authors = article_page %>% html_nodes("div.info-card-name") %>% html_text()
            cor_authors = authors[1]
            #print(cor_authors)
            list_cor_author[row_count] = cor_authors
            
            authors <- paste("'",as.character(authors),"'",collapse=", ",sep="")
            #print(authors)
            list_authors[row_count] = authors 
            
            publication_date[row_count]  = article_page %>% html_node("div.citation-date") %>% html_text()
            
            if(i == 1)
            {
              list_issue_date[row_count] = paste("February ", temp_year)
            }
            if(i == 2)
            {
              list_issue_date[row_count] = paste("April ", temp_year)
            }
            if(i == 3)
            {
              list_issue_date[row_count] = paste("June ", temp_year)
            }
            if(i == 4)
            {
              list_issue_date[row_count] = paste("August ", temp_year)
            }
            if(i == 5)
            {
              list_issue_date[row_count] = paste("October ", temp_year)
            }
            if(i == 6)
            {
              list_issue_date[row_count] = paste("December ", temp_year)
            }
            
            
            abstract[row_count]  = article_page %>% html_node("section.abstract")  %>%html_text() #Get the abstract of the currect article
            keywords[row_count]  = article_page %>% html_node("div.kwd-group") %>%html_text() #get the keywprds of the current article
            full_text[row_count] = article_page %>% html_node("div.widget.widget-ArticleFulltext.widget-instance-OUP_Article_FullText_Widget") %>% html_text()  #Get the full text of the current article in the iteration
            full_text[row_count] = gsub("[\r\n]","",full_text[row_count])
            
             temp_email <- article_page %>%html_nodes("div.info-author-correspondence")%>% html_nodes("a") %>% html_text()
             temp_email <- paste("'",as.character(temp_email),"'",collapse=", ",sep="")
             
             List_corresponding_email[row_count] = temp_email
            
          }
          
          
        }
        
        Sys.sleep(20)
      }
      
      
      
    }  
    #print(list_titles)
    
    Article_data = data.frame(list_titles,list_authors,list_cor_author,List_corresponding_email,list_issue_date,publication_date, abstract,keywords,full_text)
    #print(Article_data)
    write.table(Article_data, "ArticleData.txt", sep="\t")
    #write.csv(Article_data, "mydata.csv")
  }
}

