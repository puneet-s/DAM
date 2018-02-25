clean_patent_text = function(text_column){
  require(stringr)
  require(dplyr)
  text_in_func <- text_column  %>% data_frame() %>% rename(text=".")%>%
    mutate(text = str_replace_all(text, "include", " "),
           text = str_replace_all(text, "[Mm]ethod", " "),
           text = str_replace_all(text, "[Cc]alculate", " "),
           text = str_replace_all(text, "[Aa]pparatus", " "),
           text = str_replace_all(text, "compris", " "),
           text = str_replace_all(text, "insert", " "),
           text = str_replace_all(text, "[Dd]etermin", " "),
           text = str_replace_all(text, "[Pp]rovid", " "),
           text = str_replace_all(text, "change", " "),
           text = str_replace_all(text, "base", " "))
  return(text_in_func)
}
