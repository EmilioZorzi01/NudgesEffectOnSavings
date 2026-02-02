# data extraction

data = read_xlsx("C:\\Users\\mauri\\Desktop\\Datan.xlsx")
data = dummy_cols(data, select_columns = "Parsimony", remove_first_dummy = FALSE)
data = data %>% 
  mutate(across(everything(), ~as.numeric(as.character(.))))
attach(data)