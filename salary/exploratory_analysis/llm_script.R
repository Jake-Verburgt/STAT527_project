# Load necessary library
library(tidyverse)

data <- tibble(read.csv("salary.csv"))

dept_names <- unique(data$department)

# Remove any leading or trailing whitespace
dept_names <- trimws(dept_names)

# Create a data frame
dept_mapping <- data.frame(
  Original = dept_names,
  stringsAsFactors = FALSE
)

# Define a function to clean department names
clean_department <- function(name) {
  # Convert to lowercase for consistent matching
  name_lower <- tolower(name)
  
  # Remove incomplete words (e.g., trailing 'a', 'an', 'and', '&', etc.)
  name_clean <- sub("[,\\s]*\\b(?:a|an|and|the|&|of|for|in|on|with|by|to|from|at|vs|via|stu|\\b)\\s*$", "", name_lower)
  
  # Expand abbreviations
  name_clean <- gsub("&", "and", name_clean)
  name_clean <- gsub("dept", "department", name_clean)
  name_clean <- gsub("adm", "administration", name_clean)
  name_clean <- gsub("engrg?|engr", "engineering", name_clean)
  name_clean <- gsub("mgmt", "management", name_clean)
  name_clean <- gsub("physi", "physics", name_clean)
  name_clean <- gsub("chem", "chemistry", name_clean)
  name_clean <- gsub("techno?l?", "technology", name_clean)
  name_clean <- gsub("compu?r?", "computer", name_clean)
  name_clean <- gsub("sci(en[cs])?", "science", name_clean)
  name_clean <- gsub("econ", "economics", name_clean)
  name_clean <- gsub("acct", "accounting", name_clean)
  name_clean <- gsub("commu?n?", "communication", name_clean)
  name_clean <- gsub("biolo?", "biology", name_clean)
  name_clean <- gsub("psycho?", "psychology", name_clean)
  name_clean <- gsub("liber", "liberal", name_clean)
  name_clean <- gsub("residen", "residence", name_clean)
  name_clean <- gsub("hist", "history", name_clean)
  name_clean <- gsub("registrat?n?", "registrar", name_clean)
  name_clean <- gsub("edu(ca)?tion", "education", name_clean)
  name_clean <- gsub("engage", "engagement", name_clean)
  name_clean <- gsub("agri(cultu)?re?", "agriculture", name_clean)
  name_clean <- gsub("mechani", "mechanical", name_clean)
  name_clean <- gsub("philo", "philosophy", name_clean)
  name_clean <- gsub("math(ematics)?", "mathematics", name_clean)
  name_clean <- gsub("departm(en)?t", "department", name_clean)
  name_clean <- gsub("manag?e?ment", "management", name_clean)
  name_clean <- gsub("divi?s?i?on", "division", name_clean)
  name_clean <- gsub("inter?n?ationa?l?", "international", name_clean)
  name_clean <- gsub("advanceme?n?t", "advancement", name_clean)
  name_clean <- gsub("informa?t?ion", "information", name_clean)
  name_clean <- gsub("enroll?m?en?t?", "enrollment", name_clean)
  name_clean <- gsub("success net", "success network", name_clean)
  name_clean <- gsub("col", "college", name_clean)
  name_clean <- gsub("univ(er)?s?i?t?y?", "university", name_clean)
  name_clean <- gsub("divi?s?i?on", "division", name_clean)
  name_clean <- gsub("presi?d?e?n?t?", "president", name_clean)
  name_clean <- gsub("assist", "assistant", name_clean)
  name_clean <- gsub("dir(ec)?t?o?r?", "director", name_clean)
  name_clean <- gsub("vice", "vice", name_clean)
  name_clean <- gsub("chancello?r?", "chancellor", name_clean)
  name_clean <- gsub("hou?si?n?g?", "housing", name_clean)
  name_clean <- gsub("stu(dent)?", "student", name_clean)
  name_clean <- gsub("profess?i?o?n?a?l?", "professional", name_clean)
  name_clean <- gsub("admini?s?tr?a?t?i?o?n?", "administration", name_clean)
  name_clean <- gsub("dept", "department", name_clean)
  name_clean <- gsub("instruc?t?i?o?n?", "instruction", name_clean)
  name_clean <- gsub("developme?n?t", "development", name_clean)
  name_clean <- gsub("poli(tic)?al?", "political", name_clean)
  name_clean <- gsub("departme?n?t", "department", name_clean)
  name_clean <- gsub("hist", "history", name_clean)
  name_clean <- gsub("poli(tic)?al?", "political", name_clean)
  name_clean <- gsub("accountin?g?", "accounting", name_clean)
  name_clean <- gsub("marketin?g?", "marketing", name_clean)
  name_clean <- gsub("admi?n?", "administration", name_clean)
  name_clean <- gsub("budg(et)?", "budget", name_clean)
  name_clean <- gsub("service?s?", "services", name_clean)
  name_clean <- gsub("assess", "assessment", name_clean)
  name_clean <- gsub("equipment", "equipment", name_clean)
  name_clean <- gsub("athlet(ic)?s?", "athletics", name_clean)
  name_clean <- gsub("psycholog?y?", "psychology", name_clean)
  name_clean <- gsub("economi?c?s?", "economics", name_clean)
  name_clean <- gsub("mail", "mail", name_clean)
  name_clean <- gsub("payroll", "payroll", name_clean)
  name_clean <- gsub("graduate", "graduate", name_clean)
  name_clean <- gsub("graduate", "graduate", name_clean)
  name_clean <- gsub("dean", "dean", name_clean)
  name_clean <- gsub("sciences?", "science", name_clean)
  name_clean <- gsub("biological", "biological", name_clean)
  name_clean <- gsub("farm", "farm", name_clean)
  name_clean <- gsub("family", "family", name_clean)
  name_clean <- gsub("student", "student", name_clean)
  name_clean <- gsub("child", "child", name_clean)
  name_clean <- gsub("human", "human", name_clean)
  name_clean <- gsub("resources", "resources", name_clean)
  name_clean <- gsub("counsel?i?n?g?", "counseling", name_clean)
  name_clean <- gsub("learning", "learning", name_clean)
  name_clean <- gsub("center", "center", name_clean)
  name_clean <- gsub("field", "field", name_clean)
  name_clean <- gsub("extension", "extension", name_clean)
  name_clean <- gsub("graphic", "graphic", name_clean)
  name_clean <- gsub("language", "language", name_clean)
  name_clean <- gsub("cultu?ral?", "cultural", name_clean)
  name_clean <- gsub("cooperative", "cooperative", name_clean)
  
  # Capitalize each word
  name_clean <- tools::toTitleCase(name_clean)
  
  # Remove any extra whitespace
  name_clean <- trimws(name_clean)
  
  return(name_clean)
}

# Apply the cleaning function
dept_mapping$Cleaned <- sapply(dept_mapping$Original, clean_department)

# View the mapping
print(dept_mapping)

