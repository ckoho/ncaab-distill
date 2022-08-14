usethis::use_git()
usethis::use_github()
library(distill)
distill::create_post("Home Court Advantage")
distill::create_post(
  "ELO Rating System",
  author = "Colin Kohoutek",
  slug = "auto", # generates a website slug (URL)
  date_prefix = TRUE, # adds date for sorting
  draft = FALSE, 
  edit = interactive()
)
