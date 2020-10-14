# R-Language


  # Data Mining
	R language
	https://www.tutorialspoint.com/r/index.htm
	
	Shiny R to build R interface
	
	Remove HTML tags in R:
	https://www.thetopsites.net/article/51295573.shtml 
	library(rvest)

	strip_html <- function(s) {
	    html_text(read_html(s))
	}
	Example output:

	> strip_html("junk junk<a href=\"/wiki/abstraction_(mathematics)\" title=\"abstraction (mathematics)\"> junk junk")
	[1] "junk junk junk junk"
	
# mutate() with ifelse() to flag certain field and create new field
	https://rstudio-pubs-static.s3.amazonaws.com/116317_e6922e81e72e4e3f83995485ce686c14.html#/5
	mutate(gradebook, Pass.Fail = ifelse(grade > 60, "Pass", "Fail"))

# Good habit to use table("certain column") to identify anaomly with filtered result
	i.e. filter condition is ID, then do table(data.frame$ID) to check result

# Good habit to check duplicated results
	do data.frame %>% count(field_name) %>% count(n)
	
# ggplot with different group
	ggplot(df, aes(x = column_year, y = clolumn_value, color = column_group, group=column_group)) +
  	geom_line() 
	
# Excel using column value to filter another column
	use countif function
	
# Use pivot_wider to generate tables
	pivot_wider(names_from= , values_from= )
	
# R case_when()

# R coalesce() function 
	# Or match together a complete vector from missing pieces
	y <- c(1, 2, NA, NA, 5)
	z <- c(NA, NA, 3, 4, 5)
	coalesce(y, z)
	#> [1] 1 2 3 4 5

# Right join with expand
	summarize(..., ...) %>%
	right_join(expand(., column1, column2) 
	dot mean current dataframe
