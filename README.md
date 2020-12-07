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
	
# R conditions if not working
	rows = highlight$diff_flag==TRUE // if not working, try down below
	rows = which(highlight$diff_flag==TRUE) // this should work
	
# Mannully give order to the strings in the column 
	mutate(COL_NAME = factor(COL_NAME, levels = c("A", "B", "C"))) %>%
	arrange(COL_NAME) %>%
	slice(1L)
	
# SQL Listagg with conditions
	two fields : dname, job
	SELECT d.dname,   
       	(select LISTAGG(job,', ')    
               WITHIN GROUP (ORDER BY job)   
          from (select unique job job, e.dname name   
                  from scott.emp e  
                 ) X
		 where x.name = d.dname
		 group by x.name
		 ) jobs  
  	FROM scott.dept d
	
# Oracle Add "commit" at end after drops
	Drop procedure xxx;
	Commit;
	
# R RegExp resource
	Stringr REGEX: https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_strings.pdf
	R REGEX: https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html
	You can also create your own character classes using []:
	[abc]: matches a, b, or c.
	[a-z]: matches every character between a and z (in Unicode code point order).
	[^abc]: matches anything except a, b, or c.
	[\^\-]: matches ^ or -.
	
	https://regexr.com/
	
	
# Melt dataframe to long format table
	lf <- melt(result, id.vars = "Col_Name", 
           variable.name = "X_Name",
           value.name="Y_Name")
	   
# Put 0 in the output when doing summarizing
	group_by(variable1, variable2) %>%
  	summarise(new_col = n(), .groups = "drop") %>%
  	complete(variable1, variable2, fill = list(new_col = 0))
	
	
