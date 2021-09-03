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
	
	"\\bSELECT\\b" will match only SELECT, instead of matching SELECTED
	"\\/\\*" will match /*
	",\\(SELECT" will match ,(SELECT. \\ will escape (
	
	"^.*\\bCASE\\b(?!.*\\bAS\\b).*$" string contains CASE but doesn't contains AS, this is negative LOOKAHEAD
	CASE WHEN                                       //////////// this line will be matched
	CASE WHEN abssdad END             AS asdsad
	"^.*(?<!\\bCASE\\b.*)\\bEND\\b.*$" string contains END but doesn't contains CASE, this is negative LOOKBEHIND
	END                                                         AS aaaa ///// this line will be matched
	CASE WHEN abssdad END             AS asdsad
	apply(sapply(c(",\\(SELECT", " AS"), grepl, df$col),1,all) return TRUE/FALSE if string contains both SELECT and AS
	
	https://javascript.info/regexp-lookahead-lookbehind
	
	https://regexr.com/
	
	
# Melt dataframe to long format table
	lf <- melt(result, id.vars = "Col_Name", 
           variable.name = "X_Name",
           value.name="Y_Name")
	   
# Put 0 in the output when doing summarizing
	group_by(variable1, variable2) %>%
  	summarise(new_col = n(), .groups = "drop") %>%
  	complete(variable1, variable2, fill = list(new_col = 0))
	
# Merge excel cell and create title for table
	mergeCells(wb, "Output",
           cols = 1:ncol(df_print),
           rows = 1)

	addStyle(wb, "Output",
         cols = 1,
         rows = 1,
         style = createStyle(halign = "center"))

	writeData(wb, "Output", "Table Title",
          startCol = 1, startRow = 1)
	  
# Difference between <- <<- and =
	use <<- to assign value in the function if no return exists in the function  
	
# Difference between grep and grepl
	x <- c("d", "a", "c", "abba") 
	grep("a", x)                      
	# 2 4
	grepl("a", x)                     
	# FALSE  TRUE FALSE  TRUE
	
# Remove unnecessary line breaks and spaces in "col_name" column
	df_Fin$col_name <- sapply(df_Fin$col_name", function(x) { gsub("[\r\n]", " ", x) })	
	
# R Summaries counts
	counts <- df %>%
	group_by (col_1) %>%
	summaries(appl = n(), .groups="drop")
	
	Easier Way:
	counts <- df %>%
    	count(col = col_1, name = "appl")
	
# Case When tips
	case_when(
    		factor < 5 ~ "level_1",
    		factor < 15 ~ "level_2", // dont need to do factor >= 5 & factor < 15  
    		factor >= 15 ~ "level_3",
    		TRUE ~ "UNKNOWN"
  		)
	
# rbind data type issue
	last_row <- c(NA, 
                  sum(df$data_type_is_double), 
                  sum(df$data_type_is_double_2))
	then, this vector is double, rbind(df, last_row) -> all column will stay double
	
	last_row <- c("TOTAL", 
                  sum(df$data_type_is_double), 
                  sum(df$data_type_is_double_2))
	then, this vector is character, rbind(df, last_row) -> all column will stay character
	
# format date directly in openxslx
	options("openxlsx.datetimeFormat" = "yyyy-mm-dd hh:mm:ss")
	## custom datetime formats can be made up of any combination of:
	## d, dd, ddd, dddd, m, mm, mmm, mmmm, mmmmm, yy, yyyy, h, hh, m, mm, s, ss, AM/PM
	
# group_by tips in R with tidyverse
	  group_by(!!!syms(names(.)[names(.) != "COL_NAME"]))
	  this code will group_by every columns in dataframe except COL_NAME column
  
# argument 'pattern' has length > 1 
	Warning message:
	In gsub(substr(COL_NAME, 5, 6), "01", COL_NAME) :
	  argument 'pattern' has length > 1 and only the first element will be used
  
	COL_NAME <- mapply(function(x)gsub(substr(x, 5,6), "01", x),
                       COL_NAME)

# sprintf function
	sprintf("blah blah blah in %s", paste(comp, collapse = ", "))
	this will print every element in comp and paste to the end of senetence

# difference between df$col and df["col"]

# format date in R 
	https://www.statmethods.net/input/dates.html

# R how to create following dataframe:
	Institution     Year
	university_1    2016
	university_1    2017
	university_2    2016
	university_2    2017
	
	list_output <- data.frame(Institution = c("university_1", "university_2"), 
                        	  YEAR = rep(c("2016", 
                                     		"2017",
                                     		"2018",
                                     		"2019",
                                     		"2020"), 2)) %>%
  			arrange(Institution, YEAR)

# % format in excel output
	percent <- createStyle(numFmt = "##.0 %")
	
# R date's type of double
	typeof(df$date)
	double
	
# Append a totals row and/or column to a data.frame
	janitor::adorn_totals(name = "Grant Value", fill = " ")
	fill	 if there are non-numeric columns, what should fill the bottom row of those columns
	name	 name of the totals row and/or column
	
# R create Excel Sheet Format Style, differentiate row colors
	writeDataTable(wb, "tabName", 
               testDf,
               # get colour banded rows
               tableStyle = "TableStyleLight18")
	

# R join to compare difference between two Excel
	http://zevross.com/blog/2014/08/05/using-the-r-function-anti_join-to-find-unmatched-records/
	# which records occur in table1 but not in table2
	anti_join(table1, table2, by=c("state", "county"))
	
# R script to pull SQL fields

# SQL Split single comma delimited string / or in the table into rows in Oracle
	https://lalitkumarb.wordpress.com/2014/12/02/split-comma-delimited-string-into-rows-in-oracle/
	https://lalitkumarb.wordpress.com/2015/03/04/split-comma-delimited-strings-in-a-table-in-oracle/
	
# SQL function to perform task row by row
	Example: want to add a column that matchs person with their countries seperated by comma
		Person_Name        Country_Name
		AAA, BBB, CCC	   China, US, Canada
		
	SQL: 
		select P.Person_Name,
		       get_all_countries(P.Person_Name)
		from person P
		
	Sample code to create function:

	create or replace FUNCTION   get_all_countries
                 (person_name_in person.person_name%TYPE)

        CURSOR COUNTRY_CUR IS
        SQL script here
        ;

	-- ---------------------------

	COUNTRY_CD                country.country_cd%type;
	CONCATE_COUNTRY_CD                  LONG := '';

	BEGIN
	   OPEN COUNTRY_CUR;
	   LOOP
	    FETCH COUNTRY_CUR INTO COUNTRY_CD;
	    EXIT WHEN COUNTRY_CUR%NOTFOUND;

	  IF  CONCATE_COUNTRY_CD is null THEN
		   CONCATE_COUNTRY_CD := COUNTRY_CD ;
	       ELSE
		  CONCATE_COUNTRY_CD := CONCATE_COUNTRY_CD||'; '||COUNTRY_CD;
		END IF;

	   END LOOP;


	   CLOSE COUNTRY_CUR;

	RETURN CONCATE_COUNTRY_CD;
	--
	EXCEPTION
		WHEN OTHERS THEN
			CLOSE COUNTRY_CUR;
			RAISE;
	END;
	-- END PL/SQL BLOCK (do not remove this line) --------------------
	
# Shiny R plot linear regression line
	  output$plot_1 <- renderPlot({
    		plot(women$height, women$weight, 
         	main = "Average Women's Height vs Weight")
	    if(input$line) {
	 -- should be y ~ x, if x ~ y, line won't be generated
      	model <- lm(women$weight ~ women$height, data=women)
      	abline(model)
    	}
  	})

# ggplot sometimes need to as.factor columns
	- if not factored, there will be not color in the plot
	titanic$Survived <- as.factor(titanic$Survived)
	titanic$Sex <- as.factor(titanic$Sex)

	ggplot(titanic, aes(x=Sex, fill = Survived)) +
		theme_bw() +
		geom_bar() +
		labs(y = "Number of Passengers", title = "Survival Rate by Gender")
