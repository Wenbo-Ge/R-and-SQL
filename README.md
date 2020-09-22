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
