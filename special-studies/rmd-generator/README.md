Design Document for Automatic Report Generator
==============================================

The goal is to be able to input several different kinds of objects, for example, 
the linear model (lm) object, then be able to output a .Rmd file that summarizes 
what is in the object.

Of necessity, everything should be self-contained in the document and attached 
files. So far now I am going to store the object with is a .Rdata object.

What we want to see

+	Introduction
	-	The model type
	-	The model meta-formula
+	Graphs
	-	With pictures
+	Summaries
	-	With numbers
+	Conclusion
	-	Analysis based on test statistic
+	Appendix
	-	Original data

Linear Model (lm)
-----------------

