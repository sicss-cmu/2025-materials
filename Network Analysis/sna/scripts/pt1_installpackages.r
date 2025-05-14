#####################################
# Social Network Analysis Workshop  #
# Part 1: Installing R Libraries    #
# by Scott Leo Renshaw              #
#####################################

# Make sure you made your new RSTUDIO project in the Workshop folder on
# your desktop.

#These R libraries will be necessary to follow along with the workshop.

#You can install libraries using the install.packages() function in R.
install.packages("statnet",dependencies = TRUE) 

#When you want to load a package into R, you use the library() function

library("statnet") #Because statnet contains sna, network, and ERGM, we only need
                   #to load that single package, and it will load the rest.
#
#-A Brief Intro to R------------------------------------------------------------
#
a <- 25  #Assign a to 25.                                                  
a        #Evaluate, what is stored in a?                                                
#[1] 25

sqrt(a)           # Perform an operation on a.
b <- sqrt(a)      # Perform operation and assign to new object b.
b                 # Evaluate, what is stored in b?                                       

# Boolean logic, TRUE and FALSE
a == a        # A is A? TRUE!          
a != b        # A is not B? TRUE!
a == b        # A is B? FALSE!

sqrt(a) == b  # Is the square root of A == B? TRUE!


#Useful Functions to remember:
ls()                   # List objects in global environment (a, b)
help(sqrt)             # Learn more about a function...
?sqrt                  # Equivalent to help(sqrt)
help.start()           # Manuals! More help!

#Object Naming

#Won't work: 
1cat <- "one cat" # Cannot start object name with numeric.

#Error: unexpected symbol in "1cat"

#Will work:
onecat <- "One cat!" # Works!
onecat # Works!

#Object classes: Character, Numeric, Logical

class(onecat)   # character
class(a)        # numeric
class(a == b)   # logical

#There are others, but not important for this workshop (Raw, Integer, Complex)

#Data Structures: Atomic Vector, Lists, Matrix, Data Frame, and Factors.
#We will cover some of these in our workshop today!

ls() #What objects have we created?

#Remove items from the global environment.
rm(a) 
rm(b)
rm(onecat)

#What's left?

ls() #empty character string, no variables.

