
################################################################
#
#
#                      INTRO TO R
#
# The R website:
#
#   http://cran.r-project.org
#
#   (Google "R" -> one of the first entries)
#
# Downloading R:
#
#   ->  Sidebar "Download, Packages": CRAN 
#   ->  any site in the US
#   ->  Windows
#   ->  base
#   ->  "Download R-2.... for Windows (32 bit build)"
#   ->  installation dialog in setup wizzard
#
#   The setup program should self-install and create an icon on your desktop.
#   Clicking the icon should open up an R interpreter window ("R Gui").
#
#   The base is really just the base.  There are many contributed
#   library packages whose binaries can be downloaded from
#
#   ->  Packages
#
#   You will not have to download them explicitly, though;
#   there are R functions that allow you to get them while running R.
#   In the R Gui you can also go through the "Packages" item in the toolbar.
#    
################
#   The R-studio website:
#
#    https://rstudio.com 
#
#
################
#   The github website
#
#   https://github.com
#
#
#
# OPERATION OF R:
#
#
#
# 1) Copy R code from this file into the R interpreter window.
#    Use shortcuts: In the editor highlight lines, hit <Ctrl>-C,
#                   then move to the R window and hit <Ctrl>-V.
#    Examples:
       1+2
       1:10
       2^(1:20)
       runif(10)
       rnorm(10)
       1:10 + rnorm(10)
#
# 2) Experiment with R code
#      by editing THIS file in the editor window, or
#      by editing the command line in the R window (if it's one-liners).
#    
# 
#
##################
#
#
# * R is an INTERPRETED LANGUAGE:
#     Users type expressions and see results immediately.
#     Example:
        for(i in 1:10) { if(i%%2==0) print(i) }
#     As opposed to:
#     - ... languages (C, Fortran)
#     - ... software (such as SAS' JMP)
#    
#
# * R is HIGH-LEVEL:
#     It operates on complex data structures such as 
#     vectors, matrices, arrays, lists, dataframes,
#     as opposed to C and Fortran that operate on individual numbers only.
#     (This requires some getting used to for C programmers.)
#    
#
# * PRIMARY BEHAVIOR: Whatever is typed, print the results.
      2
      print(2)   # same
      "a"
      print("a") # same
#   (Q: Why is there '[1]' preceding the results?  A: ...)
#   Vector of length greater than 1:
      1:3
      print(1:3) # same
#
#
# * SYNTAX:
#   - Largely scientific/math notation; base 10.
#   - A wealth of functions.
#   - Comments run from a "#" to the end of the line; no multiline comments.
#   - Spaces are irrelevant, except inside strings:
        2+3; 2  +    3; "ab"; "a   b"
#   - Statements can run over multiple lines:
        2 + 3 +     # \
        4           # / One statement
#     But if a statement is syntactically complete at
#     the end of the line, it won't continue:
        2 + 3       # \
        + 4         # / Two statements
#   - Statements can be separated by ";".
        2; 3^3; sqrt(9)
#    
#---
#
# * BASIC DATA TYPES:
#
#
#   - NUMERIC: double precision by default (How many bytes?)
#     Integers are represented as doubles, although the print function
#     shows them as integer:
        -2.000
        1E5
        2E-3
#     The usual unary and binary operations and analytic functions:
#       +, -, *, /, %%, %/%, ^, log, sqrt, sin, acos...
        2+3            # Add.
        5.3*1E10       # Multiply.
        10%%3          # Modulo.
        exp(1)         # Exponentiation.
        log(2.718282)  # Log of the number 'e'; 'log' is e-based.
        log10(10)      # 10-based log
        pi             # That's the number 'greek pi', 3.14159
        sin(pi/2)      # Angles are to be given in arcs, not degrees.
        sin(pi)        # Dito.
        acos(0)        # This is the inverse of cos, arccos, hence pi/2.
        pi/2           # This is the only hardwired constant: 3.14159...
#
#
#   - STRINGS: can be single or double quoted, but the print function
#     uses double quotes.
        'a'; "a"; 'abc'; "abc"
#     (In C and Python strings are character vectors.
#      In R strings are basic types; there is no single character type.
#      Characters are just strings of length 1.
#      There is no indexed access to individual characters and
#      substrings in R; one uses the "substring" function instead:
        substring("abcxyz",4,6)
#     Other basic string manipulations:
        paste("my","word")
        nchar("Supercalifragilistikexpialidocious")
#     There are two hardwired character vectors that contain the lower and
#     upper case letters:
        letters
        LETTERS
#
#
#   - LOGICAL values: have two names each, but the print function
#     always uses the longer.
        TRUE; FALSE; T; F
#     They are implemented as the values 1 and 0 for T and F, respectively.
#     They are the result of the usual comparisons: <, >, <=, >=, ==, !=
        1<2; 1>2; "ab" <= "abcd"
        "ab" > "ac"; "ab" != "AB"
        "ab" != 2; 0==F; 1==T
#
#
#   - MISSING values NA, Inf, -Inf:
        NA; NaN; Inf; -Inf; 1/0; Inf==1/0; 0/0
#     Watch out: the following does not give T!!!
        NA==1
#     If you want to test for NA, you must use the function is.na():
        is.na(NA)
#
#
#   - FUNCTIONS: 

# * R is a FUNCTIONAL LANGUAGE:
#   Functions return values that in turn can be arguments to functions.
#   Expressions evaluate inside out, e.g., log(2*2.5))^3:
        2.5; 2*2.5; log(2*2.5); log(2*2.5)^3
#
#
# * STATEMENTS/EXPRESSIONS:
#     There are two types of expressions: assignments and side effects.
#     1) Assignments allocate data structures and
#        make variables point to them.  
           x <- 1:3   # Allocate a vector 1,2,3 and make 'x' point to it.
#     2) Side effects are essentially display operations 
#        such as printing, plotting, showing help; unlike assignments,
#        they don't change the computational state of the R system.
           x
           print(x)
           plot(x)
           help("+")    # Show the help page of addition.
           help(sqrt)   # Show the help page of the square root function.
           help("sqrt") # Dito.
#     3) Composite Statements:
           {print(1); plot(1:10)}
#        Will be needed in loops and functions.
#
#
#   - Assignments to variables come in four equivalent syntactic forms:
#       x <- ...
#       x = ...
#       ... -> x
#       assign("x",...)
#     Examples:
        x <- sqrt(2 * 3)    # Root of product of 2 and 3
        x = sqrt(2 * 3)     # Both can be used: '=' and '<-'
        sqrt(2 * 3) -> x    # This can be used, too, if you must...
        y <- c(1,3,10,1,1,1111,0,1,1)  # combine 1,3,10... into a vector 'y'
        z <- 1:3           # Assign the vector containing 1,2,3 to a 'z'.
        assign("x", sqrt(2*4));  print(x)
#     Note that variables jump into existence upon assignment.
#     Unlike C and Fortran, there is no need to declare variables.
#     The variables are not 'typed', that is, any variable can
#     point to data of any type, such as numbers, logicals, vectors,...
#
#---
#
# * HELP: help(function) or help("function") shows function documentation.
        help(sqrt)
#         (Emacs users: call help.start() before using help.)
#     In the output of this function, check out the section
#     with header "See Also:".  It will tell you that you
#     can find related functions by calling
        help("Arithmetic")
        help("log")
        help("sin")
        help("Special")
#          
        help(c)
        help("c")   # Same as help(c)  
        help("*")   # help(*) does not work
#
# * APROPOS: apropos("char") lists all functions whose name contains
#     the string "char".
        apropos("char")
#     This is often useful for finding related functions.  
#     Apropos combined with the section "See Also:" in the output
#     of help() is a powerful tool for searching functions.
#     There are about 1,700 built-in functions, and more if you
#     download special-purpose packages from the R website.
#
# * Printing a function: allows you to see the arguments in a simple way
        runif   # same as: print(runif)
        rnorm   # (functions are "first class citizens", like numbers, vectors,...)
#
#---
#
# * MANAGEMENT OF DATA AND FUNCTIONS: 
#     - Listing R objects, both data and functions: either of
          ls();  objects()
#       This lists all data structures and functions that YOU defined.
#     - Removing data and functions:
          x <- 1:10
          rm(x)
          x
#     - Looking for partly remembered data and functions:
#       In case you remember only part of a name, you can look it up
#       with a partial pattern:
          xx <- 10
          ls(pattern="x")
#       This will list any dataset and function whose name contains "x"
#       such as 'last.warning'.
#     - List all functions that come with the base package of R:
          ls("package:base")   # Over 1,100 functions...
#     - About packages: 
#     . Packages are namespaces for data AND functions.
#       (You can't have a dataset 'x' and a function 'x' at the same time.)
#     . You can list the packages in your environment:
          search()
#     . When you use a name, R goes through the search list
#       shown by 'search()', package by package, stopping when
#       it finds the name.  This implies that the same name can appear
#       in multiple packages, but only the one in the first package
#       will be found.
          ls <- 2:5   # mask 'ls' in "package:base" with user data
          ls
          rm(ls)      # remove user data, unmasking the function 'ls()' 
          ls
#
#---
#
# * QUITTING:
        q()
#     R asks whether you want to save the workspace; 
#     usually you say "yes".  Splus simply quits.
#
#---
#
# * SEMANTICS:
#     Every assignment creates a copy of the assigned object.
#     Assignment is by value, not by reference (unlike Python and C).
        a <- c(1,3,5) # 
        a 
        b <- a        # 'b' gets its own copy.
        b             # We couldn't tell from this, though.
        a[1] <- 2     # Assign the value 2 to the first element of 'a'.
        # This yields a test of whether 'a' and 'b' point to the same object.
        # If they did, then 'b' would also have 2 in the first position.
        a             # We know this.
        b             # Uhu!  'b' was not changed by 'a[1] <- 2'.
        # Therefore, 'b' has its own copy.
#
#--- 2008/09/04 

# * SYNTAX: see
      help(Syntax)

#
# * VECTORS:
#     A vector is a sequence of items of the SAME basic data type.
        c(1,3,4.5)   # Collect three values in a vector.
        c("a","ab")  # Collect two strings in a vector.
        c(T,F,T)     # Collect three logical values in a vector.
        c(2.1,T)     # Not an error.  Coercion of T to 1.
        c(2,"a",T)   # Not an error.  Coercion of 1 and T to strings.
#     If the items are not of the same type, they are coerced:
#       string <-- numeric <-- logical
#     (If the items are of variable types and should not be coerced,
#      use lists instead of vectors.  See below.)
#
#---
#
# * INDEXING AND SUBSELECTING VECTORS:
#
#   R/S, among all languages, has probably the most powerful set of
#   tools for getting at elements of vectors:
#     * selection/mapping with positive integer indeces
#     * delection with negative integer indeces
#     * selection with logical indeces
#     * selection by name when vector entries are named ("associative array")

#   - Numeric indexes: ONE-based (unlike C, but like Fortran)
        a <- c(1,3,5)
        a[1]; a[2]; a[3]
#     (This is unlike Python and C which use ZERO-based indexing.)
#   - Vector indexes:
        a[c(1,3)]
#     or, equivalently, except for the allocation of another vector 'b':
        b <- c(1,3);  a[b]
#   - Vector expansion: amounts to mapping the indexes, using 'a' as a map.
        a[c(1,2,1,2,1,1,1,3,3,3)]
#   - Exclusions with negative numeric indexes:
        a[-1]
        d <- a[-1]
        a[c(-1,-3)]
        b <- c(-1,-3);  a[b]  # dito
        a[c(-1,-2,-3)]   # Nothing left.
        a <- a[-1]       # Actually remove the first element of 'a'.
#   - Logical selection: 
        a <- c(1,3,5)
        a[c(T,F,T)]
        b <- c(T,F,T);  a[b]  # dito
        a>2; a[a>2]      # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        b <- (a>2);  a[b]  # dito
#     Caution: If the index vector is not of equal length,
#     it will be cyclically repeated:
        a[F]             # c(F,F,F)   'F' is repeated 3 times
        a[T]             # c(T,T,T)
        a[c(T,F)]        # c(T,F,T)   
        a[c(T,T,F,F,F)]  # If too long, the index vector is truncated.
        (1:12)[c(T,T,F)] # Leave out every third item.
        # (The above scheme can be used to create arbitrary repeat patterns.)
#   - Vectors can be indexed repeatedly:
        a[c(1,3)][2]     # Select item two of a[c(1,3)], i.e. item 3 of 'a'.
#         (Looks like a matrix element in C, but isn't!!)
        (a[c(1,3)])[2]   # This is what the previous expression really means.  
#         Think of a[c(1,2)] as the result of some selection function.
        a[c(1,3)][c(1,2,1,2)]
#   - Vector indexing and subsetting can be used for assignment:
        a[1:2] <- c(10,20);  a    # Print 'a' to see the effect of the assignment.
        a[c(1,3)] <- c(100,200);  a
        a[-2] <- c(1000,2000);  a
        a[c(F,T,T)] <- c(10000,20000);  a
        a[2:3] <- 0;  a    # "0" is repeated to fill both vector elements.
        b <- 1:10
        b[3:6] <- c(10,20); b # "c(10,20)" is cyclically repeated.
#     If the length does not divide, there is a warning message,
#     but cyclical fill-in is done anyway.
#
#---
#
# * SOME FUNCTIONS THAT CREATE VECTORS
#
#   - Manual entry of a vector:
        x <- c(-1,2,5)
#    
#   - Equispaced sequences of numbers:
        3:10
        10:3
        seq(3, 10)
        seq(3, 10, by=1/3)   # Third argument is names "by"
        seq(3, 10, len=8)
        seq(letters)         # List of indexes into 'letters'
#
#   - Repetitions:
        rep(3, 10)
        rep(1:3, 5)
#     Here is something more complex that "rep" can also do:
        rep(c(1,2,3,4), c(2,3,2,4))
        rep(1:3, rep(2,3))
#    
#   - Logical values:
        rep(T,5)
        ((1:10) > 5)
        (1:10)[(1:10) > 5]
#    
#   - Random numbers:
        x <- runif(5);  x # Five uniform random numbers in [0,1]; see below.
        y <- rnorm(5);  y # Five standard normal random numbers; see below.
#
#   - Random permutations and samples:
        x <- sample(10);  x  # Random permutation of the numbers 1,2,...,10.
        sample               # can also do sampling with replacement...
        sample(letters, 10, replace=T)

#   - Read a vector from file: The file 'sp.dat' is a very long time series;
#       download it from the class website.
        x <- scan("DATA/sp.dat", n=1000)  # First n values
        x <- scan("DATA/sp.dat")     # Reads 734656 numbers; may take a few sec.
#     'x' contains 8 bytes for each element, hence this many MB:
        8*length(x)/2^20     # (2^20 equals one MegaByte)
        object.size(x)       # size in bytes; why is it not exactly 8*length(x)?
#     You may not want to keep this 'x' around because of its length:
        rm(x)
#     Btw, if you run out of memory, try this:
        options(memory=1E10, object.size=1E10)   
#    
#---
#
# * AUTOMATIC VECTOR EXTENSION:
#
#     Array-bound errors for vectors do NOT exist for positive indexes!!!   
#     Vectors are shown as NA if indexed out of bounds, and
#     automatically extended if assigned out of bounds.
        x <- runif(5)  # x has length 5.
        x[6]           # NA, not an error message.
        x[10] <- 9.99  # Not an error!  Element 10 now exists.
        x              # So do elements 6,7,8,9, filled in with NA.
        length(x)        # Assignment can extend the length.
#     However, negative indexes (used for exclusion) can be out of bounds:
        x[-11]           # Out of bounds.
        x[-9]            # Ok, due to fill-in after assigning element 10.
#     Automatic vector extension makes vectors very different from
#     matrices and arrays, for which automatic extension does not exist.
#
#---
#
# * NAMED VECTOR ELEMENTS:
#
#   - Vector elements can be named:
        x <- c(first=1,second=3,third=5,fourth=7);   x
#   - Equivalently, naming elements can be performed in a separate step
#     by assigning the "names()" attribute:
        x <- c(1,3,5,7)
        names(x) <- c("first","second","third","fourth") 
        x
        names(x)
#   - Element names can be used for indexing/subsetting:
        x["second"]
        x[c("second","fourth")]
        nm <- c("second","third","second","third");  x[nm]
#   - Named assignment/extension is possible:
        x[c("fifth","fourth")] <- c(10,11);  x
#     Note: "fourth" existed and is re-assigned;
#           "fifth" did not exist and is an extension of "x".
#   - If names are not unique, the first matching name is selected:
        c(a=1,b=2,a=3)["a"]
#   - Example of automatic use: 'table()'
        x <- sample(letters,size=1000,replace=T)
        table(x)
#
#  
#---
#
# * VECTORS WITH NAMED ELEMENTS AS ASSOCIATIVE ARRAYS:
#
#     R's vectors with named elements are really a limited form
#     of 'associative arrays'.  Associative arrays are best explained
#     in terms of an example:
#       Assume you want to look up, say, salaries of persons given by name.
#       Assume that salaries and names are stored in parallel vectors:
          slry <- c(35000, 100000, 12000)
          nm   <- c("John", "Bill", "Jim")
#       Turn "slry" into an associative array by giving its elements names:
          names(slry) <- nm
#       Salaries are now "associated" with names for lookup by name,
#       the names are the "keys", the salaries the "values".
#       For example, Bill's salary is: 
          slry["Bill"]          # "Bill" is the key, slry["Bill"] the value.
#
#       Another way to create the same: names arguments to c()
          slry <- c(John=35000, Bill=100000, Jim=12000)
          slry
#
#     NOTE: If one wants to use numbers as keys, they have to be
#     converted to strings first.  The conversion happens automatically
#     through coercion, as in
        names(slry) <- c(10,20,30);  slry
        names(slry) 
#     Thus the keys are now the strings "10", "20", "30":
        slry["20"]    
#     Caution: In lookups, numbers are NOT coerced to strings,
#              because numbers act as indexes into the vector
        x <- seq(10,40,by=10)
        names(x) <- c(2,1,4,3) # Coerced to strings: "2","1",...
        x[2]    # Second element of 'x'.
        x["2"]  # Same as x[1], which has the name "2".
#       
#
#---
#
# * RANKING, SORTING, REVERSING, PERMUTING:
        x <- c(5:1,5:10);  x
        rank(x)
        sort(x)
        rev(x)
        sample(x)        # random permutation of 'x'; can also
        sample(x, size=100, replace=T) # random sample of size 100 w. replt.
        sample(x, size=100)            # why does this not work?
#     Here is one of the more important functions: 
        order(x)         # !!!!!!!!!!!!!!!
        sort(x)
        x[order(x)]   # Same!
#     Sorts also character data lexicographically:
        x <- sample(letters);  x   # permute letters
        sort(sample(letters))

#     This is how you perform parallel sorting:
        x <- runif(10)
        y <- -x - 100   # 'y' is just another vector of the same length as 'x'.
        x; y            # Unordered parallel vectors
        ord <- order(x)
        x[ord]; y[ord]  # Sorts both "x" and "y" in ascending order of "x".
#
#
#---
#
# * SIMPLE STATISTICS:
        length(x)
        sum(x)
        mean(x)
        var(x)
        sd(x)
        min(x)
        max(x)
        range(x)
        median(x)
#    
#---
#
# * CUMULATIVE OPERATIONS:
        x <- 1:10
        cumsum(x)
        cumprod(x)
        x <- 1:10 * c(1,-1)
        cummax(x)
        cummin(x)
#    
#---
#
# * SIMPLE NUMERIC FUNCTIONS/TRANSFORMATIONS:
#     Most functions that accept one number and return another number
#     will naturally "vectorize" in R, namely, apply element by element.
        x <- runif(20, min=-10, max=+10)
        round(x)
        trunc(x)
        ceiling(x)
        abs(x)    
        sqrt(x)    # Comment?
        log(x^2)
        exp(1)
        exp(x/100)
        cos(pi)    # "pi" is predefined; the number e=exp(1) is not.
        acos(0.5)  # What is 'acos()'?
#
#
################ 
#
# * MATRICES:
#     Matrices in R are vectors with additional "meta-information"
#     to structure them in a rectangular form.  
#     The elements of the vector fill the matrix column by column.
#     ==> COLUMN MAJOR ORDER, as in Fortran, but unlike in C.
#     Reformatting as a matrix is achieved by giving the vector
#     a dimension attribute consisting of the numbers of rows and cols.
#
#   - Reformatting vectors as matrices by filling successive cols or rows:
        matrix(1:12, ncol=4)           # Column major (default)
        matrix(1:12, nrow=3)           # Same; ncol is inferred
        matrix(1:12, ncol=4, byrow=T)  # Row major, forced with "byrow".
        matrix(1:12, nrow=3, byrow=T)  # Same
        matrix(0:1, nrow=2, ncol=4)    # What happened?
        matrix(0, nrow=2, ncol=4)      #     "
        matrix(letters, ncol=2)        # Elements are now of type 'character'.
        matrix(paste("Letter",letters), ncol=2)
#     When reading data in text files, 'byrow=T' is needed 
#     for row-by-row input (download 'laser.dat' from the course page first):
        m <- matrix(scan("Data/laser.dat", skip=1), ncol=4, byrow=T)
#                                     ^^^^^^ Skip first line.
        m
#     Later we will see a more convenient function for reading
#     tabular data ('read.table()').

#   - Whether something is a matrix can be checked with 'is.matrix()':
        is.matrix(matrix(1:12,3));   is.matrix(1:12)
        x <- 2;     is.matrix(x)
        x <- 1:10;  is.matrix(x)
        x <- matrix(0, nrow=3, ncol=5);  is.matrix(x)
        is.matrix(matrix(0, nrow=3, ncol=5))  # tautology
#
#   - The dimension attribute: it is the sole difference between
#     a vector and a matrix.  It can be queried:
        dim(m)                # Vector of length 2 with the two dimensions.
        dim(m)[1];  dim(m)[2] # Each dimension separately.
        nrow(m);  ncol(m)     # Same.
#     Vectors can be turned into matrices by assigning the
#     dimension attribute:
        m <- 1:12           # Just a vector.
        dim(m) <- c(3,4)    # Now a matrix.
        m
        is.matrix(m)        # TRUE
        dim(m) <- NULL      # Stripping to a vector.
        m
        is.matrix(m)        # FALSE

#   - The dimension name attributes: row- and col-names
        colnames(m) <- letters[1:4]
        rownames(m) <- LETTERS[1:3]
        m
        colnames(m)
        rownames(m)

#   - Indexing/subselecting rows and columns:        (differs from C!)
        m <- matrix(1:12, ncol=4)
        m[1,4]                    # Element in row 1, column 4.
        m[1:3,]                   # First 3 rows.
        m[,3:4]                   # Last 2 columns.
        m[1:3,3:4]                # Submatrix of size 3x2 (unlike Python!)
        m[c(1,2,1,2),]            # Repeat rows 1 and 2.
        m[,c(1,2,1,2)]            # Repeat columns 1 and 2.
        m[c(1,2,1,2),c(1,2,1,2)]  # Repeat left-upper 2x2 matrix 4 times.
        m[-1,]                    # Select all but the first row.
        m[,-c(2,4)]               # Select all but columns 2 and 4.
        m["A",]                   # Only works if col/rownames have been assigned.
        m[c("A","C"),]
        m[c("A","C"),"a"]

#    - ATTENTION: Selection of individual rows and cols generates vectors
#                who do no longer know that they were rows or columns.
#                R has no concept of col-vectors and row-vectors:
                   m[,1]
                   is.matrix(m[,1])
                   is.matrix(m[1,])
#                Vector is vector, period (unlike Matlab).
#                You can force the issue by turning vectors into
#                Nx1 or 1xN matrices:
        x <- 1:10
        dim(x) <- c(10,1);  x
        dim(x) <- c(1,10);  x
#
#   - Index/subsetting can be used for assignment:
        m[1,2]     <- 0
        m[1,]      <- 11:14
        m[,1]      <- 0
        m[1,c(F,F,T,T)]   <- c(7,10)
#
#   - Associative array feature for matrices:
        rownames(m) <- c("x","y","z")       # like 'names(vec)'
        colnames(m) <- c("a","b","c","d")
        m["x","d"]           # number
        m["x",]              # vector
        m[,"c"]              # vector
        m[c("x","z"),c("c","a")]   # submatrix (different from Python!)
        m[c("x","z","x","y"),c("c","a","a")] # col-row-rearrangement
#

# Roadmap:
# - wrap up matrices
# - arrays
# - lists
# - dataframes
# - loops and conditionals
# - character manipulation functions
# - fcts related to distributions
# - plotting functions
# - writing FUNCTIONS
# - querying object types


#   - Column and row binding:
#     Two functions that permit collecting cols and rows to form matrices.
        x <- runif(5)                    # Play data.
        cbind(1:5, x, x, rep(10,5))      # Column binding.
        rbind(1:5, x)                    # Row binding.
#     (Vectors are NOT thought of as columns or rows on their own;
#      they take on these roles inside the "cbind" and "rbind" functions.)
#     Both functions accept an arbitrary number of conforming arguments.
#     You can also bind whole matrices:
        cbind(rbind(1:3,11:13,21:23), cbind(1001:1003,2001:2003))
#     A more friendly way of writing the same is:
        m1 <- rbind(1:3,11:13,21:23)        # 3x3
        m2 <- cbind(1001:1003,2001:2003)    # 3x2
        cbind(m1, m2)                       # 3x5
#     Conforming for 'cbind()' means the arguments have equal number
#     of rows, for 'rbind()' it means equal number of columns.  
#     If vector arguments are not conforming, R extends them cyclically
#     or clips them but may give you a warning if the shorter arguments
#     are not of fractional length of the longest argument:
        cbind(1:3,0)        # second arg has size 1, = fraction of 3
        cbind(1:6,1:3)      # size 3 is a fraction of size 6
        cbind(1:3,1:2)      # size 2 is not a fraction of size 3 => warning
        cbind(1:3,matrix(11:14,2)) # clipping: the second arg dictates nrow
#     Don't rely on cyclic extension except for the simplest cases
#     such as repeating constants.

#   - Coercion of matrices to vectors:
#     A matrix can always be treated as a vector.
#     The following does not create an error message:
        m <- matrix(1:12, nrow=3)
        m[,1]
#     Recall the column-major convention for storing matrices in R.        
#
#   - Some useful functions for generating patterned matrices:
        diag(5)
        outer(1:3,1:4)
        outer(1:3,1:4, FUN=paste, sep="")
        x <- outer(1:3,1:4)
        row(x)
        col(x)
        row(x)>col(x)
        x[row(x)>col(x)]

#   

################
#
# * ARRAYS: the generalization of matrices to more than 2 indexes
#
          a <- array(1:24, dim=c(2,3,4))
          a
          a[1,2,1]
          a[,2:3,c(1,4)]
          a[,,c(T,F,T,F)]
          a[,,-3]
#       Main use: contingency tables
#         e.g., table frequencies of sex by education by income bracket ...
#       The associative array feature exists also when a dimnames are given
#         To understand this, we need to talk about lists first.
#
#
################  read/study up to here for HW 1 #####################
#
# * LISTS:
#
#   - Recall: Vectors and matrices can carry only one basic type
#             of data at a time, numeric OR logical OR character.
          matrix(c(T,F),nrow=4,ncol=4)
          matrix(paste(LETTERS,letters)[1:25],nrow=5)
#     Lists are data structures without this restriction.
#     Lists are sequences of anything.  They can contain:
#       basic data types, vectors, matrices, arrays, lists (recursion),...
#     In particular, they can contain arbitrarily nested lists of lists.
#     Examples:
        list(1,"1",T,F,1E100,NA,-Inf,NULL,1:5,letters[2:5],list(1,2,"a"))
        # Balanced binary tree:
        list(list(list(1,2),list(3,4)),list(list(5,6),list(7,8)))  
#
        mylist <- list(vec=1:3, mat=cbind(1:2,3:4), flags=c(T,T,F,T),
                       lst=list(1:3,1:4))
        mylist
#     Component names are optional.  They can be glued on by force:
        x <- list(1,"a",T,NULL);  names(x) <- LETTERS[1:4];  x

#   - Access to LIST ITEMS is by index with "[[..]]", or by "$name"
#     if names exist:
        mylist[[2]]
        mylist$mat
        mylist[["mat"]]
#     This is also how lists are printed:
        list(1,T)
        list(a=1,b=T)
#
#   - Access to SUBLISTS is as if the list were a vector:
        mylist[2:3]                           # Sublist of length 2.
        mylist[2]                             # Sublist of length 1.
        mylist[c(1,4,1,4,2,3,2,3)]            # Out-of-order and expansion.
        mylist[c("lst","mat","vec","mat")]    # Named access.
        mylist["vec"]                         # Sublist of length 1.
        mylist[["vec"]]                       # Vector!!!
        mylist[c(T,F,T,F)]                    # Logical selection
#         
#   - Sublists are lists and require list item access to get at
#     the components:
        mylist[2:3][[1]]
        mylist[2:3]$mat
        mylist[2][[1]]       # Again, "mylist[2]" is a list of length 1...
        mylist[2]$mat        # Dito.
#
#   - Primary use of lists in statistics: 
#     collecting the results of a complex statistical or numerical
#     analysis, such as a regression or an eigendecomposition:
        eigen(matrix(1,3,3))
#     is a list with two named items: $values and $vectors,
#     the first being a vector containing the eigenvalues,
#     the second being a matrix with the eigenvectors in the columns.
#
#   
#
################
#
# * DATAFRAMES:
#
#   - In statistics, data are usually rectangular tables, cases by variables.
#     Problem: Variables are not all of the same type.
#              Some may be quantitative, hence stored as numeric data,.
#              Other variables may be categorical and stored either
#              with numeric or character/string codes for categories.
#              => Matrices cannot accommodate variables of both types...
#     Solution: Data frames.  They are similar to matrices,
#               but columns may differ in basic data types.
#               (The entries have to be basic, not complex.)
#
#     Main use of dataframes: data tables with mixed-type variables
#
#     Dataframes are printed like matrices, but they are internally
#     implemented as lists.
#
#   - The function "data.frame()" can bundle conforming vectors,
#     matrices, other dataframes into a single dataframe:
        myframe <- data.frame(somenumbers=1:3,
                              somecharacters=c("a","b","c"),
                              somelogicals=c(T,F,T))

        myframe
#
#   - Auxiliary functions for dataframes:
        is.data.frame(myframe)          # Checking type.
        as.data.frame(cbind(1:4,11:14)) # Coercing a matrix to a dataframe.
#
#   - Many matrix manipulations carry over to dataframes,
#     with one exception:
        dim(myframe)
        nrow(myframe)
        ncol(myframe)
        rownames(myframe)
        colnames(myframe)
        myframe[,3]      # Col 3 coerced to vector.
        myframe[2,]      # << Sub-dataframe with row 3, NOT coerced to vector!
        myframe[,2:3]    # Sub-dataframe consisting of cols 2 and 3.
        myframe[2:3,]    # Sub-dataframe consisting of rows 2 and 3.
#     Why is 'myframe[2,]' not coerced to a vector?
#     Because the items might be of differing types!
#     Hence the row 'myframe[2,]' is still a dataframe...
#
#     (Of course matrix multiplication does not work:)
        myframe %*% matrix(rep(1,10), nrow=5)

#   - 'read.table()' reads a tabular ASCII/txt file into a dataframe,
#     possibly with row and column names:
        read.table("laser.dat", header=T)
#       * Variables x, y, z, err, ..., pi, letters, LETTERS, ...
#       * Their values, that is, the data structures such as
#           values, vectors, matrices,...
#           to which the variables point.
#       * The black box has an engine, the intepreter,
#           that acts on the state of the box.
#       * The state of the black box is changed by executing
#           assignments, such as        x <- runif(100)
#       * On quitting q() you decide whether to save the newly
#           formed variables and their values.
#       * Learn about the state of the black box by executing:
#         . ls() to learn what variables are bound
#         . print(x) to learn what the value of the variable is
#       * There are two additional black boxes:
#         . par(), ... to learn about the values and data structures
#           of two sub-boxes:
#         - Plotting box:
#         . State: plotting parameters such as character size, margin width,...
#         . Reports the settings of the plotting parameters:
              par()
#         . State can be changed by executing, say,
              par(mfrow=c(2,2))
#         - Random number generator:
#         . State: the seed vector
              .Random.seed
#         . State changes when 1) a random number is generated, or
#                              2) .Random.seed is set by the user
#         - Printing and other parameters, e.g., line width, number of digits...
              options()
#
#
################################################################
#
# Changing the working directory:
# In the standard interface, one can change the working directory...
#    - temporarily: -> File -> Change Dir
#    - permanently: R-click on R icon -> Edit Properties -> Start in:
#
################################################################

