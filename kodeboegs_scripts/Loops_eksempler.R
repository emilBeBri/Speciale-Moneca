####################
#                  #
#    Exercise 1    #
#                  #
####################

# The repeat{} loop processes a block of code until the condition specified by the break statement, (that is mandatory within the repeat{} loop), is met.

# The structure of a repeat{} loop is:
# repeat {
# commands
# if(condition) {
# break
# }
# }
# For the first exercise, write a repeat{} loop that prints all the even numbers from 2 – 10, via incrementing the variable, “i <- 0“.


i <- 0

repeat{
  i <- i + 2
  print(i)
  if(i == 10) {
    break
    }
}
## [1] 2
## [1] 4
## [1] 6
## [1] 8
## [1] 10


####################
#                  #
#    Exercise 2    #
#                  #
####################

# Using the following variables:

# msg <- c("Hello")
# i <- 1

# Write a repeat{} loop that breaks off the incrementation of, “i“, after 5 loops, and prints “msg” at every increment.



msg <- c("Hello")
i <- 1

repeat {
  i <- i + 1
  print(msg)
  if(i > 5) {
    break
  }
}
## [1] "Hello"
## [1] "Hello"
## [1] "Hello"
## [1] "Hello"
## [1] "Hello"

####################
#                  #
#    Exercise 3    #
#                  #
####################

# while() loop will repeat a group of commands until the condition ceases to apply. The structure of a while() loop is:

# while(condition) {
# commands
# }

# With, i <- 1, write a while() loop that prints the odd numbers from 1 through 7.

i <- 1

while(i < 8) {
  print(i)
  i <- i + 2
  }
## [1] 1
## [1] 3
## [1] 5
## [1] 7

####################
#                  #
#    Exercise 4    #
#                  #
####################

# Using the following variables:

# msg <- c("Hello")
# i <- 1

# Write a while() loop that increments the variable, “i“, 6 times, and prints “msg” at every iteration.


msg <- c("Hello")
i <- 1

while (i < 7) {
  print(msg)
  i = i + 1
}
## [1] "Hello"
## [1] "Hello"
## [1] "Hello"
## [1] "Hello"
## [1] "Hello"
## [1] "Hello"


####################
#                  #
#    Exercise 5    #
#                  #
####################

# The for() loop repeats commands until the specified length of the condition is met. The structure of a for() loop is:
# for(condition) { commands }

# For example:
# for(i in 1:4) {
# print("variable"[i])
# }

# for(i in seq("variable")) {
# print(i)
# }

# for(i in seq_along("variable")) {
# print("variable"[i])
# }

# for(letter in "variable") {
# print(letter)
# }

# For this exercise, write a for() loop that prints the first four numbers of this sequence: x <- c(7, 4, 3, 8, 9, 25)

x <- c(7, 4, 3, 8, 9, 25)

for(i in 1:4) {
  print(x[i])
  }
## [1] 7
## [1] 4
## [1] 3
## [1] 8

####################
#                  #
#    Exercise 6    #
#                  #
####################

# For the next exercise, write a for() loop that prints all the letters in y <- c("q", "w", "e", "r", "z", "c").


y <- c("q", "w", "e", "r", "z", "c")

for(letter in y) {
  print(letter)
  }
## [1] "q"
## [1] "w"
## [1] "e"
## [1] "r"
## [1] "z"
## [1] "c"

####################
#                  #
#    Exercise 7    #
#                  #
####################

# The break statement is used within loops to exit from the loop. If the break statement is within a nested loop, the inner loop is exited, and the outer loop is resumed.

# Using i <- 1, write a while() loop that prints the variable, “i“, (that is incremented from 1 – 5), and uses break to exit the loop if “i” equals 3.

i <- 1

while(i < 5) {
  i <- i + 1
  if (i == 3) break
  print(i)
  }
## [1] 2
####################
#                  #
#    Exercise 8    #
#                  #
####################

# Write a nested loop, where the outer for() loop increments “a” 3 times, and the inner for() loop increments “b” 3 times. The break statement exits the inner for() loop after 2 incrementations. The nested loop prints the values of variables, “a” and “b“.

for (a in 1:3)
  {
    for (b in 1:3)
    {
      print(c(a, b))
      if (b == 2) break
    }
  }
## [1] 1 1
## [1] 1 2
## [1] 2 1
## [1] 2 2
## [1] 3 1
## [1] 3 2

####################
#                  #
#    Exercise 9    #
#                  #
####################

# The next statement is used within loops in order to skip the current evaluation, and instead proceed to the next evaluation.

# Therefore, write a while() loop that prints the variable, “i“, that is incremented from 2 – 5, and uses the next statement, to skip the printing of the number 3.

i <- 1

while(i < 5) {
  i <- i + 1
  if (i == 3) {
    next
  }
  print(i)
  }
## [1] 2
## [1] 4
## [1] 5
####################
#                  #
#    Exercise 10   #
#                  #
####################

# Finally, write a for() loop that uses next to print all values except “3” in the following variable: i <- 1:5

i <- 1:5

for (val in i) {
  if (val == 3){
    next
  }
  print(val)
}
## [1] 1
## [1] 2
## [1] 4
## [1] 5