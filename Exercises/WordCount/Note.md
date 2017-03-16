# Word count
A simple program that count the occurrences of words in a file.

Summary of steps
* Read contents from a file
* Split it into a list of strings of valid characters (in this case, only a-z and A-Z)
* Go through each string in the list of string one by one
* Create a Map from String to Int with Int being the number of occurrences of the same string in the list
  If the string is already in the Map, increment count by 1 if not create a new entry with count 1
* Write results of the map ordered by the number of occurrences descending (for file contents comparison)
  (Output file will have the same name as input file with .out extension)

# Test case 1
Why functional programming matters
# Test case 2
Minecraft Wikipedia
# Test case 3
Distributed computing Wikipedia
# Test case 4
Dev notes
