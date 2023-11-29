def main():
   x = True 
   if x:
      x = "Squirrel"
   print(x) 
   # If x is originally True, then print(x) will result in "Squirrel"; 
   # however, if it is False, then print(x) will result in False
   # This is important because the Java transcription will break these variables into two distinct variables
   # which of those two variables should Java print - x or A?

main()
