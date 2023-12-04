def main():
   x = False 
   if x:
      x = ["Squirrel"]
   print(x)
   # If x is originally True, then print(x) will result in "Squirrel"; 
   # however, if it is False, then print(x) will result in False
   # This is important because the Java transcription will break these variables into two distinct variables
   # which of those two variables should Java print - x or A?

   if True:
      y = [3]
   print(y)

   if True:
      z = [True]
   else:
      z = [1.3, 4.2]
   print(z)
   
main()
