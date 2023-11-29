def main():
   x = True 
   while x:
      x = 0
      # Loop immediately ceases because x is no longer True 
         # This is the case here, but not necessarily always
         # Python "truthy" values include almost every type I have implemented-ish?
      # Could set `x` to False after generating a new variable but doesn't fix cases like `while x == 0:`
   print(x)

main()
