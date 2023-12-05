def main():
   x = True 
   x = fac(10)
   print(x) 

def fac(x):
   return(facHelp(x,x))

def facHelp(x,y):
   if x == 1:
      return y 
   else:
      return(facHelp(x-1,y*(x-1)))

main()
