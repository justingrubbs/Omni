def main():
   x = add(3,2)
   y = mul(x,10)
   z = True 
   while z:
      x += 1
      z = False 
   x = "Squirrel"

def add(x,y):
   return x+y

def mul(x,y):
   return x * y 


main()
