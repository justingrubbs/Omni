def main():
   x = add(6,3)
   y = add(2,4)
   z = mul(x,y)
   print(y)
   print(z)
   if x == y:
      print(x)
   if y == z: 
      print(y)
   else:
      print(z)
   # if z == y:
   #    print(z)
   # elif y == z:
   #    print(y)

def add(x,y):
   z = True
   return x+y

def mul(x,y):
   x = 3
   return x*y
