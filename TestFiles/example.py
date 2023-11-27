def main():
   x = True 
   if x:
      x = 22
      x += 1
   print(x)
   print(add10(x))

def add10(x):
   return x+10


main()
