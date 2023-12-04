def main():
   run_game(82, 2)
   print("Thank you for playing! ")
   
def run_game(x,y):
   num = False 
   num = "String"
   num = [3]
   num = x
   done = False
   num_guess = 1
   guess = y
   while not done:
      guess += 1
      if (num == guess):
         done = True
         print("Correct! Number of attempts:")
         print(num_guess)
      else:
         num_guess = num_guess + 1
         if (guess < num):
            print("Too low")
         else:
            print("Too high")
   test()
   return

def test():
   a = 3
   a = True 
   a = 'c'
   return

main()
