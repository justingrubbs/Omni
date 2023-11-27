package TestFiles;
public class omni {
   public static void main(String[] args) {
      System.out.println(coolMath());
   }
   public static int add(int x, int y) {
      return x + y;
   }
   public static int mul(int x, int y) {
      return x * y;
   }
   public static int coolMath() {
      int a = 0;
      int x = 200;
      int y = 10;
      boolean z = true; 
      if (z) {
         a = add(x,y);
         a += 1;
      }
      return a;
   }
}
