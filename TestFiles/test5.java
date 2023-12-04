package TestFiles;
public class test5 {
   public static void main(String[] args) { 
      int x = add(3,2);
      System.out.println(x);
      String A = add("Hello"," world");
      System.out.println(A);
   }
   public static int add(int x, int y) {
      return (x+y);
   }
   public static String add(String x, String y) {
      return (x+y);
   }
}
