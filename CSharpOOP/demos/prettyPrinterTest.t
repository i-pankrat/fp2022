  $ (cd ../../../../default && demos/prettyPrinterTest.exe)
  public class Program
  {
    public static void Main() 
    {
      int a = 5;
    }
  }
  
  public abstract class A
  {
    abstract int TestF();
  }
  
  public class B : A
  {
    public int Pub;
    
    public B(int temp) 
    {
      this.Pub = temp;
    }
    
    public B() 
    {
      this.Pub = 1;
    }
    
    public override int TestF() 
    {
      return this.Pub;
    }
  }
