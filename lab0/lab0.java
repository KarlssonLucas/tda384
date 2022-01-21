public class lab0 {
    public static void main (String[] args) {
        HelloWorld hw = new HelloWorld();         

        Thread t = new Thread(hw);
        Thread u = new Thread(hw);
        t.start();
        u.start();
    }
}

public class HelloWorld implements Runnable {
    @Override
    public void run () {
        System.out.println("Hello world, thread id: " + Thread.currentThread().getId());
    }
}



