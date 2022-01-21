import TSim.*;
import java.util.concurrent.*;

class Shared {
  static int count = 0;
}

public class Lab1 {

  private Train t1;
  private Train t2;
  
  public Lab1(int speed1, int speed2) {
    Semaphore semtop = new Semaphore(0, true);
    Semaphore semmiddle = new Semaphore(1, true);
    Semaphore sembottom = new Semaphore(0, true);
    TSimInterface tsi = TSimInterface.getInstance();
    t1 = new Train(1, speed1, Direction.DOWN, semtop, semmiddle, sembottom);
    t2 = new Train(2, speed2, Direction.UP, semtop, semmiddle, sembottom);
    t1.start();
    t2.start();

    Thread test = new Test(semmiddle);
    test.start();
  }

  enum Direction {
    UP,
    DOWN
  }

  private class Test extends Thread {
    Semaphore sem;
    public Test(Semaphore sem) {
      this.sem = sem;
    }

    public void run() {
      try {
      while (true) {
        Thread.sleep(1000);
        System.out.println(sem.availablePermits());
      }
    } catch (Exception e) {

    }
    }
  }

  private class Train extends Thread {

    private Semaphore semtop;
    private Semaphore semmiddle;
    private Semaphore sembottom;
    private int trainId;
    private int speed;
    private TSimInterface tsi;
    private Direction dir;
    private boolean hasMiddle;

    public Train (int trainId, int speed, Direction dir, Semaphore semtop, Semaphore semmiddle, Semaphore sembottom) {
      super();
      this.trainId = trainId;
      this.dir = dir;
      this.semtop = semtop;
      this.semmiddle = semmiddle;
      this.sembottom = sembottom;
      this.speed = speed;
      tsi = TSimInterface.getInstance();
      try {
        tsi.setSpeed(trainId, speed);
      } catch (CommandException e) {
        e.printStackTrace();    // or only e.getMessage() for the error
        System.exit(1);
      }
      
    }

    private boolean isActive(int xPos, int yPos, SensorEvent e) {
      return (e.getXpos() == xPos && e.getYpos() == yPos 
              && e.getStatus() == SensorEvent.ACTIVE);
    }

    private boolean isInActive(int xPos, int yPos, SensorEvent e) {
      return (e.getXpos() == xPos && e.getYpos() == yPos 
              && e.getStatus() == SensorEvent.INACTIVE);
    }

    public void run () {
      try {
      while (true) {
          SensorEvent e = tsi.getSensor(trainId);
        
          // Switches Top section

          if (isActive(16, 7, e) && dir == Direction.DOWN) {
            tsi.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
          }

          if (isActive(18, 7, e) && dir == Direction.UP) {
            tsi.setSwitch(17, 7, TSimInterface.SWITCH_LEFT);
          }

          if (isActive(17, 8, e) && dir == Direction.DOWN) {
            tsi.setSwitch(17, 7, TSimInterface.SWITCH_LEFT);
          }

          // Switches RIGHT Middle section

          if (isActive(16, 9, e) && dir == Direction.DOWN) {
            if (semmiddle.tryAcquire()) {
              tsi.setSwitch(15, 9, TSimInterface.SWITCH_RIGHT);
              hasMiddle = true;
            } else {
              tsi.setSwitch(15, 9, TSimInterface.SWITCH_LEFT);
            }
          } else if (isActive(16, 9, e) && dir == Direction.UP && hasMiddle) {
            semmiddle.release();
            hasMiddle = false;
          } 

          if (isActive(14, 9, e) && dir == Direction.UP) {
            tsi.setSwitch(15, 9, TSimInterface.SWITCH_RIGHT);
          } else if (isActive(14, 9, e) && dir == Direction.DOWN) {
            semtop.release();
          }

          if (isActive(15, 10, e) && dir == Direction.DOWN) {
            semtop.release();
          } else if (isActive(15, 10, e) && dir == Direction.UP) {
            tsi.setSwitch(15, 9, TSimInterface.SWITCH_LEFT);
          }



          // Switches MIDDLE middle section

          if (isActive(9, 9, e) && dir == Direction.UP) {
            tsi.setSpeed(trainId, 0);
            semtop.acquire();
            tsi.setSpeed(trainId, speed);
          }

          if (isActive(9, 9, e) && dir == Direction.DOWN) {
            tsi.setSpeed(trainId, 0);
            sembottom.acquire();
            tsi.setSpeed(trainId, speed);
          }

          if (isActive(9, 10, e) && dir == Direction.DOWN) {
            semtop.release();
          } else if (isActive(9, 10, e) && dir == Direction.UP) {
            sembottom.release();
          }

          // Switches LEFT middle section

          if (isActive(5, 9, e) && dir == Direction.DOWN) {
            tsi.setSwitch(4, 9, TSimInterface.SWITCH_LEFT);
          }

          if (isActive(4, 10, e) && dir == Direction.DOWN) {
            tsi.setSwitch(4, 9, TSimInterface.SWITCH_RIGHT);
          } else if (isActive(4, 10, e) && dir == Direction.UP) {
            sembottom.release();
          }

          if (isActive(3, 9, e) && dir == Direction.UP) {
            if (semmiddle.tryAcquire()) {
              tsi.setSwitch(4, 9, TSimInterface.SWITCH_LEFT);
              hasMiddle = true;
            } else {
              tsi.setSwitch(4, 9, TSimInterface.SWITCH_RIGHT);
            }
          } else if (isActive(3, 9, e) && dir == Direction.DOWN && hasMiddle) {
            semmiddle.release();
            hasMiddle = false;
          }

          // Switches Bottom section

          if (isActive(2, 11, e) && dir == Direction.DOWN) {
            tsi.setSwitch(3, 11, TSimInterface.SWITCH_RIGHT);
          }


          // House Sensors

          if (isActive(15, 5, e) && dir == Direction.UP) {
            tsi.setSpeed(trainId, 0);
            Thread.sleep(1000 + 20 * Math.abs(speed));
            speed = -speed;
            tsi.setSpeed(trainId, speed);
            dir = Direction.DOWN;
          }

          if (isActive(15, 13, e) && dir == Direction.DOWN) {
            tsi.setSpeed(trainId, 0);
            Thread.sleep(1000 + 20 * Math.abs(speed));
            speed = -speed;
            tsi.setSpeed(trainId, speed);
            dir = Direction.UP;
          }

        }
        }catch (Exception e) {
          System.out.println(e.getMessage());  
        }
        
      
    }

  }

}


