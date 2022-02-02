import TSim.*;
import java.util.concurrent.*;

class Shared {
  static int count = 0;
}

public class Lab1 {

  private Train t1;
  private Train t2;
  
  public Lab1(int speed1, int speed2) {
    Semaphore semtoptop = new Semaphore(0, true);
    Semaphore semtopbottom = new Semaphore(1, true);
    Semaphore semmiddletop = new Semaphore(1, true);
    Semaphore semmiddlebottom = new Semaphore(1, true);
    Semaphore sembottomtop = new Semaphore(0, true);
    Semaphore sembottombottom = new Semaphore(1, true);
    Semaphore semleft = new Semaphore(1, true);
    Semaphore semright = new Semaphore(1, true);
    Semaphore semint = new Semaphore(1, true);
    t1 = new Train(1, speed1, Direction.DOWN, semtoptop, semtopbottom, semmiddletop, semmiddlebottom, sembottomtop, sembottombottom, semleft, semright, semint);
    t2 = new Train(2, speed2, Direction.UP, semtoptop, semtopbottom, semmiddletop, semmiddlebottom, sembottomtop, sembottombottom, semleft, semright, semint);
    t1.start();
    t2.start();
  }

  public enum Direction {
    UP,
    DOWN;

    public static Direction flip (Direction dir) {
      if (dir != Direction.UP) {
        return Direction.UP;
      }
      return Direction.DOWN;
    }

  }

  private class Test extends Thread {
    private Semaphore sem;

    public Test(Semaphore sem) {
      this.sem = sem;
    }

    public void run() {
      try {
        while(true) {
        sleep(1000);
        System.out.println(sem.availablePermits());
        }
      } catch (Exception e) {
        //TODO: handle exception
      }
     
    }
  }

  private class Train extends Thread {

    private Semaphore semtoptop;
    private Semaphore semtopbottom;
    private Semaphore semmiddletop;
    private Semaphore semmiddlebottom;
    private Semaphore sembottomtop;
    private Semaphore sembottombottom;
    private Semaphore semright;
    private Semaphore semleft;
    private Semaphore semint;
    private int trainId;
    private int speed;
    private TSimInterface tsi;
    private Direction dir;
    private boolean hasMiddle;

    public Train (int trainId, int speed, Direction dir, Semaphore semtoptop, Semaphore semtopbottom, Semaphore semmiddletop, Semaphore semmiddlebottom, Semaphore sembottomtop, Semaphore sembottombottom, Semaphore semleft, Semaphore semright, Semaphore semint) {
      super();
      this.trainId = trainId;
      this.dir = dir;
      this.semtoptop = semtoptop;
      this.semtopbottom = semtopbottom;
      this.semmiddletop = semmiddletop;
      this.semmiddlebottom = semmiddlebottom;
      this.sembottomtop = sembottomtop;
      this.sembottombottom = sembottombottom;
      this.semleft = semleft;
      this.semright = semright;
      this.semint = semint;
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

    public void run () {
      try {
      while (true) {
          SensorEvent e = tsi.getSensor(trainId);

          // TOPTOP

          if (isActive(14, 7, e) && dir == Direction.DOWN) {
            tsi.setSpeed(trainId, 0);
            semright.acquire();
            tsi.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);

            tsi.setSpeed(trainId, speed);
            semtoptop.release();
          } else if (isActive(14, 7, e) && dir == Direction.UP) {
            semright.release();
          }

          // Intersection

          if (((isActive(6, 6, e) || isActive(9, 5, e)) && dir == Direction.DOWN) || 
              ((isActive(10, 8, e) || isActive(11, 7, e)) && dir == Direction.UP )) {
                tsi.setSpeed(trainId, 0);
                semint.acquire();
                tsi.setSpeed(trainId, speed);
          } else if (((isActive(6, 6, e) || isActive(9, 5, e)) && dir == Direction.UP) || 
                    ((isActive(10, 8, e) || isActive(11, 7, e)) && dir == Direction.DOWN )) {
                        semint.release();
                      }

          // TOPBOTTOM

          if (isActive(15, 8, e) && dir == Direction.DOWN) {
            tsi.setSpeed(trainId, 0);
            semright.acquire();
            tsi.setSpeed(trainId, speed);
            tsi.setSwitch(17, 7, TSimInterface.SWITCH_LEFT);
            semtopbottom.release();
          } else if (isActive(15, 8, e) && dir == Direction.UP) {
            semright.release();
          }

          // RIGHT

          if (isActive(18, 7, e) && dir == Direction.UP) {
            if (semtoptop.tryAcquire()) {
              tsi.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
            } else {
              semtopbottom.acquire();
              tsi.setSwitch(17, 7, TSimInterface.SWITCH_LEFT);
            }
          }

          if (isActive(16, 9, e) && dir == Direction.DOWN) {
            if (semmiddletop.tryAcquire()) {
              tsi.setSwitch(15, 9, TSimInterface.SWITCH_RIGHT);

            } else {
              semmiddlebottom.acquire();
              tsi.setSwitch(15, 9, TSimInterface.SWITCH_LEFT);
            }
          }

          // BOTTOMTOP

          if (isActive(6, 11, e) && dir == Direction.UP) {
            tsi.setSpeed(trainId, 0);
            semleft.acquire();
            tsi.setSpeed(trainId, speed);
            sembottomtop.release();
            tsi.setSwitch(3, 11, TSimInterface.SWITCH_LEFT);
          } else if (isActive(6, 11, e) && dir == Direction.DOWN) {
            semleft.release();
          }

          // BOTTOMBOTTOM

          if (isActive(4, 13, e) && dir == Direction.UP) {
            tsi.setSpeed(trainId, 0);
            semleft.acquire();
            tsi.setSpeed(trainId, speed);
            sembottombottom.release();
            tsi.setSwitch(3, 11, TSimInterface.SWITCH_RIGHT);
          } else if (isActive(4, 13, e) && dir == Direction.DOWN) {
            semleft.release();
          }

          // LEFT 

          if (isActive(2, 11, e) && dir == Direction.DOWN) {
            if (sembottomtop.tryAcquire()) {
              tsi.setSwitch(3, 11, TSimInterface.SWITCH_LEFT);
            } else {
              sembottombottom.acquire();
              tsi.setSwitch(3, 11, TSimInterface.SWITCH_RIGHT);
            }
          }

          if (isActive(3, 9, e) && dir == Direction.UP) {
            if (semmiddletop.tryAcquire()) {
              tsi.setSwitch(4, 9, TSimInterface.SWITCH_LEFT);
            } else {
              semmiddlebottom.acquire();
              tsi.setSwitch(4, 9, TSimInterface.SWITCH_RIGHT);
            }
          }

          // MIDDLE TOP

          if (isActive(9, 9, e) && dir == Direction.UP) {
            tsi.setSpeed(trainId, 0);
            semright.acquire();
            tsi.setSpeed(trainId, speed);
          } else if (isActive(9, 9, e) && dir == Direction.DOWN) {
            tsi.setSpeed(trainId, 0);
            semleft.acquire();
            tsi.setSpeed(trainId, speed);
          }

          if (isActive(6, 9, e) && dir == Direction.DOWN) {
            tsi.setSwitch(4, 9, TSimInterface.SWITCH_LEFT);
            semmiddletop.release();
          } else if (isActive(6, 9, e) && dir == Direction.UP) {
            semleft.release();
          }

          if (isActive(13, 9, e) && dir == Direction.UP) {
            tsi.setSwitch(15, 9, TSimInterface.SWITCH_RIGHT);
            semmiddletop.release();
          } else if (isActive(13, 9, e) &&  dir == Direction.DOWN) {
            semright.release();
          }

          // MIDDLE BOTTOM

          if (isActive(9, 10, e) && dir == Direction.UP) {
            tsi.setSpeed(trainId, 0);
            semright.acquire();
            tsi.setSpeed(trainId, speed);
          } else if (isActive(9, 10, e) && dir == Direction.DOWN) {
            tsi.setSpeed(trainId, 0);
            semleft.acquire();
            tsi.setSpeed(trainId, speed);
          }

          if (isActive(13, 10, e) && dir == Direction.UP) {
            tsi.setSwitch(15, 9, TSimInterface.SWITCH_LEFT);
            semmiddlebottom.release();
          } else if (isActive(13, 10, e) && dir == Direction.DOWN) {
            semright.release();
          }
          
          if (isActive(6, 10, e) && dir == Direction.DOWN) {
            tsi.setSwitch(4, 9, TSimInterface.SWITCH_RIGHT);
            semmiddlebottom.release();
          } else if (isActive(6, 10, e) && dir == Direction.UP) {
            semleft.release();
          }
          // HOUSES

          if (isActive(15, 13, e) || isActive(15, 11, e) || isActive(15, 3, e) || isActive(15, 5, e)) {
            tsi.setSpeed(trainId, 0);
            sleep(1000 + (20 * Math.abs(speed)));
            speed = -speed;
            tsi.setSpeed(trainId, speed);
            dir = Direction.flip(dir);
          }
          
        }
        }catch (Exception e) {
          System.out.println(e.getMessage());  
        }
        
      
    }

  }

}


