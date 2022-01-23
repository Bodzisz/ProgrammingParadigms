package list11;

import java.util.concurrent.Semaphore;

// a)
class IntCell {
    private int n = 0;
    private boolean readyToSet = false;

    public synchronized int getN() {
        while (readyToSet) {
            try {
                wait();
            }
            catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
        readyToSet = true;
        notify();
        return n;
    }

    public synchronized void setN(int n) {
        while(!readyToSet) {
            try {
                wait();
            }
            catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
        this.n = n;
        readyToSet = false;
        notify();
    }
}

class Count extends Thread {
    private static final IntCell n = new IntCell();

    @Override
    public void run() {
        int temp;
        for (int i = 0; i < 200000; i++) {
            temp = n.getN();
            n.setN(temp + 1);
        }
    }

    public int getN() {
        return n.getN();
    }
}

// b)
//class IntCell {
//    private int n = 0;
//
//    public int getN() {
//        return n;
//    }
//
//    public void setN(int n) {
//        this.n = n;
//    }
//}
//
//class Count extends Thread {
//    private static final IntCell n = new IntCell();
//    private static final Semaphore semaphore = new Semaphore(1);
//
//    @Override
//    public void run() {
//        int temp;
//        for (int i = 0; i < 200000; i++) {
//            try {
//                semaphore.acquire();
//                temp = n.getN();
//                n.setN(temp + 1);
//            } catch (InterruptedException e) {
//                e.printStackTrace();
//            }
//            finally {
//                semaphore.release();
//            }
//        }
//    }
//
//    public int getN() {
//        return n.getN();
//    }
//}


public class Zad2 {
    public static void main(String[] args) {
        Count p = new Count();
        Count q = new Count();
        p.start();
        q.start();
        try {
            p.join();
            q.join();
        }
        catch (InterruptedException e) {
            e.printStackTrace();
        }
        System.out.println("The value of n is " + p.getN());
    }
}
