package list08;

public class QueueTests {
    public static void main(String[] args) throws FullException, EmptyException {
        MyQueueImpl<Integer> queue = new MyQueueImpl<>(3);
        try {
            queue.first();
            System.out.println("Exception was not thrown");
        }
        catch (EmptyException e) {
            System.out.println("Exception was thrown with message: " + e.getMessage());
        }

        queue.enqueue(1);
        queue.enqueue(2);
        queue.enqueue(3);

        try {
            queue.enqueue(4);
            System.out.println("Exception was not thrown");
        }
        catch (FullException e) {
            System.out.println("Exception was thrown with message: " + e.getMessage());
        }

        boolean assertionsResult = !queue.isEmpty() && queue.isFull() && queue.first() == 1;

        queue.dequeue();
        if(queue.isFull()) {
            assertionsResult = false;
        }

        System.out.println("Assertions result: " + assertionsResult);
    }
}
