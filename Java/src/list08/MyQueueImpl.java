package list08;

import java.util.ArrayList;

public class MyQueueImpl<E> implements MyQueue<E> {
    private final ArrayList<E> list;
    private int size;
    private final int DEFAULT_SIZE = 10;

    public MyQueueImpl() {
        list = new ArrayList<>();
        size = DEFAULT_SIZE;
    }

    public MyQueueImpl(int size) {
        list = new ArrayList<>();
        this.size = size;
    }

    @Override
    public void enqueue(E x) throws FullException {
        if(list.size() == size) {
            throw new FullException("Queue is full");
        }
        else {
            list.add(x);
        }
    }

    @Override
    public void dequeue() {
        list.remove(0);
    }

    @Override
    public E first() throws EmptyException {
        if(list.isEmpty()) {
            throw new EmptyException("Queue is empty");
        }
        else  {
            return list.get(0);
        }
    }

    @Override
    public boolean isEmpty() {
        return list.isEmpty();
    }

    @Override
    public boolean isFull() {
        return list.size() == size;
    }
}
