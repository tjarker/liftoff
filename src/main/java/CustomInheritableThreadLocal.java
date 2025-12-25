package liftoff.coroutine;

public abstract class CustomInheritableThreadLocal<T> extends InheritableThreadLocal<T> {
  
    protected abstract T inheritance(T parentValue);

    @Override
    protected T childValue(T parentValue) {
        return inheritance(parentValue);
    }
  
}
