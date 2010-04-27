package jp.gr.java_conf.mizu.pegex

/**
  * This class represents stacks of ints.
  * This class is used only for performance.
  * @author Kota Mizushima */
class IntStack(initialCapacity: Int) {
  private[this] var array = new Array[Int](initialCapacity)
  private[this] var _size = 0
  private def ensureCapacity(newCapacity: Int) {
    if(newCapacity > array.length) {
      val newArray = new Array[Int]((newCapacity * 1.5).asInstanceOf[Int])
      System.arraycopy(array, 0, newArray, 0, array.length)
      array = newArray
    }
  }
  
  def this() { this(10) }
  def clear() {
    array = new Array[Int](initialCapacity)
    _size = 0
  }
  def push(e: Int) {
    ensureCapacity(_size + 1)
    array(_size) = e
    _size += 1
  }
  def peek: Int = array(_size - 1)
  def pop: Int = { _size -= 1; array(_size) }
  def size: Int = _size
  def isEmpty: Boolean = _size == 0
}
