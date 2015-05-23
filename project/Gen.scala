object Gen {

  def genTrove() = {
    val ints = Seq("Int", "Long")
    val floats = Seq("Float", "Double")
    val primitives = for (k <- ints; v <- ints ++ floats) yield (k, v)
    val op = for (k <- ints) yield (k, "Object")
    val po = for (v <- ints ++ floats) yield ("Object", v)
    val maps = (primitives ++ op ++ po) map { case (t1, t2) => mkMapClass(t1, t2) }

    val sets = (for (t <- ints) yield mkSetClass(t))

    (maps ++ sets).mkString(" ")
  }




  // ========== Map ===========


  def mkMapClass(_K: String, _V: String) = {
    val V = if (_V == "Object") "T" else _V
    val K = if (_K == "Object") "T" else _K
    val T = if (_K == "Object" || _V == "Object") "[T]" else ""

    val AtroxType = s"${_K}${_V}Map${T}"
    val TroveType = s"gnu.trove.map.hash.T${_K}${_V}HashMap${T}"

    s"""
package atrox.trove {

  import gnu.trove.map.hash.T${_K}${_V}HashMap
  import gnu.trove.procedure.T${_K}${_V}Procedure
  import gnu.trove.procedure.T${_V}Procedure

  final class $AtroxType(initialSize: Int = 16, loadFactor: Float = 0.5f, _map: $TroveType = null) extends collection.mutable.Map[${K}, ${V}] {

    private[this] val map: T${_K}${_V}HashMap${T} =
      (if (_map != null) _map else new T${_K}${_V}HashMap${T}(initialSize, loadFactor))

    def += (kv: (${K}, ${V})): this.type = {
      val (k, v) = kv
      update(k, v)
      this
    }

    def -= (k: ${K}): this.type = {
      map.remove(k)
      this
    }

    def get(k: ${K}): Option[${V}] =
      if (map.containsKey(k)) Some(map.get(k)) else None

    def iterator: collection.Iterator[(${K}, ${V})] = {
      val iter = map.iterator
      new collection.Iterator[(${K}, ${V})] {
        def hasNext = iter.hasNext
        def next(): (${K}, ${V}) = {
          iter.advance()
          (iter.key(), iter.value())
        }
      }
    }

    // =====

    override def apply(k: ${K}): ${V} =
      if (map.containsKey(k)) map.get(k) else throw new java.util.NoSuchElementException("key not found: "+k)

    def getOrDefaultValue(k: $K): $V =
      map.get(k)

    override def clear(): Unit =
      map.clear()

    override def contains(k: ${K}): Boolean =
      map.contains(k)

    def containsValue(v: $V): Boolean =
      map.containsValue(v)

    //override def count(f: ((${K}, ${V}) => Boolean)): Int = ???

    override def empty: $AtroxType =
      new $AtroxType()

    override def hashCode = map.hashCode

    override def update(k: ${K}, v: ${V}): Unit =
      map.put(k, v)

    override def getOrElseUpdate(k: ${K}, v: => ${V}): ${V} = {
      val res = map.get(k)
      if (res == ${if (_V == "Object") "null" else "map.getNoEntryValue"} && !map.containsKey(k)) {
        val value = v  
        map.put(k, value)
        value
      } else {
        res
      }
    }

    def putIfAbsent(k: ${K}, v: ${V}): Unit =
      map.putIfAbsent(k, v)

    override def getOrElse[VV >: ${V}](k: ${K}, v: => VV): VV =
      if (map.containsKey(k)) map.get(k) else v

    override def isEmpty = map.isEmpty

    override def nonEmpty = !map.isEmpty

    override def size = map.size


    override def valuesIterator: collection.Iterator[${V}] = {
      val iter = map.iterator
      new collection.Iterator[${V}] {
        def hasNext = iter.hasNext
        def next(): ${V} = {
          iter.advance()
          iter.value()
        }
      }
    }

    def valuesArray: Array[${V}] = ${
      if (_V == "Object") s"map.values.asInstanceOf[Array[T]]"
      else                s"map.values"
    }


    override def keySet = ${
      if (_K == "Object") s"scala.collection.mutable.Set${T}(map.keys.asInstanceOf[Array${T}]: _*)"
      else                s"new ${K}Set(_set = map.keySet.asInstanceOf[gnu.trove.set.T${K}Set])"
    }

    def keysArray: Array[${K}] = ${
      if (_K == "Object") s"map.keys.asInstanceOf[Array${T}]"
      else                s"map.keys"
    }


    override def filter(p: ((${K}, ${V})) => Boolean): $AtroxType =
      filter { (k, v) => p((k, v)) }

    def filter(p: (${K}, ${V}) => Boolean): $AtroxType = {
      val filteredTroveMap = new $TroveType(size)

      val iter = map.iterator
      while (iter.hasNext) {
        iter.advance()
        val k: $K = iter.key()
        val v: $V = iter.value()
        if (p(k, v)) {
          filteredTroveMap.put(k, v)
        }
      }

      new $AtroxType(_map = filteredTroveMap)
    }

    override def filterNot(p: ((${K}, ${V})) => Boolean): $AtroxType =
      filter { (k, v) => !p((k, v)) }

    def filterNot(p: (${K}, ${V}) => Boolean): $AtroxType =
      filter { (k, v) => !p(k, v) }

    override def filterKeys(p: $K => Boolean): $AtroxType =
      filter { (k, v) => p(k) }


    override def retain(p: (${K}, ${V}) => Boolean): this.type = {
      map.retainEntries(new gnu.trove.procedure.T${_K}${_V}Procedure${T} {
        def execute(x: ${K}, y: ${V}): Boolean = p(x, y)
      })
      this
    }


    ${ if (_V != "Object") s"""
    
    def increment(k: ${K}): this.type = {
      map.increment(k)
      this
    }

    def adjustOrPutValue(k: ${K}, adjustAmount: ${V}, putAmount: ${V}): this.type = {
      map.adjustOrPutValue(k, adjustAmount, putAmount)
      this
    }

    def adjustValue(k: ${K}, adjustAmount: ${V}): this.type = {
      map.adjustValue(k, adjustAmount)
      this
    }

    override def toString = map.toString

    """ }
    
  }
}
"""
  }



  // ========== Set ===========


  def mkSetClass(E: String) = {
    s"""
package atrox.trove {

  import gnu.trove.set.T${E}Set
  import gnu.trove.set.hash.T${E}HashSet

  final class ${E}Set(initialSize: Int = 16, loadFactor: Float = 0.5f, _set: gnu.trove.set.T${E}Set = null) extends collection.mutable.Set[${E}] {

    private val set: T${E}Set =
      (if (_set != null) _set else new T${E}HashSet(initialSize, loadFactor))


    // ==== abstract methods ====

    def += (e: ${E}): this.type = {
      set.add(e)
      this
    }

    def -= (e: ${E}): this.type = {
      set.remove(e)
      this
    }

    def contains (e: ${E}): Boolean =
      set.contains(e)

    def iterator: collection.Iterator[${E}] = {
      val iter = set.iterator
      new collection.Iterator[${E}] {
        def hasNext = iter.hasNext
        def next(): ${E} = iter.next()
      }
    }

    // =====

    override def ++= (xs: TraversableOnce[${E}]): this.type = {
      for (x <- xs) set.add(x)
      this
    }

    def ++= (xs: Array[${E}]): this.type = {
      set.addAll(xs)
      this
    }

    def ++= (xs: ${E}Set): this.type = {
      set.addAll(xs.set)
      this
    }

    override def --= (xs: TraversableOnce[${E}]): this.type = {
      for (x <- xs) set.remove(x)
      this
    }

    def --= (xs: ${E}Set): this.type = {
      set.removeAll(xs.set)
      this
    }

    def --= (xs: Array[${E}]): this.type = {
      set.removeAll(xs)
      this
    }

    def retainAll(xs: Seq[$E]): this.type = {
      import scala.collection.JavaConverters._
      set.retainAll(xs.asJava)
      this
    }

    def retainAll(xs: Array[$E]): this.type = {
      set.retainAll(xs)
      this
    }

    def retainAll(xs: ${E}Set): this.type = {
      set.retainAll(xs.set)
      this
    }

    override def apply(e: ${E}): Boolean = set.contains(e)

    def containsAll(xs: Array[$E]): Boolean = set.containsAll(xs)

    def containsAll(xs: ${E}Set): Boolean = set.containsAll(xs.set)

    override def clear(): Unit = set.clear()

    override def hashCode = set.hashCode

    override def update(e: ${E}, v: Boolean): Unit =
      if (v) set.add(e) else set.remove(e)

    override def foreach[U](f: ${E} => U): Unit = {
      val iter = set.iterator
      while (iter.hasNext)
        f(iter.next())
    }

    def toArray: Array[${E}] = set.toArray

    override def toVector: collection.immutable.Vector[${E}] = {
      val b = new collection.immutable.VectorBuilder[${E}]
      b.sizeHint(set.size)
      val iter = set.iterator
      while (iter.hasNext) {
        b += iter.next()
      }
      b.result
    }

    def max: ${E} = {
      var m: ${E} = ${E}.MinValue
      val iter = set.iterator
      while (iter.hasNext) {
        val v = iter.next()
        if (v > m) m = v
      }
      m
    }

    def min: ${E} = {
      var m: ${E} = ${E}.MaxValue
      val iter = set.iterator
      while (iter.hasNext) {
        val v = iter.next()
        if (v < m) m = v
      }
      m
    }

    def sum: ${E} = {
      var m: ${E} = 0
      val iter = set.iterator
      while (iter.hasNext) {
        m += iter.next()
      }
      m
    }

    def product: ${E} = {
      var m: ${E} = 1
      val iter = set.iterator
      while (iter.hasNext) {
        m *= iter.next()
      }
      m
    }

    override def isEmpty = set.isEmpty

    override def nonEmpty = !set.isEmpty

    override def size = set.size
  }

}
"""
  }

}
