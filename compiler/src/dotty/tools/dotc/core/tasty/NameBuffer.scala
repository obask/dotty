package dotty.tools
package dotc
package core
package tasty

import collection.mutable
import Names.{Name, chrs}
import Decorators._, NameOps._
import TastyBuffer._
import scala.io.Codec
import TastyName._
import TastyFormat._

class NameBuffer extends TastyBuffer(10000) {
  import NameBuffer._

  private val nameRefs = new mutable.LinkedHashMap[TastyName, NameRef]

  def nameIndex(name: TastyName): NameRef = nameRefs.get(name) match {
    case Some(ref) =>
      ref
    case None =>
      val ref = NameRef(nameRefs.size)
      nameRefs(name) = ref
      ref
  }
  def nameIndex(name: Name): NameRef = {
    val tname =
      if (name.isShadowedName) Shadowed(nameIndex(name.revertShadowed))
      else Simple(name.toTermName)
    nameIndex(tname)
  }

  def nameIndex(str: String): NameRef = nameIndex(str.toTermName)

  def fullNameIndex(name: Name): NameRef = {
    val pos = name.lastIndexOf('.')
    if (pos > 0)
      nameIndex(Qualified(fullNameIndex(name.take(pos)), nameIndex(name.drop(pos + 1))))
    else
      nameIndex(name)
  }

  private def withLength(op: => Unit, lengthWidth: Int = 1): Unit = {
    val lengthAddr = currentAddr
    for (i <- 0 until lengthWidth) writeByte(0)
    op
    val length = currentAddr.index - lengthAddr.index - 1
    putNat(lengthAddr, length, lengthWidth)
  }

  def writeNameRef(ref: NameRef) = writeNat(ref.index)

  def pickleName(name: TastyName): Unit = name match {
    case Simple(name) =>
      val bytes =
        if (name.length == 0) new Array[Byte](0)
        else Codec.toUTF8(chrs, name.start, name.length)
      writeByte(UTF8)
      writeNat(bytes.length)
      writeBytes(bytes, bytes.length)
    case Qualified(qualified, selector) =>
      writeByte(QUALIFIED)
      withLength { writeNameRef(qualified); writeNameRef(selector) }
    case Signed(original, params, result) =>
      writeByte(SIGNED)
      withLength(
          { writeNameRef(original); writeNameRef(result); params.foreach(writeNameRef) },
          if ((params.length + 2) * maxIndexWidth <= maxNumInByte) 1 else 2)
    case Expanded(prefix, original) =>
      writeByte(EXPANDED)
      withLength { writeNameRef(prefix); writeNameRef(original) }
    case ModuleClass(module) =>
      writeByte(OBJECTCLASS)
      withLength { writeNameRef(module) }
    case SuperAccessor(accessed) =>
      writeByte(SUPERACCESSOR)
      withLength { writeNameRef(accessed) }
    case DefaultGetter(method, paramNumber) =>
      writeByte(DEFAULTGETTER)
      withLength { writeNameRef(method); writeNat(paramNumber) }
    case Shadowed(original) =>
      writeByte(SHADOWED)
      withLength { writeNameRef(original) }
  }

  override def assemble(): Unit = {
    var i = 0
    for ((name, ref) <- nameRefs) {
      assert(ref.index == i)
      i += 1
      pickleName(name)
    }
  }
}

object NameBuffer {
  private val maxIndexWidth = 3  // allows name indices up to 2^21.
  private val payloadBitsPerByte = 7 // determined by nat encoding in TastyBuffer
  private val maxNumInByte = (1 << payloadBitsPerByte) - 1
}
