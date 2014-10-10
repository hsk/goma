package calc

object main extends App {

  case class EInt(x:Int)
  case class EAdd(x:E,y:E)
  case class EMul(x:E,y:E)

  trait E { def eval():Int }
  implicit class E_EInt(e:EInt) extends E {def eval():Int = e.x }
  implicit class E_EAdd(e:EAdd) extends E {def eval():Int = e.x.eval() + e.y.eval() }
  implicit class E_EMul(e:EMul) extends E {def eval():Int = e.x.eval() * e.y.eval() }

  val i = EInt(1)
  printf("eval 1 = %d\n", i.eval())

  val add = EAdd(EInt(1),EInt(2))
  printf("eval 1 + 2 = %d\n", add.eval())

  val mul = EMul(EAdd(EInt(1),EInt(2)),EInt(111))
  printf("eval (1 + 2) * 111 = %d\n", mul.eval())
  

}