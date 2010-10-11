package funicular.test

import funicular._

object Test8 {
  class E1 extends Exception
  class E2 extends Exception
  class E3 extends Exception

  def main(args : Array[String]) : Unit = {
    try {
      finish {
        async {
          throw new E1
          ()
        }

        async {
          throw new E2
          ()
        }

        async {
          throw new E3
          ()
        }
      }
    }
    catch {
      case MultipleExceptions(e1::e2::e3::Nil) => {
        println(e1)
        println(e2)
        println(e3)
      }
    }
  }
}
