package funicular.constraints

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

import scala.collection.mutable.HashMap

class Constraints(val global: Global) extends Plugin {
  import global._
  
  addAnnotationChecker(ConstraintChecker)

  val name = "constraints"
  val description = "checks constrained types"
  val components = List[PluginComponent]()
  
  /*
  private object Component extends PluginComponent {
    val global: Constraints.this.global.type = Constraints.this.global
    val phaseName = Constraints.this.name

    // remove for 2.8.x
    //val runsAfter = "refchecks"

    // add for nsc 2.8.x
    val runsAfter = List[String]("refchecks", "typer")
    override val runsBefore = List[String]("tailcalls")

    def newPhase(_prev: Phase) = new ConstraintsPhase(_prev)
    
    class ConstraintsPhase(prev: Phase) extends StdPhase(prev) {
      override def name = Constraints.this.name
      
      def apply(unit: CompilationUnit) {
        newTraverser(unit).traverse(unit.body)
      }
    }
      
    def newTraverser(unit: CompilationUnit) = new ForeachTreeTraverser(check(unit))
    
    def check(unit: CompilationUnit)(tree: Tree): Unit = tree match {
      case Literal(x) => x
//      case Apply(Select(rcvr, nme.DIV), List(Literal(Constant(0)))) =>
//      if (rcvr.tpe <:< definitions.IntClass.tpe) {
//          unit.error(tree.pos, "definite division by zero")
//      }
      case _ => ()
    }
    
  }
  */

    object ConstraintChecker extends AnnotationChecker {
        import stp.STP.{VC, Expr, VALID, Type => STPType}
        type Env = HashMap[Path, Expr]

        val PTR_SIZE = 16

        val vc = new VC

        def addTypeConstraints(vc: VC, env: Env, self: Expr, tpe: Type): Unit = tpe match {
            case ConstantType(value) => {
                val eqn = vc.eqExpr(self, constraintBody(vc, env, Literal(value)))
                println("assert constant " + eqn)
                vc.assertFormula(eqn)
            }
            case NotNullType(tpe2) => {
                addTypeConstraints(vc, env, self, tpe2)
                val eqn = vc.notExpr(vc.eqExpr(self, constraintBody(vc, env, Literal(Constant(null)))))
                println("assert not null " + eqn)
                vc.assertFormula(eqn)
            }
            case SingleType(pre, sym) => {
                addTypeConstraints(vc, env, self, tpe.underlying)
                /*
                vc.assertFormula(vc.eqExpr(self, constraintBody(vc, env, Literal(value))))
                */
            }
            case s @ AnnotatedType(_, t, _) => {
                addTypeConstraints(vc, env, self, t)

/*
                val tree = whereAnnotations(s)
                val env2 = new Env
                constraintVars(vc, env2, tree)
                val x = constraintTerm(vc, env2, tree)
                val y = self(vc, env2, tree, s)
                */
            }
            case _ => ()
        }

        def assertFieldEqns(vc: VC, env: Env): Unit = {
            for (p <- env.keys) {
                for (q <- env.keys) {
                    (p,q) match {
                      case (f :: x, g :: y) if f == g =>
                        val xx = env.getOrElse(x, null)
                        val yy = env.getOrElse(y, null)
                        val pp = env.getOrElse(p, null)
                        val qq = env.getOrElse(q, null)
                        if (xx != null && yy != null)
                            vc.assertFormula(vc.impliesExpr(vc.eqExpr(xx, yy), vc.eqExpr(pp, qq)))
                      case _ => ()
                    }
                }
            }
        }

        def annotationsConform(t1: Type, t2: Type): Boolean = {
        		println("t1 " + t1)
        		println("t2 " + t2)

          vc.push

          try {
            val tree1 = whereAnnotation(t1)
            val tree2 = whereAnnotation(t2)
            
            println("tree1 " + tree1)
            println("tree2 " + tree2)

            val env = new HashMap[Path, Expr]
            constraintVars(vc, env, tree1)
            constraintVars(vc, env, tree2)

            assertFieldEqns(vc, env)

            val x1 = constraintTerm(vc, env, tree1)
            val x2 = constraintTerm(vc, env, tree2)

            println("x1 " + x1)
            println("x2 " + x2)

            val y1 = self(vc, env, tree1, t1)
            val y2 = self(vc, env, tree2, t2)

            println("y1 " + y1)
            println("y2 " + y2)
            
            val eqn = vc.eqExpr(y1, y2)
            println("self eqn " + eqn)
            vc.assertFormula(eqn)

            addTypeConstraints(vc, env, y1, t1)
            addTypeConstraints(vc, env, y2, t2)

            if (x1 != null && x2 != null) {
              val implies = vc.impliesExpr(x1, x2)
              println("implies " + implies)
              val q = vc.query(implies) match {
                case VALID => true
                case _ => false
              }

              println("  --> " + q)
              
              return q
            }            

            false
          }
          finally {
            vc.pop
          }
        }
        
        def whereAnnotation(tpe: Type): Tree = {
            val C = definitions.getClass("funicular.constraints.where")
            tpe match {
                case AnnotatedType(attribs, u, _) => {
                    for (a <- attribs) {
                        a match {
                            case AnnotationInfo(c, List(arg), assocs) if (c eq C.tpe) => {
                                return arg
                            }
                            case _ => ()
                        }
                    }
                }
                case _ => ()
            }
            null
        }
        
        import scala.collection.mutable.HashMap
        
        var next: Int = 0

        def fresh = {
            next += 1
            "tmp$" + next
        }

        type Path = List[String]

        def path(t: Tree): Path = t match {
            case Ident(name) => List(name.toString)
            case Select(qual, name) => name.toString :: path(qual)
            case This(qual) => "this" :: path(qual)
            case _ => null
        }

        def path(name: Name): Path = List(name.toString)

        implicit def name2path(n: Name) = path(n)
        implicit def tree2path(t: Tree) = path(t)

        def type2vcType(vc: VC, s: Type): STPType = {
            val t = s match {
                case AnnotatedType(_, u, _) => u
                case _ => s
            }
            if (t <:< definitions.BooleanClass.tpe)
                vc.bvType(1)
            else if (t <:< definitions.LongClass.tpe)
                vc.bvType(64)
            else if (t <:< definitions.IntClass.tpe)
                vc.bv32Type
            else if (t <:< definitions.AnyRefClass.tpe)
                vc.bvType(PTR_SIZE)
            else vc.bv32Type
        }

        def constraintVars(vc: VC, env: Env, t: Tree): Expr = {
          if (t == null)
              return null

          val cached = env.getOrElse(path(t), null)

          if (cached != null)
              return cached

          t match {
            case Function(List(x), body) => {
                env.put(x.name, vc.varExpr(x.name.toString, type2vcType(vc, x.tpe)))
                null
            }
            case Ident(name) => {
                val e = vc.varExpr(name.toString, type2vcType(vc, t.tpe))
                env.put(t, e)
                e
            }
            case Select(qual, name) => {
                // also add q == p ==> q.f == p.f
                val e = vc.varExpr(fresh, type2vcType(vc, t.tpe))
                env.put(t, e)
                constraintVars(vc, env, qual)
                e
            }
            case Apply(fun, args) => {
                constraintVars(vc, env, fun)
                args foreach(e2 => constraintVars(vc, env, e2))
                null
            }
            case _ => null
          }
        }
        
        def self(vc: VC, env: Env, t: Tree, tpe: Type): Expr = {
          t match {
            case Function(List(x), body) => env.getOrElse(path(x.name), vc.varExpr(x.name.toString, type2vcType(vc, tpe)))
            case _ => vc.varExpr(fresh, type2vcType(vc, tpe))
          }
        }
        
        def constraintTerm(vc: VC, env: Env, t: Tree): Expr = {
          if (t == null)
            return vc.trueExpr
          t match {
            case Function(List(x), body) => constraintBody(vc, env, body)
            case _ => null
          }
        }
        
        def constraintBody(vc: VC, env: Env, t: Tree): Expr = {
            implicit def tree2expr(t: Tree) = 
              constraintBody(vc, env, t) match {
                case null => vc.varExpr(fresh, type2vcType(vc, t.tpe))
                case x => x
              }


            t match {
              case Literal(Constant(true)) => vc.trueExpr
              case Literal(Constant(false)) => vc.falseExpr
              case Literal(Constant(null)) => vc.bvConstExprFromInt(PTR_SIZE, 0)
              case Literal(Constant(n)) if n.isInstanceOf[Long] => vc.bvConstExprFromLL(64, n.asInstanceOf[Long])
              case Literal(Constant(n)) if n.isInstanceOf[Int] => vc.bv32ConstExprFromInt(n.asInstanceOf[Int])
              case If(cond, thenp, elsep) => vc.iteExpr(cond, thenp, elsep)
              case This(qual) => {
                val e = env.getOrElse(path(t), null)
                if (e != null)
                    addTypeConstraints(vc, env, e, t.tpe)
                e
              }
              case Select(qual, name) => {
                val e = env.getOrElse(path(t), null)
                if (e != null)
                    addTypeConstraints(vc, env, e, t.tpe)
                e
              }
              case Ident(name) => {
                val e = env.getOrElse(path(t), null)
                if (e != null)
                    addTypeConstraints(vc, env, e, t.tpe)
                e
              }
              case Apply(Select(left, nme.ADD), List(right)) if (left.tpe <:< definitions.IntClass.tpe && right.tpe <:< definitions.IntClass.tpe) => 
                vc.bv32PlusExpr(left, right)
              case Apply(Select(left, nme.SUB), List(right)) if (left.tpe <:< definitions.IntClass.tpe && right.tpe <:< definitions.IntClass.tpe) => 
                vc.bv32MinusExpr(left, right)
              case Apply(Select(left, nme.MUL), List(right)) if (left.tpe <:< definitions.IntClass.tpe && right.tpe <:< definitions.IntClass.tpe) => 
                vc.bv32MultExpr(left, right)
              case Apply(Select(left, nme.DIV), List(right)) if (left.tpe <:< definitions.IntClass.tpe && right.tpe <:< definitions.IntClass.tpe) =>
                vc.sbvDivExpr(32, left, right)
              case Apply(Select(left, nme.MOD), List(right)) if (left.tpe <:< definitions.IntClass.tpe && right.tpe <:< definitions.IntClass.tpe) =>
                vc.sbvModExpr(32, left, right)
              case Apply(Select(left, nme.LT), List(right)) if (left.tpe <:< definitions.IntClass.tpe && right.tpe <:< definitions.IntClass.tpe) =>
                vc.sbvLtExpr(left, right)
              case Apply(Select(left, nme.GT), List(right)) if (left.tpe <:< definitions.IntClass.tpe && right.tpe <:< definitions.IntClass.tpe) =>
                vc.sbvGtExpr(left, right)
              case Apply(Select(left, nme.LE), List(right)) if (left.tpe <:< definitions.IntClass.tpe && right.tpe <:< definitions.IntClass.tpe) =>
                vc.sbvLeExpr(left, right)
              case Apply(Select(left, nme.GE), List(right)) if (left.tpe <:< definitions.IntClass.tpe && right.tpe <:< definitions.IntClass.tpe) =>
                vc.sbvGeExpr(left, right)
              case Apply(Select(left, nme.EQ), List(right)) if (left.tpe <:< definitions.IntClass.tpe && right.tpe <:< definitions.IntClass.tpe) =>
                vc.eqExpr(left, right)
              case Apply(Select(left, nme.NE), List(right)) if (left.tpe <:< definitions.IntClass.tpe && right.tpe <:< definitions.IntClass.tpe) =>
                vc.notExpr(vc.eqExpr(left, right))
              case _ => null
            }
        }

        override def addAnnotations(tree: Tree, tpe: Type): Type = {
            tpe
        }
    }
}
