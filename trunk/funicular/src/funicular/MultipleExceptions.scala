package funicular

class MultipleExceptions(val exceptions: Collection[Throwable]) extends Exception {
    override def printStackTrace = {
        super.printStackTrace
        println("Caused by:")
        for (th <- exceptions)
           th.printStackTrace
    }
}

object MultipleExceptions {
    def unapply(v: Any) = {
        if (v.isInstanceOf[MultipleExceptions])
            Some(v.asInstanceOf[MultipleExceptions].exceptions.toList)
        else
            None
    }
}
