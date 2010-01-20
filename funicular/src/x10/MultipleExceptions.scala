package x10

class MultipleExceptions(exceptions: Collection[Throwable]) extends Exception {
    override def printStackTrace = {
        super.printStackTrace
        println("Caused by:")
        for (th <- exceptions)
           th.printStackTrace
    }
}
