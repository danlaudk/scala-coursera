package scalashop

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */


  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    def stripBlur(xPos:Int, src:Img, dst: Img, radius: Int): Unit = {
      for(y <- 0 until src.height)
        dst(xPos, y) = boxBlurKernel(src, xPos, y , radius)
    }
    for(x <- from until end) stripBlur(x, src, dst, radius)
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    //    Use Scala ranges to create a list of splitting points (hint: use the by method on ranges).
    // Then use collection combinators on the list of splitting points to create a list of start and end tuples, one for each strip
    // (hint: use the zip and tail methods).
    // Finally, use the task construct to start a parallel task for each strip,
    val splittingPoints = 0 to src.width by (src.width / numTasks max 1)

    splittingPoints
      .zip(splittingPoints.tail) //auto dropped
      .map{ case (start, stop) => task[Unit](blur(src, dst, start, stop, radius)) }
      .foreach(_.join)

    // and then call join on each task to wait for its completion.

  }

}


