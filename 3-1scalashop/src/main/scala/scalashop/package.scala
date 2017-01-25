
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }
  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    var x_counter = clamp(x-radius,0, src.width-1)
    val toChannels: Array[RGBA => Int] = Array(red _, green _, blue _, alpha _)
    var channelValues = Array(0, 0, 0, 0)
    var count = 0

    while (x_counter <= clamp(x+radius,0, src.width-1) ) {
      var y_counter = clamp(y-radius,0, src.height-1)
      while (y_counter <= clamp(y+radius,0, src.height-1) ) {
        for (channel <- 0 to 3) {
//          var clampedY = clamp(y_counter, 0, src.height - 1)
//          var clampedX = clamp(x_counter, 0, src.width - 1)
          channelValues(channel) = channelValues(channel) + toChannels(channel)(src(x_counter, y_counter))
        }
        count += 1
        y_counter += 1
      }
      x_counter += 1
    }
    rgba(channelValues(0)/count, channelValues(1)/count, channelValues(2)/count, channelValues(3)/count) //avg
  }

}
