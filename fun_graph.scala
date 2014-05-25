/* Functional graphics.

   Computer graphic images are represented as functions from
   real 2D coordinates to values. Animations are represented
   as functions from time to images. This module was written
   to support a talk at the Melbourne Scala Users Group.

   Motivated by the paper "Functional Images" by Conal Elliot

   http://conal.net/papers/functional-images/fop-conal.pdf

   Author: Bernie Pope
   Date: 1/4/2013
*/ 

import swing._
import java.awt.{Color}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.{File}
import math.{cos, sin, abs, round, sqrt, pow, Pi, min, max}

// Convert images of various types into color images
object ColorImplicits {
   import ImageFun._
   import Math._

   // true = white, false = black
   implicit def BoolToColor(image:Image[Boolean]):Image[Color] = 
      mapImage((value:Boolean) => if (value) Color.white else Color.black, image)

   implicit def DoubleToColor(image:Image[Double]):Image[Color] = 
      mapImage((value:Double) => {
         val intensity = clampIntensity((value * 255).toInt)
         new Color(intensity, intensity, intensity)}, image)
}

object Math {
   // Cartesian distance between two points.
   def distance(x1:Double, y1:Double, x2:Double, y2:Double):Double =
      sqrt(pow(y1 - y2, 2) + pow(x1 - x2, 2))
   
   // Modulus which returns a positive result, even for
   // negative numerators.
   def modDouble(x:Double, y:Double):Double = {
      val m = x % y
      if (m < 0) m + y else m
   }
   
   // Modulus which returns a positive result, even for
   // negative numerators.
   def modInt(x:Int, y:Int):Int = {
      val m = x % y
      if (m < 0) m + y else m
   }
   
   // Make sure intensity value is in the range [0, 255] inclusive.
   def clampIntensity(intensity:Int) =
      min(max(intensity, 0), 255)
}

object ImageFun {

   import Math._

   type Image[T] = (Double, Double) => T
   type ImageTrans[T] = Image[T] => Image[T]
   type Animation[T] = Double => Image[T]
   type CoordTrans = (Double, Double) => (Double, Double)

   def mapImage[A,B](fun:A => B, image:Image[A]):Image[B] =
      (col, row) => fun(image(col, row))

   def combineImage[A,B,C](image1:Image[A], image2:Image[B], combine:(A, B) => C):Image[C] =
      (col, row) => combine(image1(col, row), image2(col, row))

   def combineMany[T](images:List[Image[T]], combine:(T, T) => T):Image[T] =
      images.reduceLeft((image1, image2) => combineImage(image1, image2, combine))

   /* Want to write:
   def coordTrans[T](trans:CoordTrans):ImageTrans[T] =
      (image:Image[T]) => image.tupled compose trans
   */

   // Transform the coordinates of an image by some function
   def coordTrans[T](trans:CoordTrans):ImageTrans[T] =
      (image:Image[T]) =>
         (col:Double, row:Double) => 
            image.tupled(trans(col, row))
   
   // Apply an image transformation about a point
   def aboutPoint[T](transform:ImageTrans[T], col:Double, row:Double):ImageTrans[T] =
      translate(-col, -row) andThen transform andThen translate(col, row)
   
   // Translate an image
   def translate[T](colDelta:Double, rowDelta:Double):ImageTrans[T] =
      coordTrans((col, row) => (col - colDelta, row - rowDelta))

   // Scale image about the origin
   def scaleOrigin[T](factor:Double):ImageTrans[T] =
      coordTrans((col, row) => (col / factor, row / factor))

   // Scale image about a center point
   def scale[T](factor:Double,
         centerCol:Double, centerRow:Double):ImageTrans[T] =
      aboutPoint(scaleOrigin(factor), centerCol, centerRow)

   // Rotate image clockwise about the origin, angle in radians
   def rotateOrigin[T](angle:Double):ImageTrans[T] = {
      val cosAngle = cos(angle)
      val sinAngle = sin(angle)
      coordTrans(
         (col, row) =>
            (col * cosAngle - row * sinAngle, col * sinAngle + row * cosAngle))
   }
   
   // Rotate image clockwise about some point, angle in radians
   def rotate[T](angle:Double,
         centerCol:Double, centerRow:Double):ImageTrans[T] =
      aboutPoint(rotateOrigin(angle), centerCol, centerRow)
   
   // Return an image from a bitmap file, given a filepath to the image.
   // Bitmap is tiled infinitely in all directions.
   def bitmap(filepath:String):Image[Color] = {
      val pixels = ImageIO.read(new File(filepath))
      val numRows = pixels.getHeight
      val numCols = pixels.getWidth
      (col:Double, row:Double) => {
         val colInt = modInt(col.toInt, numCols)
         val rowInt = modInt(row.toInt, numRows)
         new Color(pixels.getRGB(colInt, rowInt))
      }
   }
}

object ImageTransExamples {

   import Math._
   import ImageFun._
   import ImageExamples._
   
   // Scale the magnification of a image based on a wave function, based on
   // distance of point from the origin.
   def waveScaleOrigin[T](phaseShift: Double, vertShift:Double,
         amp:Double, period:Double):ImageTrans[T] = {
      (image:Image[T]) => {
         (col:Double, row:Double) => {
            val scaleAmount = waveIntensityOrigin(phaseShift, vertShift, amp, period)(col, row)
            scaleOrigin(scaleAmount)(image)(col, row)
         }
      }
   }

   // Scale the magnification of a image based on a wave function, based on
   // distance of point from some other point.
   def waveScale[T](phaseShift: Double, vertShift:Double, amp:Double,
               period:Double, centerCol:Double, centerRow:Double):ImageTrans[T] = {
      aboutPoint(waveScaleOrigin(phaseShift, vertShift, amp, period),
                 centerCol, centerRow) 
   }

   // Translate an image vertically based on a wave function.
   def waveTranslate[T](phaseShift: Double, vertShift:Double, amp:Double, period:Double):ImageTrans[T] = {
      (image:Image[T]) =>
         (col, row) => {
            val trans = waveIntensityOrigin(phaseShift, vertShift, amp, period)(col, 0)
            translate(0, trans)(image)(col, row)
         }
   }
}

object ImageExamples {

   import Math._
   import ImageFun._

   val redImage = constImage(Color.red)

   // Images entirely one value 
   def constImage[T](value:T):Image[T] =
      (_, _) => value 

   def grid(cellSize:Double, lineThickness:Double):Image[Boolean] = {
      (col, row) =>
         (modDouble(col, cellSize) >= lineThickness && 
          modDouble(row, cellSize) >= lineThickness)
   }

   /* Compute a two dimensional wave based on cosine, and distance from
      the origin.

      General form of a cosine wave:

      F(x) = A cos(Bx - C) + D
      
      A = amplitude
      B = compression factor on x axis, B = 2Pi / period
      C/B = phase shift 
      D = vertical shift
   */

   def waveIntensityOrigin(phaseShift: Double, vertShift:Double,
         amp:Double, period:Double):Image[Double] = {
      val compress = 2 * Pi / period
      val phaseFactor = phaseShift * compress
      (col:Double, row:Double) => {
         val d = distance(col, row, 0, 0) 
         amp * + cos(compress * d - phaseFactor) + vertShift
      }
   }

   def waveIntensity(phaseShift: Double, vertShift:Double, amp:Double,
         period:Double, col:Double, row:Int):Image[Double] =
      translate(col, row)(waveIntensityOrigin(phaseShift, vertShift, amp, period))

   val gridImage = grid(20, 5)
   def scaleRotate[T](factor:Double, angle:Double):ImageTrans[T] =
      scaleOrigin(factor) andThen rotateOrigin(angle)
   val scaledRotatedGrid = scaleRotate(2, Pi/4)(gridImage)
   val bitmapImage = bitmap("floyd.png")
   val scaledRotatedBitmap = scaleRotate(0.25, Pi/4)(bitmapImage)

   def waveImage:Image[Double] = {
      val wave1 = waveIntensity(0, 0.3, 0.2, 50, 300, 200)
      val wave2 = waveIntensity(0, 0.2, 0.1, 70, 50, 100)
      combineImage(wave1, wave2, (x:Double, y:Double) => x + y)
   }
}

object AnimationExamples {

   import ImageFun._
   import ImageExamples._
   import ImageTransExamples._

   def waveAnimation(time:Double):Image[Double] = {
      val wave1 = waveIntensity(time * 6, 0.3, 0.2, 50, 300, 200)
      val wave2 = waveIntensity(time * 2, 0.2, 0.1, 70, 50, 100)
      combineImage(wave1, wave2, (x:Double, y:Double) => x + y)
   }

   def waveGridAnimation(time:Double):Image[Boolean] = {
      val gridImage = ImageExamples.grid(20, 2)
      waveScale(time * 2, 1, 0.3, 100, 200, 150)(gridImage)
   }

   def waveBitmapAnimation(time:Double):Image[Color] = {
      waveScale(time * 2, 2, 0.8, 100, 200, 150)(scaledRotatedBitmap)
   }

   def waveTranslateAnimation(time:Double):Image[Color] = {
      val floydBitmap = bitmap("floyd.png")
      waveTranslate(time * 10, 0, 10, 100)(floydBitmap)
   }
}

class Display(cols:Int, rows:Int) {

   import ImageFun._

   private val buffer = new BufferedImage(cols, rows, BufferedImage.TYPE_INT_RGB)
   private val panel = new Panel {
      override def paint(graphics:Graphics2D) {
         graphics.drawImage(buffer, 0, 0, null)
      } 
      preferredSize = new Dimension(cols, rows)
   }
   private val frame = new MainFrame {
      title = "Functional Images"
      contents = panel
      centerOnScreen()
   }

   // Quantize an image function and draw into a pixel buffer
   def rasterize(image:Image[Color]) = {
      for (x <- 0 to cols - 1; y <- 0 to rows - 1)
         buffer.setRGB(x, y, image(x, y).getRGB)
   }

   def repaint() = panel.repaint()
   def open() = frame.open()
}

// Run an animation in a window indefinitely
class Animate(cols:Int, rows:Int, animation:ImageFun.Animation[Color]) {
   def show() = {
      val display = new Display(cols, rows)
      var time = 0.0
      var timeDelta = 1.0
      display.open()
      while(true) {
         display.rasterize(animation(time))
         display.repaint()
         time += timeDelta
      }
   }
}

// Draw a still image in a window
class Draw(cols:Int, rows:Int, image:ImageFun.Image[Color]) {
   def show() = {
      val display = new Display(cols, rows)
      display.rasterize(image)
      display.open()
   }
}

object Main {
   
   import AnimationExamples._
   import ImageExamples._
   import ColorImplicits._

   val usage = """
      fun_graph demo 

      demo must be one of:
         red_image             an entirely red image 
         grid                  black and white grid
         scaled_rotated_grid   grid rotated 45 degrees
         bitmap                a tiled bitmap
         scaled_rotated_bitmap a tiled bitmap
         wave_image            grey scale waves image
         wave_animation        grey scale waves animation
         translate_wave        translate pixels in a bitmap as vertical wave
         grid_wave             translate scale of a grid as wave from center of image
         bitmap_wave           translate scale of a bitmap as wave from center of image
      """

   def main(args: Array[String]) = {
      if (args.length == 0)
         println(usage)
      else
          args(0) match {
             case "red_image" =>
                new Draw(400, 300, redImage).show()
             case "grid" =>
                new Draw(400, 300, gridImage).show()
             case "scaled_rotated_grid" =>
                new Draw(400, 300, scaledRotatedGrid).show()
             case "bitmap" =>
                new Draw(400, 300, bitmapImage).show()
             case "scaled_rotated_bitmap" =>
                new Draw(400, 300, scaledRotatedBitmap).show()
             case "wave_image" =>
                new Draw(400, 300, waveImage).show()
             case "wave_animation" =>
                new Animate(400, 300, waveAnimation).show()
             case "translate_wave" =>
                new Animate(400, 300, waveTranslateAnimation).show()
             case "grid_wave" =>
                new Animate(400, 300, waveGridAnimation).show()
             case "bitmap_wave" =>
                new Animate(400, 300, waveBitmapAnimation).show()
             case _ =>
                println("fun_graph: Unrecognised demo name")
                println(usage)
          }
   }
}
