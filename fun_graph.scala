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
import javax.swing.{Timer}
import java.io.{File}
import math.{cos, sin, abs, round, sqrt, pow, Pi}

/*
// For the parallel version of rasterize

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
*/

object ImageFun {

   type Image[T] = (Double, Double) => T
   type ImageTrans[T] = Image[T] => Image[T]
   type Animation[T] = Double => Image[T]
   type CoordTrans = (Double, Double) => (Double, Double)

   private def distance(x1:Double, y1:Double, x2:Double, y2:Double):Double =
       sqrt(pow(y1 - y2, 2) + pow(x1 - x2, 2))
   
   // Modulus which returns a positive result, even for
   // negative numerators.
   private def modDouble(x:Double, y:Double):Double = {
      val m = x % y
      if (m < 0) m + y else m
   }
   
   // Modulus which returns a positive result, even for
   // negative numerators.
   private def modInt(x:Int, y:Int):Int = {
      val m = x % y
      if (m < 0) m + y else m
   }

   /* Want to write:
   def coordTrans[T](trans:CoordTrans):ImageTrans[T] =
      (image:Image[T]) => image.tupled compose trans
   */

   // Transform the coordinates of an image by some function
   def coordTrans[T](trans:CoordTrans):ImageTrans[T] =
      (image:Image[T]) =>
         (col:Double, row:Double) => 
            image.tupled(trans(col, row))
   
   // Constant color images
   val red:Image[Color] = (_, _) => new Color(255, 0, 0)
   val blue:Image[Color] = (_, _) => new Color(0, 0, 255)
   val green:Image[Color] = (_, _) => new Color(0, 255, 0)
   
   // Black and white grid with vertical/horizontal lines
   def grid(cellSize:Double, lineThickness:Double):Image[Color] = {
      val black = new Color(0, 0, 0)
      val white = new Color(255, 255, 255)
      (col, row) =>
         if (modDouble(col, cellSize) < lineThickness || 
             modDouble(row, cellSize) < lineThickness)
            black
         else
            white
   }

   /* Want to write it like this, but Scala's won't type it
   def aboutPoint[T](imageTrans:ImageTrans[T],
         centerCol:Double, centerRow:Double):ImageTrans[T] = {
      translate(-centerCol, -centerRow) andThen
      imageTrans andThen
      translate(centerCol, centerRow)
   }
   */

   // Apply an image transformation about some point
   def aboutPoint[T](imageTrans:ImageTrans[T],
         centerCol:Double, centerRow:Double):ImageTrans[T] = {
      val t1:ImageTrans[T] = translate(-centerCol, -centerRow) andThen imageTrans 
      val t2:ImageTrans[T] = t1 andThen translate(centerCol, centerRow)
      t2
   }
   
   // Scale image about the origin
   def scaleOrigin[T](factor:Double):ImageTrans[T] =
      coordTrans((col, row) => (col / factor, row / factor))
   
   // Translate an image
   def translate[T](colDelta:Double, rowDelta:Double):ImageTrans[T] =
      coordTrans((col, row) => (col - colDelta, row - rowDelta))

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
   
   /* General form of a cosine wave:

      F(x) = A cos(Bx - C) + D
      
      A = amplitude
      B = compression factor on x axis, B = 2Pi / period
      C/B = phase shift 
      D = vertical shift
   */

   // Scale an image using a cosine wave, centered at the origin.
   def waveOrigin[T](phaseShift: Double, vertShift:Double,
                     amp:Double, period:Double):ImageTrans[T] = {
      val compress = 2 * Pi / period
      val phaseFactor = phaseShift * compress
      (image:Image[T]) => {
         (col:Double, row:Double) => {
            val d = distance(col, row, 0, 0) 
            val scaleFactor = amp * + cos(compress * d - phaseFactor) + vertShift
            scaleOrigin(scaleFactor)(image)(col, row)
         }
      }
   }

   // Scale an image using a cosine wave about a point.
   def wave[T](phaseShift: Double, verticalShift:Double, amp:Double,
               period:Double, centerCol:Double, centerRow:Double):ImageTrans[T] = {
      aboutPoint(waveOrigin(phaseShift, verticalShift, amp, period),
                 centerCol, centerRow) 
   }
}

class Display(cols:Int, rows:Int) {

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

   /*
   // Parallel version
   // Quantize an image function and draw into a pixel buffer
   def rasterize(image:ImageFun.Image[Color]) = {
      val firstHalf = future {
         for (y <- 0 to rows/2 - 1; x <- 0 to cols - 1)
            buffer.setRGB(x, y, image(x, y).getRGB)
      }
      val secondHalf = future {
         for (y <- rows/2 to rows - 1; x <- 0 to cols - 1)
            buffer.setRGB(x, y, image(x, y).getRGB)
      }
      Await.result(firstHalf, Duration.Inf)
      Await.result(secondHalf, Duration.Inf)
   }
   */

   // Quantize an image function and draw into a pixel buffer
   def rasterize(image:ImageFun.Image[Color]) = {
      for (x <- 0 to cols - 1; y <- 0 to rows - 1)
         buffer.setRGB(x, y, image(x, y).getRGB)
   }

   def repaint() = panel.repaint()
   def open() = frame.open()
}

class Animate(cols:Int, rows:Int, animation:ImageFun.Animation[Color]) {
   def show() = {
      val display = new Display(cols, rows)
      var time = 0.0
      var timeDelta = 1.0
      val timer = new Timer(100, Swing.ActionListener(event => {
         display.rasterize(animation(time))
         display.repaint()
         time += timeDelta
      }))
      display.open()
      timer.start()
   }
}

class Draw(cols:Int, rows:Int, image:ImageFun.Image[Color]) {
   def show() = {
      val display = new Display(cols, rows)
      display.rasterize(image)
      display.open()
   }
}

object Examples {
   val testGrid = ImageFun.grid(20, 2)
   //val floydBitmap = ImageFun.bitmap("floyd.png")
   //def rotate45[T]:ImageFun.ImageTrans[T] = ImageFun.rotate(Pi/4, 0, 0)
   //val scaleRotateBitmap = ImageFun.scale(0.1, 0, 0)(rotate45(floydBitmap))

   def waveAnimation(time:Double):ImageFun.Image[Color] = {
      ImageFun.wave(time * 2, 2, 0.8, 100, 200, 150)(testGrid)
      //ImageFun.wave(time * 2, 2, 0.8, 100, 200, 150)(scaleRotateBitmap)
   }
}

object Main {
   def main(args: Array[String]) = {
      new Animate(400, 300, Examples.waveAnimation).show()
   }
}
