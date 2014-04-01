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
import math.{cos, sin, toRadians, abs, round, sqrt, pow}


object Main {

   type Image[T] = (Double, Double) => T
   type ImageTrans[T] = Image[T] => Image[T]
   type Animation[T] = Double => Image[T]

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
   
   // Quantize an image function and draw into a pixel buffer
   def drawBuffer(cols: Int, rows: Int, buffer:BufferedImage) =
      (image:Image[Color]) => {
         for (x <- 0 to cols - 1; y <- 0 to rows - 1)
            buffer.setRGB(x, y, image(x, y).getRGB)
      }
   
   def animate(cols: Int, rows: Int) = {
      var time = 0.0
      var timeDelta = 1.0
   
      (animation:Animation[Color]) => {
         val buffer = new BufferedImage(cols, rows, BufferedImage.TYPE_INT_RGB)
   
         val panel = new Panel {
            override def paint(graphics:Graphics2D) {
               graphics.drawImage(buffer, 0, 0, null)
            } 
            preferredSize = new Dimension(cols, rows)
         }
   
         val timer = new javax.swing.Timer(100, Swing.ActionListener(event => {
            drawBuffer(cols, rows, buffer)(animation(time))
            panel.repaint()
            time += timeDelta
         }))
   
         val frame = new MainFrame {
            title = "Functional Images"
            contents = panel
            centerOnScreen()
         }
   
         frame.open()
         timer.start()
      }
   }
   
   def draw(cols: Int, rows: Int) =
      (image:Image[Color]) => {
         val buffer = new BufferedImage(cols, rows, BufferedImage.TYPE_INT_RGB)
   
         drawBuffer(cols, rows, buffer)(image)
   
         val panel = new Panel {
            override def paint(graphics:Graphics2D) {
               graphics.drawImage(buffer, 0, 0, null)
            } 
            preferredSize = new Dimension(cols, rows)
         }
   
         val frame = new MainFrame {
            title = "Functional Images"
            contents = panel
            centerOnScreen()
         }
   
         frame.open()
      }
   
   // Constant color images
   val red:Image[Color] = (_, _) => new Color(255, 0, 0)
   val blue:Image[Color] = (_, _) => new Color(0, 0, 255)
   val green:Image[Color] = (_, _) => new Color(0, 255, 0)
   
   // Black and white grid with vertical/horizontal lines
   def grid(cellSize:Double, lineThickness:Double):Image[Color] =
      (col, row) =>
         if (modDouble(col, cellSize) < lineThickness || 
             modDouble(row, cellSize) < lineThickness)
            new Color(0, 0, 0)
         else
            new Color(255, 255, 255)
   
   // Scale image about the origin
   def scaleOrigin[T](factor:Double):ImageTrans[T] =
      (image:Image[T]) =>
         (col:Double, row:Double) =>
            image(col / factor, row / factor)
   
   // Translate an image
   def translate[T](colDelta:Double, rowDelta:Double):ImageTrans[T] =
      (image:Image[T]) =>
         (col:Double, row:Double) =>
            image(col - colDelta, row - rowDelta)
   
   // Scale image about a center point
   def scale[T](factor:Double, centerCol:Double, centerRow:Double):ImageTrans[T] = {
      val t1:ImageTrans[T] = translate(-centerCol, -centerRow) andThen scaleOrigin(factor)
      val t2:ImageTrans[T] = t1 andThen translate(centerCol, centerRow)
      t2
   }
   
   /* Want to write it like this, but Scala's type inference sucks
   def scale[T](factor:Double, centerCol:Double, centerRow:Double):ImageTrans[T] = {
      translate(-centerCol, -centerRow) andThen
      scaleOrigin(factor) andThen
      translate(centerCol, centerRow)
   }
   */
   
   def rotate[T](angle:Double, centerCol:Double, centerRow:Double):ImageTrans[T] =
      (image:Image[T]) => {
         val angleRadians = toRadians(angle)
         val cosAngle = cos(angleRadians)
         val sinAngle = sin(angleRadians)
         (col:Double, row:Double) => {
            val colTrans = col - centerCol
            val rowTrans = row - centerRow
            val newCol = colTrans * cosAngle - rowTrans * sinAngle
            val newRow = colTrans * sinAngle + rowTrans * cosAngle
            image(newCol + centerCol, newRow + centerRow)
         }
      }
   
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
   
   def waveAnimation(time:Double):Image[Color] = {
      //waveTime(time * 10, 1, 3, 200, 150)(testBitmap)
      //waveTime(time * 10, 0.8, 0.5, 200, 150)(testGrid)
      waveTime(time * 10, 0.5, 0.5, 200, 150)(testScaleRotateBitmap)
   }
   
   
   def waveTime[T](time: Double, amp:Double, period:Double, centerCol:Double, centerRow:Double):ImageTrans[T] = {
      (image:Image[T]) => {
         (col:Double, row:Double) => {
           val d = distance(col, row, centerCol, centerRow) 
           val scaleFactor = 2 + cos(toRadians(d * period + time))/2 * amp
           scale(scaleFactor, centerRow, centerCol)(image)(col, row)
         }
      }
   }
   
   def wave[T](amp:Double, period:Double, centerCol:Double, centerRow:Double):ImageTrans[T] = {
      (image:Image[T]) => {
         (col:Double, row:Double) => {
           val d = distance(col, row, centerCol, centerRow) 
           val scaleFactor = 2 + cos(toRadians(d * period))/2 * amp
           scale(scaleFactor, centerRow, centerCol)(image)(col, row)
         }
      }
   }
   
   //val testDraw = draw(800, 600)
   //testDraw(blue)
   val testGrid:Image[Color] = grid(20, 3)
   //testDraw(testGrid)
   //def testScale[T]:ImageTrans[T] = scale(2, 0, 0)
   //testDraw(scale(2,0,0)(testGrid))
   def testRotate[T]:ImageTrans[T] = rotate(45, 0, 0)
   //testDraw(testRotate(testGrid))
   //def testRotateScale[T]:ImageTrans[T] = testRotate andThen testScale 
   //testDraw(testRotateScale(testGrid))
   val testBitmap = bitmap("floyd.png")
   val testScaleRotateBitmap = scale(0.1, 0, 0)(testRotate(testBitmap))
   //testDraw(testScaleRotateBitmap)
   //testDraw(wave(1, 3, 400, 300)(testBitmap))
   
   
   def main(args: Array[String]) =
       animate(400, 300)(waveAnimation)
       //animate(800, 600)(waveAnimation)
   
}
