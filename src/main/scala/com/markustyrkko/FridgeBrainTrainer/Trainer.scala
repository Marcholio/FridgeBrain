package com.markustyrkko.FridgeBrainTrainer

import scala.swing._
import java.io.File
import javax.swing.ImageIcon
import Swing._
import scala.swing.event.ButtonClicked
import com.markustyrkko.FridgeBrain.Brain
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import scala.collection.mutable.Buffer
import java.io.BufferedReader
import java.io.FileReader

object Trainer extends App {
  val trainingData = new File("trainingData/").listFiles().toIterator
  val origins = new File("originals/").listFiles().toIterator
  val dataMap = scala.collection.mutable.Map[String, String]()
  try {
  	val br = new BufferedReader(new FileReader("trainingMapping.txt"))
	  val str = Stream.continually(br.readLine()).takeWhile(_ != null).mkString("\n")
	  for(e <- str.split('\n')) {
	  	dataMap(e.split('=')(0)) = e.split('=')(1)
	  }
  } catch {
  	case _: Throwable => 
  }
  
  
  var currentImg = trainingData.next()
  
	val main = new MainFrame()
  main.minimumSize = new Dimension(400, 400)
  main.title = "FridgeBrain trainer"
  main.centerOnScreen()
  main.visible = true
  
  val wrapper = new BoxPanel (Orientation.Vertical)
  
  val imgWrapper = new BoxPanel (Orientation.Horizontal)
  imgWrapper.contents += VStrut(1)
  
  val img = new Label()
  img.icon = new ImageIcon(currentImg.toString())
  imgWrapper.contents += img
  
  imgWrapper.contents += VStrut(1)
  
  wrapper.contents += HStrut(1)
  wrapper.contents += imgWrapper
  
  wrapper.contents += HStrut(1)
  val beer = new Button("Beer")
  main.listenTo(beer)
  wrapper.contents += beer
  
  wrapper.contents += HStrut(1)
  val longDrink = new Button("Long drink")
  main.listenTo(longDrink)
  wrapper.contents += longDrink
  
  wrapper.contents += HStrut(1)
  val otherIN = new Button("otherIN")
  main.listenTo(otherIN)
  wrapper.contents += otherIN
  
  wrapper.contents += HStrut(1)
  val otherOUT = new Button("otherOUT")
  main.listenTo(otherOUT)
  wrapper.contents += otherOUT
  
  wrapper.contents += HStrut(1)
  val fast = new Button("FAST")
  main.listenTo(fast)
  wrapper.contents += fast
  
  wrapper.contents += HStrut(1)
  val quit = new Button("quit")
  main.listenTo(quit)
  wrapper.contents += quit
  
  main.contents = wrapper
  
  val beers = Array.ofDim[Buffer[Array[Array[(Int, Int, Int)]]]](9)
  val longs = Array.ofDim[Buffer[Array[Array[(Int, Int, Int)]]]](9)
  val ins = Array.ofDim[Buffer[Array[Array[(Int, Int, Int)]]]](9)
  val outs = Array.ofDim[Buffer[Array[Array[(Int, Int, Int)]]]](9)
  
  for(b <- beers) {
  	beers(beers.indexOf(b)) = Buffer[Array[Array[(Int, Int, Int)]]]()
  }
  
  for(l <- longs) {
  	longs(longs.indexOf(l)) = Buffer[Array[Array[(Int, Int, Int)]]]()
  }
  
  for(i <- ins) {
  	ins(ins.indexOf(i)) = Buffer[Array[Array[(Int, Int, Int)]]]()
  }
  
  for(o <- outs) {
  	outs(outs.indexOf(o)) = Buffer[Array[Array[(Int, Int, Int)]]]()
  }
  
  main.reactions += {
  	case ButtonClicked(b) => {
  		if(!b.text.equals("quit")) {
  			var img = ImageIO.read(currentImg)
  			var pos = currentImg.toString().split('\\')(1).split('-')(0).toInt
  			Brain.bwSub(img)
  			
  			b.text match {
					case "Beer" => beers(pos) += pixels(img)
					case "Long drink" => longs(pos) += pixels(img)
					case "otherIN" => ins(pos) += pixels(img)
					case "otherOUT" => outs(pos) += pixels(img)
					case "FAST" => 
						while(trainingData.hasNext) {
							img = ImageIO.read(currentImg)
							pos = currentImg.toString().charAt(13).asDigit
							Brain.bwSub(img)
							dataMap(currentImg.toString) match {
								case "Beer" => beers(pos) += pixels(img)
								case "Long drink" => longs(pos) += pixels(img)
								case "otherIN" => ins(pos) += pixels(img)
								case "otherOUT" => outs(pos) += pixels(img)
							}
							currentImg = trainingData.next
						}
				}
  			if(trainingData.hasNext) {
  				changeCurrentImg()	
  			} else {
  				calculate()
  			}
  		} else {
  			calculate()
  		}
  	}
  }
  
  def changeCurrentImg() {
  	currentImg = trainingData.next()
  	img.icon = new ImageIcon(currentImg.toString())
  }
  
  def pixels(img: BufferedImage): Array[Array[(Int, Int, Int)]] = {
  	val width = img.getWidth()
  	val height = img.getHeight()
  	val pix = Array.ofDim[(Int, Int, Int)](width, height)
  	for(y <- 0 until height; x <- 0 until width) {
  		val color = new Color(img.getRGB(x, y))
  		pix(x)(y) = (color.getRed, color.getGreen, color.getBlue)
  	}
  	
  	return pix
  }
  
  def average(data: Buffer[Array[Array[(Int, Int, Int)]]]): Array[Array[(Double, Double, Double)]] = {
  	val sumArray = Array.ofDim[(Double, Double, Double)](data(0).length, data(0)(0).length)
  	
  	for(img <- data; y <- 0 until img(0).length; x <- 0 until img.length) {
  		if(sumArray(x)(y) == null) {
				sumArray(x)(y) = (img(x)(y)._1.toDouble, img(x)(y)._2.toDouble, img(x)(y)._3.toDouble)
			} else {
				val r = sumArray(x)(y)._1 + img(x)(y)._1
				val g = sumArray(x)(y)._2 + img(x)(y)._2
				val b = sumArray(x)(y)._3 + img(x)(y)._3
				sumArray(x)(y) = (r, g, b)
			}
  	}
  	
  	for(arr <- sumArray; i <- 0 until arr.length) {
  		arr(i) = ((arr(i)._1 / data.length), (arr(i)._2 / data.length), (arr(i)._3 / data.length))
  	}
  	return sumArray
  }
  
  def saveImg(pixels: Array[Array[(Double, Double, Double)]], name: String) {
  	val img = new BufferedImage(pixels.length, pixels(0).length, ImageIO.read(currentImg).getType)
  	for(y <- 0 until pixels(0).length; x <- 0 until pixels.length) {
  		val r = Math.min(pixels(x)(y)._1.toInt, 255) << 16
  		val g = Math.min(pixels(x)(y)._2.toInt, 255) << 8
  		val b = Math.min(pixels(x)(y)._3.toInt, 255)
  		img.setRGB(x, y, r + g + b)
  	}
  	ImageIO.write(img, "jpg", new File("./models/" + name + ".jpg"))
  }
  
  def calculate() {
  	main.dispose()
		for(beer <- beers) {
			if(beer.length > 0) {
				saveImg(average(beer), beers.indexOf(beer) + "/Beer")	
			}
		}
  	
  	for(long <- longs) {
  		if(long.length > 0) {
  			saveImg(average(long), longs.indexOf(long) + "/Long")	
  		}
  	}
  	
  	for(in <- ins) {
  		if(in.length > 0) {
  			saveImg(average(in), ins.indexOf(in) + "/In")	
  		}
  	}
  	for(out <- outs) {
  		if(out.length > 0) {
  			saveImg(average(out), outs.indexOf(out) + "/Out")	
  		}
  	}
  }
}