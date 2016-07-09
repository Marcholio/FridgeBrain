package com.markustyrkko.FridgeBrainTrainer

import javax.imageio.ImageIO
import java.io.File
import java.io.ByteArrayOutputStream
import java.awt.Color
import java.awt.image.BufferedImage

object PrepareTrainer extends App {

	val dir = new File("./originals/")
	val files = dir.listFiles()
	var outputLoc = "./trainingData/"
	
	for(f <- files) {
		println(((files.indexOf(f).toDouble + 1) / files.length) * 100.0)
		if(outputLoc.equals("./trainingData/") && files.indexOf(f) > files.length * 0.7) {
			//outputLoc = "./examData/"
		}
		val img = ImageIO.read(f)
	  val height = img.getHeight
	  val width = img.getWidth
	  val xStep = 66
	  val yStep = 130
	  
	  for(y <- 0 until yStep by yStep; x <- 0 until width - xStep by xStep) {
			val subImg = img.getSubimage(x, y, xStep, yStep)
			ImageIO.write(subImg, "jpg", new File(outputLoc + x / xStep + "-" + System.currentTimeMillis() + ".jpg"))
	  }
	}
}
