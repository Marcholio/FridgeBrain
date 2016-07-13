package com.markustyrkko.FridgeBrainTrainer

import javax.imageio.ImageIO
import java.io.File
import java.io.ByteArrayOutputStream
import java.awt.Color
import java.awt.image.BufferedImage
import com.markustyrkko.FridgeBrain.Brain

object PrepareTrainer extends App {

	val dir = new File("./originals/")
	val files = dir.listFiles()
	var outputLoc = "./trainingData/"
	
	val sectors = Brain.getSectors()
	
	for(f <- files) {
		println(((files.indexOf(f).toDouble) / (files.length - 1)) * 100.0)
		if(outputLoc.equals("./trainingData/") && files.indexOf(f) > files.length * 0.7) {
			//outputLoc = "./examData/"
		}
		val img = ImageIO.read(f)
	  val height = img.getHeight
	  val width = img.getWidth
	  
	  var y = 0
	  var pos = 0
	  
	  for(row <- sectors.keySet.toList.reverse) {
	  	var x = 0
  		val rowHeight = sectors(row)._1
	  	val widthIterator = sectors(row)._2.toIterator
	  	while(widthIterator.hasNext) {
	  		val sectorWidth = widthIterator.next
	  		
		  	val subImg = img.getSubimage(x, y, sectorWidth, rowHeight)

		  		ImageIO.write(subImg, "jpg", new File(outputLoc + pos + "-" + System.currentTimeMillis() + ".jpg"))
		  		x += sectorWidth
		  		pos += 1
	  	}
	  	y += rowHeight
	  }
	}
}
