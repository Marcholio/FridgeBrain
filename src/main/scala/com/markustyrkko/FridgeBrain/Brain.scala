package com.markustyrkko.FridgeBrain

import javax.imageio.ImageIO
import java.io.File
import java.io.ByteArrayOutputStream
import java.awt.Color
import java.awt.image.BufferedImage
import com.sun.prism.BasicStroke
import java.io.ByteArrayInputStream
import java.io.IOException

object Brain extends App {
	
	/**
	 * Analyzes given image and returns result of it's content as String
	 * @param bytes		Array of bytes from http server containing image data
	 * @return				String containing information about image content
	 */
	def analyze(bytes: Array[Byte]): String = {
		// Convert bytes to BufferedImage
		val img = getImage(bytes)
		ImageIO.write(img, "jpg", new File("img.jpg"))
		
		// Image dimensions
	  val height = img.getHeight
	  val width = img.getWidth
	  
	  // Subimage dimensions
	  val xStep = 66
	  val yStep = 130
	  
	  // Drink counters
	  var beers = 0
	  var longs = 0
	  
	  // Currently analyzes only top row of fridge containing beers and long drinks
	  for(y <- 0 until yStep by yStep; x <- 0 until width - xStep by xStep) {
			// Get separate subimage
	  	val subImg = copyImg(img.getSubimage(x, y, xStep, yStep))
			
	  	// Convert subimage to black-white map
	  	bwSub(subImg)
	  	
	  	// Compare map to known models of each drink and empty space
			val result = getMatch(subImg, x / xStep)
			
			// Increase counters if necessary
			if(result.toString().equals(Drink.BEER.toString())) {
				beers += 1
			} else if(result.toString().equals(Drink.LONG.toString())) {
				longs += 1
			}
	  }
		return "There are approximately " + 2 * beers + " beer(s) and " + 2 * longs + " long drink(s) in the fridge"
	}
	
	/**
	 * Converts array of bytes to BufferedImage
	 * @param bytes		Array of bytes containing image data
	 * @return				BufferedImage from bytes
	 */
	def getImage(bytes: Array[Byte]): BufferedImage = {
		try {
			return ImageIO.read(new ByteArrayInputStream(bytes))
		} catch {
			case _: Exception =>
				return null
		}
	}
	
	/**
	 * Returns deep copy of image
	 * @param img		Image to be copied
	 * @return			Separate image that has identical data with original image
	 */
	def copyImg(img: BufferedImage): BufferedImage = {
		val newImg = new BufferedImage(img.getWidth, img.getHeight, img.getType)
		img.copyData(newImg.getRaster)
		return newImg
	}
  
	/**
	 * Converts BufferedImage to black-white map of colors
	 * @param image		Image to be converted
	 */
  def bwSub(image: BufferedImage) {
  	// Get average color of image and calculate components' sum
  	val avg = getAvgColor(image)
  	val avgSum = avg.getRed + avg.getGreen + avg.getBlue
  	
  	// Loop through every pixel
		for(y <- 0 until image.getHeight; x <- 0 until image.getWidth) {
			// Get pixel color and calclulate components' sum
			val c = new Color(image.getRGB(x, y))
			val cSum = c.getRed + c.getGreen + c.getBlue
			
			// Set under average pixels to black and over average pixels to white
			if(cSum <= avgSum) {
				image.setRGB(x, y, 0)
			} else {
				image.setRGB(x, y, 16777215)
			}
		}
  }
  
  /**
   * Calculates average color of the image
   * @param image		Image to be analyzed
   * @return				Average color as java.awt.Color
   */
  def getAvgColor(image: BufferedImage): Color = {
  	// Variables for each component
  	var r, g, b = 0
		
  	// Loop through all pixels
		for(y <- 0 until image.getHeight; x <- 0 until image.getWidth) {
			// Get color and add its components to sum
			val c = new Color(image.getRGB(x, y))
			r += c.getRed
			g += c.getGreen
			b += c.getBlue
		}
		
  	// Calculate averages for each component
		r = r / (image.getWidth * image.getHeight)
		g = g / (image.getWidth * image.getHeight)
		b = b / (image.getWidth * image.getHeight)
		
		return new Color(r, g, b)
  }
  
  /**
   * Returns value representing most likely match for each subimage
   * @param image		Subimage to be analyzed
   * @param	pos			Position of the image in the fridge
   * @return				Value of Drink enumeration representing the most likely match for given position
   */
  def getMatch(image: BufferedImage, pos: Int): Drink.Value = {
  	// Load models from memory for each possible option
  	val beerModel = ImageIO.read(getClass().getResourceAsStream("/models/" + pos + "/Beer.jpg"))
  	val longModel = ImageIO.read(getClass().getResourceAsStream("/models/" + pos + "/Long.jpg"))
  	val inModel = ImageIO.read(getClass().getResourceAsStream("/models/" + pos + "/In.jpg"))
  	val outModel = ImageIO.read(getClass().getResourceAsStream("/models/" + pos + "/Out.jpg"))

  	// Compare image to each model
  	val beerMatch = matcher(beerModel, image)
  	val longMatch = matcher(longModel, image)
  	val inMatch = matcher(inModel, image)
  	val outMatch = matcher(outModel, image)
  	
  	// Uncomment for debugging purposes
  	//println(pos + ": BEER=" + beerMatch + "% LONG=" + longMatch + "% IN=" + inMatch + "% OUT=" + outMatch)
  	
  	// Following limits are based on empirical study and should be modified if necessary
  	/*
  	 * If the match for empty spot is over 70%, it's the most likely option,
  	 * no matter what other matches were
  	 */
  	if(inMatch > 70.0) {
  		return Drink.EMPTY
  	
  	/*
  	 * If match for beer is over 62% and greater than match for long drink,
  	 * it most likely is beer
  	 */
  	} else if(beerMatch > 62.0 && beerMatch > longMatch) {
  		return Drink.BEER
  	
  	/*
  	 *  If match for long drink is over 65% (and it's also probably greater than match for beer),
  	 *  it most likely is a long drink
  	 */
  	} else if(longMatch > 65.0) {
  		return Drink.LONG
  	
  	// Otherwise it's probably not a drink
  	} else {
  		return Drink.EMPTY
  	}
  }
  
  /**
   * Matches image with model and returns percentage value representing suitability of the match
   * @param model		Image from memory which given image is compared to
   * @param image		subimage to be compared
   * @return				Double value representing percentage value for match (0-100)
   */
  def matcher(model: BufferedImage, image: BufferedImage): Double = {
  	// Calculate one pixel's impact on total percentage
  	val pixelPercentage = 1.0 / (model.getHeight * model.getWidth)
  	var total = 0.0
  	
  	// Loop through all pixels
  	for(y <- 0 until model.getHeight; x <- 0 until model.getWidth) {
  		// Get one component of each image (doesn't matter which since images are grayscale)
  		val modelColor = new Color(model.getRGB(x, y)).getRed.toDouble / 255
  		val imageColor = new Color(image.getRGB(x, y)).getRed.toDouble / 255
  		
  		/*
  		 * Calculate percentage for one pixel (0-100)
  		 * Example 1:
  		 * 		In image, pixel is black (0) and in model it's 0.25
  		 * 		The match is 75%
  		 * Example 2:
  		 * 		In image, pixel is black (0) and in model it's 0.75
  		 * 		The match is 25%
  		 */
  		val matchPercentage = Math.abs(1 - imageColor - modelColor)
  		
  		// Add it to total by weighting pixel match with one pixel's impact
  		total += matchPercentage * pixelPercentage
  	}
  	return total * 100
  }
}
