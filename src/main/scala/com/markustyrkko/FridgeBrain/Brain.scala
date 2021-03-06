package com.markustyrkko.FridgeBrain

import javax.imageio.ImageIO
import java.io.File
import java.io.ByteArrayOutputStream
import java.awt.Color
import java.awt.image.BufferedImage
import com.sun.prism.BasicStroke
import java.io.ByteArrayInputStream
import java.io.IOException
import scala.util.parsing.json.JSONObject
import com.google.gson.stream.JsonReader
import com.google.gson.JsonStreamParser
import java.io.InputStreamReader
import com.google.gson.JsonObject
import java.io.BufferedReader
import scala.util.parsing.json.JSONArray
import com.google.gson.JsonArray
import com.google.gson.JsonParser
import scala.collection.mutable.Map
import scala.collection.mutable.Buffer
import scala.xml.XML

object Brain extends App {
	
	// Uncomment to test as separate application without server
//	private val baos = new ByteArrayOutputStream()
//	ImageIO.write(ImageIO.read(new File("Fridge.jpg")), "jpg", baos)
//	println(analyze(baos.toByteArray()))
	
	private var contents: Map[FridgeItem.Value, Int] = null
	private var categories: Map[FridgeItem.Value, Buffer[String]] = null
	
	/**
	 * Analyzes given image and returns result of it's content as String
	 * @param bytes		Array of bytes from http server containing image data
	 * @return				String containing information about image content
	 */
	def analyze(bytes: Array[Byte]): String = {
		// Convert bytes to BufferedImage
		val img = getImage(bytes)
		
		// Check if image is valid to begin with
		if(getAvgColor(img).getRed < 73) return "Invalid image"
		
		// Image dimensions
	  val height = img.getHeight
	  val width = img.getWidth
	  
	  val sectors = getSectors()
	  
	  categories = getCategories()
	  
	  contents = Map[FridgeItem.Value, Int]()
	  
	  // Variables for subimage position
	  var y = 0
	  var pos = 0
	  
	  for(row <- sectors.keySet.toList.reverse) {
	  	var x = 0
  		val rowHeight = sectors(row)._1
	  	val widthIterator = sectors(row)._2.toIterator
	  	while(widthIterator.hasNext) {
	  		val sectorWidth = widthIterator.next
	  		
	  		// Get separate subimage
		  	val subImg = copyImg(img.getSubimage(x, y, sectorWidth, rowHeight))
				
		  	// Convert subimage to black-white map
		  	bwSub(subImg)
		  	
		  	// Compare map to known models of each drink and empty space
				val result = getMatch(subImg, pos)

				increaseCount(result)
				
	  		x += sectorWidth
	  		pos += 1
	  	}
	  	y += rowHeight
	  }

		return getMessage()
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
	 * Reads sector information and returns Map containing information about each row
	 * @return		Map with row name as key and Tuple(Height, List[Width]) as value
	 */
	def getSectors(): Map[String, (Int, List[Int])] = {
		// Read json to String
		val sectors = getClass().getResourceAsStream("/sectors.json")
		val reader = new BufferedReader(new InputStreamReader(sectors, "UTF-8"))
		var line = reader.readLine()
		var jsonString = ""
		while(line != null) {
			jsonString += line
			line = reader.readLine()
		}
		reader.close()
		
		// Parse Json to Map
		var result = Map[String, (Int, List[Int])]()
		val parser = new JsonParser
		val jsonArray = parser.parse(jsonString).getAsJsonArray
		for(i <- 0 until jsonArray.size()) {
			val row = jsonArray.get(i).getAsJsonObject
			val height = row.get("height").getAsInt
			
			var j = 1
			var width = row.get("width" + j)
			var widthList = List[Int]()
			
			// Loop through widths
			while(width != null) {
				widthList = widthList :+ width.getAsInt
				j += 1
				width = row.get("width" + j)
			}
			result("row" + i) = (height, widthList)
		}
		return result
	}
	
	/**
	 * Reads category information from categories.xml file
	 * @return	Map with category as a key and list of brand names as String as value
	 */
	def getCategories(): Map[FridgeItem.Value, Buffer[String]] = {
		val result = Map[FridgeItem.Value, Buffer[String]]()
		
		val resource = getClass().getResourceAsStream("/categories.xml")
		val reader = new BufferedReader(new InputStreamReader(resource, "UTF-8"))
		var line = reader.readLine()
		var xmlString = ""
		while(line != null) {
			xmlString += line
			line = reader.readLine()
		}
		reader.close()
		
		val categoriesXML = XML.loadString(xmlString).child.filter(_.label.equals("category"))
		for (c <- categoriesXML) {
			val item = FridgeItem.withName(c.attribute("name").get.toString())
			result(item) = Buffer[String]()
			
			val brandsXML = c.child.filter(_.label.equals("brand"))
			for (b <- brandsXML) {
				result(item) += b.attribute("name").get.toString()
			}
		}
		return result
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
  def getMatch(image: BufferedImage, pos: Int): FridgeItem.Value = {
  	val matches = Map[FridgeItem.Value, Buffer[(String, Double)]]()
  	
  	// Load models from memory for each possible option and compare image to it
  	for(item <- FridgeItem.values) {
  		for(brand <- categories(item)) {
  			try {
	  			val model = ImageIO.read(new File("../models/" + pos + "/" + item.toString() + " - " + brand + ".jpg"))
	  			if(matches.keySet.contains(item)) {
	  				matches(item) +=	Tuple2(brand, matcher(model, image))
	  			} else {
	  				matches(item) = Buffer[(String, Double)](Tuple2(brand, matcher(model, image)))
	  			}
	  		} catch {
	  			case _: Throwable => matches(item) = Buffer[(String, Double)](Tuple2(brand, 0.0))
	  		}
  		}
  	}

  	// Uncomment for debugging purposes
  	//println(pos + ": " + matches.toString().replace("ArrayBuffer", ""))
  	
  	// Following limits are based on empirical study and should be modified if necessary
  	// Beer
  	if (matches(FridgeItem.Beer).maxBy(_._2)._2 > 69 && matches(FridgeItem.Empty).head._2 < 65) {
  		updateModel("Beer - " + matches(FridgeItem.Beer).maxBy(_._2)._1, image, pos)
  		return FridgeItem.Beer
  	
  	// Butter
  	} else if (matches(FridgeItem.Butter).maxBy(_._2)._2 > 70 && matches(FridgeItem.Empty).head._2 < 80) {
  		updateModel("Butter - " + matches(FridgeItem.Butter).maxBy(_._2)._1, image, pos)
  		return FridgeItem.Butter
  	
  	// Cheese
  	} else if (matches(FridgeItem.Cheese).maxBy(_._2)._2 > 55) {
  		updateModel("Cheese - " + matches(FridgeItem.Cheese).maxBy(_._2)._1, image, pos)
  		return FridgeItem.Cheese
  	
  	// Ham
  	} else if (matches(FridgeItem.Ham).maxBy(_._2)._2 > 78) {
  		updateModel("Ham - " + matches(FridgeItem.Ham).maxBy(_._2)._1, image, pos)
  		return FridgeItem.Ham
  		
  	// Jam
  	} else if (matches(FridgeItem.Jam).maxBy(_._2)._2 > 70 && matches(FridgeItem.Jam).maxBy(_._2)._2 > matches(FridgeItem.Empty).head._2) {
  		updateModel("Jam - " + matches(FridgeItem.Jam).maxBy(_._2)._1, image, pos)
  		return FridgeItem.Jam
  		
  	// Long_drink
  	} else if (matches(FridgeItem.Long_drink).maxBy(_._2)._2 > 75) {
  		updateModel("Long_drink - " + matches(FridgeItem.Long_drink).maxBy(_._2)._1, image, pos)
  		return FridgeItem.Long_drink
  		
  	// Processed cheese
  	} else if (matches(FridgeItem.Processed_cheese).maxBy(_._2)._2 > 84) {
  		updateModel("Processed_cheese - " + matches(FridgeItem.Processed_cheese).maxBy(_._2)._1, image, pos)
  		return FridgeItem.Processed_cheese
  	
  	// Empty
  	} else {
  		updateModel("Empty - " + matches(FridgeItem.Empty).maxBy(_._2)._1, image, pos)
  		return FridgeItem.Empty
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
  
  /**
   * The learning method of the brain. Modifies existing model with given sub image with 1:50 ratio.
   */
  def updateModel(fileName: String, subImg: BufferedImage, pos: Int) {
  	// Get existing model from resources
  	val model = ImageIO.read(new File(System.getProperty("user.dir") + "/models/" + pos + "/" + fileName + ".jpg"))
  	val newModel = copyImg(model)
  	
  	// Loop through all pixels
  	for(y <- 0 until model.getHeight; x <- 0 until model.getWidth) {
  		// Get new color with 98% of original model and 2% of new image and set it to new model
  		val color = (new Color(model.getRGB(x, y)).getRed * 0.98 + new Color(subImg.getRGB(x, y)).getRed * 0.02).toInt
  		newModel.setRGB(x, y, (new Color(color, color, color)).getRGB)
  		
  	}
  	ImageIO.write(newModel, "jpg", new File(System.getProperty("user.dir") + "/models/" + pos + "/" + fileName + ".jpg"))
  }
  
  /**
   * Handles result matching and calls increment matching given result
   * @param result		Result of matching
   */
  def increaseCount(result: FridgeItem.Value) {
  	// Beers and long drinks need increment by two because they are in two rows
  	if(result.toString().equals("Beer") || result.toString().equals("Long_drink")) {
  		if(!contents.keySet.contains(result)) contents(result) = 2
  		else contents(result) += 2
  	} else {
  		if(!contents.keySet.contains(result)) contents(result) = 1
  		else contents(result) += 1
  	}
  }
  
  /**
   * Returns message that can be sent to user via Telegram
   * @return		Message containing contents of fridge
   */
  def getMessage(): String = {
  	var msg = "Contents:\n"
  	for(item <- contents.keySet) {
  		if(!item.toString.equals("Empty") && !item.toString.equals("Out")) {
  			msg += item.toString.replace("_", " ") + ": " + contents(item) + "\n"
  		}
  	}
  	msg += "Missing:\n"
  	for(item <- FridgeItem.values) {
  		if(!contents.keySet.contains(item) && !item.toString().equals("Out")) {
  			msg += item.toString.replace("_", " ") + "\n"
  		}
  	}
  	return msg
  }
}
