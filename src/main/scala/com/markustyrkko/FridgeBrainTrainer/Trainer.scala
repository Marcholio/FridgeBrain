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
import java.io.BufferedWriter
import java.io.FileInputStream
import java.io.PrintWriter
import java.io.FileWriter
import scala.xml._

object Trainer extends App {
	
	println("Which position you want to train? (0-19)")
	val selectedPosition = readLine()
	
	// Get all files from trainingData and originals folders
  val trainingData = new File("trainingData/").listFiles().toIterator
  val origins = new File("originals/").listFiles().toIterator
  
  // Produce a map from trainingMapping file which will enable automatical training after initial run
  val dataMap = scala.collection.mutable.Map[String, String]()
  try {
  	val br = new BufferedReader(new FileReader("trainingMapping.txt"))
	  val str = Stream.continually(br.readLine()).takeWhile(_ != null).mkString("\n")
	  
	  // File contains data with format: filename=matching item
	  for(e <- str.split('\n')) {
	  	dataMap(e.split('=')(0)) = e.split('=')(1)
	  }
  } catch {
  	case _: Throwable => 
  }
  
  // Get first image of iterator
  var currentImg = trainingData.next()
  
  // =======MAIN FRAME=========
	val main = new MainFrame()
  main.minimumSize = new Dimension(400, 400)
  main.title = "FridgeBrain trainer"
  main.centerOnScreen()
  main.visible = true
  
  // Wrapper directly inside main frame
  val wrapper = new BoxPanel (Orientation.Vertical)
  
  // ========IMAGE=========
  val imgWrapper = new BoxPanel (Orientation.Horizontal)
  imgWrapper.contents += VStrut(1)
  
  // Image
  val img = new Label()
  img.icon = new ImageIcon(currentImg.toString())
  imgWrapper.contents += img
  
  imgWrapper.contents += VStrut(1)
  
  wrapper.contents += HStrut(1)
  wrapper.contents += imgWrapper
  
  // ========BUTTONS==========
  val btnWrapper = new BoxPanel (Orientation.Horizontal)
  btnWrapper.contents += VStrut(1)
  
  // Read all categories from file
  val categoriesXML = XML.loadFile("resources/categories.xml").child.filter(_.label.equals("category"))
  
  /*
   * Map to contain all categories
   * for example:
   * "Beer: Sandels" -> positions 0-19
   * 							Position X includes Buffer to store all images of Sandels beer in that position as pixel arrays
   */
  val items: collection.mutable.Map[String, Array[Buffer[PixelArray]]] = collection.mutable.Map[String, Array[Buffer[PixelArray]]]()
  for(c <- categoriesXML) {
  	val categoryName = c.attribute("name").get.toString
  	
  	// Create panel for each category so that buttons in that category will be placed vertically
  	val categoryWrapper = new BoxPanel (Orientation.Vertical)
  	categoryWrapper.contents += new Label(categoryName)
  	
  	val brandsXML = c.child.filter(_.label.equals("brand"))
  	
		// Add buttons for each brand and listeners for them
	  for(brand <- brandsXML) {
  		addButton(categoryName + " - " + brand.attribute("name").get.toString(), categoryWrapper)
  		
  		// Array for each position
  		val positions = Array.ofDim[Buffer[PixelArray]](20)
  		
  		// Initialize each position with buffer to save training images
  		for (pos <- positions) {
  			positions(positions.indexOf(pos)) = Buffer[PixelArray]()
  		}
  		
  		// Add positions to map with brand as a key
  		items(categoryName + " - " + brand.attribute("name").get.toString()) = positions
  	}
  	
  	// Add category to buttons wrapper
  	btnWrapper.contents += categoryWrapper
  	btnWrapper.contents += VStrut(1)
  }
  
  // Add button for quitting
  val generalWrapper = new BoxPanel (Orientation.Vertical)
  addButton("quit", generalWrapper)
  btnWrapper.contents += generalWrapper
  btnWrapper.contents += VStrut(1)
  
  // Add buttons wrapper to main wrapper
  wrapper.contents += HStrut(1)
  wrapper.contents += btnWrapper
  wrapper.contents += HStrut(1)
  
  main.contents = wrapper
  
  // =======REACTIONS======
  main.reactions += {
  	case ButtonClicked(b) => {
  		if(!b.text.equals("quit")) {
  			
  			// Get black-white map of current image
  			val img = ImageIO.read(currentImg)
  			val pos = currentImg.toString().split('\\')(1).split('-')(0).toInt
  			Brain.bwSub(img)
  			
  			// Save map to right position buffer
  			items(b.text)(pos) += pixels(img)
  			
  			// Get next image or calculate result
  			if(trainingData.hasNext) {
  				changeCurrentImg()	
  			} else {
  				generateModels()
  			}
  		} else {
  			generateModels()
  		}
  	}
  }
  
  /**
   * Adds button to given wrapper and adds listener to main frame
   * @param name		Text of the button
   * @param wrapper	Wrapper which to button is added to
   */
  def addButton(name: String, wrapper: BoxPanel) {
  	val btn = new Button(name)
		wrapper.contents += HStrut(1)
		wrapper.contents += btn
		main.listenTo(btn)
  }
  
  /**
   * Changes displayed image
   */
  def changeCurrentImg() {
  	if(selectedPosition.equalsIgnoreCase("all")) {
  		currentImg = trainingData.next()
  	} else {
  		do {
  			currentImg = trainingData.next()
  		} while(trainingData.hasNext && !currentImg.toString().startsWith("trainingData\\" + selectedPosition + "-"))
  	}
  	
  	
  	img.icon = new ImageIcon(currentImg.toString())
  }
  
  /**
   * Generates models from given material
   */
  def generateModels() {
  	main.dispose()
  	for(item <- items.keySet) {
  		for(position <- items(item)) {
  			if(position.length > 0) {
  				saveImg(average(position), items(item).indexOf(position) + "/" + item)
  			}
  		}
  	}
  }
  
  /**
   * Gets pixel array from given buffered image
   * @param img		Image to get pixels from
   * @return			PixelArray object containing information of given image as integer
   */
  def pixels(img: BufferedImage): PixelArray = {
  	val width = img.getWidth()
  	val height = img.getHeight()
  	val pix = Array.ofDim[Int](width, height)
  	
  	// Loop thtough all pixels and save value of red component
  	for(y <- 0 until height; x <- 0 until width) {
  		val color = new Color(img.getRGB(x, y))
  		
  		// One component is enough since images are always black-white maps
  		pix(x)(y) = color.getRed
  	}
  	
  	return new PixelArray(pix)
  }
  
  /**
   * Calculates average color for each pixel from given list of pixel arrays
   * @param data		List of pixel arrays to be analyzed
   * @return				Pixel array containing average color for each pixel in given data
   */
  def average(data: Buffer[PixelArray]): PixelArray = {
  	val sumArray = data(0).copy()
  	
  	// Calculate sum for all pixels
  	for(i <- 1 until data.length; y <- 0 until sumArray.pixels(0).length; x <- 0 until sumArray.pixels.length) {
  		sumArray.pixels(x)(y) = sumArray.pixels(x)(y) + data(i).pixels(x)(y)
  	}
  	
  	// Calculate average for all pixels
  	for(arr <- sumArray.pixels; i <- 0 until arr.length) {
  		arr(i) = arr(i) / data.length
  	}
  	
  	return sumArray
  }
  
  /**
   * Saves given pixel array as buffered image with given name
   * @param pixels		Pixel array to be converted to buffered image
   * @param name			Name of the new file relative to current folder
   */
  def saveImg(pixelArr: PixelArray, name: String) {
  	val img = new BufferedImage(pixelArr.pixels.length, pixelArr.pixels(0).length, ImageIO.read(currentImg).getType)
  	
  	// Loop through all pixels and set image color pixel by pixel
  	for(y <- 0 until pixelArr.pixels(0).length; x <- 0 until pixelArr.pixels.length) {
  		val color = Math.min(pixelArr.pixels(x)(y), 255)
  		val r = color << 16
  		val g = color << 8
  		img.setRGB(x, y, r + g + color)
  	}
  	
  	// Save image
  	ImageIO.write(img, "jpg", new File("./resources/models/" + name + ".jpg"))
  }
}