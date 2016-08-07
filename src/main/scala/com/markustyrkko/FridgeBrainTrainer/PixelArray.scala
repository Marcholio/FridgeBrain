package com.markustyrkko.FridgeBrainTrainer

class PixelArray(p: Array[Array[Int]]) {
	val pixels = p
	
	def copy(): PixelArray = {
		return new PixelArray(pixels)
	}
}