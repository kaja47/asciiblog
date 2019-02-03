package asciiblog

import java.awt.image. { BufferedImage, Kernel, ConvolveOp }
import java.awt.{ Color, Font, AlphaComposite, RenderingHints => RH }
import javax.imageio. { ImageIO, IIOImage, ImageWriteParam }
import java.io.File

object ImageTools {

  def saveJpg(f: File, img: BufferedImage, quality: Float) = {
    val ios = ImageIO.createImageOutputStream(f)
    val writer = ImageIO.getImageWritersByFormatName("jpeg").next()
    val params = writer.getDefaultWriteParam()
    params.setCompressionMode(ImageWriteParam.MODE_EXPLICIT)
    params.setCompressionQuality(quality)
    writer.setOutput(ios)
    writer.write(null, new IIOImage(img, null, null), params)
    writer.dispose()
  }

  def createCardImage(title: String, subtitle: String, w: Int = 480, h: Int = 240, background: Color = Color.WHITE, foreground: Color = Color.BLACK): BufferedImage = {
    val img = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    val g = img.createGraphics()
    g.setRenderingHint(RH.KEY_TEXT_ANTIALIASING, RH.VALUE_TEXT_ANTIALIAS_ON)

    g.setColor(background)
    g.fillRect(0, 0, w, h)

    g.setColor(foreground)

    val line1 = title
    val line2 = subtitle

    val font1 = new Font("Lato Medium", Font.PLAIN, 36)
    val font2 = new Font("Lato Medium", Font.PLAIN, 14)

    val fm = g.getFontMetrics(font1)
    val ratio = math.min(1, (w - 12f) / fm.stringWidth(line1))

    g.setFont(font1.deriveFont(font1.getSize * ratio))
    val w1 = g.getFontMetrics.stringWidth(line1)
    val h1 = g.getFontMetrics.getAscent

    val w2 = g.getFontMetrics(font2).stringWidth(line2)
    val h2 = (g.getFontMetrics(font2).getAscent * 1.5).toInt

    val totalHeight = h1 + h2

    val y1 = h/2 - totalHeight/2 + (h1*0.9).toInt
    val x1 = math.min((w-w1)/2, (w-w2)/2)

    val y2 = y1+h2
    val x2 = x1

    g.drawString(line1, x1, y1)

    //g.drawRect(x1, h/2 - totalHeight/2, 1, 1)
    //g.drawRect(x1, y1, 1, 1)
    //g.drawRect(x2, y2, 1, 1)

    g.setFont(font2)
    g.drawString(line2, x2, y2)

    g.dispose()

    img
  }

  def resizeImage(src: BufferedImage, targetWidth: Int, targetHeight: Int = -1, leeway: Double, sharpenStrength: Float): BufferedImage = {
    require(leeway >= 1.0)

    if (targetHeight <= 0 && targetWidth * leeway > src.getWidth) return src // never scale the image up
    if (targetHeight * leeway > src.getHeight && targetWidth * leeway > src.getWidth) return src

    val (width, height) =
      if (targetHeight > 0) (targetWidth, targetHeight)
      else (targetWidth, (1.0 * targetWidth / src.getWidth * src.getHeight).toInt)

    val zoom = math.min(1.0 * src.getWidth / width, 1.0 * src.getHeight / height)
    val wz = math.max(1, (width * zoom).toInt)
    val hz = math.max(1, (height * zoom).toInt)
    val x = (src.getWidth - wz) / 2
    val y = (src.getHeight - hz) / 2
    val crop = src.getSubimage(x, y, wz, hz)
    sharpen(progressiveResize(crop, width, height), sharpenStrength)
  }

  /** adapted from https://github.com/coobird/thumbnailator/blob/master/src/main/java/net/coobird/thumbnailator/resizers/ProgressiveBilinearResizer.java */
  private def progressiveResize(src: BufferedImage, width: Int, height: Int): BufferedImage = {
    val tpe = if (src.getType == BufferedImage.TYPE_CUSTOM) BufferedImage.TYPE_INT_ARGB else src.getType
    val dest = new BufferedImage(width, height, tpe)

    var (currentWidth, currentHeight) = (src.getWidth, src.getHeight)

    if ((width * 2 >= currentWidth) && (height * 2 >= currentHeight)) {
      val g = dest.createGraphics()
      g.drawImage(src, 0, 0, width, height, null)
      g.dispose()
      return dest
    }

    val tmp = new BufferedImage(currentWidth, currentHeight, dest.getType)

    val g = tmp.createGraphics()
    g.setRenderingHint(RH.KEY_INTERPOLATION, RH.VALUE_INTERPOLATION_BILINEAR)
    g.setComposite(AlphaComposite.Src)

    var (startWidth, startHeight) = (width, height)

    while (startWidth < currentWidth && startHeight < currentHeight) {
      startWidth *= 2
      startHeight *= 2
    }

    currentWidth = startWidth / 2
    currentHeight = startHeight / 2

    g.drawImage(src, 0, 0, currentWidth, currentHeight, null)

    while ((currentWidth >= width * 2) && (currentHeight >= height * 2)) {
      currentWidth /= 2
      currentHeight /= 2

      if (currentWidth < width) { currentWidth = width }
      if (currentHeight < height) { currentHeight = height }

      g.drawImage(
          tmp,
          0, 0, currentWidth, currentHeight,
          0, 0, currentWidth * 2, currentHeight * 2,
          null
      )
    }

    g.dispose()

    val destg = dest.createGraphics()
    destg.drawImage(tmp, 0, 0, width, height, 0, 0, currentWidth, currentHeight, null)
    destg.dispose()

    dest
  }

  def sharpen(src: BufferedImage, strength: Float): BufferedImage = {
    require(strength >= 0f && strength < 1f)
    if (strength == 0f) return src
    val sharpenKernel = Array.fill[Float](9)(-strength)
    sharpenKernel(4) = 8 * strength + 1
    val kernel = new Kernel(3, 3, sharpenKernel)
    val op = new ConvolveOp(kernel, ConvolveOp.EDGE_NO_OP, null)
    op.filter(src, null)
  }

}
