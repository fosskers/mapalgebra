package mapalgebra

import java.util.concurrent.TimeUnit

import geotrellis.raster.{ Tile, MultibandTile }
import geotrellis.raster.render.{ColorMap, ColorRamp, ColorRamps, Png}
import geotrellis.raster.histogram.{Histogram, StreamingHistogram}
import geotrellis.raster.io.geotiff.{ SinglebandGeoTiff, MultibandGeoTiff }
import org.openjdk.jmh.annotations._

// --- //

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class IOBench {

  @Benchmark
  def singleband: Tile = SinglebandGeoTiff("/home/colin/code/haskell/mapalgebra/data/gray512.tif").tile
  // def singleband: Tile = SinglebandGeoTiff("../data/gray512.tif").tile
  @Benchmark
  def multiband: MultibandTile = MultibandGeoTiff("/home/colin/code/haskell/mapalgebra/data/512x512.tif").tile
  // def multiband: MultibandTile = MultibandGeoTiff("../data/512x512.tif").tile

  /** PROs:
    *   - Simple, one method call.
    *   - Maintains `Extent` and `CRS` information in GeoTIFF metadata.
    * CONs:
    *   - Pixel type isn't transparent. Is this writing bytes? Ints? Doubles?
    *   - Returns `Unit` instead of safer `IO[Unit]`.
    */
  def writeItGray(tile: SinglebandGeoTiff): Unit = tile.write("output/path/image.tif")

  /** Renders straight to an encoded `Png` - there is no "coloured Raster" stage.
    * A bit tricky to discover the precise things that need to be called in order to
    * create a `ColorMap`.
    */
  def colourIt(tile: Tile): Png = {
    val cr: ColorRamp = ColorRamps.Viridis
    val ht: Histogram[Double] = StreamingHistogram.fromTile(tile)
    val cm: ColorMap = ColorMap.fromQuantileBreaks(ht, cr)

    tile.renderPng(cm)
  }
}
