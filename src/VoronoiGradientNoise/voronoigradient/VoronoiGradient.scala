package voronoigradient

import java.util.Random
import geom.V2
import geom.V3
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File

object VoronoiGradient {
  def main(args: Array[String]) = {
    val rand = new Random(1319284)
    val nPts = 256
    var pts = Vector.fill(nPts)(V2(rand.nextGaussian, rand.nextGaussian))
    def step() = {
      val (inner, outer) = pts.partition { x => x.norm < 1.2 }
      val near = inner.map(p => p -> (pts.sortBy(x => (p - x).norm2).drop(1).view)).toMap
      def nearest(p: V2) = {
        pts.minBy { v => (p - v).norm2 }
      }
      def corner(p1: V2, p2: V2, p3: V2) = {
        val Vector(v1, v2, v3) = Vector(p1, p2, p3).sortBy { v => v.x }.sortBy { v => v.y }
        geom.center(p1, p2, p3)
      }
      val tiles = inner.map(p => p -> {
        val n = near(p)(0)
        var ns = Vector[V2]()
        while (ns.lastOption != Some(n)) {
          val o = ns.lastOption.getOrElse(n)
          val it = near(p).find { nx =>
            if (nx == o || ((p - nx) cp (p - o)) < 0) false
            else {
              val c = corner(p, o, nx)
              val n = (nearest(c) - c).norm
              val err = V3(
                  n - (p - c).norm,
                  n - (o - c).norm,
                  n - (nx - c).norm).norm2
              err < 0.0000000000001
            }
          }.get
          ns :+= it
        }
        val neighs = ns.zip(ns.tail :+ ns.head)
        val corners = neighs.map { case (p1, p2) => corner(p, p1, p2) }
        corners
      }).toMap
      (inner, outer, tiles)
    }
    for (i <- 0 until 8) {
      val (inner, outer, tiles) = step()
      val sz = 64
      val img = new BufferedImage(sz, sz, BufferedImage.TYPE_INT_ARGB)
      for (xP <- 0 until sz) {
        for (yP <- 0 until sz) {
          val x = (xP.toDouble / sz - 0.5) * 2 * 1.2 * 1.1
          val y = (yP.toDouble / sz - 0.5) * 2 * 1.2 * 1.1
          val v = V2(x, y)
          val t = pts.minBy { x => (v - x).norm2 }
          if (inner.contains(t)) {
            val col = t.hashCode & 0x00FFFFFF | 0xFF000000
            img.setRGB(xP, yP, col)
          }
        }
      }
      println(s"$i")
      ImageIO.write(img, "png", new File(s"res$i.png"))
      pts = inner.map { p =>
        val tile = tiles(p)
        tile.reduce(_ + _) / tile.length
      } ++ outer
    }
    val (inner, outer, tiles) = step()
    var cornerCache = Map[V2, V2]()
    def gradient(corner: V2) = {
      if (!cornerCache.contains(corner)) {
        cornerCache = cornerCache + (corner -> V2(rand.nextGaussian(), rand.nextGaussian()))
      }
      cornerCache(corner)
    }
    val sz = 2048
    val img = new BufferedImage(sz, sz, BufferedImage.TYPE_INT_ARGB)
    for (xP <- 0 until sz) {
      for (yP <- 0 until sz) {
        val x = (xP.toDouble / sz - 0.5) * 2 * 1.2 * 1.1
        val y = (yP.toDouble / sz - 0.5) * 2 * 1.2 * 1.1
        val v = V2(x, y)
        val t = pts.minBy { x => (v - x).norm2 }
        if (inner.contains(t)) {
          val poly = tiles(t)
          val parts = poly.inits.toVector.init zip poly.tails.toVector.init.reverse
          val (sum, weight) = parts.map { case (begin, end) =>
            val us = end.head
            val them = end.tail ++ begin.init
            val edges = them zip them.tail
            val weight = edges.map { case (v0, v1) => (v0 - v) cp (v1 - v) }.product
            val displace = (v - us) * gradient(us)
            (displace * weight, weight)
          }.reduce[(Double, Double)] { case ((a0, b0), (a1, b1)) => (a0 + a1, b0 + b1) }
          val q = sum / weight * 2
          val c = ((q + 0.5) * 256).toInt max 0 min 255
          val col = (0xFF << 24) | (c << 16) | (c << 8) | c
          img.setRGB(xP, yP, col)
        }
      }
    }
    ImageIO.write(img, "png", new File(s"res.png"))
    ()
  }
}