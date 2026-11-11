package com.github.luben.zstd

import org.scalatest.flatspec.AnyFlatSpec

import java.io._
import java.nio._
import scala.io._
import scala.util.Using

class ZstdDictSpec extends AnyFlatSpec {

  def source = Source.fromFile("src/test/resources/xml")(Codec.ISO8859).map{_.toByte}

  def train(legacy: Boolean, sampleSize: Int): Array[Byte] = {
    val src = source.sliding(1024, 1024).take(sampleSize).map(_.toArray)
    val trainer = new ZstdDictTrainer(1024 * sampleSize, 32 * sampleSize)
    for (sample <- src) {
      trainer.addSample(sample)
    }
    val dict = trainer.trainSamples(legacy)
    assert(Zstd.getDictIdFromDict(dict) != 0)
    dict
  }

  def wrapInDirectByteBuffer(array: Array[Byte]): ByteBuffer = {
    //use a slightly oversized buffer and a nonzero offset to test we use the position/limit too
    val bb = ByteBuffer.allocateDirect(array.length + 13);
    bb.limit(bb.capacity())
    bb.position(7)
    bb.limit(bb.position() + array.length)
    bb.put(array)
    bb.flip()
    bb
  }

  "Zstd" should "report error when failing to make a dict" in {
    val src = source.sliding(28, 28).take(4).map(_.toArray)
    val trainer = new ZstdDictTrainer(1024 * 1024, 32 * 1024)
    for (sample <- src) {
      trainer.addSample(sample)
    }
    intercept[com.github.luben.zstd.ZstdException] {
      trainer.trainSamples(false)
    }
  }

  val input = source.toArray
  val legacyS = List(true, false)
  val levels = List(1)
  for {
       legacy <- legacyS
       dict = train(legacy, 1024)
       dict2 = train(legacy, 512)
       dictInDirectByteBuffer = wrapInDirectByteBuffer(dict)
       level <- levels
  } {

    "Zstd" should s"round-trip compression/decompression with dict at level $level with legacy $legacy" in {
      val compressed = Zstd.compressUsingDict(input, dict, level)
      val decompressed = Zstd.decompress(compressed, dict, input.length)
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"round-trip compression/decompression ByteBuffers with dict at level $level with legacy $legacy" in {
      val size = input.length
      val inBuf = ByteBuffer.allocateDirect(size)
      inBuf.put(input)
      inBuf.flip()
      val compressed = ByteBuffer.allocateDirect(Zstd.compressBound(size).toInt);
      Zstd.compress(compressed, inBuf, dict, level)
      compressed.flip()
      val decompressed = ByteBuffer.allocateDirect(size)
      Zstd.decompress(decompressed, compressed, dict)
      decompressed.flip()
      val out = new Array[Byte](decompressed.remaining)
      decompressed.get(out)
      assert(input.toSeq == out.toSeq)
    }

    it should s"round-trip compression/decompression with fast dict at level $level with legacy $legacy" in {
      val size = input.length
      val cdict = new ZstdDictCompress(dict, level)
      val compressed = Zstd.compress(input, cdict)
      cdict.close
      val ddict = new ZstdDictDecompress(dict)
      val decompressed = Zstd.decompress(compressed, ddict, size)
      ddict.close
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"round-trip compression/decompression in place with fast dict at level $level with legacy $legacy" in {
      val size = input.length
      val cdict = new ZstdDictCompress(dict, level)
      val compressed = new Array[Byte](size)
      val compressed_size = Zstd.compress(compressed, input, cdict)
      cdict.close
      val ddict = new ZstdDictDecompress(dict)
      val decompressed = new Array[Byte](size)
      Zstd.decompressFastDict(decompressed, 0, compressed, 0, compressed_size.toInt, ddict)
      ddict.close
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"round-trip compression/decompression ByteBuffers with fast dict at level $level with byReference $legacy" in {
      val byReference = legacy // Reuse the variance flag here.
      val size = input.length
      val inBuf = ByteBuffer.allocateDirect(size)
      inBuf.put(input)
      inBuf.flip()
      val cdict = new ZstdDictCompress(dictInDirectByteBuffer, level, byReference)
      val compressed = ByteBuffer.allocateDirect(Zstd.compressBound(size).toInt);
      Zstd.compress(compressed, inBuf, cdict)
      compressed.flip()
      cdict.close
      val ddict = new ZstdDictDecompress(dictInDirectByteBuffer, byReference)
      val decompressed = ByteBuffer.allocateDirect(size)
      Zstd.decompress(decompressed, compressed, ddict)
      decompressed.flip()
      ddict.close
      val out = new Array[Byte](decompressed.remaining)
      decompressed.get(out)
      assert(Zstd.getDictIdFromFrameBuffer(compressed) == Zstd.getDictIdFromDictDirect(dictInDirectByteBuffer))
      assert(input.toSeq == out.toSeq)
    }

    it should s"round-trip compression/decompression with byte[]/fast dict at level $level with legacy $legacy" in {
      val size = input.length
      val compressed = Zstd.compressUsingDict(input, dict, level)
      val ddict = new ZstdDictDecompress(dict)
      val decompressed = Zstd.decompress(compressed, ddict, size)
      ddict.close
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"round-trip compression/decompression with fast/byte[] dict at level $level with legacy $legacy" in {
      val size = input.length
      val cdict = new ZstdDictCompress(dict, 0, dict.size, level)
      val compressed = Zstd.compress(input, cdict)
      cdict.close
      val decompressed = Zstd.decompress(compressed, dict, size)
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"compress with a byte[] and decompress with a ByteBuffer using byte[] dict $level with legacy $legacy" in {
      val size = input.length
      val compressed = Zstd.compressUsingDict(input, dict, level)
      val compressedBuffer = ByteBuffer.allocateDirect(compressed.size)
      compressedBuffer.put(compressed)
      compressedBuffer.limit(compressedBuffer.position())
      compressedBuffer.flip()

      val decompressedBuffer = Zstd.decompress(compressedBuffer, dict, size)
      val decompressed = new Array[Byte](size)
      decompressedBuffer.get(decompressed)
      assert(Zstd.getDictIdFromFrameBuffer(compressedBuffer) == Zstd.getDictIdFromDict(dict))
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"compress with a byte[] and decompress with a ByteBuffer using fast dict $level with legacy $legacy" in {
      val size = input.length
      val cdict = new ZstdDictCompress(dict, 0, dict.size, level)
      val compressed = Zstd.compress(input, cdict)
      val compressedBuffer = ByteBuffer.allocateDirect(compressed.size)
      compressedBuffer.put(compressed)
      compressedBuffer.flip()
      cdict.close

      val ddict = new ZstdDictDecompress(dict)
      val decompressedBuffer = Zstd.decompress(compressedBuffer, ddict, size)
      val decompressed = new Array[Byte](size)
      decompressedBuffer.get(decompressed)
      ddict.close
      assert(Zstd.getDictIdFromFrameBuffer(compressedBuffer) == Zstd.getDictIdFromDict(dict))
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"compress with a ByteBuffer and decompress with a byte[] using fast dict $level with legacy $legacy" in {
      val size = input.length
      val cdict = new ZstdDictCompress(dict, 0, dict.size, level)
      val inputBuffer = ByteBuffer.allocateDirect(size)
      inputBuffer.put(input)
      inputBuffer.flip()
      val compressedBuffer: ByteBuffer = Zstd.compress(inputBuffer, cdict)
      val compressed = new Array[Byte](compressedBuffer.limit() - compressedBuffer.position())
      compressedBuffer.get(compressed)
      cdict.close
      val ddict = new ZstdDictDecompress(dict)
      val decompressed = Zstd.decompress(compressed, ddict, size)
      ddict.close
      assert(Zstd.getDictIdFromFrameBuffer(compressedBuffer) == Zstd.getDictIdFromDict(dict))
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"compress with a ByteBuffer and decompress with a byte[] using byte[] dict $level with legacy $legacy" in {
      val size = input.length
      val cdict = new ZstdDictCompress(dict, 0, dict.size, level)
      val inputBuffer = ByteBuffer.allocateDirect(size)
      inputBuffer.put(input)
      inputBuffer.flip()
      val compressedBuffer: ByteBuffer = Zstd.compress(inputBuffer, dict, level)
      val compressed = new Array[Byte](compressedBuffer.limit() - compressedBuffer.position())
      compressedBuffer.get(compressed)
      cdict.close
      val decompressed = Zstd.decompress(compressed, dict, size)
      assert(Zstd.getDictIdFromFrameBuffer(compressedBuffer) == Zstd.getDictIdFromDict(dict))
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"round-trip streaming compression/decompression with byte[] dict with legacy $legacy " in {
      val size  = input.length
      val os    = new ByteArrayOutputStream(Zstd.compressBound(size.toLong).toInt)
      val zos   = new ZstdOutputStream(os, 1)
      zos.setDict(dict)
      val block = 128 * 1024
      var ptr   = 0
      while (ptr < size) {
        val chunk = if (size - ptr > block) block else size - ptr
        zos.write(input, ptr, chunk)
        ptr += chunk
      }
      zos.flush
      zos.close
      val compressed = os.toByteArray
      // now decompress
      val is    = new ByteArrayInputStream(compressed)
      val zis   = new ZstdInputStream(is)
      zis.setDict(dict)
      val output= Array.fill[Byte](size)(0)
      ptr       = 0

      while (ptr < size) {
        val chunk = if (size - ptr > block) block else size - ptr
        zis.read(output, ptr, chunk)
        ptr += chunk
      }
      zis.close
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == output.toSeq)
    }

    it should s"round-trip streaming compression/decompression with fast dict with legacy $legacy " in {
      val size  = input.length
      val cdict = new ZstdDictCompress(dict, 0, dict.size, 1)
      val os    = new ByteArrayOutputStream(Zstd.compressBound(size.toLong).toInt)
      val zos   = new ZstdOutputStream(os, 1)
      zos.setDict(cdict)
      val block = 128 * 1024
      var ptr   = 0
      while (ptr < size) {
        val chunk = if (size - ptr > block) block else size - ptr
        zos.write(input, ptr, chunk)
        ptr += chunk
      }
      zos.close
      val compressed = os.toByteArray

      // now decompress
      val ddict = new ZstdDictDecompress(dict)
      val is    = new ByteArrayInputStream(compressed)
      val zis   = new ZstdInputStream(is)
      zis.setDict(ddict)
      val output= Array.fill[Byte](size)(0)
      ptr       = 0

      while (ptr < size) {
        val chunk = if (size - ptr > block) block else size - ptr
        zis.read(output, ptr, chunk)
        ptr += chunk
      }
      zis.close
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == output.toSeq)
    }

    it should s"round-trip streaming compression/decompression with multiple fast dicts with legacy $legacy " in {
      // given: compress using first one dictionary, then another
      val cdict = new ZstdDictCompress(dict, 0, dict.length, 1)
      val cdict2 = new ZstdDictCompress(dict2, 0, dict2.length, 1)

      val compressedWithDict1 = compressWithDict(cdict)
      val compressedWithDict2 = compressWithDict(cdict2)

      // when: decompress with the both dictionaries configured and multiple dict references enabled
      val ddict = new ZstdDictDecompress(dict)
      val ddict2 = new ZstdDictDecompress(dict2)

      val dicts = ddict::ddict2::Nil
      val uncompressed1 = uncompressWithMultipleDicts(compressedWithDict1, dicts)
      val uncompressed2 = uncompressWithMultipleDicts(compressedWithDict2, dicts)

      // then: both compressed inputs decompressed successfully
      assert(uncompressed1.toSeq == input.toSeq)
      assert(Zstd.getDictIdFromFrame(compressedWithDict1) == Zstd.getDictIdFromDict(dict))

      assert(uncompressed2.toSeq == input.toSeq)
      assert(Zstd.getDictIdFromFrame(compressedWithDict2) == Zstd.getDictIdFromDict(dict2))
    }

    it should s"round-trip streaming compression/decompression with multiple fast dicts with legacy $legacy and disabled multiple dict references" in {
      // given: compress using first one dictionary, then another
      val cdict = new ZstdDictCompress(dict, 0, dict.length, 1)
      val cdict2 = new ZstdDictCompress(dict2, 0, dict2.length, 1)

      val compressedWithDict1 = compressWithDict(cdict)
      val compressedWithDict2 = compressWithDict(cdict2)

      // when: decompress with the both dictionaries configured and multiple dict references disabled
      //       -> should be used only the second one
      val ddict = new ZstdDictDecompress(dict)
      val ddict2 = new ZstdDictDecompress(dict2)

      val dicts = ddict :: ddict2 :: Nil
      val uncompressed2 = uncompressWithMultipleDicts(compressedWithDict2, dicts, multipleDdicts = false)

      // then: decompression of compressed with the first dict should fail with dictionary mismatch,
      //       the second one should be decompressed successfully
      val caughtException = intercept[ZstdIOException] {
        uncompressWithMultipleDicts(compressedWithDict1, dicts, multipleDdicts = false)
      }
      assert(caughtException.getMessage == "Dictionary mismatch")

      assert(uncompressed2.toSeq == input.toSeq)
      assert(Zstd.getDictIdFromFrame(compressedWithDict2) == Zstd.getDictIdFromDict(dict2))
    }

    def compressWithDict(cdict: ZstdDictCompress): Array[Byte] = {
      val os = new ByteArrayOutputStream(Zstd.compressBound(input.length.toLong).toInt)
      Using(new ZstdOutputStream(os, 1)) { zos =>
        zos.setDict(cdict)
        zos.write(input)
      }
      os.toByteArray
    }

    def uncompressWithMultipleDicts(
      compressed: Array[Byte],
      dicts: List[ZstdDictDecompress],
      multipleDdicts: Boolean = true
    ): Array[Byte] = {
      Using.resources(
        new ZstdInputStream(new ByteArrayInputStream(compressed))
          .setRefMultipleDDicts(multipleDdicts),
        new ByteArrayOutputStream()
      ) { (zis, os) =>
        dicts.foreach(zis.setDict)

        zis.transferTo(os)
        os.toByteArray
      }
    }

    it should s"round-trip streaming ByteBuffer compression/decompression with byte[] dict with legacy $legacy" in {
      val size  = input.length
      val os    = ByteBuffer.allocateDirect(Zstd.compressBound(size.toLong).toInt)
      // compress
      val ib    = ByteBuffer.allocateDirect(size)
      ib.put(input)
      ib.flip

      val osw = new ZstdDirectBufferCompressingStream(os, 1)
      osw.setDict(dict)
      osw.compress(ib)
      osw.flush
      osw.close
      os.flip

      assert(Zstd.getDictIdFromFrameBuffer(os) == Zstd.getDictIdFromDict(dict))

      // now decompress
      val zis   = new ZstdDirectBufferDecompressingStream(os)
      zis.setDict(dict)
      val output= Array.fill[Byte](size)(0)
      val block = ByteBuffer.allocateDirect(128 * 1024)
      var offset = 0
      while (zis.hasRemaining) {
        block.clear()
        val read = zis.read(block)
        block.flip()
        block.get(output, offset, read)
        offset += read
      }
      zis.close
      assert(input.toSeq == output.toSeq)
    }

    it should s"round-trip streaming ByteBuffer compression/decompression with fast dict with legacy $legacy" in {
      val cdict = new ZstdDictCompress(dict, 0, dict.size, 1)
      val size  = input.length
      val os    = ByteBuffer.allocateDirect(Zstd.compressBound(size.toLong).toInt)

      // compress
      val ib    = ByteBuffer.allocateDirect(size)
      ib.put(input)
      ib.flip

      val osw = new ZstdDirectBufferCompressingStream(os, 1)
      osw.setDict(cdict)
      osw.compress(ib)
      osw.close
      os.flip

      assert(Zstd.getDictIdFromFrameBuffer(os) == Zstd.getDictIdFromDict(dict))

      // now decompress
      val zis   = new ZstdDirectBufferDecompressingStream(os)
      val ddict = new ZstdDictDecompress(dict)
      zis.setDict(ddict)
      val output= Array.fill[Byte](size)(0)
      val block = ByteBuffer.allocateDirect(128 * 1024)
      var offset = 0
      while (zis.hasRemaining) {
        block.clear()
        val read = zis.read(block)
        block.flip()
        block.get(output, offset, read)
        offset += read
      }
      zis.close
      assert(input.toSeq == output.toSeq)
    }
  }

  // offset/length pairs that point outside of a 16 bytes dictionary buffer
  val shortDict = new Array[Byte](16)
  val invalidDictRanges = List(
    (0, 17),             // one byte past the end
    (8, 9),              // offset + length past the end
    (0, 50000000),       // far past the end
    (-8, 8),             // negative offset, before the start
    (0, -1),             // negative length
    (17, 0),             // offset past the end
    (Int.MaxValue, 2)    // offset + length overflows
  )

  for ((offset, length) <- invalidDictRanges) {
    "ZstdDictCompress" should s"reject out of bounds dictionary offset $offset and length $length" in {
      intercept[IllegalArgumentException] {
        new ZstdDictCompress(shortDict, offset, length, 1)
      }
    }

    "ZstdDictDecompress" should s"reject out of bounds dictionary offset $offset and length $length" in {
      intercept[IllegalArgumentException] {
        new ZstdDictDecompress(shortDict, offset, length)
      }
    }
  }

  "ZstdDictCompress and ZstdDictDecompress" should "round-trip with a valid sub-range of a larger dictionary buffer" in {
    val dict = train(false, 512)
    val padded = new Array[Byte](dict.length + 13)
    System.arraycopy(dict, 0, padded, 7, dict.length)
    val cdict = new ZstdDictCompress(padded, 7, dict.length, 1)
    val compressed = Zstd.compress(input, cdict)
    cdict.close
    val ddict = new ZstdDictDecompress(padded, 7, dict.length)
    val decompressed = Zstd.decompress(compressed, ddict, input.length)
    ddict.close
    assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
    assert(input.toSeq == decompressed.toSeq)
  }

  "ZstdInputStreamNoFinalizer" should "refuse to configure a closed stream" in {
    val dict  = train(false, 512)
    val ddict = new ZstdDictDecompress(dict)
    val zis   = new ZstdInputStreamNoFinalizer(new ByteArrayInputStream(Array[Byte]()))
    zis.setDict(ddict)
    zis.close
    // the native stream has been freed, none of these may reach it
    val setters: List[(String, () => Any)] = List(
      "setDict(byte[])"             -> (() => zis.setDict(dict)),
      "setDict(ZstdDictDecompress)" -> (() => zis.setDict(ddict)),
      "setLongMax"                  -> (() => zis.setLongMax(20)),
      "setRefMultipleDDicts"        -> (() => zis.setRefMultipleDDicts(true))
    )
    for ((name, setter) <- setters) {
      val e = intercept[IOException] {
        setter()
      }
      assert(e.getMessage == "Stream closed", s"$name on a closed stream")
    }
    // closing the stream released the dict and the refused setDict did not acquire it again
    ddict.close
  }

  "ZstdOutputStreamNoFinalizer" should "refuse to configure a closed stream" in {
    val dict  = train(false, 512)
    val cdict = new ZstdDictCompress(dict, 1)
    val zos   = new ZstdOutputStreamNoFinalizer(new ByteArrayOutputStream(), 1)
    zos.setCloseFrameOnFlush(true)
    zos.setDict(cdict)
    zos.write(input, 0, 1024)
    // flushing closes the frame, so after close() only the closed-stream check
    // stands between the setters and the freed native stream
    zos.flush
    zos.close
    val setters: List[(String, () => Any)] = List(
      "setChecksum"               -> (() => zos.setChecksum(true)),
      "setLevel"                  -> (() => zos.setLevel(3)),
      "setLong"                   -> (() => zos.setLong(20)),
      "setWorkers"                -> (() => zos.setWorkers(0)),
      "setOverlapLog"             -> (() => zos.setOverlapLog(0)),
      "setJobSize"                -> (() => zos.setJobSize(0)),
      "setTargetLength"           -> (() => zos.setTargetLength(0)),
      "setMinMatch"               -> (() => zos.setMinMatch(4)),
      "setSearchLog"              -> (() => zos.setSearchLog(1)),
      "setChainLog"               -> (() => zos.setChainLog(10)),
      "setHashLog"                -> (() => zos.setHashLog(10)),
      "setWindowLog"              -> (() => zos.setWindowLog(20)),
      "setStrategy"               -> (() => zos.setStrategy(1)),
      "setDict(byte[])"           -> (() => zos.setDict(dict)),
      "setDict(ZstdDictCompress)" -> (() => zos.setDict(cdict))
    )
    for ((name, setter) <- setters) {
      val e = intercept[IOException] {
        setter()
      }
      assert(e.getMessage == "StreamClosed", s"$name on a closed stream")
    }
    // closing the stream released the dict and the refused setDict did not acquire it again
    cdict.close
  }

  def refuseToConfigureClosedBufferStream(zis: BaseZstdBufferDecompressingStreamNoFinalizer): Unit = {
    val dict  = train(false, 512)
    val ddict = new ZstdDictDecompress(dict)
    zis.setDict(ddict)
    zis.close
    // a closed stream has nothing left to read
    assert(!zis.hasRemaining)
    // the native stream has been freed, none of these may reach it
    val setters: List[(String, () => Any)] = List(
      "setDict(byte[])"             -> (() => zis.setDict(dict)),
      "setDict(ZstdDictDecompress)" -> (() => zis.setDict(ddict)),
      "setLongMax"                  -> (() => zis.setLongMax(20))
    )
    for ((name, setter) <- setters) {
      val e = intercept[IOException] {
        setter()
      }
      assert(e.getMessage == "Stream closed", s"$name on a closed stream")
    }
    // closing the stream released the dict and the refused setDict did not acquire it again
    ddict.close
  }

  "ZstdDirectBufferDecompressingStreamNoFinalizer" should "refuse to configure a closed stream" in {
    refuseToConfigureClosedBufferStream(new ZstdDirectBufferDecompressingStreamNoFinalizer(ByteBuffer.allocateDirect(0)))
  }

  "ZstdBufferDecompressingStreamNoFinalizer" should "refuse to configure a closed stream" in {
    refuseToConfigureClosedBufferStream(new ZstdBufferDecompressingStreamNoFinalizer(ByteBuffer.allocate(0)))
  }
}
