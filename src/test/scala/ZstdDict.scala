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

  def readFully(is: InputStream): Array[Byte] = {
    val os  = new ByteArrayOutputStream()
    val buf = new Array[Byte](16 * 1024)
    var n   = is.read(buf)
    while (n >= 0) {
      os.write(buf, 0, n)
      n = is.read(buf)
    }
    os.toByteArray
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

    it should s"keep a fast dict alive while it is used by a ZstdInputStream with legacy $legacy" in {
      val cdict = new ZstdDictCompress(dict, 0, dict.length, 1)
      val compressed = compressWithDict(cdict)
      cdict.close

      val ddict = new ZstdDictDecompress(dict)
      val zis   = new ZstdInputStreamNoFinalizer(new ByteArrayInputStream(compressed))
      zis.setDict(ddict)
      // the stream references the native DDict, so freeing it while the
      // stream is still open must be refused (use after free otherwise)
      intercept[IllegalStateException] {
        ddict.close
      }
      val output = readFully(zis)
      zis.close
      assert(input.toSeq == output.toSeq)
      // closing the stream releases the dict
      ddict.close
    }

    it should s"release the previous fast dict when a ZstdInputStream switches dicts with legacy $legacy" in {
      val cdict2 = new ZstdDictCompress(dict2, 0, dict2.length, 1)
      val compressed = compressWithDict(cdict2)
      cdict2.close

      val ddict  = new ZstdDictDecompress(dict)
      val ddict2 = new ZstdDictDecompress(dict2)
      val zis    = new ZstdInputStreamNoFinalizer(new ByteArrayInputStream(compressed))
      zis.setDict(ddict)
      zis.setDict(ddict2)
      // the first dict is no longer referenced by the stream
      ddict.close
      // the active one is still in use
      intercept[IllegalStateException] {
        ddict2.close
      }
      val output = readFully(zis)
      zis.close
      ddict2.close
      assert(input.toSeq == output.toSeq)
    }

    it should s"keep a fast dict alive while it is used by a ZstdOutputStream with legacy $legacy" in {
      val cdict = new ZstdDictCompress(dict, 0, dict.length, 1)
      val os    = new ByteArrayOutputStream()
      val zos   = new ZstdOutputStreamNoFinalizer(os, 1)
      zos.setDict(cdict)
      // the stream references the native CDict, so freeing it while the
      // stream is still open must be refused (use after free otherwise)
      intercept[IllegalStateException] {
        cdict.close
      }
      zos.write(input)
      zos.close
      // closing the stream releases the dict
      cdict.close
      val compressed = os.toByteArray
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))

      val ddict = new ZstdDictDecompress(dict)
      val decompressed = Zstd.decompress(compressed, ddict, input.length)
      ddict.close
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"release the previous fast dict when a ZstdOutputStream switches dicts with legacy $legacy" in {
      val cdict  = new ZstdDictCompress(dict, 0, dict.length, 1)
      val cdict2 = new ZstdDictCompress(dict2, 0, dict2.length, 1)
      val os     = new ByteArrayOutputStream()
      val zos    = new ZstdOutputStreamNoFinalizer(os, 1)
      zos.setDict(cdict)
      zos.setDict(cdict2)
      // the first dict is no longer referenced by the stream
      cdict.close
      // the active one is still in use
      intercept[IllegalStateException] {
        cdict2.close
      }
      zos.write(input)
      zos.close
      cdict2.close
      val compressed = os.toByteArray
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict2))

      val ddict2 = new ZstdDictDecompress(dict2)
      val decompressed = Zstd.decompress(compressed, ddict2, input.length)
      ddict2.close
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"keep a fast dict alive while it is loaded in a ZstdCompressCtx with legacy $legacy" in {
      val cdict = new ZstdDictCompress(dict, 0, dict.length, 1)
      val cctx  = new ZstdCompressCtx()
      cctx.loadDict(cdict)
      // the context references the native CDict, so freeing it while the
      // context is still open must be refused (use after free otherwise)
      intercept[IllegalStateException] {
        cdict.close
      }
      val compressed = cctx.compress(input)
      cctx.close
      // closing the context releases the dict
      cdict.close
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      val decompressed = Zstd.decompress(compressed, dict, input.length)
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"release the fast dict when a ZstdCompressCtx loads another dict or is reset with legacy $legacy" in {
      val cdict  = new ZstdDictCompress(dict, 0, dict.length, 1)
      val cdict2 = new ZstdDictCompress(dict2, 0, dict2.length, 1)
      val cdict3 = new ZstdDictCompress(dict2, 0, dict2.length, 1)
      val cctx   = new ZstdCompressCtx()
      try {
        cctx.loadDict(cdict)
        cctx.loadDict(cdict2)
        // the first dict is no longer referenced by the context
        cdict.close
        // the active one is still in use
        intercept[IllegalStateException] {
          cdict2.close
        }
        val compressed = cctx.compress(input)
        assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict2))
        // loading a byte[] dict drops the reference to the fast dict
        cctx.loadDict(dict2)
        cdict2.close
        val compressed2 = cctx.compress(input)
        assert(Zstd.getDictIdFromFrame(compressed2) == Zstd.getDictIdFromDict(dict2))
        // as does resetting the context
        cctx.loadDict(cdict3)
        intercept[IllegalStateException] {
          cdict3.close
        }
        cctx.reset()
        cdict3.close
        val compressed3 = cctx.compress(input)
        assert(Zstd.getDictIdFromFrame(compressed3) == 0)
      } finally {
        cctx.close
      }
    }

    it should s"keep the loaded fast dict when a ZstdCompressCtx fails to load another one with legacy $legacy" in {
      val cdict  = new ZstdDictCompress(dict, 0, dict.length, 1)
      val cdict2 = new ZstdDictCompress(dict2, 0, dict2.length, 1)
      val cctx   = new ZstdCompressCtx()
      cctx.loadDict(cdict)
      // start a frame: the dictionary can not be changed in the middle of it
      val src = ByteBuffer.allocateDirect(1024)
      src.put(input, 0, 1024)
      src.flip()
      val dst = ByteBuffer.allocateDirect(Zstd.compressBound(1024).toInt)
      cctx.compressDirectByteBufferStream(dst, src, EndDirective.CONTINUE)
      intercept[ZstdException] {
        cctx.loadDict(cdict2)
      }
      // the dict that failed to load is not retained by the context
      cdict2.close
      // while the previously loaded one is still referenced
      intercept[IllegalStateException] {
        cdict.close
      }
      cctx.close
      cdict.close
    }

    it should s"keep a fast dict alive while it is loaded in a ZstdDecompressCtx with legacy $legacy" in {
      val compressed = Zstd.compressUsingDict(input, dict, 1)
      val ddict = new ZstdDictDecompress(dict)
      val dctx  = new ZstdDecompressCtx()
      dctx.loadDict(ddict)
      // the context references the native DDict, so freeing it while the
      // context is still open must be refused (use after free otherwise)
      intercept[IllegalStateException] {
        ddict.close
      }
      val decompressed = dctx.decompress(compressed, input.length)
      dctx.close
      // closing the context releases the dict
      ddict.close
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"release the fast dict when a ZstdDecompressCtx loads another dict or is reset with legacy $legacy" in {
      val compressed2 = Zstd.compressUsingDict(input, dict2, 1)
      val ddict  = new ZstdDictDecompress(dict)
      val ddict2 = new ZstdDictDecompress(dict2)
      val ddict3 = new ZstdDictDecompress(dict2)
      val dctx   = new ZstdDecompressCtx()
      try {
        dctx.loadDict(ddict)
        dctx.loadDict(ddict2)
        // the first dict is no longer referenced by the context
        ddict.close
        // the active one is still in use
        intercept[IllegalStateException] {
          ddict2.close
        }
        assert(input.toSeq == dctx.decompress(compressed2, input.length).toSeq)
        // loading a byte[] dict drops the reference to the fast dict
        dctx.loadDict(dict2)
        ddict2.close
        assert(input.toSeq == dctx.decompress(compressed2, input.length).toSeq)
        // as does resetting the context
        dctx.loadDict(ddict3)
        intercept[IllegalStateException] {
          ddict3.close
        }
        dctx.reset()
        ddict3.close
      } finally {
        dctx.close
      }
    }

    it should s"keep a fast dict alive while it is used by a ZstdDirectBufferCompressingStream with legacy $legacy" in {
      val cdict = new ZstdDictCompress(dict, 0, dict.length, 1)
      val size  = input.length
      val os    = ByteBuffer.allocateDirect(Zstd.compressBound(size.toLong).toInt)
      val ib    = ByteBuffer.allocateDirect(size)
      ib.put(input)
      ib.flip

      val osw = new ZstdDirectBufferCompressingStreamNoFinalizer(os, 1)
      osw.setDict(cdict)
      // the stream is going to use the native CDict, so freeing it must be refused
      intercept[IllegalStateException] {
        cdict.close
      }
      osw.compress(ib)
      // the initialized native stream references the CDict until it is closed
      intercept[IllegalStateException] {
        cdict.close
      }
      osw.close
      // closing the stream releases the dict
      cdict.close
      os.flip

      assert(Zstd.getDictIdFromFrameBuffer(os) == Zstd.getDictIdFromDict(dict))
      val compressed = new Array[Byte](os.remaining)
      os.get(compressed)
      val decompressed = Zstd.decompress(compressed, dict, size)
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"release the previous fast dict when a ZstdDirectBufferCompressingStream switches dicts with legacy $legacy" in {
      val cdict  = new ZstdDictCompress(dict, 0, dict.length, 1)
      val cdict2 = new ZstdDictCompress(dict2, 0, dict2.length, 1)
      val size   = input.length
      val os     = ByteBuffer.allocateDirect(Zstd.compressBound(size.toLong).toInt)
      val ib     = ByteBuffer.allocateDirect(size)
      ib.put(input)
      ib.flip

      val osw = new ZstdDirectBufferCompressingStreamNoFinalizer(os, 1)
      osw.setDict(cdict)
      osw.setDict(cdict2)
      // the first dict is no longer referenced by the stream
      cdict.close
      // the active one is still in use
      intercept[IllegalStateException] {
        cdict2.close
      }
      // replacing it with a byte[] dict releases it as well
      osw.setDict(dict2)
      cdict2.close
      osw.compress(ib)
      osw.close
      os.flip

      assert(Zstd.getDictIdFromFrameBuffer(os) == Zstd.getDictIdFromDict(dict2))
    }

    def decompressDirectBufferStream(zis: ZstdDirectBufferDecompressingStreamNoFinalizer): Array[Byte] = {
      val output = Array.fill[Byte](input.length)(0)
      val block  = ByteBuffer.allocateDirect(128 * 1024)
      var offset = 0
      while (zis.hasRemaining) {
        block.clear()
        val read = zis.read(block)
        block.flip()
        block.get(output, offset, read)
        offset += read
      }
      output
    }

    it should s"keep a fast dict alive while it is used by a ZstdDirectBufferDecompressingStream with legacy $legacy" in {
      val compressed = Zstd.compressUsingDict(input, dict, 1)
      val src        = ByteBuffer.allocateDirect(compressed.length)
      src.put(compressed)
      src.flip

      val ddict = new ZstdDictDecompress(dict)
      val zis   = new ZstdDirectBufferDecompressingStreamNoFinalizer(src)
      zis.setDict(ddict)
      // the stream references the native DDict, so freeing it while the
      // stream is still open must be refused (use after free otherwise)
      intercept[IllegalStateException] {
        ddict.close
      }
      val output = decompressDirectBufferStream(zis)
      zis.close
      // closing the stream releases the dict
      ddict.close
      assert(input.toSeq == output.toSeq)
    }

    it should s"release the previous fast dict when a ZstdDirectBufferDecompressingStream switches dicts with legacy $legacy" in {
      val compressed = Zstd.compressUsingDict(input, dict2, 1)
      val src        = ByteBuffer.allocateDirect(compressed.length)
      src.put(compressed)
      src.flip

      val ddict  = new ZstdDictDecompress(dict)
      val ddict2 = new ZstdDictDecompress(dict2)
      val zis    = new ZstdDirectBufferDecompressingStreamNoFinalizer(src)
      zis.setDict(ddict)
      zis.setDict(ddict2)
      // the first dict is no longer referenced by the stream
      ddict.close
      // the active one is still in use
      intercept[IllegalStateException] {
        ddict2.close
      }
      val output = decompressDirectBufferStream(zis)
      zis.close
      ddict2.close
      assert(input.toSeq == output.toSeq)
    }

    it should s"keep all fast dicts alive while a ZstdInputStream references multiple dicts with legacy $legacy" in {
      val cdict  = new ZstdDictCompress(dict, 0, dict.length, 1)
      val cdict2 = new ZstdDictCompress(dict2, 0, dict2.length, 1)
      val compressed = compressWithDict(cdict) ++ compressWithDict(cdict2)
      cdict.close
      cdict2.close

      val ddict  = new ZstdDictDecompress(dict)
      val ddict2 = new ZstdDictDecompress(dict2)
      val zis    = new ZstdInputStreamNoFinalizer(new ByteArrayInputStream(compressed))
      zis.setRefMultipleDDicts(true)
      zis.setDict(ddict)
      zis.setDict(ddict2)
      // the native stream references both DDicts, freeing any of them
      // while the stream is still open must be refused
      intercept[IllegalStateException] {
        ddict.close
      }
      intercept[IllegalStateException] {
        ddict2.close
      }
      // disabling multiple dict references does not drop the dicts registered
      // in the native stream, they are used again once re-enabled
      zis.setRefMultipleDDicts(false)
      intercept[IllegalStateException] {
        ddict.close
      }
      zis.setRefMultipleDDicts(true)
      val output = readFully(zis)
      zis.close
      // closing the stream releases all the dicts
      ddict.close
      ddict2.close
      assert((input ++ input).toSeq == output.toSeq)
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
    // the stream references the native DDict until it is closed
    intercept[IllegalStateException] {
      ddict.close
    }
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
