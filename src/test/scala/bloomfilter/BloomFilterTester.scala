// See README.md for license details.

package bloomfilter

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random


class BloomFilterTester extends AnyFlatSpec with ChiselScalatestTester {

    import BloomFilterCmd._

    def bf_clear(dut: BloomFilter, array_size: Int) = {
        dut.io.in.ready.expect(true.B)

        dut.io.in.valid.poke(true.B)
        dut.io.in.bits.cmd.poke(Cmd_Clear)
        dut.io.in.bits.data.poke(0.U)
        dut.io.out.valid.expect(false.B)

        (0 until array_size) foreach {
            i => {
                dut.clock.step()
                dut.io.in.valid.poke(false.B)
                dut.io.in.ready.expect(false.B)
            }
        }

        dut.clock.step()
        dut.io.in.ready.expect(true.B)
        dut.io.out.valid.expect(true.B)

        dut.clock.step()
        dut.io.out.valid.expect(false.B)
    }

    def bf_insert(dut: BloomFilter, data: Int) = {
        dut.io.in.ready.expect(true.B)

        dut.io.in.valid.poke(true.B)
        dut.io.in.bits.cmd.poke(Cmd_Insert)
        dut.io.in.bits.data.poke(data.U)

        dut.clock.step()

        dut.io.in.valid.poke(false.B)
        dut.io.out.valid.expect(true.B)

        dut.clock.step()
        dut.io.out.valid.expect(false.B)
    }

    def bf_lookup(dut: BloomFilter, data: Int, exists: Boolean) = {
        dut.io.in.ready.expect(true.B)

        dut.io.in.valid.poke(true.B)
        dut.io.in.bits.cmd.poke(Cmd_Lookup)
        dut.io.in.bits.data.poke(data.U)

        dut.clock.step()

        dut.io.in.valid.poke(false.B)

        dut.io.out.valid.expect(true.B)
        dut.io.out.bits.exists.expect(exists.B)
    }



    case class BloomFilterTestAction(act: BloomFilterCmd.Type, data: Int, exists: Boolean)

    def bf_pipeline(dut: BloomFilter, actions: Seq[BloomFilterTestAction]) = {
        
        (0 to actions.length).foreach {
            cycle => {
                if (cycle < actions.length) {
                    // push command
                    val action = actions(cycle)

                    dut.io.in.ready.expect(true.B)
                    dut.io.in.valid.poke(true.B)
                    dut.io.in.bits.cmd.poke(action.act)
                    dut.io.in.bits.data.poke(action.data.U)
                } else {
                    dut.io.in.valid.poke(false.B)
                }

                if (cycle > 0) {
                    // check result
                    val action = actions(cycle - 1)

                    dut.io.out.valid.expect(true.B)
                    // don't care action.exists if it's an insertion
                    if (action.act == Cmd_Lookup) {
                        dut.io.out.bits.exists.expect(action.exists.B)
                    }
                } else {
                    dut.io.out.valid.expect(false.B)
                }

                dut.clock.step()
            }
        }
    }
    
    behavior of "BloomFilter"
    it should "correctly clear internal mem" in {
        val hash_funcs = Seq(new HashFunc_Modulo(32, 4), new HashFunc_Modulo2(32, 4))
        val param = new BloomFilterParams(hash_funcs, 32, 16)
        test(new BloomFilter(param)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            bf_clear(dut, param.array_size)
        }
    }

    it should "correctly insert a new value and be able to read back" in {
        val hash_funcs = Seq(new HashFunc_Modulo(32, 4), new HashFunc_Modulo2(32, 4))
        val param = new BloomFilterParams(hash_funcs, 32, 16)
        test(new BloomFilter(param)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            bf_clear(dut, param.array_size)
            // insert into bloom filter
            bf_insert(dut, 123)
            dut.clock.step()
            // check correctness of existing data
            bf_lookup(dut, 123, true)
            // check for non-existing data (should return false)
            bf_lookup(dut, 321, false)
        }
    }

    it should "has same behavior as scala model" in {
        val hash_funcs = Seq(new HashFunc_Modulo(32, 4), new HashFunc_Modulo2(32, 4))
        val param = new BloomFilterParams(hash_funcs, 32, 16)
        val scalaModel = new BloomFilterModel(param)

        test(new BloomFilter(param)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            bf_clear(dut, param.array_size)
            scalaModel.clear()

            (0 until 8).foreach { i => {
                val data = Random.nextInt(500)
                val isInsert = Random.nextInt(100) < 50

                if (isInsert) {
                    bf_insert(dut, data)
                    scalaModel.insert(data)
                }

                bf_lookup(dut, data, scalaModel.lookup(data)) 
            }}

        }
    }


    it should "correctly complete pipelined requests" in {
        val requests = Seq(
            BloomFilterTestAction(Cmd_Insert, 111, false),
            BloomFilterTestAction(Cmd_Insert, 222, false),
            BloomFilterTestAction(Cmd_Lookup, 222, true),
            BloomFilterTestAction(Cmd_Lookup, 333, false),
            BloomFilterTestAction(Cmd_Insert, 333, false),
            BloomFilterTestAction(Cmd_Lookup, 333, true),
        )
        val hash_funcs = Seq(new HashFunc_Modulo(32, 4), new HashFunc_Modulo2(32, 4))
        val param = new BloomFilterParams(hash_funcs, 32, 16)
        test(new BloomFilter(param)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            bf_clear(dut, param.array_size)
            // insert into bloom filter
            bf_pipeline(dut, requests)
        }
    }


    it should "correctly run murmur3 hash algorithm" in {
        val hash_funcs = Seq(new HashFunc_Modulo(32 ,5), new HashFunc_MurMur3(32, 5, 0x1234))
        val param = new BloomFilterParams(hash_funcs, 32, 32)
        val scalaModel = new BloomFilterModel(param)

        test(new BloomFilter(param)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            bf_clear(dut, param.array_size)
            scalaModel.clear()

            (0 until 8).foreach { i => {
                val data = Random.nextInt(500)
                val isInsert = Random.nextInt(100) < 50

                if (isInsert) {
                    bf_insert(dut, data)
                    scalaModel.insert(data)
                }

                bf_lookup(dut, data, scalaModel.lookup(data)) 
            }}

        }
    }


    it should "correctly run murmur3, modulo and fnv hash algorithm" in {
        val hash_funcs = Seq(
            new HashFunc_Modulo(32, 5), 
            new HashFunc_MurMur3(32, 5, 0x1234),
            new HashFunc_FNV1A(32, 5)
        )
        val param = new BloomFilterParams(hash_funcs, 32, 32)
        val scalaModel = new BloomFilterModel(param)

        test(new BloomFilter(param)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            bf_clear(dut, param.array_size)
            scalaModel.clear()

            (0 until 8).foreach { i => {
                val data = Random.nextInt(500)
                val isInsert = Random.nextInt(100) < 50

                if (isInsert) {
                    bf_insert(dut, data)
                    scalaModel.insert(data)
                }

                bf_lookup(dut, data, scalaModel.lookup(data)) 
            }}

        }
    }

}
