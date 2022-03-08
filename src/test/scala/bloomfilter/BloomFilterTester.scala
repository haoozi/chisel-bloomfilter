// See README.md for license details.

package bloomfilter

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec


class BloomFilterTester extends AnyFlatSpec with ChiselScalatestTester {

    def bf_clear(dut: BloomFilter, array_size: Int) = {
        dut.io.in.ready.expect(true.B)

        dut.io.in.valid.poke(true.B)
        dut.io.in.bits.cmd.lookup.poke(false.B)
        dut.io.in.bits.cmd.insert.poke(false.B)
        dut.io.in.bits.cmd.clear.poke(true.B)
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
        dut.io.in.bits.cmd.lookup.poke(false.B)
        dut.io.in.bits.cmd.insert.poke(true.B)
        dut.io.in.bits.cmd.clear.poke(false.B)
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
        dut.io.in.bits.cmd.lookup.poke(true.B)
        dut.io.in.bits.cmd.insert.poke(false.B)
        dut.io.in.bits.cmd.clear.poke(false.B)
        dut.io.in.bits.data.poke(data.U)

        dut.clock.step()

        dut.io.in.valid.poke(false.B)

        dut.io.out.valid.expect(true.B)
        dut.io.out.bits.exists.expect(exists.B)
    }

    object BFAction extends Enumeration {
        type BFAction = Value
        val Lookup, Insert = Value
    }
    import BFAction._



    case class BloomFilterTestAction(act: BFAction, data: Int, exists: Boolean)

    def bf_pipeline(dut: BloomFilter, actions: Seq[BloomFilterTestAction]) = {
        
        (0 to actions.length).foreach {
            cycle => {
                if (cycle < actions.length) {
                    // push command
                    val action = actions(cycle)

                    dut.io.in.ready.expect(true.B)
                    dut.io.in.valid.poke(true.B)
                    dut.io.in.bits.cmd.lookup.poke((action.act == Lookup).B)
                    dut.io.in.bits.cmd.insert.poke((action.act == Insert).B)
                    dut.io.in.bits.cmd.clear.poke(false.B)
                    dut.io.in.bits.data.poke(action.data.U)
                } else {
                    dut.io.in.valid.poke(false.B)
                }

                if (cycle > 0) {
                    // check result
                    val action = actions(cycle - 1)

                    dut.io.out.valid.expect(true.B)
                    // don't care action.exists if it's an insertion
                    if (action.act == Lookup) {
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
            bf_clear(dut, 16)
        }
    }

    it should "correctly insert a new value and be able to read back" in {
        val hash_funcs = Seq(new HashFunc_Modulo(32, 4), new HashFunc_Modulo2(32, 4))
        val param = new BloomFilterParams(hash_funcs, 32, 16)
        test(new BloomFilter(param)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            bf_clear(dut, 16)
            // insert into bloom filter
            bf_insert(dut, 123)
            dut.clock.step()
            // check correctness of existing data
            bf_lookup(dut, 123, true)
            // check for non-existing data (should return false)
            bf_lookup(dut, 321, false)
        }
    }


    it should "correctly complete pipelined requests" in {
        val requests = Seq(
            BloomFilterTestAction(Insert, 111, false),
            BloomFilterTestAction(Insert, 222, false),
            BloomFilterTestAction(Lookup, 222, true),
            BloomFilterTestAction(Lookup, 333, false),
            BloomFilterTestAction(Insert, 333, false),
            BloomFilterTestAction(Lookup, 333, true),
        )
        val hash_funcs = Seq(new HashFunc_Modulo(32, 4), new HashFunc_Modulo2(32, 4))
        val param = new BloomFilterParams(hash_funcs, 32, 16)
        test(new BloomFilter(param)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            bf_clear(dut, 16)
            // insert into bloom filter
            bf_pipeline(dut, requests)
        }
    }

}
