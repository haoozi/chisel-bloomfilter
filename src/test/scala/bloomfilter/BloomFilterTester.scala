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
    
    behavior of "BloomFilter"
    it should "correctly clear internal mem" in {
        val hash_funcs = Seq(new HashFunc_Modulo(32, 4), new HashFunc_Modulo2(32, 4))
        test(new BloomFilter(hash_funcs, 32, 16)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            bf_clear(dut, 16)
        }
    }

    it should "correctly insert a new value and be able to read back" in {
        val hash_funcs = Seq(new HashFunc_Modulo(32, 4), new HashFunc_Modulo2(32, 4))
        test(new BloomFilter(hash_funcs, 32, 16)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
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

}
