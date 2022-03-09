# Bloom Filter

This repo contains a BloomFilter chisel module, a scala-implemented reference model, several hash functions and test code. 

This implementation is a 2-staged bloom filter, which means lookup result will be given in next cycle (limited by SyncReadMem).

## How to run
```sbt test```

## Status
### Completed
1. Test clear memory

2. Test insert data and readback

3. Test pipelined lookup/insert

4. Test Chisel implementation against Scala model

### Hash Functions

Following hash functions are implemented:

1. Modulo (hash = input % hash_width)

2. Murmur3 [Wikipedia](https://en.wikipedia.org/wiki/MurmurHash)

3. FNV [Wikipedia](https://en.wikipedia.org/wiki/Fowler–Noll–Vo_hash_function)



### Future Enhancement

1. Currently the ```BloomFilter``` chisel module will instantiates ```n_hash_funcs + 1``` number of memory write ports (extra 1 port is for clearing chisel SyncReadMem). However even in worst case only ```n_hash_funcs``` write ports will be used at same time (write and clear won't happen at same time). The extra port can be merged.

2. Futher more, clearing speed can be increased by using existing hash function write ports.