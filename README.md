# Bloom Filter

Bloom filter in chisel

## How to run
```sbt test```

## Status
### Completed
Test clear memory

Test insert data and readback

Test pipelined lookup/insert

Test Chisel implementation against Scala model

### Hash Functions

Following hash functions are implemented:

Modulo (hash = input % hash_width)

Murmur3 [Wikipedia](https://en.wikipedia.org/wiki/MurmurHash)

FNV [Wikipedia](https://en.wikipedia.org/wiki/Fowler–Noll–Vo_hash_function)



