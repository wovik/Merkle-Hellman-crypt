# merkle-hellman-crypto

  

Implementation of Merkle–Hellman knapsack cryptosystem in Haskell.

Program needs `stack` to run.

  

Example usage:

- `stack run -- gen 10 test` - will generate public and private keys files `test.pub` and `test.priv`

- `stack run -- enc test.pub 1000100001` - will encrypt message 1000100001 with key `test.pub`

- `stack run -- dec test.priv 4971052` - will decrypt cryptogram `4971052` with key `test.priv`