package com.how_hard_can_it_be.primes;

import java.util.BitSet;
import java.util.LinkedList;
import java.util.List;

/**
 * Sieve of Eratosthenes.
 * @author john
 *
 */
public class Eratosthenes implements PrimeFinder
{

    public int[] primesNotGreaterThan( int aCeiling)
    {
        List<Integer> primes = new LinkedList<Integer>();
        int n = aCeiling + 1;
        boolean isPrime[] = new boolean[n];
        // BitSet is a nice space-saver, but if you're trying to find primes below 10^9, you will
        // probably want to increase your max. heap size.  Note that a much better solution, since you're probably
        // going to go through the primes in sequence is to not return an array of prime integers but instead
        // set up a coroutine that spins through this bitset, handing out the next prime in sequence.  Trying to
        // return an array of 50 million or so primes still results in an out-of-memory exception.
//        BitSet isPrime = new BitSet(n);
        for (int i = 0; i < n; i++)
        {
        	isPrime[i] = true;
//            isPrime.set(i);
        }
        isPrime[0] = isPrime[1] = false;
//        isPrime.clear(0);
//        isPrime.clear(1);
        
        int rootCeiling = (int) Math.floor(Math.sqrt(aCeiling));
        
        for (int i = 2; i <= rootCeiling; i++)
        	if (isPrime[i])
//            if (isPrime.get(i))
            {
                // This little cross-out trick comes from http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes#Implementation
                // i^2 + k*i < n
                // k < (n - i^2)/i
                int iSquared = i*i;
                for (int k = 0; k <= (aCeiling - iSquared) / i; k++)
                {
                    int j = iSquared + k * i;
//                for (int j = 2 * i; j < n; j += i)
                    isPrime[j] = false;
//                    isPrime.clear(j); 
                }
            }
        for (int i = 2; i < n; i++)
        	if (isPrime[i])
//            if (isPrime.get(i))
                primes.add( i);
        
        int[] retval = new int[primes.size()];
        for (int i=0; i < primes.size(); i++)
            retval[i] = primes.get(i);
        return retval;
    }

}
