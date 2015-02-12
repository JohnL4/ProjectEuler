package com.how_hard_can_it_be.primes;

import java.util.LinkedList;
import java.util.List;

import com.how_hard_can_it_be.utils.Log;

/**
 * Utilities useful in dealing with prime numbers.
 * @author John Lusk
 *
 */
public class PrimeUtils
{
    static int[] primes;

    /**
     * Returns list of prime factors of aNumberToFactor.  Returned list will not include aNumberToFactor if it 
     * happens to be prime.
     * @param aNumberToFactor
     * @return
     */
    static public List<Factor> factors(long aNumberToFactor)
    {
        List<Factor> factors = new LinkedList<Factor>();
        int rootNumber = (int) Math.floor( Math.sqrt( aNumberToFactor));
        Log.note( String.format( "Root of %d is %d", aNumberToFactor, rootNumber));
        if (primes == null || primes.length == 0 || primes[primes.length-1] < rootNumber)
        {
            if (primes == null || primes.length == 0)
                Log.note( "\t\tInitialize primes");
            else
                Log.note(String.format(
                        "\t\tCalculating new primes because primes[%d] = %d, which is less than %d (sqrt(%d))",
                        primes.length - 1, primes[primes.length - 1], rootNumber, aNumberToFactor));
            PrimeFinder primeFinder = new Eratosthenes();
            primes = primeFinder.primesNotGreaterThan( rootNumber * 10);
            Log.note( String.format( "\t\tHighest prime found = %d", primes[primes.length - 1]));
        }
        long remainingToFactor = aNumberToFactor;
        for (int prime : primes)
        {
            if (prime < aNumberToFactor)
            {
                // The number of times the current prime factor occurs.
                int factorCount = 0;
                while (remainingToFactor % prime == 0)
                {
                    factorCount++;
                    remainingToFactor /= prime;
                }
                if (factorCount >= 1)
                    factors.add(new Factor(prime, factorCount));
            }
        }
        if (remainingToFactor == 1){}
        else
            factors.add( new Factor( remainingToFactor, 1));
        return factors;
    }

}
