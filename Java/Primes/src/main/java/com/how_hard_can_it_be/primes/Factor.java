package com.how_hard_can_it_be.primes;

import java.io.PrintStream;
import java.util.List;

/**
 * A factor of a number.
 * @author John Lusk
 *
 */
public class Factor
{
    private long factor;
    private int count;
    
    /**
     * 
     * @param aRemainingToFactor The factor
     * @param aCount The number of times the factor occurs in the number being factored.
     */
    public Factor( long aRemainingToFactor, int aCount)
    {
        factor = aRemainingToFactor;
        count = aCount;
    }
    
    public long getFactor()
    {
        return factor;
    }
    
    public int getCount()
    {
        return count;
    }
    
    public void setCount( int aCount)
    {
        count = aCount;
    }

    public static void dump(PrintStream aStream, List<Factor> aListOfFactors)
    {
        for (Factor factor : aListOfFactors)
        {
            if (factor.getCount() == 1)
                aStream.printf( "%8d%8s", factor.getFactor(), "");
            else
                aStream.printf( "%8d ^ %-5d", factor.getFactor(), factor.getCount());
        }
        
    }
    
    
    
}
