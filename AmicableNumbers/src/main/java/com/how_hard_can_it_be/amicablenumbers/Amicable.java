package com.how_hard_can_it_be.amicablenumbers;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import com.how_hard_can_it_be.primes.Factor;
import com.how_hard_can_it_be.primes.PrimeUtils;
import com.how_hard_can_it_be.utils.Log;

public class Amicable
{
    public static List<Factor> allFactorsLessThan( long aNumber)
    {
        List<Factor> retval = new LinkedList<Factor>();
        
        List<Factor> primeFactorsList = PrimeUtils.factors( aNumber);
        
//        Log.note( String.format( "Found %d prime factors:", primeFactorsList.size()));
//        Factor.dump( System.err, primeFactorsList);
//        System.err.println();
        
        Factor[] primeFactor = primeFactorsList.toArray( new Factor[0]);
        Factor[] factorComponent = new Factor[ primeFactor.length];
        
        for (int i = 0; i < primeFactor.length; i++)
        {
            factorComponent[i] = new Factor( primeFactor[i].getFactor(), 0);
        }
        
        do
        {
            long factor = makeFactor( factorComponent);
            if (factor < aNumber)
            {
//                Log.note( String.format( "%d", factor));
                retval.add( new Factor( factor, 1));
            }
            increment( factorComponent, primeFactor);
        } while (! maxedOut( factorComponent, primeFactor));
//        System.out.println();
        
        return retval;
    }

    /**
     * Make a factor from the given factor components by multiplying them together.
     * @param aFactorComponent
     * @return
     */
    private static long makeFactor( Factor[] aFactorComponent)
    {
        long retval = 1;
        for (Factor factor : aFactorComponent)
        {
            retval *= Math.pow( factor.getFactor(), factor.getCount());
        }
        return retval;
    }

    /**
     * "Increment" the component factors in such a way that {@link #makeFactor(Factor[])} will return the "next" 
     * factor in the sequence of factors of the number whose prime factors are given by aPrimeFactor.
     * @param aFactorComponent
     * @param aPrimeFactor
     */
    private static void increment( Factor[] aFactorComponent, Factor[] aPrimeFactor)
    {
        for (int i = 0; i < aFactorComponent.length; i++)
        {
            Factor component = aFactorComponent[i];
            {
                component.setCount( component.getCount()+1);
                if (component.getCount() <= aPrimeFactor[i].getCount())
                    break; // No "carry"
                else
                    if (i < aFactorComponent.length - 1)
                        component.setCount( 0); // No break, but reset, because we'll move on to the next "digit".
            }
        }
    }

    /**
     * Return true iff the given list of factor components can no longer be {@link #increment(Factor[], Factor[])}'d to
     * a legal value.
     * @param aFactorComponent
     * @param aPrimeFactor
     * @return
     */
    private static boolean maxedOut( Factor[] aFactorComponent, Factor[] aPrimeFactor)
    {
        boolean retval = false;
        for (int i = 0; i < aFactorComponent.length; i++)
            if (aFactorComponent[i].getCount() > aPrimeFactor[i].getCount())
            {
                retval = true;
                break;
            }
        return retval;
    }
}
